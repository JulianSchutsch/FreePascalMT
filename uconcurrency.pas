//-----------------------------------------------------------------------------
//   Copyright 2013 Julian Schutsch
//
//   This file is part of FreePascalMT
//
//   ParallelSim is free software: you can redistribute it and/or modify
//   it under the terms of the GNU Affero General Public License as published
//   by the Free Software Foundation, either version 3 of the License, or
//   (at your option) any later version.
//
//   FreePascalMT is distributed in the hope that it will be useful,
//   but WITHOUT ANY WARRANTY; without even the implied warranty of
//   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//   GNU Affero General Public License for more details.
//
//   You should have received a copy of the GNU Affero General Public License
//   along with FreePascalMT.  If not, see <http://www.gnu.org/licenses/>.
//-----------------------------------------------------------------------------
unit uconcurrency;
{$mode objfpc}{$H+}
interface

uses Classes, SysUtils, uqueue,

  {$IFDEF WINDOWS}
  windows;
  {$ELSE}
  unix,pthreads;
  {$ENDIF}

// Mutex reimplementation (see critical section in system(rtl))
// Required for combination with conditional variables
type PMutex = ^TMutex;
     TMutex = object
  private
    {$IFDEF WINDOWS}
    FCriticalSection : CRITICAL_SECTION;
    {$ELSE}
    FMutex : pthread_mutex_t;
    {$ENDIF}
  public
    constructor Init;
    destructor Done;
    procedure Acquire;
    procedure Release;
  end;

// Header section for conditional variables on windows
{$IFDEF WINDOWS}
{ Only windows Vista and later supports this! }
type PCONDITION_VARIABLE=^TCONDITION_VARIABLE;
     TCONDITION_VARIABLE=record
       x:Pointer;
     end;

procedure InitializeConditionVariable(ConditionVariable: PCONDITION_VARIABLE) external 'kernel32' name 'InitializeConditionVariable';
procedure SleepConditionVariableCS(ConditionVariable: PCONDITION_VARIABLE; CriticalSection : LPCRITICAL_SECTION; dwMilliSeconds :DWORD) external 'kernel32' name 'SleepConditionVariableCS';
procedure WakeConditionVariable(ConditionVariable: PCONDITION_VARIABLE) external 'kernel32' name 'WakeConditionVariable';
procedure WakeAllConditionVariable(ConditionVariable: PCONDITION_VARIABLE) external 'kernel32' name 'WakeAllConditionVariable';
{$ENDIF}

type TCondition = object
  private
    {$IFDEF WINDOWS}
    FConditionVariable : TCONDITION_VARIABLE;
    {$ELSE}
    FConditionVariable : pthread_cond_t;
    {$ENDIF}
  public
    constructor Init;
    destructor Done;
    procedure Wake;
    procedure WakeAll;
    procedure Wait(Mutex: PMutex);
  end;

// Ringbuffer based Bounded Queue.
// Blocking send, Blocking receive
type generic TBoundedQueue<Element> = class
  private
    FBuffer           : array of Element;
    FReadPosition     : Cardinal;
    FWritePosition    : Cardinal;
    FMutex            : TMutex;
    FReadCondition    : TCondition;
    FWrittenCondition : TCondition;
	FAbortMutex       : TMutex;
	FAbortCondition   : TCondition;
    FAbortThread      : TThread;    { Not owner }
  public
    constructor Create(BufferSize : Cardinal);
    destructor Destroy;override;
	// Wake up a specific waiting thread (either sending or receiving)
    procedure Abort(Thread : TThread);
	// Send Data, returns true on success and false on abort
    function Send(Data : Element; Thread : TThread):Boolean;
	// Receive data, returns true on success and false on abort
    function Receive(out Data : Element; Thread : TThread):Boolean;
  end;

// Queue based unbounded queue.
// Non blocking send, blocking receive.
type generic TUnboundedQueue<Element> = class
  private
    type TQ=specialize TQueue<Element>;
    var
    FQueue            : TQ;
    FMutex            : TMutex;
    FWrittenCondition : TCondition;
	FAbortCondition   : TCondition;
	FAbortMutex       : TMutex;
    FAbortThread      : TThread;  { Not owner}
  public
    constructor Create;
    destructor Destroy;override;
	// Wake up a specific waiting thread (receiving)
    procedure Abort(Thread : TThread);
	// Send data
    procedure Send(Data : Element);
	// Receive data, returns true on success and false on abort
    function Receive(out Data : Element; Thread: TThread):Boolean;
  end;

implementation

constructor TUnboundedQueue.Create;
begin
  inherited Create;
  FQueue:=TQ.Create;
  FMutex.Init;
  FWrittenCondition.Init;
  FAbortMutex.Init;
  FAbortCondition.Init;
end;

destructor TUnboundedQueue.Destroy;
begin
  FAbortMutex.Done;
  FAbortCondition.Done;
  FMutex.Done;
  FWrittenCondition.Done;
  FQueue.Free;
end;

procedure TUnboundedQueue.Abort(Thread : TThread);
begin
  FAbortMutex.Acquire;
  FAbortThread := Thread;
  FMutex.Acquire;
  FWrittenCondition.WakeAll;
  FMutex.Release;
  FAbortMutex.Release;
end;

procedure TUnboundedQueue.Send(Data : Element);
begin
  FMutex.Acquire;
  FQueue.Push(Data);
  FWrittenCondition.Wake;
  FMutex.Release;
end;

function TUnboundedQueue.Receive(out Data : Element; Thread: TThread):Boolean;
begin
  // The entire procedure is covered by a lock
  // and only released for waiting or leaving
  // Abort can only be set with released lock!
  FMutex.Acquire;
  // Test if the thread has been aborted
  if(FAbortThread = Thread) then
  begin
     FAbortThread := Nil;
	 FAbortCondition.Wake;
     FMutex.Release;
     Exit(False);
  end;
  // Check if something is in the queue
  while(FQueue.Empty) do
  begin
    FWrittenCondition.Wait(@FMutex);
	// Abort can be set here, test
    if(FAbortThread = Thread) then
    begin
      FAbortThread := Nil;
	  FAbortCondition.Wake;
      FMutex.Release;
      Exit(False);
    end;
  end;
  // Get element from queue
  Data := FQueue.Pop;
  FMutex.Release;
  // Successfull receive
  Exit(True); 
end;

procedure TBoundedQueue.Abort(Thread : TThread);
begin
  // Only one abort at the same time please
  FAbortMutex.Acquire;
  // Set current thread to abort
  FAbortThread := Thread;
  // Make sure, abort is only possible at certain points within or outside a receive/send
  FMutex.Acquire;
  // Wake all waiting threads
  FReadCondition.WakeAll;
  FWrittenCondition.WakeAll;
  // Wait for abort to be accepted
  FAbortCondition.Wait(@FMutex);
  // Get out of the locks
  FMutex.Release;
  FAbortMutex.Release;
end;

function TBoundedQueue.Send(Data : Element; Thread : TThread):Boolean;
begin
  // The entire procedure is covered by a lock
  // and only released for waiting or leaving
  // Abort can only be set with released lock!
  FMutex.Acquire;
  // Test for abort
  if(FAbortThread = Thread) then
  begin
    FAbortThread := Nil;
	FAbortCondition.Wake;
    FMutex.Release;
    Exit(False);
  end;

  // Any space left in the ring buffer?
  while((FWritePosition+1) mod Length(FBuffer) = FReadPosition) do
  begin
    FReadCondition.Wait(@FMutex);
    if(FAbortThread = Thread) then
    begin
      FAbortThread:=Nil;
	  FAbortCondition.Wake;
      FMutex.Release;
      Exit(False);
    end;
  end;

  // Write to ring buffer and update write position
  FBuffer[FWritePosition] := Data;
  FWritePosition := (FWritePosition+1) mod Length(FBuffer);
  // Signal that something has been written
  FWrittenCondition.Wake;
  FMutex.Release;
  Exit(True);
end;

function TBoundedQueue.Receive(out Data : Element; Thread : TThread):Boolean;
begin
  FMutex.Acquire;
  if(FAbortThread = Thread) then
  begin
     FAbortThread := Nil;
	 FAbortCondition.Wake;
     FMutex.Release;
     Exit(False);
  end;

  while(FReadPosition = FWritePosition) do
  begin
    FWrittenCondition.Wait(@FMutex);
    if(FAbortThread = Thread) then
    begin
      FAbortThread := Nil;
	  FAbortCondition.Wake;
      FMutex.Release;
      Exit(False);
    end;
  end;

  Data := FBuffer[FReadPosition];
  FReadPosition := (FReadPosition+1) mod Length(FBuffer);
  FReadCondition.Wake;
  FMutex.Release;
  Exit(True);

end;

constructor TBoundedQueue.Create(BufferSize : Cardinal);
begin
  SetLength(FBuffer,BufferSize);
  FMutex.Init;
  FWrittenCondition.Init;
  FReadCondition.Init;
  FAbortMutex.Init;
  FAbortCondition.Init;
end;

destructor TBoundedQueue.Destroy;
begin
  FReadCondition.Done;
  FWrittenCondition.Done;
  FAbortCondition.Done;
  FAbortMutex.Done;
  FMutex.Done;
end;

procedure TCondition.Wait(Mutex: PMutex);
begin
  {$IFDEF WINDOWS}
  SleepConditionVariableCS(@FConditionVariable,@Mutex^.FCriticalSection,INFINITE);
  {$ELSE}
  pthread_cond_wait(@FConditionVariable, @Mutex^.FMutex);
  {$ENDIF}
end;

procedure TCondition.Wake;
begin
  {$IFDEF WINDOWS}
  WakeConditionVariable(@FConditionVariable);
  {$ELSE}
  pthread_cond_signal(@FConditionVariable);
  {$ENDIF}
end;

procedure TCondition.WakeAll;
begin
  {$IFDEF WINDOWS}
  WakeAllConditionVariable(@FConditionVariable);
  {$ELSE}
  pthread_cond_broadcast(@FConditionVariable);
  {$ENDIF}
end;

constructor TCondition.Init;
begin
  {$IFDEF WINDOWS}
  InitializeConditionVariable(@FConditionVariable);
  {$ELSE}
  pthread_cond_init(@FConditionVariable,Nil);
  {$ENDIF}
end;

destructor TCondition.Done;
begin
  {$IFDEF WINDOWS}
  {$ELSE}
  pthread_cond_destroy(@FConditionVariable);
  {$ENDIF}
end;

procedure TMutex.Acquire;
begin
  {$IFDEF WINDOWS}
  EnterCriticalSection(@FCriticalSection);
  {$ELSE}
  pthread_mutex_lock(@FMutex);
  {$ENDIF}
end;

procedure TMutex.Release;
begin
  {$IFDEF WINDOWS}
  LeaveCriticalSection(@FCriticalSection);
  {$ELSE}
  pthread_mutex_unlock(@FMutex);
  {$ENDIF}
end;

constructor TMutex.Init;
begin
  {$IFDEF WINDOWS}
  InitializeCriticalSection(@FCriticalSection);
  {$ELSE}
  pthread_mutex_init(@FMutex,Nil);
  {$ENDIF}
end;

destructor TMutex.Done;
begin
  {$IFDEF WINDOWS}
  DeleteCriticalSection(@FCriticalSection);
  {$ELSE}
  pthread_mutex_destroy(@FMutex);
  {$ENDIF}
end;

end.