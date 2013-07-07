unit uconcurrency;

{$mode objfpc}{$H+}

interface

uses Classes, SysUtils, uqueue,
  {$IFDEF WINDOWS}
  windows;
  {$ELSE}
  unix,pthreads;
  {$ENDIF}

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

type TEvent = object
  private
    {$IFDEF WINDOWS}
    FEvent : HANDLE;
    {$ELSE}
    FSet   : Boolean;
    FMutex : TMutex;
    FCond  : TCondition;
    {$ENDIF}
  public
    procedure SSet;
    procedure Reset;
    procedure Wait;
    constructor Init;
    destructor Done;
  end;

type generic TBoundedQueue<Element> = class
  private
    FBuffer           : array of Element;
    FReadPosition     : Cardinal;
    FWritePosition    : Cardinal;
    FMutex            : TMutex;
    FReadCondition    : TCondition;
    FWrittenCondition : TCondition;
    FAbortThread      : TThread;    { Not owner }
  public
    constructor Create(BufferSize : Cardinal);
    destructor Destroy;override;
    procedure Abort(Thread : TThread);
    function Send(Data : Element; Thread : TThread):Boolean;
    function Receive(out Data : Element; Thread : TThread):Boolean;
  end;

type generic TUnboundedQueue<Element> = class
  private
    type TQ=specialize TQueue<Element>;
    var
    FQueue            : TQ;
    FMutex            : TMutex;
    FWrittenCondition : TCondition;
    FAbortThread      : TThread;  { Not owner}
  public
    constructor Create;
    destructor Destroy;override;
    procedure Abort(Thread : TThread);
    procedure Send(Data : Element);
    function Receive(out Data : Element; Thread: TThread):Boolean;
  end;

implementation

constructor TUnboundedQueue.Create;
begin
  inherited Create;
  FQueue:=TQ.Create;
  FMutex.Init;
  FWrittenCondition.Init
end;

destructor TUnboundedQueue.Destroy;
begin
  FMutex.Done;
  FWrittenCondition.Done;
  FQueue.Free;
end;

procedure TUnboundedQueue.Abort(Thread : TThread);
begin
  FAbortThread := Thread;
  FMutex.Acquire;
  FWrittenCondition.WakeAll;
  FMutex.Release;  
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
  FMutex.Acquire;
  if(FAbortThread = Thread) then
  begin
     FAbortThread := Nil;
     FMutex.Release;
     Exit(False);
  end;

  while(FQueue.Empty) do
  begin
    FWrittenCondition.Wait(@FMutex);
    if(FAbortThread = Thread) then
    begin
      FAbortThread := Nil;
      FMutex.Release;
      Exit(False);
    end;
  end;

  Data := FQueue.Pop;
  FMutex.Release;
  Exit(True); 
end;

procedure TBoundedQueue.Abort(Thread : TThread);
begin
  FAbortThread := Thread;
  FMutex.Acquire;
  FReadCondition.WakeAll;
  FWrittenCondition.WakeAll;
  FMutex.Release;
end;

function TBoundedQueue.Send(Data : Element; Thread : TThread):Boolean;
begin
  FMutex.Acquire;
  if(FAbortThread = Thread) then
  begin
    FAbortThread := Nil;
    FMutex.Release;
    Exit(False);
  end;

  while((FWritePosition+1) mod Length(FBuffer) = FReadPosition) do
  begin
    FReadCondition.Wait(@FMutex);
    if(FAbortThread = Thread) then
    begin
      FAbortThread:=Nil;
      FMutex.Release;
      Exit(False);
    end;
  end;

  FBuffer[FWritePosition] := Data;
  FWritePosition := (FWritePosition+1) mod Length(FBuffer);
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
     FMutex.Release;
     Exit(False);
  end;

  while(FReadPosition = FWritePosition) do
  begin
    FWrittenCondition.Wait(@FMutex);
    if(FAbortThread = Thread) then
    begin
      FAbortThread := Nil;
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
  Writeln('Resize Buffer');
  SetLength(FBuffer,BufferSize);
  Writeln('Create Mutex');
  FMutex.Init;
  Writeln('Create Condition');
  FWrittenCondition.Init;
  Writeln('Create Condition2');
  FReadCondition.Init;
end;

destructor TBoundedQueue.Destroy;
begin
  FReadCondition.Done;
  FWrittenCondition.Done;
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

procedure TEvent.Wait;
begin
  {$IFDEF WINDOWS}
  WaitForSingleObject(FEvent, INFINITE);
  {$ELSE}
  if FSet then
  begin
    Exit;
  end;
  FMutex.Acquire;
  if FSet then
  begin
    FMutex.Release;
    Exit;
  end;
  FCond.Wait(@FMutex);
  if not FSet then
  begin
    Writeln('Not set?');
  end;
  FMutex.Release;
  {$ENDIF}
end;

procedure TEvent.Reset;
begin
  {$IFDEF WINDOWS}
  ResetEvent(FEvent);
  {$ELSE}
  FSet:=False;
  {$ENDIF}
end;

procedure TEvent.SSet;
begin
  {$IFDEF WINDOWS}
  SetEvent(FEvent);
  {$ELSE}
  FSet := True;
  FMutex.Acquire;
  FCond.WakeAll;
  FMutex.Release;
  {$ENDIF}
end;

constructor TEvent.Init;
begin
  {$IFDEF WINDOWS}
  FEvent:=CreateEvent(Nil,True,False,'');
  ResetEvent(FEvent);
  {$ELSE}
  FMutex.Init;
  FCond.Init;
  {$ENDIF}
end;

destructor TEvent.Done;
begin
  {$IFDEF WINDOWS}
  CloseHandle(FEvent);
  FEvent:=0;
  {$ELSE}
  FMutex.Done;
  FCond.Done;
  {$ENDIF}
end;

end.