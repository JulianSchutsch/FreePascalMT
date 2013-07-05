unit uconcurrency;

{$mode objfpc}{$H+}

interface

uses Classes, SysUtils,
  {$IFDEF WINDOWS}
  windows;
  {$ELSE}
  posix;
  {$ENDIF}

type PMutex = ^TMutex;
     TMutex = object
  private
    {$IFDEF WINDOWS}
    FCriticalSection : CRITICAL_SECTION;
    {$ENDIF}
  public
    constructor Init;
    destructor Done;
    procedure Aquire;
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
    {$ENDIF}
  public
    constructor Init;
    destructor Done;
    procedure Wake;
    procedure WakeAll;
    procedure Sleep(Mutex: PMutex);
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

type TEvent = object
  private
    {$IFDEF WINDOWS}
    FEvent : HANDLE;
    {$ENDIF}
  public
    procedure SSet;
    procedure Reset;
    procedure Wait;
    constructor Init;
    destructor Done;
  end;

implementation

procedure TEvent.Wait;
begin
  WaitForSingleObject(FEvent, INFINITE);
end;

procedure TEvent.Reset;
begin
  ResetEvent(FEvent);
end;

procedure TEvent.SSet;
begin
  SetEvent(FEvent);
end;

constructor TEvent.Init;
begin
  FEvent:=CreateEvent(Nil,True,False,'TEVENT_EVENT'); 
  ResetEvent(FEvent);
end;

destructor TEvent.Done;
begin
  CloseHandle(FEvent);
end;

procedure TBoundedQueue.Abort(Thread : TThread);
begin
  FAbortThread := Thread;
  FMutex.Aquire;
  FReadCondition.WakeAll;
  FWrittenCondition.WakeAll;
  FMutex.Release;
end;

function TBoundedQueue.Send(Data : Element; Thread : TThread):Boolean;
begin
  FMutex.Aquire;
  if(FAbortThread = Thread) then
  begin
    FAbortThread := Nil;
    FMutex.Release;
    Exit(False);
  end;

  while((FWritePosition+1) mod Length(FBuffer) = FReadPosition) do
  begin
    FReadCondition.Sleep(@FMutex);
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
  FMutex.Aquire;
  if(FAbortThread = Thread) then
  begin
     FAbortThread := Nil;
     FMutex.Release;
     Exit(False);
  end;

  while(FReadPosition = FWritePosition) do
  begin
    FWrittenCondition.Sleep(@FMutex);
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

procedure TCondition.Sleep(Mutex: PMutex);
begin
  SleepConditionVariableCS(@FConditionVariable,@Mutex^.FCriticalSection,INFINITE);
end;

procedure TCondition.Wake;
begin
  WakeConditionVariable(@FConditionVariable);
end;

procedure TCondition.WakeAll;
begin
  WakeAllConditionVariable(@FConditionVariable);
end;

constructor TCondition.Init;
begin
  {$IFDEF WINDOWS}
  InitializeConditionVariable(@FConditionVariable);
  {$ENDIF}
end;

destructor TCondition.Done;
begin
end;

procedure TMutex.Aquire;
begin
  {$IFDEF WINDOWS}
  EnterCriticalSection(@FCriticalSection);
  {$ENDIF}
end;

procedure TMutex.Release;
begin
  {$IFDEF WINDOWS}
  LeaveCriticalSection(@FCriticalSection);
  {$ENDIF}
end;

constructor TMutex.Init;
begin
  {$IFDEF WINDOWS}
  Writeln('IC');
  InitializeCriticalSection(@FCriticalSection);
  WRiteln('IC/');
  {$ENDIF}
end;

destructor TMutex.Done;
begin
  {$IFDEF WINDOWS}
  DeleteCriticalSection(@FCriticalSection);
  {$ENDIF}
end;

end.

