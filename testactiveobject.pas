unit testactiveobject;

{$mode objfpc}{$H+}

interface
uses uconcurrency, ufuture,classes,sysutils;

type TIntegerFuture = specialize TFuture<Integer>;
type TStringFuture  = specialize TFuture<String>;
type PIntegerFuture = ^TIntegerFuture;
type PStringFuture = ^TStringFuture;

type TExampleActive=class(TThread)
  private
    // Internal data structure to describe a request
    type TActivationEntry=record
      case Id: Integer of
        1:(Fut1 : PIntegerFuture);
        2:(Fut2 : PStringFuture);
      end;
    // Queue holding any number of requests, no send synchronisation required
    type TActivationQueue=specialize TUnboundedQueue<TActivationEntry>;
    var
    FQueue         : TActivationQueue;
    FTerminatedAck : PRTLEvent;
    FTerminated    : Boolean;

    // Actual functions executed in the thread
    function i_1():Integer;
    function i_2():String;

    public

    // Thread main
    procedure Execute;override;

    // Proxy functions for i_1 and i_2
    procedure proxy_1(Res:PIntegerFuture);
    procedure proxy_2(Res:PStringFuture);

    constructor Create;
    destructor Destroy;override;

  end;

implementation

function TExampleActive.i_1:Integer;
begin
  Exit(1000);
end;

function TExampleActive.i_2:String;
begin
  Exit('Hallo');
end;

procedure TExampleActive.Execute;
var Entry:TActivationEntry;
begin
  while not FTerminated do
  begin
    if FQueue.Receive(Entry,Self) then
    begin
      // Manual dispatching
      case Entry.Id of
        1:Entry.Fut1^.SSet(i_1);
        2:Entry.Fut2^.SSet(i_2);
      end;
    end;
    Sleep(1000); // Waste some time, just for demonstration
  end;
  RTLEventSetEvent(FTerminatedAck);
end;

procedure TExampleActive.proxy_1(Res:PIntegerFuture);
var Entry:TActivationEntry;
begin
  // Initialize future and send request for i_1
  Res^.Init;
  Entry.Id   := 1;
  Entry.Fut1 := Res;
  FQueue.Send(Entry);
end;

procedure TExampleActive.proxy_2(Res:PStringFuture);
var Entry:TActivationEntry;
begin
  // Initialize future and send request for i_2
  Res^.Init;
  Entry.Id   := 2;
  Entry.Fut2 := Res;
  FQueue.Send(Entry);
end;

constructor TExampleActive.Create;
begin
  FTerminatedAck:=RTLEventCreate;
  FQueue:=TActivationQueue.Create;
  inherited Create(False);
end;

destructor TExampleActive.Destroy;
begin
  // Shutdown thread, it is possible that its currently waiting in FQueue, therefore abort that
  FTerminated:=True;
  FQueue.Abort(Self);
  // Wait to ensure FQueue is not required anymore
  RTLEventWaitFor(FTerminatedAck);
  RTLEventDestroy(FTerminatedAck);
  FQueue.Free;
  inherited Destroy;
end;

end.