unit testactiveobject;

{$mode objfpc}{$H+}

interface
uses uconcurrency, ufuture,classes;

type TIntegerFuture = specialize TFuture<Integer>;
type TStringFuture  = specialize TFuture<String>;
type PIntegerFuture = ^TIntegerFuture;
type PStringFuture = ^TStringFuture;

type TExampleActive=class(TThread)
  private
    type TActivationEntry=record
      case Id: Integer of
        1:(Fut1 : PIntegerFuture);
        2:(Fut2 : PStringFuture);
      end;
    type TActivationQueue=specialize TUnboundedQueue<TActivationEntry>;
    var
    FQueue         : TActivationQueue;
    FTerminatedAck : TEvent;
    FTerminated    : Boolean;
    function i_1():Integer;
    function i_2():String;
    public
    procedure Execute;override;
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
      case Entry.Id of
        1:Entry.Fut1^.SSet(i_1);
        2:Entry.Fut2^.SSet(i_2);
      end;
    end;
  end;
  FTerminatedAck.SSet;
end;

procedure TExampleActive.proxy_1(Res:PIntegerFuture);
var Entry:TActivationEntry;
begin
  Res^.Init;
  Entry.Id   := 1;
  Entry.Fut1 := Res;
  FQueue.Send(Entry);
end;

procedure TExampleActive.proxy_2(Res:PStringFuture);
var Entry:TActivationEntry;
begin
  Res^.Init;
  Entry.Id   := 2;
  Entry.Fut2 := Res;
  FQueue.Send(Entry);
end;

constructor TExampleActive.Create;
begin
  FTerminatedAck.Init;
  FQueue:=TActivationQueue.Create;
  inherited Create(False);
end;

destructor TExampleActive.Destroy;
begin
  FTerminated:=True;
  FQueue.Abort(Self);
  FTerminatedAck.Wait;
  FTerminatedAck.Done;
  FQueue.Free;
  inherited Destroy;
end;

end.