unit ufuture;

{$mode objfpc}{$H+}

interface
uses uconcurrency;

type generic TFuture<Data>=object
  private
    FEvent : PRTLEvent;
    FValue : Data;
  public
    function Get:Data;
    procedure SSet(Value:Data);
    constructor Init;
    destructor Done;
  end;

implementation

function TFuture.Get:Data;
begin
  RTLEventWaitFor(FEvent);
  RTLEventDestroy(FEvent);
  FEvent:=Nil;
  Exit(FValue);
end;

procedure TFuture.SSet(Value:Data);
begin
  FValue:=Value;
  RTLEventSetEvent(FEvent);
end;

constructor TFuture.Init;
begin
  FEvent:=RTLEventCreate;
end;

destructor TFuture.Done;
begin
  if FEvent<>Nil then
  begin
    RTLEventDestroy(FEvent);
    FEvent:=Nil;
  end;
end;

end.