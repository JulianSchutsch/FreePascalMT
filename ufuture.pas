unit ufuture;

{$mode objfpc}{$H+}

interface
uses uconcurrency;

type generic TFuture<Data>=object
  private
    FEvent : TEvent;
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
  FEvent.Wait;
  FEvent.Done;
  Exit(FValue);
end;

procedure TFuture.SSet(Value:Data);
begin
  FValue:=Value;
  FEvent.SSet;
end;

constructor TFuture.Init;
begin
  FEvent.Init;
end;

destructor TFuture.Done;
begin
  FEvent.Done;
end;

end.