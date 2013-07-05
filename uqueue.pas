unit uqueue;

{$mode objfpc}{$H+}

interface

type generic TQueue<T>=class
  private
    type PElement=^TElement;
         TElement=record
           Value    : T;
           Previous : PElement;
         end;
    var
    First : PElement;
    Last  : PElement;
  public
    procedure Push(Value : T);
    function Pop:T;
    function Empty:Boolean;
  end;

implementation
uses sysutils;

function TQueue.Empty:Boolean;
begin
  Exit(First=Nil);
end;

procedure TQueue.Push(Value : T);
var N : PElement;
begin
  new(N);
  N^.Previous := Nil;
  N^.Value    := Value;
  if Last<>Nil then
  begin
    Last^.Previous:=N;
  end
  else
  begin
    First:=N;
  end;
  Last:=N;
end;

function TQueue.Pop : T;
var N : PElement;
begin
  if First=Nil then raise Exception.Create('Empty Queue');
  N      := First;
  Result := First^.Value;
  First  := First^.Previous;
  if First=Nil then
  begin
    Last:=Nil;
  end;
  Dispose(N);
end;

end.