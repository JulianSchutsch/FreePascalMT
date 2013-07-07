program appactiveobject;
uses heaptrc,{$IFDEF UNIX}cthreads,{$ENDIF}testactiveobject;

var ExampleActive : TExampleActive;
    Future1 : TIntegerFuture;
    Future2 : TStringFuture;
    Result1 : Integer;
    Result2 : String;
begin
  ExampleActive:=TExampleActive.Create;
  ExampleActive.Proxy_1(@Future1);
  ExampleActive.Proxy_2(@Future2);
  Result1:=Future1.Get;
  Result2:=Future2.Get;
  Writeln('Results: ',Result1,',',Result2);
  ExampleActive.Free;
end.