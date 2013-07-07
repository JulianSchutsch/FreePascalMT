program appactiveobject;
uses heaptrc,{$IFDEF UNIX}cthreads,{$ENDIF}testactiveobject;

var ExampleActive : TExampleActive;

    Future1 : TIntegerFuture;
    Future2 : TStringFuture;

begin
  // Create an active object and start two requests
  ExampleActive:=TExampleActive.Create;
  ExampleActive.Proxy_1(@Future1);
  ExampleActive.Proxy_2(@Future2);

  // Wait for the requests to finish and print
  Writeln('Results: ',Future1.Get,',',Future2.Get);

  ExampleActive.Free;

end.