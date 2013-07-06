program appleaderfollower;
uses heaptrc,{$IFDEF unix}cthreads,{$ENDIF}testleaderfollower,sysutils;
var Test : TLeaderFollower;
begin
  Test:=TLeaderFollower.Create(10);
  Sleep(10000);
  Test.Free;
end.