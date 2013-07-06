unit TestLeaderFollower;

{$mode objfpc}{$H+}

interface
uses uconcurrency,classes,sysutils;
type TLeaderFollower=class
  private
    FMutex      : TMutex;
    FCondition  : TCondition;
    FHasLeader  : Boolean;
    FTerminated : Boolean;
    type TLFThread=class(TThread)
      FID : Cardinal;
      FLF : TLeaderFollower;
      procedure Execute;override;
    end;
    var
    FThreads : array of TLFThread;
  public
    constructor Create(ThreadCount : Cardinal);
    destructor Destroy;override;
  end;
implementation

procedure TLeaderFollower.TLFThread.Execute;
begin
  // Warning, FTerminated not volatile, but should be read in every loop anyway
  while not FLF.FTerminated do
  begin
    FLF.FMutex.Acquire;
    while FLF.FHasLeader and (not FLF.FTerminated) do
    begin
      FLF.FCondition.Wait(@FLF.FMutex);
    end;
    if not FLF.FTerminated then
    begin
      FLF.FHasLeader:=True;
      FLF.FMutex.Release;

      Writeln('Leading    : ',FID);
      Sleep(100);
      Writeln('Processing : ',FID);
      FLF.FMutex.Acquire;
      FLF.FHasLeader:=False;
      FLF.FCondition.Wake;
      FLF.FMutex.Release;
      Sleep(100);

    end
    else
    begin
      FLF.FMutex.Release;
    end;
  end;

end;

constructor TLeaderFollower.Create(ThreadCount : Cardinal);
var i : Integer;
begin
  inherited Create;
  FMutex.Init;
  FCondition.Init;
  SetLength(FThreads,ThreadCount);
  for i:=0 to High(FThreads) do
  begin
    FThreads[i]:=TLFThread.Create(False);
    FThreads[i].FId:=i;
    FThreads[i].FLF:=Self;
  end;
end;

destructor TLeaderFollower.Destroy;
var i : Integer;
begin
  FTerminated:=True;
  FCondition.WakeAll;
  for i:=0 to High(FThreads) do
  begin
    FThreads[i].Free;
  end;
  inherited Destroy;
end;

end.