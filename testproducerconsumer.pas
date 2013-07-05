unit testproducerconsumer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,uconcurrency;

type EQueueType=(BoundedQueue);

type TDataBoundedQueue = specialize TBoundedQueue<Pointer>;

type TProducerConsumer=class
  private
    var
    FQueueType    : EQueueType;
    FBoundedQueue : TDataBoundedQueue;
    FSendPacketsEachThread : Cardinal;
    FTotalReceivedPackets  : Cardinal;
    FTotalPackets          : Cardinal;
    FActiveThreads         : Cardinal;
    FCompleteEvent         : TEvent;

    type TProducerThread=class(TThread)
      private
        FProducerConsumer : TProducerConsumer;
        FRemainingPackets : Cardinal;
      public
        procedure Execute;override;
    end;

    type TConsumerThread=class(TThread)
      private
        FProducerConsumer : TProducerConsumer;
        FReceivedPackets : Cardinal;
      public
        procedure Execute;override;
    end;

    var
    SendThreads            : array of TProducerThread;
    RecvThreads            : array of TConsumerThread;

    procedure LeavingThread;

  public
    procedure Wait;
    constructor Create(QueueType        : EQueueType;
                       QueueLength      : Cardinal;
                       Sending          : Cardinal;
                       Receiving        : Cardinal;
                       PacketEachThread : Cardinal;
                       Data             : Cardinal);
    destructor Destroy;override;
  end;

implementation

procedure TProducerConsumer.LeavingThread;
begin
  if InterlockedDecrement(FActiveThreads)=0 then
  begin
    FCompleteEvent.SSet;
  end;
end;

procedure TProducerConsumer.TConsumerThread.Execute;
var data: Pointer;
begin
  while InterlockedIncrement(FProducerConsumer.FTotalReceivedPackets)<=FProducerConsumer.FTotalPackets do
  begin
    case FProducerConsumer.FQueueType of
      BoundedQueue:if not FProducerConsumer.FBoundedQueue.Receive(data,Self) then
      begin
        Writeln('Unexpected shutdown in consumer thread');
        Break;
      end;
    end;
  end;
  FProducerConsumer.LeavingThread;

end;

procedure TProducerConsumer.TProducerThread.Execute;
begin
  FRemainingPackets:=FProducerConsumer.FSendPacketsEachThread;
  while(FRemainingPackets>0) do
  begin
    case FProducerConsumer.FQueueType of
      BoundedQueue:if not FProducerConsumer.FBoundedQueue.Send(Nil,Self) then
        begin
          Writeln('Unexpected shutdown in producer thread!');
          Break;
        end;
    end;
    FRemainingPackets:=FRemainingPackets-1;
  end;
  FProducerConsumer.LeavingThread;
end;

procedure TProducerConsumer.Wait;
begin
  FCompleteEvent.Wait;
  Writeln('Total Received:',FTotalReceivedPackets);
end;

destructor TProducerConsumer.Destroy;
var i : Integer;
begin
  for i:=0 to High(SendThreads) do
  begin
    SendThreads[i].Free;
  end;
  SetLength(SendThreads,0);
  for i:=0 to High(RecvThreads) do
  begin
    RecvThreads[i].Free;
  end;
  SetLength(RecvThreads,0);
  FBoundedQueue.Free;
  FCompleteEvent.Done;
  inherited Destroy;
end;

constructor TProducerConsumer.Create(QueueType        : EQueueType;
                                     QueueLength      : Cardinal;
                                     Sending          : Cardinal;
                                     Receiving        : Cardinal;
                                     PacketEachThread : Cardinal;
                                     Data             : Cardinal);

var i : Integer;

begin
  inherited Create;
  FQueueType     := QueueType;
  FTotalPackets  := Sending*PacketEachThread;
  FActiveThreads := Sending+Receiving;
  FSendPacketsEachThread := PacketEachThread;
  FCompleteEvent.Init;

  case QueueType of
    BoundedQueue:
    begin
      FBoundedQueue:=TDataBoundedQueue.Create(QueueLength);
    end;
  end;
  SetLength(SendThreads,Sending);
  for i:=0 to Sending-1 do
  begin
    SendThreads[i]:=TProducerThread.Create(True);
    SendThreads[i].FProducerConsumer:=Self;
  end;
  SetLength(RecvThreads,Receiving);
  for i:=0 to Receiving-1 do
  begin
    RecvThreads[i]:=TConsumerThread.Create(True);
    RecvThreads[i].FProducerConsumer:=Self;
  end;
  for i:=0 to Sending-1 do
  begin
    SendThreads[i].Start;
  end;
  for i:=0 to Receiving-1 do
  begin
    RecvThreads[i].Start;
  end;

end;

end.

