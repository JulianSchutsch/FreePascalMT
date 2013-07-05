unit testproducerconsumer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,uconcurrency;

type EQueueType=(BoundedQueue);


type TProducerConsumer=class
  private
    type TDataBoundedQueue = specialize TBoundedQueue<Pointer>;

    var
    FQueueType    : EQueueType;
    FBoundedQueue : TDataBoundedQueue;
    FSendPacketEachThread : Cardinal;
    FTotalReceivedPackets : Cardinal;
    FTotalPackets         : Cardinal;
    FActiveThreads        : Cardinal;

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

procedure ASyncComplete(Data:Pointer);

  public
    constructor Create(QueueType        : EQueueType;
                       QueueLength      : Cardinal;
                       Sending          : Cardinal;
                       Receiving        : Cardinal;
                       PacketEachThread : Cardinal;
                       Data             : Cardinal);
  end;

implementation

procedure TProducerConsumer.ASyncComplete(Data:Pointer);
begin
  Writeln('Complete!');
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
  if(FProducerConsumer.FBoundedQueue.Receive(data,Self)) then
  begin
    Writeln('Unexpected data in consumer thread');
  end;

end;

procedure TProducerConsumer.TProducerThread.Execute;
begin
  FRemainingPackets:=FProducerConsumer.FSendPacketEachThread;
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
end;

constructor TProducerConsumer.Create(QueueType        : EQueueType;
                                     QueueLength      : Cardinal;
                                     Sending          : Cardinal;
                                     Receiving        : Cardinal;
                                     PacketEachThread : Cardinal;
                                     Data             : Cardinal);

var SendThreads : array of TProducerThread;
    RecvThreads : array of TConsumerThread;
    i           : Integer;

begin
  FQueueType     := QueueType;
  FTotalPackets  := Sending*PacketEachThread;
  FActiveThreads := Sending+Receiving;

  case QueueType of
    BoundedQueue:
    begin
      FBoundedQueue:=TDataBoundedQueue.Create(QueueLength);
    end;
  end;

  Writeln('Create Threads');
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

