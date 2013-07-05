unit testproducerconsumer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,uconcurrency;

type EQueueType=(BoundedQueue,UnboundedQueue);

type TDataBoundedQueue = specialize TBoundedQueue<Pointer>;
type TDataUnboundedQueue = specialize TUnboundedQueue<Pointer>;

type TProducerConsumer=class
  private
    var
    FQueueType    : EQueueType;
    FBoundedQueue : TDataBoundedQueue;
    FUnboundedQueue : TDataUnboundedQueue;
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
        FReceived         : Cardinal;
        FProducerConsumer : TProducerConsumer;
        FReceivedPackets  : Cardinal;
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
      UnboundedQueue:if not FProducerConsumer.FUnboundedQueue.Receive(data,Self) then
      begin
        Writeln('Unexpected shutdown in consumer thread');
        break;
      end;
    end;
    FReceived:=FReceived+1;
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
      UnboundedQueue: FProducerConsumer.FUnboundedQueue.Send(Nil);
    end;
    FRemainingPackets:=FRemainingPackets-1;
  end;
  FProducerConsumer.LeavingThread;
end;

procedure TProducerConsumer.Wait;
var i:Integer;
begin
  FCompleteEvent.Wait;
  Writeln('Total Received:',FTotalReceivedPackets);
  for i:=0 to High(RecvThreads) do
  begin
    Writeln('Thread ',i,' received ',RecvThreads[i].FReceived);
  end;

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
  FUnboundedQueue.Free;
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
    UnboundedQueue:
    begin
      FUnboundedQueue:=TDataUnboundedQueue.Create();
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

