//-----------------------------------------------------------------------------
//   Copyright 2013 Julian Schutsch
//
//   This file is part of FreePascalMT
//
//   ParallelSim is free software: you can redistribute it and/or modify
//   it under the terms of the GNU Affero General Public License as published
//   by the Free Software Foundation, either version 3 of the License, or
//   (at your option) any later version.
//
//   FreePascalMT is distributed in the hope that it will be useful,
//   but WITHOUT ANY WARRANTY; without even the implied warranty of
//   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//   GNU Affero General Public License for more details.
//
//   You should have received a copy of the GNU Affero General Public License
//   along with FreePascalMT.  If not, see <http://www.gnu.org/licenses/>.
//-----------------------------------------------------------------------------
unit testproducerconsumer;
{$mode objfpc}{$H+}
interface

uses Classes, SysUtils,uconcurrency;

type EQueueType=(BoundedQueue,UnboundedQueue);

type TDataBoundedQueue = specialize TBoundedQueue<Pointer>;
type TDataUnboundedQueue = specialize TUnboundedQueue<Pointer>;

// Producer consumer is based on either bounded or unbounded queue,
// sending a number of messages from the source (producer) to the
// sink (consumer). There may be any number of producers and consumers.
type TProducerConsumer=class
  private
    var
    FQueueType             : EQueueType;
    FBoundedQueue          : TDataBoundedQueue;
    FUnboundedQueue        : TDataUnboundedQueue;
    FSendPacketsEachThread : Cardinal;
    FTotalReceivedPackets  : Cardinal;
    FTotalPackets          : Cardinal;
    FActiveThreads         : Cardinal;
    FCompleteEvent         : PRTLEvent;

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
	// Create a producer/consumer system with
	// a fixed number of producers (sending) and
	// consumers (receiving)
    constructor Create(QueueType        : EQueueType;
                       QueueLength      : Cardinal;
                       Sending          : Cardinal;
                       Receiving        : Cardinal;
                       PacketEachThread : Cardinal;
                       Data             : Cardinal);
    destructor Destroy;override;
  end;

implementation

// Notify waiting thread, once all producers and consumers are finished.
procedure TProducerConsumer.LeavingThread;
begin
  if InterlockedDecrement(FActiveThreads)=0 then
  begin
    RTLEventSetEvent(FCompleteEvent);
  end;
end;

procedure TProducerConsumer.TConsumerThread.Execute;
var data: Pointer;
begin
  // Receive data until there is no more expected
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
  // Notify waiting thread
  FProducerConsumer.LeavingThread;

end;

procedure TProducerConsumer.TProducerThread.Execute;
begin
  // Send a fixed number of packets/producer
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
  // Notify waiting thread
  FProducerConsumer.LeavingThread;
end;

procedure TProducerConsumer.Wait;
var i:Integer;
begin
  // Wait for all producer and consumer threads to shut down
  RTLEventWaitFor(FCompleteEvent);
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
  RTLEventDestroy(FCompleteEvent);
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
  FCompleteEvent := RTLEventCreate; //FCompleteEvent.Init;

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

