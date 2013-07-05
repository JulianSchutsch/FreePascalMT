program appproducerconsumer;
uses heaptrc,{$IFNDEF WINDOWS}cthreads,{$ENDIF}testproducerconsumer;

var test : TProducerConsumer;

begin
  test:=TProducerConsumer.Create
    (UnBoundedQueue,
     1024, // Queue length
     4,    // Sending threads
     4,    // Receiving threads
     1, // Packets each sending thread
     0);   // Amount of data in bytes
  test.wait;
  test.free;
end.