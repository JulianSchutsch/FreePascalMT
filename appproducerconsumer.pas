program appproducerconsumer;
uses testproducerconsumer;

var test : TProducerConsumer;

begin
  test:=TProducerConsumer.Create
    (BoundedQueue,
     1024, // Queue length
     8,    // Sending threads
     8,    // Receiving threads
     1000000, // Packets each sending thread
     0);   // Amount of data in bytes
  test.wait;
  test.free;
end.