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
program appproducerconsumer;
uses heaptrc,{$IFNDEF WINDOWS}cthreads,{$ENDIF}testproducerconsumer;

var test : TProducerConsumer;

begin
  test:=TProducerConsumer.Create
    (UnBoundedQueue,
     1024, // Queue length
     4,    // Sending threads
     4,    // Receiving threads
     100000, // Packets each sending thread
     0);   // Amount of data in bytes
  test.wait;
  test.free;
end.