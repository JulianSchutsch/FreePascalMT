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
unit ufuture;
{$mode objfpc}{$H+}
interface

uses uconcurrency;

// Simple future implementation.
// Please note, that "get" finalizes the Future.

// A Future is used to handle asychronous return values
// The value is not necessarily set when the Future returns,
// but there is the promise, that it will be.
// Calling "get" earlier, causes a wait until the value
// is actually set.
type generic TFuture<Data>=object
  private
    FEvent : PRTLEvent;
    FValue : Data;
  public
    function Get:Data;
    procedure SSet(Value:Data);
    constructor Init;
    destructor Done;
  end;

implementation

function TFuture.Get:Data;
begin
  // Wait for data
  RTLEventWaitFor(FEvent);
  // Same effet as TFuture.Done, eye candi
  RTLEventDestroy(FEvent);
  FEvent:=Nil;
  Exit(FValue);
end;

procedure TFuture.SSet(Value:Data);
begin
  FValue:=Value;
  RTLEventSetEvent(FEvent);
end;

constructor TFuture.Init;
begin
  FEvent:=RTLEventCreate;
end;

destructor TFuture.Done;
begin
  if FEvent<>Nil then
  begin
    RTLEventDestroy(FEvent);
    FEvent:=Nil;
  end;
end;

end.