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

// Demonstration of the main thread synchronization

program appmainthread;
{$mode objfpc}{$H+}

uses heaptrc,{$IFDEF UNIX}cthreads,{$ENDIF}testmainthread;

var SyncThread:TSyncThread;

begin

  // SyncThread will try to execute a method in the main thread
  // which is catched and executed by WaitMainThread
  // WakeMainThread is a "wait for event" wrapper for CheckSynchronize in
  // classes(RTL).
  SyncThread:=TSyncThread.Create;

  while(not SyncThread.SyncCalled) do
  begin
    WaitMainThread;
  end;
  SyncThread.Free;

end.