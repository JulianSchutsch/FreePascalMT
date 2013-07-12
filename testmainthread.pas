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
unit testmainthread;
{$mode objfpc}{$H+}
interface

uses classes;

type TSyncThread=class(TThread)
       SyncCalled : Boolean;
       procedure Execute;override;
	   procedure SyncMethod;
       constructor Create;
	   destructor Destroy;override;
     end;

procedure WaitMainThread;

implementation

uses uconcurrency;

// Wakeup event
var event : PRTLEvent;

procedure TSyncThread.SyncMethod;
begin
  Writeln('Synchronized');
  SyncCalled:=True;
end;

procedure TSyncThread.Execute;
begin
  Writeln('Synchronize');
  Synchronize(@SyncMethod);
end;

constructor TSyncThread.Create;
begin
  inherited Create(False);
end;

destructor TSyncThread.Destroy;
begin
  inherited Destroy;
end;

procedure WaitMainThread;
begin
  // Fake a need for wakeup, wait for an event
  RTLEventWaitFor(event);
  RTLEventResetEvent(event);
  CheckSynchronize();
end;

// Trivial object for WakeMainThread method
// which sets the event "event".
type WakeObj=object
       procedure WakeUp(Obj:TObject);
	 end;

procedure WakeObj.WakeUp(Obj:TObject);
begin
  RTLEventSetEvent(event);
end;
var WO:WakeObj;

// Install WakeMainThread on startup, no care taken for
// other potential wakemain candidates.
initialization
  event:=RTLEventCreate;
  WakeMainThread:=@WO.WakeUp;
// And remove it once the application shuts down
finalization
  RTLEventDestroy(event);
end.