unit Delay;

{ AFS 14 Jan 2K
 abstraction of timer mechanism
 Use this to call a proc after a short delay

 Needed for IDE regiestering

 See delayed reg. technique from sample code by Mike Remec
  http://www.miharemec.com/doc/ota-nmi.html

 usage:

 lcMyDelay := TDelay.Create;
 lcMyDelay.ObjectProc := lcSomeObject.Proc;
 lcMyDelay.DoItSoon;
 ....
 lcMyDelay.Free;

 or

 lcMyDelay := TDelay.Create;
 lcMyDelay.Proc := SomeProc;
 lcMyDelay.DoItSoon;
 ....
 lcMyDelay.Free;

}

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is Delay, released May 2003.
The Initial Developer of the Original Code is Anthony Steele. 
Portions created by Anthony Steele are Copyright (C) 1999-2000 Anthony Steele.
All Rights Reserved. 
Contributor(s): Anthony Steele. 

The contents of this file are subject to the Mozilla Public License Version 1.1
(the "License"). you may not use this file except in compliance with the License.
You may obtain a copy of the License at http://www.mozilla.org/NPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied.
See the License for the specific language governing rights and limitations 
under the License.
------------------------------------------------------------------------------*)
{*)}

interface

uses Extctrls;

type
  TProcedure = procedure;
  TObjectProcedure = procedure of object;

  TDelay = class(TObject)
  private
    fiDelay: integer;

    // can call a proc, or a proc on an object (or both)
    fcProc: TProcedure;
    fcObjectProc: TObjectProcedure;

    fcTimer: TTimer;
    fbDone: Boolean;

    procedure DoItNow(Sender: TObject);
  public
    constructor Create;
    destructor Destroy; override;

    procedure DoItSoon;

    { how long to delay in Miliseconds}
    property Delay: integer read fiDelay write fiDelay;

    { done yet? }
    property Done: Boolean read fbDone;

    { proc to call }
    property Proc: TProcedure read fcProc write fcProc;
    property objectProc: TobjectProcedure read fcObjectProc write fcObjectProc;
  end;


implementation

uses SysUtils;

{ TDelay }
constructor TDelay.Create;
begin
  inherited;

  fcTimer := nil; // create the timer when needed
  fcProc := nil;
  fcObjectProc := nil;

  fiDelay := 500; // default 1/2 sec
  fbDone := False;
end;

destructor TDelay.Destroy;
begin
  FreeAndNil(fcTimer);
  inherited;
end;


procedure TDelay.DoItNow(Sender: TObject);
begin
  Assert(fcTimer <> nil);
    { no longer timing }
  fcTimer.Enabled := False;
  fcTimer.OnTimer := nil;

  if Assigned(fcProc) then
    fcProc;
  if assigned(fcObjectProc) then
    fcObjectProc;

  //FreeAndNil(fcTimer); this causes problems in IDE plug-ins
  fbDone := True;
end;

procedure TDelay.DoItSoon;
begin
  // need a timer now 
  if fcTimer = nil then
    fcTimer := TTimer.Create(nil);

  fcTimer.Interval := fiDelay;
  fcTimer.OnTimer := DoItNow;
  fcTimer.Enabled := True;
  fbDone := False;
end;

end.