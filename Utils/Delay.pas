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
