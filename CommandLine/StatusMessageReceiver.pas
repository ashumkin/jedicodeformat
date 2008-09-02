unit StatusMessageReceiver;

interface

type
  TStatusMsgReceiver = class(TObject)
  public
    procedure OnReceiveStatusMessage(const psFile, psMessage: string;
      const piY, piX: integer);
  end;

implementation

uses
  SysUtils;

procedure TStatusMsgReceiver.OnReceiveStatusMessage(const psFile, psMessage: string;
const piY, piX: integer);
var
  lsMessage: string;
begin
  if Pos(psFile, psMessage) = 0 then
    lsMessage := psFile + ': ' + psMessage
  else
    lsMessage := psMessage;

  if (piY >= 0) then
    lsMessage := lsMessage + ' at line ' + IntToStr(piY);
  if (piX >= 0) then
    lsMessage := lsMessage + ' col ' + IntToStr(piX);

  WriteLn(lsMessage);
end;


end.
