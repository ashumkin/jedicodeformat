unit StatusMessageReceiver;

interface

type
  TStatusMesssageReceiver = class(TObject)
  public
    procedure OnReceiveStatusMessage(const psFile, psMessage: string;
      const piY, piX: integer);
  end;

implementation

uses
  SysUtils;

{

attempt at an emacs version
procedure TStatusMesssageReceiver.OnReceiveStatusMessage(const psFile, psMessage: string;
const piY, piX: integer);
var
  lsPrefix: string;
  lsMessage: string;
begin
  lsPrefix := '';
  if (Pos('Exception', psMessage) > 0) or (Pos('Error', psMessage) > 0) then
  begin
    lsPrefix := 'Error:';
  end;

  if (piX < 0) or (piY < 0) then
  begin
    // format with no line and col
    lsMessage := Format('%s %s %s', [psFile, lsPrefix, psMessage]);
  end
  else
  begin
    // format with a line and col
    lsMessage := Format('%s(%s,%s) %s %s',
      [psFile, IntToStr(piY), IntToStr(piX), lsPrefix, psMessage]);
  end;

  WriteLn(lsMessage);
end;
}

procedure TStatusMesssageReceiver.OnReceiveStatusMessage(const psFile, psMessage: string;
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
