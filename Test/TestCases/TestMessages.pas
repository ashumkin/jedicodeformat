unit TestMessages;

{ simple test for windows message syntax }

interface

uses Messages;

Type

TFoo = class
  public
    procedure CMTextChanged(var Message: TMessage); message WM_SETFONT;

end;

implementation

{ TFoo }

procedure TFoo.CMTextChanged(var Message: TMessage);
begin

end;

end.
