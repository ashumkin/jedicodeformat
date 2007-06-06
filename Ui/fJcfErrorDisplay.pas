unit fJcfErrorDisplay;

interface

{ AFS 22 Sept 2003
  Exception handler form
  that allows the exception etx to be copied out }

uses
  SysUtils, Classes, Controls, Forms, StdCtrls;

type
  TExceptionDialog = class(TForm)
    btnOk: TButton;
    mExceptionMessage: TMemo;
    procedure btnOkClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    procedure DisplayException(const pE: Exception);
    procedure DisplayErrorMessage(const sMessage: string;
      const psCaption: string; const piY, piX: integer);
  end;

procedure ShowExceptionDialog(const pE: Exception);
procedure ShowErrorMessageDialog(const psMessage: string; const psCaption: string;
  const piY, piX: integer);

implementation

uses JclStrings, ParseError, JcfFontSetFunctions;

{$R *.dfm}

procedure ShowExceptionDialog(const pE: Exception);
var
  frm: TExceptionDialog;
begin
  frm := TExceptionDialog.Create(nil);
  try
    frm.DisplayException(pe);
  finally
    frm.Free;
  end;
end;

procedure ShowErrorMessageDialog(const psMessage: string; const psCaption: string;
  const piY, piX: integer);
var
  frm: TExceptionDialog;
begin
  frm := TExceptionDialog.Create(nil);
  try
    frm.DisplayErrorMessage(psMessage, psCaption, piY, piX);
  finally
    frm.Free;
  end;
end;


procedure TExceptionDialog.btnOkClick(Sender: TObject);
begin
  Close;
end;

procedure TExceptionDialog.DisplayException(const pE: Exception);
var
  lcParseError: TEParseError;
begin
  if (pE is TEParseError) then
  begin
    lcParseError := TEParseError(pE);

    Caption := 'JCF Parse error';
    mExceptionMessage.Text := lcParseError.Message + ' near ' +
      lcParseError.TokenMessage;

    if (lcParseError.XPosition > 0) or (lcParseError.YPosition > 0) then
    begin
      mExceptionMessage.Text := mExceptionMessage.Text + AnsiLineBreak +
        'At line ' + IntToStr(lcParseError.YPosition) + ' col ' +
        IntToStr(lcParseError.XPosition);

      if lcParseError.FileName <> '' then
        mExceptionMessage.Text :=
          mExceptionMessage.Text + ' in ' + lcParseError.FileName;
    end;
  end
  else
  begin
    Caption := 'Exception ' + pE.ClassName;
    mExceptionMessage.Text := 'Type: ' + pE.ClassName + AnsiLineBreak;
    mExceptionMessage.Text := mExceptionMessage.Text + pE.Message;
  end;


  ShowModal;
end;

procedure TExceptionDialog.DisplayErrorMessage(const sMessage: string;
  const psCaption: string; const piY, piX: integer);
begin
  if psCaption <> '' then
    Caption := psCaption
  else
    Caption := 'Error';

  mExceptionMessage.Text := sMessage;
  if (piY > 0) or (piX > 0) then
  begin
    mExceptionMessage.Text := mExceptionMessage.Text + AnsiLineBreak +
      ' at line ' + IntToStr(piY) + ' col ' + IntToStr(piX);
  end;

  ShowModal;
end;

procedure TExceptionDialog.FormCreate(Sender: TObject);
begin
  SetObjectFontToSystemFont(Self);
end;

procedure TExceptionDialog.FormResize(Sender: TObject);
const
  PAD = 4;
begin
  btnOk.Top  := ClientHeight - (btnOk.Height + PAD);
  btnOk.Left := (ClientWidth - btnOk.Width) div 2;

  mExceptionMessage.Left   := PAD;
  mExceptionMessage.Top    := PAD;
  mExceptionMessage.Width  := ClientWidth - (PAD * 2);
  mExceptionMessage.Height := ClientHeight - (btnOk.Height + (PAD * 3));
end;

end.
