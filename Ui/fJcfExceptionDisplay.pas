unit fJcfExceptionDisplay;

interface

{ AFS 22 Sept 2003
  Exception handler form
  that allows the exception etx to be copied out }

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TExceptionDialog = class(TForm)
    btnOk: TButton;
    mExceptionMessage: TMemo;
    procedure btnOkClick(Sender: TObject);
  private
    { Private declarations }
  public
    procedure DisplayException(Sender: TObject; E: Exception);
    procedure DisplayErrorMessage(const sMessage: string; const psCaption: string = '');
  end;

{var
  mfExceptionDialog: TExceptionDialog;
}
implementation

uses JclStrings;

{$R *.dfm}


procedure TExceptionDialog.btnOkClick(Sender: TObject);
begin
  Close;
end;

procedure TExceptionDialog.DisplayException(Sender: TObject; E: Exception);
begin
  Caption := 'Exception';

  mExceptionMessage.Text :=
    'Type: ' + E.ClassName + AnsiLineBreak +
    'Message: ' + E.Message;
  ShowModal;
end;

procedure TExceptionDialog.DisplayErrorMessage(const sMessage: string; const psCaption: string = '');
begin
  if psCaption <> '' then
    Caption := psCaption
  else
    Caption := 'Error';

  mExceptionMessage.Text := sMessage;
  ShowModal;
end;

initialization

{ not in use yet- fails badly in IDE pluggin
  mfExceptionDialog := TExceptionDialog.Create(Application);
 // don't do this in IDE - will catch delphi's exceptions
  Application.OnException := mfExceptionDialog.DisplayException;

}
finalization

// don't free it!

end.
