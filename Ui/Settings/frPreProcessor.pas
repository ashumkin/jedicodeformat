unit frPreProcessor;

interface

{ preprocessor symbols }

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, StdCtrls, JvMemo,
   { local} frmBaseSettingsFrame;

type
  TfrDefinedSymbols = class(TfrSettingsFrame)
    mSymbols: TJvMemo;
    cbEnable: TCheckBox;
    lblSymbols: TLabel;
    mOptions: TJvMemo;
    lblCompilerOptions: TLabel;
    procedure FrameResize(Sender: TObject);
  private
  public
    constructor Create(AOwner: TComponent); override;

    procedure Read; override;
    procedure Write; override;
  end;

implementation

{$R *.dfm}

uses JcfHelp, JcfSettings;

constructor TfrDefinedSymbols.Create(AOwner: TComponent);
begin
  inherited;
  fiHelpContext := HELP_CLARIFY;
end;

procedure TfrDefinedSymbols.Read;
begin
  inherited;
  with FormatSettings.PreProcessor do
  begin
    cbEnable.Checked := Enabled;
    mSymbols.Lines.Assign(DefinedSymbols);
    mOptions.Lines.Assign(DefinedOptions);
  end;

end;

procedure TfrDefinedSymbols.Write;
begin
  inherited;
  with FormatSettings.PreProcessor do
  begin
    Enabled := cbEnable.Checked;
    DefinedSymbols.Assign(mSymbols.Lines);
    DefinedOptions.Assign(mOptions.Lines);
  end;
end;

procedure TfrDefinedSymbols.FrameResize(Sender: TObject);
var
  liClientHeight: integer;
begin
  liClientHeight :=  ClientHeight -
    (cbEnable.Top + cbEnable.Height +
      lblCompilerOptions.Height + lblSymbols.Height +
      (GUI_PAD * 3));

  mSymbols.Height := (liClientHeight div 2);
  mSymbols.Left := 0;
  mSymbols.Width := ClientWidth;

  lblCompilerOptions.Top := mSymbols.Top + mSymbols.Height + GUI_PAD;
  mOptions.Top := lblCompilerOptions.Top + lblCompilerOptions.Height + GUI_PAD;
  mOptions.Height := ClientHeight - mOptions.Top;
  mOptions.Left := 0;
  mOptions.Width := ClientWidth;

end;


end.
