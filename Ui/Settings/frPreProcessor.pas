unit frPreProcessor;

interface

{ preprocessor symbols }

uses
  Classes, Controls, Forms,
  StdCtrls, JvMemo,
   { local}frmBaseSettingsFrame;

type
  TfPreProcessor = class(TfrSettingsFrame)
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

constructor TfPreProcessor.Create(AOwner: TComponent);
begin
  inherited;
  fiHelpContext := HELP_CLARIFY;
end;

procedure TfPreProcessor.Read;
begin
  inherited;
  with FormatSettings.PreProcessor do
  begin
    cbEnable.Checked := Enabled;
    mSymbols.Lines.Assign(DefinedSymbols);
    mOptions.Lines.Assign(DefinedOptions);
  end;

end;

procedure TfPreProcessor.Write;
begin
  inherited;
  with FormatSettings.PreProcessor do
  begin
    Enabled := cbEnable.Checked;
    DefinedSymbols.Assign(mSymbols.Lines);
    DefinedOptions.Assign(mOptions.Lines);
  end;
end;

procedure TfPreProcessor.FrameResize(Sender: TObject);
var
  liClientHeight: integer;
begin
  liClientHeight := ClientHeight -
    (cbEnable.Top + cbEnable.Height +
    lblCompilerOptions.Height + lblSymbols.Height +
    (GUI_PAD * 3));

  mSymbols.Height := (liClientHeight div 2);
  mSymbols.Left   := 0;
  mSymbols.Width  := ClientWidth;

  lblCompilerOptions.Top := mSymbols.Top + mSymbols.Height + GUI_PAD;
  mOptions.Top    := lblCompilerOptions.Top + lblCompilerOptions.Height + GUI_PAD;
  mOptions.Height := ClientHeight - mOptions.Top;
  mOptions.Left   := 0;
  mOptions.Width  := ClientWidth;

end;


end.
