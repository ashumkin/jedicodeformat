unit frmJcfScratchpad;

interface

uses
  { delphi }
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls, ExtActns, StdActns, ActnList,
  Buttons, Menus,
  { local } StringsConverter, ScratchPadSettings;

type
  TfrmScratchpad = class(TForm)
    StatusBar1: TStatusBar;
    pnlTop: TPanel;
    pcPages: TPageControl;
    tsInput: TTabSheet;
    tsOutput: TTabSheet;
    mInput: TMemo;
    mOutput: TMemo;
    mMessages: TMemo;
    lblMessages: TLabel;
    sbLoad: TSpeedButton;
    sbSave: TSpeedButton;
    sbGo: TSpeedButton;
    ActionList1: TActionList;
    actOpen: TAction;
    actSave: TAction;
    actGo: TAction;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    actClear: TAction;
    sbClear: TSpeedButton;
    MainMenu1: TMainMenu;
    mnuFile: TMenuItem;
    mnuFileOpen: TMenuItem;
    mnuFileSave: TMenuItem;
    mnuGo: TMenuItem;
    mnuClear: TMenuItem;
    mnuExit: TMenuItem;
    Options1: TMenuItem;
    mnuAlwaysShowParseTree: TMenuItem;
    mnuShowParseTreeonError: TMenuItem;
    mnuNeverShowParseTree: TMenuItem;
    actCopy: TAction;
    actPaste: TAction;
    SpeedButton1: TSpeedButton;
    sbPaste: TSpeedButton;
    N1: TMenuItem;
    N2: TMenuItem;
    Copy1: TMenuItem;
    procedure FormResize(Sender: TObject);
    procedure pcPagesChange(Sender: TObject);
    procedure actGoExecute(Sender: TObject);
    procedure mInputKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure actOpenExecute(Sender: TObject);
    procedure actClearExecute(Sender: TObject);
    procedure actSaveExecute(Sender: TObject);
    procedure mnuExitClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure mnuAlwaysShowParseTreeClick(Sender: TObject);
    procedure mnuNeverShowParseTreeClick(Sender: TObject);
    procedure mnuShowParseTreeonErrorClick(Sender: TObject);
    procedure actCopyExecute(Sender: TObject);
    procedure actPasteExecute(Sender: TObject);
  private
    fcConvert: TStringsConverter;
    fcSettings: TScratchpadSettings;

    procedure CheckInputState;


  public
    { Public declarations }
  end;

var
  frmScratchpad: TfrmScratchpad;

implementation

uses
  ClipBrd,
  JclStrings,
  Converter, ConvertTypes;

{$R *.dfm}

procedure TfrmScratchpad.CheckInputState;
begin
  actGo.Enabled := (mInput.Text <> '');
  actClear.Enabled := (mInput.Text <> '');
end;

procedure TfrmScratchpad.FormResize(Sender: TObject);
const
  OUTPUT_PAD = 4;
begin
  mOutput.Left := OUTPUT_PAD;
  mOutput.Top := OUTPUT_PAD;
  mOutput.Width := tsOutput.ClientWidth - (2 * OUTPUT_PAD);

  // two thirds height
  mOutput.Height := (tsOutput.Height * 2 div 3) - (2 * OUTPUT_PAD);

  lblMessages.Left := 4;
  lblMessages.Top := mOutput.Top + mOutput.Height + OUTPUT_PAD;

  mMessages.Top := lblMessages.Top + lblMessages.Height + OUTPUT_PAD;
  mMessages.Height := tsOutput.ClientHeight - (lblMessages.Top + lblMessages.Height + (OUTPUT_PAD * 2));
  mMessages.Left := OUTPUT_PAD;
  mMessages.Width := tsOutput.ClientWidth - (2 * OUTPUT_PAD);
end;

procedure TfrmScratchpad.pcPagesChange(Sender: TObject);
var
  lbHasOutput: boolean;
begin
  actOpen.Enabled := (pcPages.ActivePage = tsInput);
  actPaste.Enabled := (pcPages.ActivePage = tsInput) and  Clipboard.HasFormat(CF_TEXT);

  lbHasOutput := (pcPages.ActivePage = tsOutput) and (mOutput.Text <> '');
  actSave.Enabled := lbHasOutput;
  actCopy.Enabled := lbHasOutput;
end;

procedure TfrmScratchpad.actGoExecute(Sender: TObject);
begin
  mMessages.Clear;
  fcConvert.InputStrings := mInput.Lines;
  fcConvert.OutputStrings := mOutput.Lines;
  fcConvert.MessageStrings := mMessages.Lines;

  fcConvert.Convert;
  fcConvert.Clear;

  pcPages.ActivePage := tsOutput;
  pcPagesChange(nil);
end;

procedure TfrmScratchpad.mInputKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  CheckInputState;
end;


procedure TfrmScratchpad.FormShow(Sender: TObject);
begin
  mnuAlwaysShowParseTree.Checked := (fcConvert.ShowParseTreeOption = eShowAlways);
  mnuShowParseTreeonError.Checked := (fcConvert.ShowParseTreeOption = eShowOnError);
  mnuNeverShowParseTree.Checked := (fcConvert.ShowParseTreeOption = eShowNever);

  CheckInputState;
  pcPagesChange(nil);
end;

procedure TfrmScratchpad.actOpenExecute(Sender: TObject);
begin
  OpenDialog1.InitialDir := fcSettings.InputDir;

  if OpenDialog1.Execute then
  begin
    fcSettings.InputDir := ExtractFilePath(OpenDialog1.FileName);
    mInput.Text := FileToString(OpenDialog1.FileName);
    CheckInputState;
  end;
end;

procedure TfrmScratchpad.actClearExecute(Sender: TObject);
begin
  mInput.Text := '';
  mOutput.Text := '';
  mMessages.Text := '';
  pcPages.ActivePage := tsInput;

  CheckInputState;
  pcPagesChange(nil);
end;

procedure TfrmScratchpad.actSaveExecute(Sender: TObject);
begin
  SaveDialog1.InitialDir := fcSettings.OutputDir;

  if SaveDialog1.Execute then
  begin
    fcSettings.OutputDir := ExtractFilePath(SaveDialog1.FileName);
    StringToFile(SaveDialog1.FileName, mOutput.Text);
  end;
end;

procedure TfrmScratchpad.mnuExitClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmScratchpad.FormCreate(Sender: TObject);
begin
  fcConvert := TStringsConverter.Create;
  fcSettings := TScratchpadSettings.Create;
  fcConvert.ShowParseTreeOption := fcSettings.ShowParseTreeOption;
end;

procedure TfrmScratchpad.FormDestroy(Sender: TObject);
begin
  // write this to registry
  fcSettings.ShowParseTreeOption := fcConvert.ShowParseTreeOption;

  FreeAndNil(fcConvert);
  FreeAndNil(fcSettings);
end;

procedure TfrmScratchpad.mnuAlwaysShowParseTreeClick(Sender: TObject);
begin
  fcConvert.ShowParseTreeOption := eShowAlways;
end;

procedure TfrmScratchpad.mnuNeverShowParseTreeClick(Sender: TObject);
begin
  fcConvert.ShowParseTreeOption := eShowNever;
end;

procedure TfrmScratchpad.mnuShowParseTreeonErrorClick(Sender: TObject);
begin
  fcConvert.ShowParseTreeOption := eShowOnError;
end;

procedure TfrmScratchpad.actCopyExecute(Sender: TObject);
begin
  Clipboard.AsText := mOutput.Text;
end;

procedure TfrmScratchpad.actPasteExecute(Sender: TObject);
begin
  if Clipboard.HasFormat(CF_TEXT) then
    mInput.Text := mInput.Text  + Clipboard.AsText;
end;

end.
