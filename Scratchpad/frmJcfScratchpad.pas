unit frmJcfScratchpad;

interface

uses
  { delphi }
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls, ExtActns, StdActns, ActnList,
  Buttons, Menus,
  { Jedi }
  JvMRUList, JvMemo,
  { local } StringsConverter, ScratchPadSettings;

type
  TfrmScratchpad = class(TForm)
    sb1: TStatusBar;
    pnlTop: TPanel;
    pcPages: TPageControl;
    tsInput: TTabSheet;
    tsOutput: TTabSheet;
    mInput: TJvMemo;
    mOutput: TJvMemo;
    mMessages: TJvMemo;
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
    mnuFileSaveOut: TMenuItem;
    mnuExit: TMenuItem;
    mnuOptions: TMenuItem;
    mnuAlwaysShowParseTree: TMenuItem;
    mnuShowParseTreeonError: TMenuItem;
    mnuNeverShowParseTree: TMenuItem;
    actCopy: TAction;
    actPaste: TAction;
    N1: TMenuItem;
    mruFIles: TJvMRUManager;
    mnuEdit: TMenuItem;
    mnuEditPaste: TMenuItem;
    mnuEditCopy: TMenuItem;
    mnuEditGo: TMenuItem;
    mnuEditClear: TMenuItem;
    mnuEditCut: TMenuItem;
    mnuEditCopyOutput: TMenuItem;
    mnuEditSelectAll: TMenuItem;
    mnuEditCopyMessages: TMenuItem;
    mnuFormat: TMenuItem;
    mnuFileSaveIn: TMenuItem;
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
    procedure mruFilesClick(Sender: TObject; const RecentName,
      Caption: String; UserData: Integer);
    procedure mnuEditCopyOutputClick(Sender: TObject);
    procedure mnuEditCutClick(Sender: TObject);
    procedure mnuEditSelectAllClick(Sender: TObject);
    procedure mnuEditCopyMessagesClick(Sender: TObject);
    procedure mnuFileSaveInClick(Sender: TObject);
  private
    fcConvert: TStringsConverter;
    fcSettings: TScratchpadSettings;

    procedure CheckInputState;
    procedure DoFileOpen(const psFileName: string);
    procedure AddCheckMRU(const psFile: string);

  public
  end;

var
  frmScratchpad: TfrmScratchpad;

implementation

uses
  ClipBrd,
  JclStrings,
  Converter, ConvertTypes;

{$R *.dfm}

const FILE_FILTERS =
  'Delphi source (*.pas, *.dpr)|*.pas; *.dpr|' +
  'Text files (*.txt)|*.txt|' +
  'All files (*.*)|*.*';

procedure TfrmScratchpad.CheckInputState;
begin
  actGo.Enabled := (mInput.Text <> '');
  actClear.Enabled := (mInput.Text <> '');
end;

procedure TfrmScratchpad.DoFileOpen(const psFileName: string);
begin
  if psFileName = '' then
    exit;
  if not FileExists(psFileName) then
    exit;

  fcSettings.InputDir := ExtractFilePath(psFileName);
  mInput.Text := FileToString(psFileName);
  sb1.SimpleText := psFileName;
  AddCheckMRU(psFileName);

  CheckInputState;
end;

procedure TfrmScratchpad.AddCheckMRU(const psFile: string);
var
  liIndex: integer;
begin
  liIndex := mruFiles.Strings.IndexOf(psFile);

  if (liIndex < 0) then
  begin
    mruFiles.Add(psFile, 0);
    liIndex := mruFiles.Strings.IndexOf(psFile);
  end;

  mruFiles.Strings.Move(liIndex, 0);

  while mruFiles.Strings.Count > mruFiles.Capacity do
    mruFiles.Strings.Delete(mruFiles.Strings.Count - 1);
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
  OpenDialog1.Filter := FILE_FILTERS;

  if OpenDialog1.Execute then
  begin
    DoFileOpen(OpenDialog1.FileName);
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
  SaveDialog1.Title := 'Save output file';
  SaveDialog1.Filter := FILE_FILTERS;


  if SaveDialog1.Execute then
  begin
    fcSettings.OutputDir := ExtractFilePath(SaveDialog1.FileName);
    StringToFile(SaveDialog1.FileName, mOutput.Text);
    sb1.SimpleText := 'Saved ' + SaveDialog1.FileName;
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

  fcSettings.LoadMRUFiles(mruFiles.Strings);
  mruFiles.RemoveInvalid;
end;

procedure TfrmScratchpad.FormDestroy(Sender: TObject);
begin
  // write this to registry
  fcSettings.ShowParseTreeOption := fcConvert.ShowParseTreeOption;
  fcSettings.SaveMRUFiles(mruFiles.Strings);

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
  if pcPages.ActivePage = tsOutput then
    mOutput.CopyToClipboard
  else
    mInput.CopyToClipboard;
end;

procedure TfrmScratchpad.actPasteExecute(Sender: TObject);
begin
  if (pcPages.ActivePage = tsInput) and Clipboard.HasFormat(CF_TEXT) then
  begin
    mInput.PasteFromClipboard;
    CheckInputState;
  end;
end;

procedure TfrmScratchpad.mruFilesClick(Sender: TObject; const RecentName,
  Caption: String; UserData: Integer);
begin
  DoFileOpen(RecentName);
end;

procedure TfrmScratchpad.mnuEditCopyOutputClick(Sender: TObject);
begin
  Clipboard.AsText := mOutput.Text;
end;

procedure TfrmScratchpad.mnuEditCutClick(Sender: TObject);
begin

  if (pcPages.ActivePage = tsInput) then
  begin
    mInput.CutToClipboard;
    CheckInputState;
  end;

end;

procedure TfrmScratchpad.mnuEditSelectAllClick(Sender: TObject);
begin
  if (pcPages.ActivePage = tsInput) then
  begin
    mInput.SetFocus;
    mInput.SelectAll;
  end
  else
  begin
    mOutput.SetFocus;
    mOutput.SelectAll;
  end;
end;

procedure TfrmScratchpad.mnuEditCopyMessagesClick(Sender: TObject);
begin
  Clipboard.AsText := mMessages.Text;
end;

procedure TfrmScratchpad.mnuFileSaveInClick(Sender: TObject);
begin
  SaveDialog1.InitialDir := fcSettings.OutputDir;
  SaveDialog1.Title := 'Save input file';
  SaveDialog1.Filter := FILE_FILTERS;

  if SaveDialog1.Execute then
  begin
    fcSettings.OutputDir := ExtractFilePath(SaveDialog1.FileName);
    StringToFile(SaveDialog1.FileName, mInput.Text);
    sb1.SimpleText := 'Saved input' + SaveDialog1.FileName;
  end;
end;

end.
