unit fAllSettings;

{ AFS 7 Sept 2K
  All settings on one form at once
}

interface

uses
  { delphi }
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls, ComCtrls, ImgList,
  { local }
  JCFSettings, SettingsFrame;

type
  TFormAllSettings = class(TForm)
    tvFrames: TTreeView;
    pnlSet: TPanel;
    bbOK: TBitBtn;
    bbCancel: TBitBtn;
    BitBtn1: TBitBtn;
    procedure tvFramesChange(Sender: TObject; Node: TTreeNode);
    procedure FormCreate(Sender: TObject);
    procedure bbOKClick(Sender: TObject);
    procedure bbCancelClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure bbHelpClick(Sender: TObject);
  private
    frLastFrame: TfrSettingsFrame;

    function GetTreeNodeByName(const psName: string): TTreeNode;

    procedure GetFrameForNode(const pcNode: TTreeNode);
    function GetCurrentFrame: TfrSettingsFrame;

    procedure RemoveAll(const pbSave: Boolean);

  protected
    procedure AddCustomTreeItems; virtual;
    function GetFrameType(const psName: string): TSettingsFrameClass; virtual;

  public
    procedure Execute;
  end;


implementation

{$R *.DFM}


uses
  frFiles, frObfuscateSettings,
  frClarify, frClarifySpaces, frClarifyIndent,
  frClarifyReturns, frClarifyLongLineBreaker,
  frClarifyBlocks, frClarifyAlign,
  frExcludeFiles, frReplace,
  frReservedCapsSettings, frAnyCapsSettings, frUses, frAbout, frBasicSettings,
  JcfHelp;


{ TFormAllSettings }

procedure TFormAllSettings.Execute;
var
  lcNode: TTreeNode;
begin
  Assert(Settings <> nil);

  AddCustomTreeItems;

  tvFrames.FullExpand;

  lcNode := GetTreeNodeByName(Settings.Ui.LastSettingsPage);
  if lcNode <> nil then
    lcNode.Selected := True;

  ShowModal;
end;

procedure TFormAllSettings.GetFrameForNode(const pcNode: TTreeNode);
var
  lcType: TSettingsFrameClass;
  lf: TfrSettingsFrame;
begin
  if pcNode.Data <> nil then
    exit;

  lcType := GetFrameType(pcNode.Text);
  if lcType = nil then
    exit;

  lf := lcType.Create(self);

  { read }
  lf.Settings := Settings;
  lf.Read;

  { show }
  lf.Parent := pnlSet;
  lf.Left := 0;
  lf.Top := 0;
  lf.Width := pnlSet.ClientWidth;
  lf.Height := pnlSet.ClientHeight;

  pcNode.Data := lf;
end;

function TFormAllSettings.GetFrameType(const psName: string): TsettingsFrameClass;
begin
  Result := nil;
  // find the frame?

  // replace this with a record list name -> class map

  if AnsiSameText(psName, 'Logging') then
    Result := TfFiles
  else if AnsiSameText(psName, 'Obfuscate') then
    Result := TfObfuscateSettings
  else if AnsiSameText(psName, 'Clarify') then
    Result := TfClarify
  else if AnsiSameText(psName, 'Spaces') then
    Result := TfClarifySpaces
  else if AnsiSameText(psName, 'Indentation') then
    Result := TfClarifyIndent
  else if AnsiSameText(psName, 'Long lines') then
    Result := TfClarifyLongLineBreaker
  else if AnsiSameText(psName, 'Returns') then
    Result := TfClarifyReturns
  else if AnsiSameText(psName, 'Blocks') then
    Result := TfClarifyBlocks
  else if AnsiSameText(psName, 'Align') then
    Result := TfClarifyAlign
  else if AnsiSameText(psName, 'Object Pascal') then
    Result := TfrReservedCapsSettings
  else if AnsiSameText(psName, 'Any Word') then
    Result := TfrAnyCapsSettings
  else if AnsiSameText(psName, 'Exclusions') then
    Result := TfExcludeFiles
  else if AnsiSameText(psName, 'Find and replace') then
    Result := TfReplace
  else if AnsiSameText(psName, 'Uses') then
    Result := TfUses
  else if AnsiSameText(psName, 'About') then
    Result := TFrameAbout
  else if AnsiSameText(psName, 'Basic') then
    Result := TfrBasic;
end;

procedure TFormAllSettings.tvFramesChange(Sender: TObject; Node: TTreeNode);
var
  lf: TfrSettingsFrame;
begin
  GetFrameForNode(Node);

  if frLastFrame <> nil then
    frLastFrame.Visible := False;

  if Node.Data = nil then
    exit;

  lf := TfrSettingsFrame(Node.Data);
  lf.Visible := True;
  frLastFrame := lf;
end;

procedure TFormAllSettings.FormCreate(Sender: TObject);
begin
  frLastFrame := nil;
end;

procedure TFormAllSettings.bbOKClick(Sender: TObject);
begin
  RemoveAll(True);
  Close;
end;

procedure TFormAllSettings.bbCancelClick(Sender: TObject);
begin
  RemoveAll(False);
  Close;
end;

procedure TFormAllSettings.RemoveAll(const pbSave: Boolean);
var
  liLoop: integer;
  lcItem: TTreeNode;
  lf: TfrSettingsFrame;
begin
  { retrieve frames from the tree nodes and save them }
  for liLoop := 0 to tvFrames.Items.Count - 1 do
  begin
    lcItem := tvFrames.Items[liLoop];
    if lcItem.Data <> nil then
    begin
      lf := lcItem.Data;
      lcItem.Data := nil;

      if pbSave then
        lf.Write;

      lf.Free;
    end;
  end;
end;

function TFormAllSettings.GetTreeNodeByName(const psName: string): TTreeNode;
var
  liLoop: integer;
  lcNode: TTreeNode;
begin
  Result := nil;
  if psName = '' then
    exit;

  for liLoop := 0 to tvFrames.Items.Count - 1 do
  begin
    lcNode := tvFrames.Items[liLoop];
    if AnsiSameText(lcNode.Text, psName) then
    begin
      Result := lcNode;
      break;
    end;
  end;
end;

function TFormAllSettings.GetCurrentFrame: TfrSettingsFrame;
begin
  Result := nil;

  if tvFrames.Selected = nil then
    exit;

  Result := tvFrames.Selected.Data;
end;


procedure TFormAllSettings.bbHelpClick(Sender: TObject);
var
  lcFrame: TfrSettingsFrame;
begin
    lcFrame := GetCurrentFrame;
    if lcFrame = nil then
    begin
      Application.HelpContext(HELP_MAIN);
    end
    else
      lcFrame.ShowContextHelp;
end;

procedure TFormAllSettings.FormClose(Sender: TObject; var Action: TCloseAction);
begin

  { save the last selected node }
  if tvFrames.Selected <> nil then
    Settings.Ui.LastSettingsPage := tvFrames.Selected.Text;
end;

procedure TFormAllSettings.AddCustomTreeItems;
begin
  // here for overide;
end;

end.
