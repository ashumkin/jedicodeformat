{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is frExcludeFiles.pas, released April 2000.
The Initial Developer of the Original Code is Anthony Steele. 
Portions created by Anthony Steele are Copyright (C) 1999-2000 Anthony Steele.
All Rights Reserved. 
Contributor(s): Anthony Steele. 

The contents of this file are subject to the Mozilla Public License Version 1.1
(the "License"). you may not use this file except in compliance with the License.
You may obtain a copy of the License at http://www.mozilla.org/NPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied.
See the License for the specific language governing rights and limitations 
under the License.
------------------------------------------------------------------------------*)
{*)}

unit frExcludeFiles;

interface

uses
  { delphi }
  Windows, Messages, SysUtils, Classes, Graphics, Controls,
  StdCtrls, Forms, Dialogs,
  { jcl  }
  JvEdit, JvMemo,
  { jcf }
  frmBaseSettingsFrame;

type
  TfExcludeFiles = class(TfrSettingsFrame)
    mFiles: TJvMemo;
    lblFilesCaption: TLabel;
    mDirs: TJvMemo;
    lblDirsCaption: TLabel;
    procedure FrameResize(Sender: TObject);
    procedure mFilesDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure mDirsDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure mFilesDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure mDirsDragDrop(Sender, Source: TObject; X, Y: Integer);
  private
    fbFileDrop: Boolean;
  protected

    procedure DragItemDropped(const piFormat: integer; const psItem: string); override;

  public
    constructor Create(AOwner: TComponent); override;

    procedure Read; override;
    procedure Write; override;

  end;

implementation

{$R *.DFM}

uses
  { delphi } 
  { jcl } JclStrings,
  { local } JcfMiscFunctions, JcfHelp, JclFileUtils, JcfSettings;

constructor TfExcludeFiles.Create(AOwner: TComponent);
begin
  inherited;
  IsDropActive := True;
  fiHelpContext := HELP_EXCLUSIONS;
end;

procedure TfExcludeFiles.Read;
begin
  with FormatSettings.FileSettings do
  begin
    mFiles.Lines.Assign(ExclusionsFiles);
    mDirs.Lines.Assign(ExclusionsDirs);
  end;
end;

procedure TfExcludeFiles.Write;
begin
  with FormatSettings.FileSettings do
  begin
      ExclusionsFiles.Assign(mFiles.Lines);
      ExclusionsDirs.Assign(mDirs.Lines);
  end;
end;

procedure TfExcludeFiles.DragItemDropped(const piFormat: integer; const psItem: string);
var
  lsItem: string;
begin

  if fbFileDrop then
  begin
    { is it a valid file ? }
    if not FileExists(psItem) then
      exit;

    { get just the raw file name }
    lsItem := ExtractFileName(psItem);
    if Pos('.', lsItem) > 0 then
      lsItem := StrBefore('.', lsItem);
    if mFiles.Lines.IndexOf(lsItem) < 0 then
      mFiles.Lines.Add(lsItem);
  end
  else
  begin
    { add dir - can be file or dir }
    if not (FileExists(psItem) or DirectoryExists(psItem)) then
      exit;

    lsItem := GetLastDir(psItem);
    if mDirs.Lines.IndexOf(lsItem) < 0 then
      mDirs.Lines.Add(lsItem);
  end;
end;

{-------------------------------------------------------------------------------
  evnet handlers }

procedure TfExcludeFiles.FrameResize(Sender: TObject);
const
  PAD = 2;
var
  liHalf: integer;
begin
  liHalf := ClientHeight div 2;

  mFiles.Height := liHalf - (lblFilesCaption.Top + lblFilesCaption.Height + PAD);
  mFiles.Left := PAD;
  mFiles.Width := ClientWidth - (2 * PAD);

  lblDirsCaption.Top := mFiles.Top + mFiles.Height + PAD;
  mDirs.Left := PAD;
  mDirs.Top := lblDirsCaption.Top + lblDirsCaption.Height + PAD;
  mDirs.Height := ClientHeight - (mDirs.Top + PAD);
  mDirs.Width := ClientWidth - (2 * PAD);
end;

procedure TfExcludeFiles.mFilesDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept := True;
  fbFileDrop := True;
end;

procedure TfExcludeFiles.mDirsDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept := True;
  fbFileDrop := False;
end;

procedure TfExcludeFiles.mFilesDragDrop(Sender, Source: TObject; X,
  Y: Integer);
begin
  HandleShellDragDrop(Source);
end;

procedure TfExcludeFiles.mDirsDragDrop(Sender, Source: TObject; X, Y: Integer);
begin
  HandleShellDragDrop(Source);
end;

end.
