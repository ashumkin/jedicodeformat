{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is frLog.pas, released April 2000.
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

unit frFiles;

interface

uses
  { delphi }
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls,
  { local } frmBaseSettingsFrame;

type
  TfFiles = class(TfrSettingsFrame)
    lblFormatFileName: TLabel;
    lblStatus: TLabel;
    procedure FrameResize(Sender: TObject);
  private


  public
    constructor Create(AOwner: TComponent); override;

    procedure Read; override;
    procedure Write; override;
  end;

implementation

uses
    { delphi }
    { jcl } JclSysInfo, JclFileUtils,
    { local } FileUtils, JcfRegistrySettings, JcfSettings, JCFHelp;

{$R *.DFM}

constructor TfFiles.Create(AOwner: TComponent);
begin
  inherited;
end;

procedure TfFiles.Read;
var
  lcSet: TJCFRegistrySettings;
begin
  lcSet := GetRegSettings;
  lblFormatFileName.Caption := 'Format file is ' + lcSet.FormatConfigFileName;

  if not FileExists(lcSet.FormatConfigFileName) then
    lblStatus.Caption := 'file not found'
  else if FileIsReadOnly(lcSet.FormatConfigFileName) then
    lblStatus.Caption := 'file is read only'
  else
    lblStatus.Caption := '';
end;

procedure TfFiles.Write;
begin
end;

procedure TfFiles.FrameResize(Sender: TObject);
const
  SPACING = 8;
begin
  inherited;

  lblFormatFileName.Left := SPACING;
  lblFormatFileName.Width := ClientWidth - (lblFormatFileName.Left + SPACING);

  lblStatus.Left := SPACING;
  lblStatus.Top := lblFormatFileName.Top + lblFormatFileName.Height + SPACING;
end;

end.
