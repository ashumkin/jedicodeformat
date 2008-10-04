{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is fAbout.pas, released April 2000.
The Initial Developer of the Original Code is Anthony Steele.
Portions created by Anthony Steele are Copyright (C) 1999-2008 Anthony Steele.
All Rights Reserved.
Contributor(s): Anthony Steele.

The contents of this file are subject to the Mozilla Public License Version 1.1
(the "License"). you may not use this file except in compliance with the License.
You may obtain a copy of the License at http://www.mozilla.org/NPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied.
See the License for the specific language governing rights and limitations 
under the License.

Alternatively, the contents of this file may be used under the terms of
the GNU General Public License Version 2 or later (the "GPL") 
See http://www.gnu.org/licenses/gpl.html
------------------------------------------------------------------------------*)
{*)}

unit fAbout;

{$I JcfGlobal.inc}

interface

uses
  { delphi }
  Windows, Classes, Forms, Graphics, Controls, StdCtrls,
  Buttons, ExtCtrls, ShellAPI, SysUtils,
  { JVCL }
  JvLabel, JvExControls, JvComponent;

type
  TfrmAboutBox = class(TForm)
    bbOK:          TBitBtn;
    pnlClient:     TPanel;
    imgOpenSource: TImage;
    mWarning:      TMemo;
    mWhat:         TMemo;
    lblMPL:        TJvLabel;
    hlHomePage:    TJvLabel;

    procedure FormCreate(Sender: TObject);
    procedure imgOpenSourceClick(Sender: TObject);
    procedure lblMPLClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
  private
  public
  end;

implementation

{$ifdef FPC}
  {$R *.lfm}
{$else}
  {$R *.dfm}
{$endif}

uses
  { delphi }URLMon,
  { jcl }JclAnsiStrings,
  { local }VersionConsts, JCFHelp, JcfFontSetFunctions;

procedure ShowURL(const ps: string);
var
  lws: WideString;
begin
  lws := ps;
  HLinkNavigateString(nil, pWideChar(lws));
end;


procedure TfrmAboutBox.imgOpenSourceClick(Sender: TObject);
begin
  ShowURL('http://www.delphi-jedi.org');
end;

procedure TfrmAboutBox.lblMPLClick(Sender: TObject);
begin
  ShowURL(lblMPL.Caption);
end;

procedure TfrmAboutBox.FormCreate(Sender: TObject);
var
  ls: AnsiString;
begin
  inherited;

  SetObjectFontToSystemFont(Self);

  // show the version from the program constant
  ls := mWhat.Text;
  StrReplace(ls, '%VERSION%', PROGRAM_VERSION);
  StrReplace(ls, '%DATE%', PROGRAM_DATE);
  mWhat.Text := ls;

  hlHomePage.Caption := 'Find more information on the web at : ' + PROGRAM_HOME_PAGE;
  hlHomePage.Url := PROGRAM_HOME_PAGE;
end;


procedure TfrmAboutBox.FormResize(Sender: TObject);
const
  SPACING = 8;
begin
  mWarning.Left  := SPACING;
  mWarning.Width := pnlClient.ClientWidth - (2 * SPACING);

  lblMPL.Left  := SPACING;
  lblMPL.Width := pnlClient.ClientWidth - (2 * SPACING);

  hlHomePage.Left  := SPACING;
  hlHomePage.Width := pnlClient.ClientWidth - (2 * SPACING);
end;

procedure TfrmAboutBox.FormKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  if Key = VK_F1 then
    try
      Application.HelpContext(HELP_MAIN);
    except
      if FileExists(Application.HelpFile) then
        ShellExecute(Handle, 'open', PChar(Application.HelpFile), nil, nil, SW_SHOWNORMAL);
    end;
end;

end.
