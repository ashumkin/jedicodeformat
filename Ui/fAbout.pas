{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is fAbout.pas, released April 2000.
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

unit fAbout;

interface

uses
  { delphi }
  Windows, Classes, Graphics, Forms, Controls, StdCtrls,
  Buttons, ExtCtrls,
   { JVCL }
  JvLabel, JvExControls, JvComponent;

type
  TfrmAboutBox = class(TForm)
    bbOK: TBitBtn;
    pnlClient: TPanel;
    imgOpenSource: TImage;
    mWarning: TMemo;
    mWhat: TMemo;
    lblMPL: TJvLabel;
    hlHomePage: TJvLabel;

    procedure FormCreate(Sender: TObject);
    procedure imgOpenSourceClick(Sender: TObject);
    procedure lblMPLClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
  private
  public
  end;


implementation

{$R *.DFM}

uses
  { delphi }URLMon,
  { jcl }JclStrings,
  { local }VersionConsts, JCFHelp;

procedure ShowURL(const ps: string);
var
  lws: widestring;
begin
  lws := ps;
  HLinkNavigateString(nil, pWideChar(lws));
end;


procedure TfrmAboutBox.imgOpenSourceClick(Sender: TObject);
begin
  ShowURL('http://www.delphi-jedi.org')
end;

procedure TfrmAboutBox.lblMPLClick(Sender: TObject);
begin
  ShowURL(lblMPL.Caption)
end;

procedure TfrmAboutBox.FormCreate(Sender: TObject);
var
  ls: string;
begin
  inherited;

  // show the version from the program constant
  ls := mWhat.Text;
  StrReplace(ls, '%VERSION%', PROGRAM_VERSION);
  StrReplace(ls, '%DATE%', PROGRAM_DATE);
  mWhat.Text := ls;

  hlHomePage.Caption := 'Find more information on the web at ' + PROGRAM_HOME_PAGE;
  hlHomePage.Url     := PROGRAM_HOME_PAGE;
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
    Application.HelpContext(HELP_MAIN);

end;

end.
