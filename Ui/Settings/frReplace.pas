{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code

The Original Code is frReplace.pas, released April 2000.
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

unit frReplace;

interface

uses
    { delphi }
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls,
    { local }
  JvMemo, frmBaseSettingsFrame;

type
  TfReplace = class(TfrSettingsFrame)
    cbEnable: TCheckBox;
    mWords: TJvMemo;
    lblWordList: TLabel;
    procedure cbEnableClick(Sender: TObject);
    procedure FrameResize(Sender: TObject);
  private
    { Private declarations }
  public
    constructor Create(AOwner: TComponent); override;

    procedure Read; override;
    procedure Write; override;

  end;

implementation

{$R *.DFM}

uses Jcfhelp;

{ TfReplace }

constructor TfReplace.Create(AOwner: TComponent);
begin
  inherited;
  fiHelpContext := HELP_FIND_AND_REPLACE;
end;

procedure TfReplace.Read;
begin
  with Settings.Replace do
  begin
    cbEnable.Checked := Enabled;
    mWords.Lines.Assign(Words);
  end;
  cbEnableClick(nil);
end;

procedure TfReplace.Write;
begin
  with Settings.Replace do
  begin
    Enabled := cbEnable.Checked;
    Words.Assign(mWords.Lines);
    SplitWords;
  end;
end;

{-------------------------------------------------------------------------------
  event handlers }

procedure TfReplace.cbEnableClick(Sender: TObject);
begin
  mWords.Enabled := cbEnable.Checked;
end;

procedure TfReplace.FrameResize(Sender: TObject);
const
  PAD = 4;
begin
  mWords.Height := ClientHeight - (lblWordList.Top + lblWordList.Height + PAD);
end;


end.
