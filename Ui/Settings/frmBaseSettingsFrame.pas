{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is frmBaseSettingsFrame.pas, released April 2000.
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

unit frmBaseSettingsFrame;

{ SetttingsFrame
  AFS 29 Dec 1999
  Subclass of TFrame with common interface for settings }


interface

uses
  {delphi } Windows, Messages, SysUtils, Classes, Graphics, Controls,
  Forms, Dialogs,
  { local } frDrop, JCFSettings;

type
  TfrSettingsFrame = class(TFrameDrop)
  private
    // event handler
    fcOnChange: TNotifyEvent;

  protected
    fiHelpContext: THelpContext;

    procedure CallOnChange;

  public
    constructor Create(AOwner: TComponent); override;

    procedure Read; virtual; abstract;
    procedure Write; virtual; abstract;

    procedure ShowContextHelp;

    property OnChange: TNotifyEvent read fcOnChange write fcOnChange;
  end;

  TSettingsFrameClass = class of TfrSettingsFrame;

const
  GUI_PAD = 3;

implementation

uses JCFHelp;

{$R *.DFM}


constructor TfrSettingsFrame.Create(Aowner: TComponent);
begin
  inherited;
  fcOnChange := nil;
  fiHelpContext := 0;
end;

procedure TfrSettingsFrame.CallOnChange;
begin
  if Assigned(fcOnChange) then
    fcOnChange(self);
end;

procedure TfrSettingsFrame.ShowContextHelp;
var
  liHelpContext: integer;
begin
  liHelpContext := fiHelpContext;
  if liHelpContext <= 0 then
    liHelpContext := HELP_MAIN;

  Application.HelpContext(liHelpContext);
end;

end.