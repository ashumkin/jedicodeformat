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
  private
    fbFileDrop: Boolean;
  protected

  public
    constructor Create(AOwner: TComponent); override;

    procedure Read; override;
    procedure Write; override;

  end;

implementation

{$R *.DFM}


this unit is obsolete

uses
  { delphi } 
  { jcl } JclStrings, JclFileUtils,
  { local } JcfMiscFunctions, JcfHelp,
  JcfSettings, JcfRegistrySettings;

constructor TfExcludeFiles.Create(AOwner: TComponent);
begin
  inherited;
  IsDropActive := True;
  fiHelpContext := HELP_EXCLUSIONS;
end;

procedure TfExcludeFiles.Read;
begin
end;

procedure TfExcludeFiles.Write;
begin
end;


end.
