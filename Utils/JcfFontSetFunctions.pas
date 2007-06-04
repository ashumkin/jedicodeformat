unit JcfFontSetFunctions;

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code

The Original Code is JcfFontSetFunctions, released May 2007.
The Initial Developer of the Original Code is Jean-Fabien Connault.
Portions created by Anthony Steele are Copyright (C) 1999-2007 Anthony Steele.
All Rights Reserved.
Contributor(s):
SetObjectFontToSystemFont by Jean-Fabien Connault
Integrated to JCF by Anthony Steele

The contents of this file are subject to the Mozilla Public License Version 1.1
(the "License"). you may not use this file except in compliance with the License.
You may obtain a copy of the License at http://www.mozilla.org/NPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied.
See the License for the specific language governing rights and limitations
under the License.
------------------------------------------------------------------------------*)
{*)}

interface

type
     TFontType  = (ftAuto, ftCaption, ftContent);

procedure SetObjectFontToSystemFont(AObject: TObject; FontType: TFontType = ftAuto);

implementation

uses
  { delphi }
  StdCtrls, ComCtrls, Graphics, TypInfo,
  { jcl }
  JclSysUtils, JclSysInfo;

procedure SetCaptionFont(AObjectFont: TFont);
begin
  if IsWinVista then
  //if IsWinVista or IsWinServer2008 then
  begin
    AObjectFont.Name := 'Segoe UI';
    AObjectFont.Size := 9;
  end
  else if IsWinXP or IsWin2k or IsWin2003 then
  begin
    // MS Shell Dlg 2
    AObjectFont.Name := 'Tahoma';
    AObjectFont.Size := 8;
  end
  else
  begin
    // MS Shell Dlg
    AObjectFont.Name := 'MS Sans Serif';
    AObjectFont.Size := 8;
  end;
end;

procedure SetContentFont(AObjectFont: TFont);
begin
  //if IsWinVista or IsWinServer2008 then
  if IsWinVista then
  begin
    AObjectFont.Name := 'Calibri';
    AObjectFont.Size := 9;
  end
  else if IsWinXP or IsWin2k or IsWin2003 then
  begin
    // MS Shell Dlg 2
    AObjectFont.Name := 'Verdana';
    AObjectFont.Size := 8;
  end
  else
  begin
    // MS Shell Dlg
    AObjectFont.Name := 'Courrier New';
    AObjectFont.Size := 8;
  end;
end;


procedure SetObjectFontToSystemFont(AObject: TObject; FontType: TFontType);
var
  AObjectFont: TFont;
  AFontType:   TFontType;
begin
  if (AObject.ClassType = TFont) then
    AObjectFont := TFont(AObject)
  else
    AObjectFont := TFont(GetObjectProp(AObject, 'Font', TFont));

  if (FontType = ftAuto) then
  begin
    if (AObject.ClassType = TMemo) or (AObject.ClassType = TRichEdit) then
      AFontType := ftContent
    else
      AFontType := ftCaption;
  end
  else
    AFontType := FontType;

  if (AFontType = ftCaption) then
  begin
    SetCaptionFont(AObjectFont);
  end
  else if (AFontType = ftContent) then
  begin
    SetContentFont(AObjectFont);
  end;

end;


end.
