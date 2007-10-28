unit TestAsmOptionsIndents;

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code

The Original Code is TestAsmOptionsIndents, released October 2007.
The Initial Developer of the Original Code is Anthony Steele.
Portions created by Anthony Steele are Copyright (C) 2007 Anthony Steele.
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

interface

{ test asm options for indents }

uses
  TestFrameWork,
  BaseTestProcess, SettingsTypes, SetTransform;

type

  TTestAsmOptionsIndents = class(TBaseTestProcess)
  private
  published
  end;

implementation

uses
  JclStrings,
  JCFSettings, SetAsm;

initialization
  TestFramework.RegisterTest('Processes', TTestAsmOptionsIndents.Suite);

end.
