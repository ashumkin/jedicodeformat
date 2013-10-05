unit TestBreakConditionalCompilation;
{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code

The Original Code is TestBreakConditionalCompilation, released May 2005.
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

Alternatively, the contents of this file may be used under the terms of
the GNU General Public License Version 2 or later (the "GPL") 
See http://www.gnu.org/licenses/gpl.html
------------------------------------------------------------------------------*)
{*)}

{$I JcfGlobal.inc}

interface

{ AFS 26 April 03
  test the linebreaking around $IFDEF options }

uses
  { delphi }
  Classes,
  { DUnit }
  BaseTestProcess,
  { local }
  Converter, SettingsTypes, SetReturns;

type
  TTestBreakConditionalCompilation = class(TBaseTestProcess)
  private
    { the input file }
    fsFileIn: string;

    { the saved settings }
    feSaveRebreakLines: TWhenToRebreakLines;
    fbPreProcessorEnabled: boolean;

    feSaveBeforeCompilerDirectUses: TTriOptionStyle;
    feSaveBeforeCompilerDirectStatements: TTriOptionStyle;
    feSaveBeforeCompilerDirectGeneral: TTriOptionStyle;
    feSaveAfterCompilerDirectUses: TTriOptionStyle;
    feSaveAfterCompilerDirectStatements: TTriOptionStyle;
    feSaveAfterCompilerDirectGeneral: TTriOptionStyle;


    procedure CheckReplace(var ps: string; const psFind, psReplace: string);

  protected
    procedure SetUp; override;
    procedure TearDown; override;

  published
    procedure TestNoChange;
    procedure TestUsesBreakBeforeAdd;
    procedure TestUsesBreakBeforeRemove;

    procedure TestUsesBreakAfterAdd;
    procedure TestUsesBreakAfterRemove;

    procedure TestCodeBreakBeforeAdd;
    procedure TestCodeBreakBeforeRemove;

    procedure TestCodeBreakAfterAdd;
    procedure TestCodeBreakAfterRemove;

    procedure TestGeneralBreakBeforeAdd;
    procedure TestGeneralBreakBeforeRemove;
    procedure TestGeneralBreakAfterAdd;
    procedure TestGeneralBreakAfterRemove;
  end;

implementation

uses
  { delphi }
  SysUtils,
  { DUnit }
  TestFramework,
  { local }
  JcfStringUtils,
  JcfUnicodeFiles,
  JCFSettings;


procedure TTestBreakConditionalCompilation.Setup;
const
  IN_FILE_NAME = '..\..\Test\TestCases\TestCondCompBreaks.pas';
var
  leContentType: TFileContentType;
  lsFileContents: WideString;
begin
  inherited;

  if FileExists(IN_FILE_NAME) then
  begin
    ReadTextFile(IN_FILE_NAME, lsFileContents, leContentType);
    fsFileIn := string(lsFileContents);
  end
  else
  begin
    fsFileIn := '';
  end;

  { store settings }
  feSaveRebreakLines := JcfFormatSettings.Returns.RebreakLines;
 JcfFormatSettings.Returns.RebreakLines := rbOff;

  fbPreProcessorEnabled := JcfFormatSettings.PreProcessor.Enabled;
 JcfFormatSettings.PreProcessor.Enabled := False;

  feSaveBeforeCompilerDirectUses := JcfFormatSettings.Returns.BeforeCompilerDirectUses;
  feSaveBeforeCompilerDirectStatements := JcfFormatSettings.Returns.BeforeCompilerDirectStatements;
  feSaveBeforeCompilerDirectGeneral := JcfFormatSettings.Returns.BeforeCompilerDirectGeneral;
  feSaveAfterCompilerDirectUses := JcfFormatSettings.Returns.AfterCompilerDirectUses;
  feSaveAfterCompilerDirectStatements := JcfFormatSettings.Returns.AfterCompilerDirectStatements;
  feSaveAfterCompilerDirectGeneral := JcfFormatSettings.Returns.AfterCompilerDirectGeneral;
end;

procedure TTestBreakConditionalCompilation.TearDown;
begin
  { restore settings }
 JcfFormatSettings.Returns.RebreakLines := feSaveRebreakLines;
 JcfFormatSettings.PreProcessor.Enabled := fbPreProcessorEnabled;

 JcfFormatSettings.Returns.BeforeCompilerDirectUses := feSaveBeforeCompilerDirectUses;
 JcfFormatSettings.Returns.BeforeCompilerDirectStatements := feSaveBeforeCompilerDirectStatements;
 JcfFormatSettings.Returns.BeforeCompilerDirectGeneral := feSaveBeforeCompilerDirectGeneral;
 JcfFormatSettings.Returns.AfterCompilerDirectUses := feSaveAfterCompilerDirectUses;
 JcfFormatSettings.Returns.AfterCompilerDirectStatements := feSaveAfterCompilerDirectStatements;
 JcfFormatSettings.Returns.AfterCompilerDirectGeneral := feSaveAfterCompilerDirectGeneral;

  inherited;
end;

procedure TTestBreakConditionalCompilation.CheckReplace(var ps: string;
  const psFind, psReplace: string);
begin
 Check(Pos(psFind, ps) > 0, string('String not found: ' + psFind));
 StrReplace(ps, psFind, psReplace, [rfIgnoreCase]);

 if Pos(psFind, psReplace) = 0 then
   Check(Pos(psFind, ps) = 0, string('String still found' + psFind));
end;

procedure TTestBreakConditionalCompilation.TestNoChange;
begin
  Check(fsFileIn <> '', 'No input file');

 JcfFormatSettings.Returns.BeforeCompilerDirectUses := eLeave;
 JcfFormatSettings.Returns.BeforeCompilerDirectStatements := eLeave;
 JcfFormatSettings.Returns.BeforeCompilerDirectGeneral := eLeave;
 JcfFormatSettings.Returns.AfterCompilerDirectUses := eLeave;
 JcfFormatSettings.Returns.AfterCompilerDirectStatements := eLeave;
 JcfFormatSettings.Returns.AfterCompilerDirectGeneral := eLeave;

  TestFormatResult(string(fsFileIn), string(fsFileIn));
end;

procedure TTestBreakConditionalCompilation.TestUsesBreakBeforeAdd;
var
  lsFileOut: string;
begin
  Check(fsFileIn <> '', 'No input file');

 JcfFormatSettings.Returns.BeforeCompilerDirectUses := eAlways;

 JcfFormatSettings.Returns.BeforeCompilerDirectStatements := eLeave;
 JcfFormatSettings.Returns.BeforeCompilerDirectGeneral := eLeave;
 JcfFormatSettings.Returns.AfterCompilerDirectUses := eLeave;
 JcfFormatSettings.Returns.AfterCompilerDirectStatements := eLeave;
 JcfFormatSettings.Returns.AfterCompilerDirectGeneral := eLeave;

  lsFileOut := fsFileIn;
  CheckReplace(lsFileOut, ' {$IFDEF BAR_RAISED}', NativeLineBreak + '{$IFDEF BAR_RAISED}');
  CheckReplace(lsFileOut, ' {$ENDIF} Dialogs', NativeLineBreak + '{$ENDIF} Dialogs');
  TestFormatResult(string(fsFileIn), string(lsFileOut));
end;


procedure TTestBreakConditionalCompilation.TestUsesBreakBeforeRemove;
var
  lsFileOut: string;
begin
  Check(fsFileIn <> '', 'No input file');

 JcfFormatSettings.Returns.BeforeCompilerDirectUses := eNever;

 JcfFormatSettings.Returns.BeforeCompilerDirectStatements := eLeave;
 JcfFormatSettings.Returns.BeforeCompilerDirectGeneral := eLeave;
 JcfFormatSettings.Returns.AfterCompilerDirectUses := eLeave;
 JcfFormatSettings.Returns.AfterCompilerDirectStatements := eLeave;
 JcfFormatSettings.Returns.AfterCompilerDirectGeneral := eLeave;

  lsFileOut := fsFileIn;
  CheckReplace(fsFileIn, '{$IFDEF BAR_RAISED}', NativeLineBreak + '{$IFDEF BAR_RAISED}');
  CheckReplace(fsFileIn, '{$ENDIF} Dialogs', NativeLineBreak + '{$ENDIF} Dialogs');
  TestFormatResult(string(fsFileIn), string(lsFileOut));
end;

procedure TTestBreakConditionalCompilation.TestUsesBreakAfterAdd;
var
  lsFileOut: string;
begin
  Check(fsFileIn <> '', 'No input file');

 JcfFormatSettings.Returns.AfterCompilerDirectUses := eAlways;

 JcfFormatSettings.Returns.BeforeCompilerDirectUses := eLeave;
 JcfFormatSettings.Returns.BeforeCompilerDirectStatements := eLeave;
 JcfFormatSettings.Returns.BeforeCompilerDirectGeneral := eLeave;
 JcfFormatSettings.Returns.AfterCompilerDirectStatements := eLeave;
 JcfFormatSettings.Returns.AfterCompilerDirectGeneral := eLeave;

  lsFileOut := fsFileIn;
  CheckReplace(lsFileOut, '{$IFDEF BAR_RAISED}', '{$IFDEF BAR_RAISED}' + NativeLineBreak + ' ');
  CheckReplace(lsFileOut, 'Classes, {$ENDIF}', 'Classes, {$ENDIF}' + NativeLineBreak + ' ');
  TestFormatResult(string(fsFileIn), string(lsFileOut));
end;

procedure TTestBreakConditionalCompilation.TestUsesBreakAfterRemove;
var
  lsFileOut: string;
begin
  Check(fsFileIn <> '', 'No input file');

 JcfFormatSettings.Returns.AfterCompilerDirectUses := eNever;

 JcfFormatSettings.Returns.BeforeCompilerDirectUses := eLeave;
 JcfFormatSettings.Returns.BeforeCompilerDirectStatements := eLeave;
 JcfFormatSettings.Returns.BeforeCompilerDirectGeneral := eLeave;
 JcfFormatSettings.Returns.AfterCompilerDirectStatements := eLeave;
 JcfFormatSettings.Returns.AfterCompilerDirectGeneral := eLeave;

  lsFileOut := string(fsFileIn);
  CheckReplace(fsFileIn, '{$IFDEF BAR_RAISED} ', '{$IFDEF BAR_RAISED}' + NativeLineBreak + ' ');
  CheckReplace(fsFileIn, 'Classes, {$ENDIF} ', 'Classes, {$ENDIF}' + NativeLineBreak + ' ');
  TestFormatResult(string(fsFileIn), lsFileOut);
end;

procedure TTestBreakConditionalCompilation.TestCodeBreakBeforeAdd;
var
  lsFileOut: string;
begin
  Check(fsFileIn <> '', 'No input file');

 JcfFormatSettings.Returns.BeforeCompilerDirectStatements := eAlways;

 JcfFormatSettings.Returns.BeforeCompilerDirectUses := eLeave;
 JcfFormatSettings.Returns.BeforeCompilerDirectGeneral := eLeave;
 JcfFormatSettings.Returns.AfterCompilerDirectUses := eLeave;
 JcfFormatSettings.Returns.AfterCompilerDirectStatements := eLeave;
 JcfFormatSettings.Returns.AfterCompilerDirectGeneral := eLeave;

  lsFileOut := fsFileIn;

  CheckReplace(fsFileIn,  NativeLineBreak + '  {$IFDEF HAS_STUFF}', '{$IFDEF HAS_STUFF}');
  CheckReplace(fsFileIn, NativeLineBreak + '  {$ENDIF}', '{$ENDIF}');

  CheckReplace(lsFileOut,  '  {$IFDEF HAS_STUFF}', '{$IFDEF HAS_STUFF}');
  CheckReplace(lsFileOut, '  {$ENDIF}', '{$ENDIF}');

  TestFormatResult(string(fsFileIn), string(lsFileOut));
end;


procedure TTestBreakConditionalCompilation.TestCodeBreakBeforeRemove;
var
  lsFileOut: string;
begin
  Check(fsFileIn <> '', 'No input file');

 JcfFormatSettings.Returns.BeforeCompilerDirectStatements := eNever;

 JcfFormatSettings.Returns.BeforeCompilerDirectUses := eLeave;
 JcfFormatSettings.Returns.BeforeCompilerDirectGeneral := eLeave;
 JcfFormatSettings.Returns.AfterCompilerDirectUses := eLeave;
 JcfFormatSettings.Returns.AfterCompilerDirectStatements := eLeave;
 JcfFormatSettings.Returns.AfterCompilerDirectGeneral := eLeave;

  lsFileOut := fsFileIn;

  CheckReplace(lsFileOut,  NativeLineBreak + '  {$IFDEF HAS_STUFF}', '  {$IFDEF HAS_STUFF}');
  CheckReplace(lsFileOut, NativeLineBreak + '  {$ENDIF}', '  {$ENDIF}');
  TestFormatResult(string(fsFileIn), string(lsFileOut));
end;

procedure TTestBreakConditionalCompilation.TestCodeBreakAfterAdd;
var
  lsPrefix: string;
  lsFileOut: string;
begin
  Check(fsFileIn <> '', 'No input file');

 JcfFormatSettings.Returns.AfterCompilerDirectStatements := eAlways;

 JcfFormatSettings.Returns.BeforeCompilerDirectStatements := eLeave;
 JcfFormatSettings.Returns.BeforeCompilerDirectUses := eLeave;
 JcfFormatSettings.Returns.BeforeCompilerDirectGeneral := eLeave;
 JcfFormatSettings.Returns.AfterCompilerDirectUses := eLeave;
 JcfFormatSettings.Returns.AfterCompilerDirectGeneral := eLeave;

  lsFileOut := string(fsFileIn);

  lsPrefix := '{$IFDEF HAS_STUFF}';
  CheckReplace(fsFileIn, lsPrefix + NativeLineBreak, lsPrefix);

  lsPrefix := 'SomeStuff;' + NativeLineBreak + '  {$ENDIF}';
  CheckReplace(fsFileIn, lsPrefix + NativeLineBreak, lsPrefix);

  TestFormatResult(string(fsFileIn), lsFileOut);
end;



procedure TTestBreakConditionalCompilation.TestCodeBreakAfterRemove;
var
  lsFileOut: string;
  lsPrefix: string;
begin
  Check(fsFileIn <> '', 'No input file');

 JcfFormatSettings.Returns.AfterCompilerDirectStatements := eNever;

 JcfFormatSettings.Returns.BeforeCompilerDirectStatements := eLeave;
 JcfFormatSettings.Returns.BeforeCompilerDirectUses := eLeave;
 JcfFormatSettings.Returns.BeforeCompilerDirectGeneral := eLeave;
 JcfFormatSettings.Returns.AfterCompilerDirectUses := eLeave;
 JcfFormatSettings.Returns.AfterCompilerDirectGeneral := eLeave;

  lsFileOut := fsFileIn;

  lsPrefix := '{$IFDEF HAS_STUFF}';
  CheckReplace(lsFileOut, lsPrefix + NativeLineBreak, lsPrefix);
  lsPrefix := 'SomeStuff;' + NativeLineBreak + '  {$ENDIF}';
  CheckReplace(lsFileOut, lsPrefix + NativeLineBreak, lsPrefix);

  TestFormatResult(string(fsFileIn), string(lsFileOut));
end;


procedure TTestBreakConditionalCompilation.TestGeneralBreakBeforeAdd;
var
  lsFind, lsReplace: string;
  lsFileOut: string;
begin
  Check(fsFileIn <> '', 'No input file');

 JcfFormatSettings.Returns.BeforeCompilerDirectGeneral := eAlways;

 JcfFormatSettings.Returns.BeforeCompilerDirectStatements := eLeave;
 JcfFormatSettings.Returns.BeforeCompilerDirectUses := eLeave;
 JcfFormatSettings.Returns.AfterCompilerDirectUses := eLeave;
 JcfFormatSettings.Returns.AfterCompilerDirectGeneral := eLeave;
 JcfFormatSettings.Returns.AfterCompilerDirectStatements := eLeave;

  lsFileOut := fsFileIn;

  lsFind := NativeLineBreak + NativeLineBreak + '{$IFDEF SYM2}';
  lsReplace := '{$IFDEF SYM2}';
  CheckReplace(fsFileIn, lsFind, lsReplace);

  lsFind := '''Black socks''; {$ENDIF}';
  lsReplace := '''Black socks'';' + NativeLineBreak + NativeLineBreak + '{$ENDIF}';
  CheckReplace(lsFileOut, lsFind, lsReplace);

  lsFind := '{$IFDEF SYM2} ';
  lsReplace := '{$IFDEF SYM2}' + NativeLineBreak + NativeLineBreak;
  CheckReplace(lsFileOut, lsFind, lsReplace);

  TestFormatResult(string(fsFileIn), string(lsFileOut));
end;

procedure TTestBreakConditionalCompilation.TestGeneralBreakAfterRemove;
var
  lsFind, lsReplace: string;
  lsFileOut: string;
begin
  Check(fsFileIn <> '', 'No input file');

 JcfFormatSettings.Returns.AfterCompilerDirectGeneral := eNever;

 JcfFormatSettings.Returns.BeforeCompilerDirectStatements := eLeave;
 JcfFormatSettings.Returns.BeforeCompilerDirectUses := eLeave;
 JcfFormatSettings.Returns.BeforeCompilerDirectGeneral := eLeave;
 JcfFormatSettings.Returns.AfterCompilerDirectUses := eLeave;
 JcfFormatSettings.Returns.AfterCompilerDirectStatements := eLeave;

  lsFileOut := string(fsFileIn);

  lsFind := '{$IFDEF SYM2}';
  lsReplace := '{$IFDEF SYM2}' + NativeLineBreak + NativeLineBreak;
  CheckReplace(fsFileIn, lsFind, lsReplace);

  (*
  lsReplace := '''Black socks'';' + NativeLineBreak + '{$ENDIF}';
  lsFind := '''Black socks''; {$ENDIF}';
  CheckReplace(fsFileIn, lsFind, lsReplace);
  *)
  
  TestFormatResult(string(fsFileIn), lsFileOut);
end;

procedure TTestBreakConditionalCompilation.TestGeneralBreakAfterAdd;
var
  lsFind, lsReplace: string;
  lsFileOut: string;
begin
  Check(fsFileIn <> '', 'No input file');

 JcfFormatSettings.Returns.AfterCompilerDirectGeneral := eAlways;

 JcfFormatSettings.Returns.BeforeCompilerDirectGeneral := eLeave;
 JcfFormatSettings.Returns.BeforeCompilerDirectStatements := eLeave;
 JcfFormatSettings.Returns.BeforeCompilerDirectUses := eLeave;
 JcfFormatSettings.Returns.AfterCompilerDirectUses := eLeave;
 JcfFormatSettings.Returns.AfterCompilerDirectStatements := eLeave;

  lsFileOut := fsFileIn;
  lsFind := '{$IFDEF SYM2} ';
  lsReplace := '{$IFDEF SYM2}' + NativeLineBreak;
  CheckReplace(lsFileOut, lsFind, lsReplace);

  TestFormatResult(string(fsFileIn), string(lsFileOut));
end;

procedure TTestBreakConditionalCompilation.TestGeneralBreakBeforeRemove;
var
  lsFind, lsReplace: string;
  lsFileOut: string;
begin
  Check(fsFileIn <> '', 'No input file');

 JcfFormatSettings.Returns.AfterCompilerDirectGeneral := eNever;

 JcfFormatSettings.Returns.BeforeCompilerDirectGeneral := eLeave;
 JcfFormatSettings.Returns.BeforeCompilerDirectStatements := eLeave;
 JcfFormatSettings.Returns.BeforeCompilerDirectUses := eLeave;
 JcfFormatSettings.Returns.AfterCompilerDirectUses := eLeave;
 JcfFormatSettings.Returns.AfterCompilerDirectStatements := eLeave;

  lsFileOut := fsFileIn;
  lsFind := '{$IFDEF SYM2} ';
  lsReplace := '{$IFDEF SYM2} ' + NativeLineBreak;
  CheckReplace(fsFileIn, lsFind, lsReplace);

  TestFormatResult(string(fsFileIn), string(lsFileOut));
end;

initialization
  TestFramework.RegisterTest('Processes', TTestBreakConditionalCompilation.Suite);
end.
