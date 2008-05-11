unit TestPreprocessorTokens;

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code

The Original Code is TestPreprocessorTokens
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

{$I JcfGlobal.inc}

interface

uses
  TestFrameWork,
  PreProcessorParseTree;

type
  TTestPreprocessorTokens = class(TTestCase)
  private
    fcPreProcessor: TPreProcessorParseTree;

    function EvalPreProcessorExpression(const ps: string): boolean;

    procedure TestTrue(const ps: string);
    procedure TestFalse(const ps: string);
    procedure TestExcept(const ps: string);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestTrueExpressions;
    procedure TestFalseExpressions;
    procedure TestIllegalExpressions;
  end;


implementation

uses
  SysUtils,
  JcfRegistrySettings,
  TestConstants;


function TTestPreprocessorTokens.EvalPreProcessorExpression(const ps: string): boolean;
begin
  Result := fcPreProcessor.EvalPreProcessorExpression(ps);
end;

procedure TTestPreprocessorTokens.TestFalse(const ps: string);
begin
  Check( not EvalPreProcessorExpression(ps), ps + ' is true');
end;

procedure TTestPreprocessorTokens.TestTrue(const ps: string);
begin
  Check(EvalPreProcessorExpression(ps), ps + ' is false');
end;

procedure TTestPreprocessorTokens.TestExcept(const ps: string);
begin
  try
    EvalPreProcessorExpression(ps);
    Fail('No exception in ' + ps);
  except
    // ok
  end;
end;

procedure TTestPreprocessorTokens.Setup;
var
  lsSettingsFileName: string;
begin
  inherited;

  lsSettingsFileName := GetTestSettingsFileName;
  Check(FileExists(lsSettingsFileName), 'Settings file ' + lsSettingsFileName +
    ' not found');

  GetRegSettings.FormatConfigFileName := lsSettingsFileName;

  fcPreProcessor := TPreProcessorParseTree.Create;

  fcPreProcessor.AddDefinedSymbol('foo');
  fcPreProcessor.AddDefinedSymbol('bar');
  fcPreProcessor.AddDefinedSymbol('fish');
end;

procedure TTestPreprocessorTokens.TearDown;
begin
  inherited;

  FreeAndNil(fcPreProcessor);
end;


procedure TTestPreprocessorTokens.TestFalseExpressions;
begin
  // should return false

  // foo and bar are defined, soy and spon aren't
  TestFalse('defined(spon)');
  TestFalse('defined(spon) or defined(soy)');
  TestFalse('defined(foo) and defined(soy)');

  TestFalse('not defined(foo) and (defined(bar))');
  TestFalse('not defined(foo)');
  TestFalse('not not not defined(foo)');
  TestFalse('false');
  TestFalse('(false)');
  TestFalse('not true');
  TestFalse('true and false');
  TestFalse('(not true) and (not false)');
  TestFalse('(not true) or false');
  TestFalse('not true or false');
  TestFalse('defined(foo) and false');
end;


procedure TTestPreprocessorTokens.TestTrueExpressions;
begin

  // should return true if these are defined
  TestTrue('defined(foo)');
  TestTrue('defined(foo) and defined(bar)');
  TestTrue('defined(foo) and defined(bar) and defined(fish)');
  TestTrue('defined(foo) or defined(bar) or defined(fish)');
  TestTrue('(defined(foo))');
  TestTrue('((defined(foo)))');
  TestTrue('(((defined(foo))))');
  TestTrue('(((defined(foo)))) or defined(bar)');
  TestTrue('(defined(foo) or defined(bar))');
  TestTrue('(((defined(foo)) or defined(bar)))');
  TestTrue('not not defined(foo)');

  TestTrue('true');
  TestTrue('(true)');
  TestTrue('((true))');
  TestTrue('(((true)))');
  TestTrue('true or false');
  TestTrue('true and true');
  TestTrue('true and (true or false)');
  TestTrue('not not true');
  TestTrue('not false');

  TestTrue('defined(foo) or false');
  TestTrue('defined(foo) and true');
end;


procedure TTestPreprocessorTokens.TestIllegalExpressions;
begin
  // should not parse at all
  TestExcept('saef');
  TestExcept('saefdsafsd fsdaf');
  TestExcept('saefdsafsd fsdaf asdf adsf');
  TestExcept('true and');
  TestExcept('true true');
  TestExcept('true false');
  TestExcept('true)');
  TestExcept('(true');
  TestExcept('(true))');
  TestExcept('((true)');
  TestExcept('');
  TestExcept('(');
  TestExcept(')');
  TestExcept('and');
  TestExcept('or');
  TestExcept('foo');
  TestExcept('defined');
  TestExcept('defined foo');
  TestExcept('defined(foo');
  TestExcept('defined(foo(');
  TestExcept('(defined(foo)))');
end;

initialization
  TestFramework.RegisterTest(TTestPreprocessorTokens.Suite);
end.
