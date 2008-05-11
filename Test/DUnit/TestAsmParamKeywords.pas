unit TestAsmParamKeywords;

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is TestAsmParamKeywords, releasedNovember 2007.
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

Alternatively, the contents of this file may be used under the terms of
the GNU General Public License Version 2 or later (the "GPL") 
See http://www.gnu.org/licenses/gpl.html
------------------------------------------------------------------------------*)
{*)}

{$I JcfGlobal.inc}

interface

uses
  TestFrameWork, BaseTestProcess;

type
  TTestAsmParamKeywords = class(TBaseTestProcess)
  private

  public
  published
    procedure TestSuccess;
    procedure TestSuccessLower;
    procedure TestSuccessMixed;

    procedure TestFailure;

  end;


implementation

uses
  AsmKeywords;

{ TTestAsmParamKeywords }

procedure TTestAsmParamKeywords.TestFailure;
begin
  CheckFalse(IsAsmParamKeyword(''));
  CheckFalse(IsAsmParamKeyword('foo'));
  CheckFalse(IsAsmParamKeyword('bar'));
  CheckFalse(IsAsmParamKeyword('waggawagga'));
  CheckFalse(IsAsmParamKeyword('the quick brown asm jumped over the lazy carrot'));
  CheckFalse(IsAsmParamKeyword('fish'));
end;

procedure TTestAsmParamKeywords.TestSuccess;
begin
  CheckTrue(IsAsmParamKeyword('RAX'));
  CheckTrue(IsAsmParamKeyword('RCX'));
  CheckTrue(IsAsmParamKeyword('FS'));
  CheckTrue(IsAsmParamKeyword('R8W'));
  CheckTrue(IsAsmParamKeyword('MM3'));
  CheckTrue(IsAsmParamKeyword('XMM12'));
end;

procedure TTestAsmParamKeywords.TestSuccessLower;
begin
  CheckTrue(IsAsmParamKeyword('rax'));
  CheckTrue(IsAsmParamKeyword('rcx'));
  CheckTrue(IsAsmParamKeyword('fs'));
  CheckTrue(IsAsmParamKeyword('r8w'));
  CheckTrue(IsAsmParamKeyword('mm3'));
  CheckTrue(IsAsmParamKeyword('xmm12'));
end;

procedure TTestAsmParamKeywords.TestSuccessMixed;
begin
  CheckTrue(IsAsmParamKeyword('Rax'));
  CheckTrue(IsAsmParamKeyword('rcX'));
  CheckTrue(IsAsmParamKeyword('Fs'));
  CheckTrue(IsAsmParamKeyword('R8w'));
  CheckTrue(IsAsmParamKeyword('mM3'));
  CheckTrue(IsAsmParamKeyword('XmM12'));
end;

initialization
  TestFramework.RegisterTest('Procs', TTestAsmParamKeywords.Suite);
end.
