unit BaseTestProcess;

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is BaseTestProcess, released May 2003.
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

interface

uses
  Classes,
  JclStrings,
  TestFrameWork,
  BaseVisitor,
  Converter;

type
  TBaseTestProcess = class(TTestCase)
  private
    fcConvert: TConverter;
    fcMessages: TStringList;

    procedure OnStatusMessage(const psUnit, psMessage: string; const piY, piX: integer);

  protected

    procedure TestNoWarnings(const psUnit: string);
    procedure TestWarnings(const psUnit: string; const psWarningMatch: string); overload;
    procedure TestWarnings(const psUnit: string;
      const psWarningMatches: array of string); overload;
    procedure TestWarnings(const psUnit: string; const piMatchCount: integer;
      const psWarningMatches: array of string); overload;

    procedure TestProcessResult(processType: TTreeNodeVisitorType;
      const psIn, psOut: string);

    procedure TestFormatResult(const psIn, psOut: string);
    procedure TestFormatPartResult(const psIn, psOut: string; const piStart, piEnd: integer);

  protected
    procedure Setup; override;
    procedure TearDown; override;
  published

  end;

const
  INTERFACE_HEADER = 'unit Test;' + AnsiLineBreak +
    'interface' + AnsiLineBreak;
  UNIT_HEADER      = INTERFACE_HEADER +
    'implementation' + AnsiLineBreak;
  UNIT_FOOTER      = AnsiLineBreak + 'end.';

  SPACED_INTERFACE_HEADER = 'unit Test;' + AnsiLineBreak + AnsiLineBreak +
    'interface' + AnsiLineBreak + AnsiLineBreak;

  SPACED_UNIT_HEADER = SPACED_INTERFACE_HEADER +
    'implementation' + AnsiLineBreak;

implementation

uses SysUtils, Dialogs,
  TestConstants;

procedure TBaseTestProcess.Setup;
begin
  inherited;

  fcConvert := TConverter.Create;
  fcMessages := TStringList.Create;

  fcConvert.OnStatusMessage := OnStatusMessage;

  InitTestSettings;
end;

procedure TBaseTestProcess.TearDown;
begin
  FreeAndNil(fcConvert);

  FreeAndNil(fcMessages);
end;

procedure TBaseTestProcess.TestWarnings(const psUnit, psWarningMatch: string);
begin
  TestWarnings(psUnit, 1, [psWarningMatch]);
end;

procedure TBaseTestProcess.TestWarnings(const psUnit: string;
  const psWarningMatches: array of string);
var
  liCount: integer;
begin
  liCount := High(psWarningMatches) - Low(psWarningMatches) + 1;
  TestWarnings(psUnit, liCount, psWarningMatches);
end;

procedure TBaseTestProcess.TestWarnings(const psUnit: string;
  const piMatchCount: integer; const psWarningMatches: array of string);
var
  lbFound: boolean;
  liLoop:  integer;
begin
  fcMessages.Clear;
  fcConvert.InputCode := psUnit;
  fcConvert.Convert;

  // convert should work
  //CheckEquals(False, fcConvert.ConvertError, fcConvert.ConvertErrorMessage);
  // with messages
  CheckEquals(piMatchCount, fcMessages.Count, 'Wrong number of messages');

  for liLoop := Low(psWarningMatches) to High(psWarningMatches) do
  begin
    // containing certain text
    lbFound := (StrIPos(psWarningMatches[liLoop], fcMessages.Text) > 0);

    Check(lbFound, psWarningMatches[liLoop] + ' was not found in output ' +
      fcMessages.Text);
  end;

  fcConvert.Clear;
end;

procedure TBaseTestProcess.TestNoWarnings(const psUnit: string);
begin
  TestWarnings(psUnit, 0, []);
end;


function MarkReturns(const ps: string): string;
begin
  Result := ps;
  StrReplace(Result, AnsiLineBreak, '-q' + AnsiLineBreak, [rfReplaceAll]);
end;

function DiffText(ps1, ps2: string): string;
var
  liStartDif: integer;
begin
  liStartDif := 0;
  // strip same chars on start
  while (length(ps1) > 0) and (length(ps2) > 0) and (ps1[1] = ps2[1]) do
  begin
    ps1 := StrRestOf(ps1, 2);
    ps2 := StrRestOf(ps2, 2);
    inc(liStartDif);
  end;

  while (length(ps1) > 0) and (length(ps2) > 0) and
    (ps1[length(ps1)] = ps2[length(ps2)]) do
  begin
    ps1 := StrChopRight(ps1, 1);
    ps2 := StrChopRight(ps2, 1);
  end;

  Result := ps1 + '<->' + ps2 + AnsiLineBreak + 'at char ' + IntToStr(liStartDif);
end;

procedure TBaseTestProcess.TestProcessResult(processType: TTreeNodeVisitorType;
  const psIn, psOut: string);
begin
  // run just this process
  fcConvert.SingleProcess := processType;
  try
    TestFormatResult(psIn, psOut);
  finally
    fcConvert.SingleProcess := nil;
  end;

end;

procedure TBaseTestProcess.TestFormatResult(const psIn, psOut: string);
var
  lsOut: string;
begin
  fcMessages.Clear;
  fcConvert.InputCode := psIn;

  fcConvert.Convert;

  lsOut := fcConvert.OutputCode;

  { an extra return is attached from using a stringlist
    if it wasn't there already
    this is not ideal for TestTextAfterUnitEnd
    but better than a trim
  }
  if (StrRight(lsOut, 2) = AnsiLineBreak) then
  begin
    lsOut := StrChopRight(lsOut, 2);
  end;

  { }
  // debug
  if (lsOut <> psOut) then
  begin
    ShowMessage(MarkReturns(lsOut) +
      AnsiLineBreak + AnsiLineBreak + '-- should have been --' +
      AnsiLineBreak + AnsiLineBreak +
      MarkReturns(psOut));

    ShowMessage(DiffText(lsOut, psOut));
  end;
 { }

  CheckEquals(Length(psOut), Length(lsOut), 'Results length mismatch');
  CheckEquals(psOut, lsOut, 'Bad result text');
end;

procedure TBaseTestProcess.TestFormatPartResult(const psIn, psOut: string; const piStart, piEnd: integer);
var
  lsOut: string;
begin
  fcMessages.Clear;
  fcConvert.InputCode := psIn;

  fcConvert.ConvertPart(piStart, piEnd);

  lsOut := fcConvert.OutputCode;

  { an extra return is attached from using a stringlist
    if it wasn't there already
    this is not ideal for TestTextAfterUnitEnd
    but better than a trim
  }
  if (StrRight(lsOut, 2) = AnsiLineBreak) then
  begin
    lsOut := StrChopRight(lsOut, 2);
  end;

  { }
  // debug
  if (lsOut <> psOut) then
  begin
    ShowMessage(MarkReturns(lsOut) +
      AnsiLineBreak + AnsiLineBreak + '-- should have been --' +
      AnsiLineBreak + AnsiLineBreak +
      MarkReturns(psOut));

    ShowMessage(DiffText(lsOut, psOut));
  end;
 { }

  CheckEquals(Length(psOut), Length(lsOut), 'Results length mismatch');
  CheckEquals(psOut, lsOut, 'Bad result text');
end;

procedure TBaseTestProcess.OnStatusMessage(const psUnit, psMessage: string;
  const piY, piX: integer);
begin
  fcMessages.Add(psMessage);
end;

end.
