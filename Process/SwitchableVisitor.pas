unit SwitchableVisitor;

{ AFS 22 Feb 02
  this visitor respects the special comments "//jcf:"
  that can turn sertain processes off and on again
}

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is SwitchableVisitor, released May 2003.
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

uses BaseVisitor, VisitParseTree, FormatFlags;

type

  TSwitchableVisitor = class(TBaseTreeNodeVisitor)
  private
    // is this processs on?
    fbEnabled: Boolean;
    // on/off flags that this processor responds to
    feFormatFlags: TFormatFlags;

  protected
    // enabled state may be changed by this token
    procedure CheckEnabled(const pcToken: TObject); virtual;

    // every token is inspected, even when the visitor is disabled
    procedure InspectSourceToken(const pcToken: TObject); virtual;
    // this is only called when the processor is enabled
    procedure EnabledVisitSourceToken(const pcToken: TObject; var prVisitResult: TRVisitResult); virtual;

  public
    constructor Create; override;

    procedure VisitSourceToken(const pcToken: TObject; var prVisitResult: TRVisitResult); override;

    property FormatFlags: TFormatFlags read feFormatFlags write feFormatFlags;
  end;

implementation

uses SourceToken, TokenType, ParseError;

constructor TSwitchableVisitor.Create;
begin
  inherited;
  fbEnabled := True;

  //by default, format unless all processors are turned off
  feFormatFlags := [eAllFormat];
end;

procedure TSwitchableVisitor.CheckEnabled(const pcToken: TObject);
var
  lcToken: TSourceToken;
  leFlags: TFormatFlags;
  lsError: string;
  lbHasFlags: Boolean;
  lbOn: boolean;
begin
  lcToken := TSourceToken(pcToken);

  if lcToken.TokenType <> ttComment then
    exit;

  lbHasFlags := ReadCommentJcfFlags(lcToken.SourceCode, lsError, leFlags, lbOn);

  if not lbHasFlags then
    exit;
    
  if lsError <> '' then
    Raise TEParseError.Create(lsError, lcToken);

  // does this flag affect us? 
  if (FormatFlags * leFlags) <> [] then
    fbEnabled := lbOn;
end;


procedure TSwitchableVisitor.EnabledVisitSourceToken(const pcToken: TObject; var prVisitResult: TRVisitResult);
begin
  // here for override
end;

procedure TSwitchableVisitor.InspectSourceToken(const pcToken: TObject);
begin
  // here for override
end;

procedure TSwitchableVisitor.VisitSourceToken(const pcToken: TObject;
  var prVisitResult: TRVisitResult);
begin
  CheckEnabled(pcToken);

  InspectSourceToken(pcToken);

  if fbEnabled then
    EnabledVisitSourceToken(pcToken, prVisitResult);
end;

end.