unit TestConverter;

{ AFS 18 Jan 2003
  Processor that can specify just one process, not all
  to run
}

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is TestConverter, released May 2003.
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

uses StringsConverter, BaseVisitor;

type
  TTestConverter = class(TStringsConverter)
  private
    fbRunAll: boolean;
    fcSingleProcess: TTreeNodeVisitorType;

  protected
    procedure ApplyProcesses; override;


  public
    constructor Create;

    property RunAll: boolean Read fbRunAll Write fbRunAll;
    property SingleProcess: TTreeNodeVisitorType
      Read fcSingleProcess Write fcSingleProcess;

  end;

implementation

uses VisitSetXY, VisitSetNesting;

constructor TTestConverter.Create;
begin
  inherited;
  RunAll := True;
  fcSingleProcess := nil;
end;


procedure TTestConverter.ApplyProcesses;
var
  lcProcess: TBaseTreeNodeVisitor;
begin
  if RunAll then
    inherited
  else
  begin
    // apply a visit setXY first
    lcProcess := TVisitSetXY.Create;
    try
      GetRoot.VisitTree(lcProcess);
    finally
      lcProcess.Free;
    end;

    // and set up nesting levels
    lcProcess := TVisitSetNestings.Create;
    try
      GetRoot.VisitTree(lcProcess);
    finally
      lcProcess.Free;
    end;

    // then apply the process
    lcProcess := SingleProcess.Create;
    try
      GetRoot.VisitTree(lcProcess);
    finally
      lcProcess.Free;
    end;
  end;
end;

end.
