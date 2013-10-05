unit TestUsesFindReplace;

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is TestUsesFindReplace, released October 2003.
The Initial Developer of the Original Code is Anthony Steele. 
Portions created by Anthony Steele are Copyright (C) 1999-2003 Anthony Steele.
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

{
  AFS 4 Oct 2003
  test the use clause add, remove and replace processes }

uses BaseTestProcess;

type
  TTestUsesFindReplace = class(TBaseTestProcess)
  private
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAdd1;
    procedure TestAdd2;

    procedure TestCreateIntfUses;
    procedure TestCreateImplUses;

    procedure TestAdd4;
    procedure TestAddInterface;
    procedure TestAddBoth;
    procedure TestAddBoth2;

    procedure TestRemove1;
    procedure TestRemove2;
    procedure TestRemove3;
    procedure TestRemove4;

    procedure TestReplace1;
    procedure TestReplace2;
    procedure TestReplace3;
    procedure TestReplace4;
    procedure TestReplace5;
    procedure TestReplace6;
    procedure TestReplace7;
    procedure TestReplace8;
  end;

implementation

uses
  { DUnit }
  TestFrameWork,
  { local }
  JcfStringutils,
  JcfSettings,
  UsesClauseInsert, UsesClauseRemove, UsesClauseFindReplace;

procedure TTestUsesFindReplace.Setup;
begin
  inherited;

 JcfFormatSettings.UsesClause.InsertInterfaceEnabled      := True;
 JcfFormatSettings.UsesClause.InsertImplementationEnabled := True;

 JcfFormatSettings.UsesClause.RemoveEnabled := True;

 JcfFormatSettings.UsesClause.InsertInterface.Clear;
 JcfFormatSettings.UsesClause.InsertImplementation.Clear;
 JcfFormatSettings.UsesClause.InsertImplementation.Add('foo');


 JcfFormatSettings.UsesClause.Remove.Clear;
 JcfFormatSettings.UsesClause.Remove.Add('foo');

 JcfFormatSettings.UsesClause.Find.Clear;
 JcfFormatSettings.UsesClause.Replace.Clear;
 JcfFormatSettings.UsesClause.Find.Add('foo');
 JcfFormatSettings.UsesClause.Replace.Add('foo2');
end;


procedure TTestUsesFindReplace.TearDown;
begin
  inherited;

 JcfFormatSettings.UsesClause.InsertImplementation.Clear;
 JcfFormatSettings.UsesClause.InsertInterface.Clear;
 JcfFormatSettings.UsesClause.Remove.Clear;

 JcfFormatSettings.UsesClause.InsertInterfaceEnabled := False;
 JcfFormatSettings.UsesClause.InsertImplementationEnabled := False;
 JcfFormatSettings.UsesClause.RemoveEnabled := False;

 JcfFormatSettings.UsesClause.Find.Clear;
 JcfFormatSettings.UsesClause.Replace.Clear;
end;

procedure TTestUsesFindReplace.TestAdd1;
const
  IN_UNIT_TEXT  = UNIT_HEADER + ' uses bar;' + UNIT_FOOTER;
  OUT_UNIT_TEXT = UNIT_HEADER + ' uses bar,foo;' + UNIT_FOOTER;
begin
  TestProcessResult(TUsesClauseInsert, IN_UNIT_TEXT, OUT_UNIT_TEXT);
end;

procedure TTestUsesFindReplace.TestAdd2;
const
  IN_UNIT_TEXT  = UNIT_HEADER + ' uses bar, fish;' + UNIT_FOOTER;
  OUT_UNIT_TEXT = UNIT_HEADER + ' uses bar, fish,foo;' + UNIT_FOOTER;
begin
  TestProcessResult(TUsesClauseInsert, IN_UNIT_TEXT, OUT_UNIT_TEXT);
end;

procedure TTestUsesFindReplace.TestCreateIntfUses;
const
  IN_UNIT_TEXT = 'unit Test;' + NativeLineBreak +
    'interface' + NativeLineBreak + 'implementation' + UNIT_FOOTER;

  OUT_UNIT_TEXT = 'unit Test;' + NativeLineBreak +
    'interface uses foo;' + NativeLineBreak + 'implementation' + UNIT_FOOTER;
begin
 JcfFormatSettings.UsesClause.InsertInterface.Clear;
 JcfFormatSettings.UsesClause.InsertInterface.Add('foo');
 JcfFormatSettings.UsesClause.InsertImplementation.Clear;

  TestProcessResult(TUsesClauseInsert, IN_UNIT_TEXT, OUT_UNIT_TEXT);
end;

procedure TTestUsesFindReplace.TestCreateImplUses;
const
  IN_UNIT_TEXT  = INTERFACE_HEADER + 'implementation' + UNIT_FOOTER;
  OUT_UNIT_TEXT = INTERFACE_HEADER + 'implementation uses foo;' + UNIT_FOOTER;
begin
  TestProcessResult(TUsesClauseInsert, IN_UNIT_TEXT, OUT_UNIT_TEXT);
end;

procedure TTestUsesFindReplace.TestAdd4;
const
  IN_UNIT_TEXT  = UNIT_HEADER + ' uses bar;' + UNIT_FOOTER;
  OUT_UNIT_TEXT = UNIT_HEADER + ' uses bar,foo,t1,t2,t3;' + UNIT_FOOTER;
begin
 JcfFormatSettings.UsesClause.InsertImplementation.Add('t1');
 JcfFormatSettings.UsesClause.InsertImplementation.Add('t2');
 JcfFormatSettings.UsesClause.InsertImplementation.Add('t3');

  TestProcessResult(TUsesClauseInsert, IN_UNIT_TEXT, OUT_UNIT_TEXT);
end;

procedure TTestUsesFindReplace.TestAddInterface;
const
  IN_UNIT_TEXT  = INTERFACE_HEADER + 'uses Bar;' +
    'implementation' + NativeLineBreak + UNIT_FOOTER;
  OUT_UNIT_TEXT = INTERFACE_HEADER + 'uses Bar,IntfFoo;' +
    'implementation' + NativeLineBreak + UNIT_FOOTER;
begin
 JcfFormatSettings.UsesClause.InsertInterface.Add('IntfFoo');
 JcfFormatSettings.UsesClause.InsertImplementation.Clear;

  TestProcessResult(TUsesClauseInsert, IN_UNIT_TEXT, OUT_UNIT_TEXT);
end;

procedure TTestUsesFindReplace.TestAddBoth;
const
  IN_UNIT_TEXT  = INTERFACE_HEADER + 'uses Bar;' +
    'implementation' + NativeLineBreak + UNIT_FOOTER;
  OUT_UNIT_TEXT = INTERFACE_HEADER + 'uses Bar,IntfFoo;' +
    'implementation uses foo;' + NativeLineBreak + UNIT_FOOTER;
begin
 JcfFormatSettings.UsesClause.InsertInterface.Add('IntfFoo');
  // impl keeps the 'foo' item

  TestProcessResult(TUsesClauseInsert, IN_UNIT_TEXT, OUT_UNIT_TEXT);
end;

procedure TTestUsesFindReplace.TestAddBoth2;
const
  IN_UNIT_TEXT  = INTERFACE_HEADER +
    'implementation uses Bar;' + NativeLineBreak + UNIT_FOOTER;
  OUT_UNIT_TEXT = 'unit Test;' + NativeLineBreak +
    'interface uses IntfFoo;' + NativeLineBreak +
    'implementation uses Bar,foo;' + NativeLineBreak + UNIT_FOOTER;
begin
 JcfFormatSettings.UsesClause.InsertInterface.Add('IntfFoo');
  // impl keeps the 'foo' item

  TestProcessResult(TUsesClauseInsert, IN_UNIT_TEXT, OUT_UNIT_TEXT);
end;

procedure TTestUsesFindReplace.TestRemove1;
const
  IN_UNIT_TEXT  = UNIT_HEADER + ' uses foo, bar;' + UNIT_FOOTER;
  OUT_UNIT_TEXT = UNIT_HEADER + ' uses  bar;' + UNIT_FOOTER;
begin
  TestProcessResult(TUsesClauseRemove, IN_UNIT_TEXT, OUT_UNIT_TEXT);
end;

procedure TTestUsesFindReplace.TestRemove2;
const
  IN_UNIT_TEXT  = UNIT_HEADER + ' uses bar, foo;' + UNIT_FOOTER;
  OUT_UNIT_TEXT = UNIT_HEADER + ' uses bar ;' + UNIT_FOOTER;
begin
  TestProcessResult(TUsesClauseRemove, IN_UNIT_TEXT, OUT_UNIT_TEXT);
end;

procedure TTestUsesFindReplace.TestRemove3;
const
  IN_UNIT_TEXT  = UNIT_HEADER + ' uses bar, foo, fish;' + UNIT_FOOTER;
  OUT_UNIT_TEXT = UNIT_HEADER + ' uses bar,  fish;' + UNIT_FOOTER;
begin
  TestProcessResult(TUsesClauseRemove, IN_UNIT_TEXT, OUT_UNIT_TEXT);
end;

procedure TTestUsesFindReplace.TestRemove4;
const
  IN_UNIT_TEXT  = UNIT_HEADER + ' uses foo;' + UNIT_FOOTER;
  OUT_UNIT_TEXT = UNIT_HEADER + '  ' + UNIT_FOOTER;
begin
  TestProcessResult(TUsesClauseRemove, IN_UNIT_TEXT, OUT_UNIT_TEXT);
end;

procedure TTestUsesFindReplace.TestReplace1;
const
  IN_UNIT_TEXT  = UNIT_HEADER + ' uses foo;' + UNIT_FOOTER;
  OUT_UNIT_TEXT = UNIT_HEADER + ' uses foo2;' + UNIT_FOOTER;
begin
  TestProcessResult(TUsesClauseFindReplace, IN_UNIT_TEXT, OUT_UNIT_TEXT);
end;

procedure TTestUsesFindReplace.TestReplace2;
const
  IN_UNIT_TEXT  = UNIT_HEADER + ' uses foo, bar;' + UNIT_FOOTER;
  OUT_UNIT_TEXT = UNIT_HEADER + ' uses foo2, bar;' + UNIT_FOOTER;
begin
  TestProcessResult(TUsesClauseFindReplace, IN_UNIT_TEXT, OUT_UNIT_TEXT);
end;

procedure TTestUsesFindReplace.TestReplace3;
const
  IN_UNIT_TEXT  = UNIT_HEADER + ' uses bar, foo;' + UNIT_FOOTER;
  OUT_UNIT_TEXT = UNIT_HEADER + ' uses bar, foo2;' + UNIT_FOOTER;
begin
  TestProcessResult(TUsesClauseFindReplace, IN_UNIT_TEXT, OUT_UNIT_TEXT);
end;

procedure TTestUsesFindReplace.TestReplace4;
const
  IN_UNIT_TEXT  = UNIT_HEADER + ' uses foo, bar;' + UNIT_FOOTER;
  OUT_UNIT_TEXT = UNIT_HEADER + ' uses foo2 ;' + UNIT_FOOTER;
begin
 JcfFormatSettings.UsesClause.Find.Add('bar');
  TestProcessResult(TUsesClauseFindReplace, IN_UNIT_TEXT, OUT_UNIT_TEXT);
end;

procedure TTestUsesFindReplace.TestReplace5;
const
  IN_UNIT_TEXT  = INTERFACE_HEADER + 'uses foo, Fish;' + NativeLineBreak +
    'implementation' + NativeLineBreak +
    'uses bar, spon;' + UNIT_FOOTER;
  OUT_UNIT_TEXT = INTERFACE_HEADER + 'uses foo2, Fish;' + NativeLineBreak +
    'implementation' + NativeLineBreak +
    'uses  spon;' + UNIT_FOOTER;
begin
 JcfFormatSettings.UsesClause.Find.Add('bar');
  TestProcessResult(TUsesClauseFindReplace, IN_UNIT_TEXT, OUT_UNIT_TEXT);
end;

procedure TTestUsesFindReplace.TestReplace6;
const
  IN_UNIT_TEXT  = INTERFACE_HEADER + 'uses foo, Fish;' + NativeLineBreak +
    'implementation' + NativeLineBreak +
    'uses spon, bar;' + UNIT_FOOTER;
  OUT_UNIT_TEXT = INTERFACE_HEADER + 'uses foo2, Fish;' + NativeLineBreak +
    'implementation' + NativeLineBreak +
    'uses spon ;' + UNIT_FOOTER;
begin
 JcfFormatSettings.UsesClause.Find.Add('bar');
  TestProcessResult(TUsesClauseFindReplace, IN_UNIT_TEXT, OUT_UNIT_TEXT);
end;

procedure TTestUsesFindReplace.TestReplace7;
const
  IN_UNIT_TEXT  = INTERFACE_HEADER + 'uses foo, Fish;' + NativeLineBreak +
    'implementation' + NativeLineBreak +
    'uses bar;' + UNIT_FOOTER;
  OUT_UNIT_TEXT = INTERFACE_HEADER + 'uses foo2, Fish;' + NativeLineBreak +
    'implementation' + NativeLineBreak + ' ' +
    UNIT_FOOTER;
begin
 JcfFormatSettings.UsesClause.Find.Add('bar');
  TestProcessResult(TUsesClauseFindReplace, IN_UNIT_TEXT, OUT_UNIT_TEXT);
end;

procedure TTestUsesFindReplace.TestReplace8;
const
  IN_UNIT_TEXT  = INTERFACE_HEADER + 'uses foo, bar;' + NativeLineBreak +
    'implementation' + NativeLineBreak +
    'uses Fish;' + UNIT_FOOTER;
  OUT_UNIT_TEXT = INTERFACE_HEADER + 'uses foo2 ;' + NativeLineBreak +
    'implementation' + NativeLineBreak +
    'uses Fish;' + UNIT_FOOTER;
begin
 JcfFormatSettings.UsesClause.Find.Add('bar');
  TestProcessResult(TUsesClauseFindReplace, IN_UNIT_TEXT, OUT_UNIT_TEXT);
end;

initialization
  TestFramework.RegisterTest('Processes', TTestUsesFindReplace.Suite);

end.
