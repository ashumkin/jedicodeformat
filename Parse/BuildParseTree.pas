unit BuildParseTree;

{ AFS 27 October
 This unit turns a token stream into a full parse tree
 using the Recursive Descent method

 The tokens are then the leaves of a tree structure

 The grammer is 'Appendix A Object Pascal grammar'
 As found on the borland Web site.
 It is much extended via test cases as that is woefully incomplete
}

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code

The Original Code is BuildParseTree, released May 2003.
The Initial Developer of the Original Code is Anthony Steele.
Portions created by Anthony Steele are Copyright (C) 1999-2000 Anthony Steele.
All Rights Reserved.
Contributor(s): Anthony Steele, Adem Baba

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
  { delphi }
  Contnrs,
  { local }
  ParseTreeNode,
  ParseTreeNodeType,
  ParseError,
  SourceToken,
  SourceTokenList,
  Tokens;

type
  TBuildParseTree = class(TObject)
  Private
    fbMadeTree: boolean;
    fiTokenIndex: integer;

    fcRoot: TParseTreeNode;
    fcStack: TStack;
    fcTokenList: TSourceTokenList;

    fiTokenCount: integer;

    procedure RecogniseGoal;
    procedure RecogniseUnit;
    procedure RecogniseProgram;
    procedure RecognisePackage;
    procedure RecogniseLibrary;

    procedure RecogniseFileEnd;

    procedure RecogniseProgramBlock;
    procedure RecogniseUsesClause(const pbInFiles: boolean);
    procedure RecogniseUsesItem(const pbInFiles: boolean);

    procedure RecogniseInterfaceSection;
    procedure RecogniseInterfaceDecls;
    procedure RecogniseInterfaceDecl;
    procedure RecogniseExportedHeading;

    procedure RecogniseIdentifier(const pbCanHaveUnitQualifier: boolean);
    procedure RecogniseImplementationSection;
    procedure RecogniseDeclSections;
    procedure RecogniseDeclSection;
    procedure RecogniseInitSection;
    procedure RecogniseBlock;
    procedure RecogniseIdentList(const pbCanHaveUnitQualifier: boolean);
    procedure RecogniseIdentValue;

    procedure RecogniseLabelDeclSection;
    procedure RecogniseLabel;
    procedure RecogniseConstSection;
    procedure RecogniseConstantDecl;
    procedure CheckLabelPrefix;

    procedure RecogniseTypeSection;
    procedure RecogniseVarSection;
    procedure RecogniseProcedureDeclSection;

    // set pbAnon = true if the proc has no name
    procedure RecogniseProcedureHeading(const pbAnon, pbCanInterfaceMap: boolean);
    procedure RecogniseFunctionHeading(const pbAnon, pbCanInterfaceMap: boolean);
    procedure RecogniseCompoundStmnt;
    procedure RecogniseStatementList(const peEndTokens: TTokenTypeSet);
    procedure RecogniseStatement;

    procedure RecogniseTypeId;
    procedure RecogniseTypedConstant;
    procedure RecogniseArrayConstant;
    procedure RecogniseRecordConstant;
    procedure RecogniseRecordFieldConstant;

    procedure RecogniseTypeDecl;

    procedure RecogniseArrayType;
    procedure RecogniseClassRefType;
    procedure RecogniseEnumeratedType;
    procedure RecogniseFieldDecl;
    procedure RecogniseFieldList;
    procedure RecogniseFileType;
    procedure RecogniseOrdIdent;
    procedure RecogniseOrdinalType;
    procedure RecognisePointerType;
    procedure RecogniseProcedureType;
    procedure RecogniseRealType;
    procedure RecogniseRecordType;
    procedure RecogniseRecVariant;
    procedure RecogniseRestrictedType;
    procedure RecogniseSetType;
    procedure RecogniseSimpleType;
    procedure RecogniseStringType;
    procedure RecogniseStrucType;
    procedure RecogniseSubrangeType;
    procedure RecogniseType;
    procedure RecogniseVariantType;
    procedure RecogniseClassType;
    procedure RecogniseClassBody;
    procedure RecogniseClassDeclarations(const pbInterface: boolean);

    procedure RecogniseInterfaceType;
    procedure RecogniseObjectType;
    procedure RecogniseVariantSection;
    procedure RecogniseVarDecl;
    procedure RecogniseAddOp;
    procedure RecogniseDesignator;
    procedure RecogniseDesignatorTail;
    procedure RecogniseExpr(const pbAllowRelop: boolean);
    procedure RecogniseExprList;
    procedure RecogniseFactor;
    procedure RecogniseTerm;
    procedure RecogniseMulOp;
    procedure RecogniseRelOp;
    procedure RecogniseSetConstructor;
    procedure RecogniseSetElement;
    procedure RecogniseQualId;
    procedure RecogniseConstantExpression;

    procedure RecogniseSimpleExpression;
    procedure RecogniseSimpleStmnt;

    procedure RecogniseCaseLabel;
    procedure RecogniseCaseSelector;
    procedure RecogniseCaseStmnt;
    procedure RecogniseForStmnt;
    procedure RecogniseIfStmnt;
    procedure RecogniseRepeatStmnt;
    procedure RecogniseStructStmnt;
    procedure RecogniseWhileStmnt;
    procedure RecogniseWithStmnt;
    procedure RecogniseTryStatement;
    procedure RecogniseExceptionHandlerBlock;
    procedure RecogniseExceptionHandler;
    procedure RecogniseRaise;

    procedure RecogniseFunctionDecl;
    procedure RecogniseProcedureDecl;
    procedure RecogniseConstructorDecl;
    procedure RecogniseDestructorDecl;

    procedure RecogniseFormalParameters;
    procedure RecogniseFormalParam;
    procedure RecogniseParameter;
    procedure RecogniseActualParams;
    procedure RecogniseActualParam;

    procedure RecogniseProcedureDirectives;

    procedure RecogniseExportsSection;
    procedure RecogniseExportedProc;

    // set pbDeclaration to false if the method body is to be recognised
    procedure RecogniseConstructorHeading(const pbDeclaration: boolean);
    procedure RecogniseDestructorHeading(const pbDeclaration: boolean);
    procedure RecogniseObjHeritage;

    procedure RecogniseContainsClause;
    procedure RecogniseInterfaceHeritage;
    procedure RecogniseProperty;
    procedure RecognisePropertyInterface;
    procedure RecognisePropertyParameterList;
    procedure RecognisePropertySpecifiers;
    procedure RecognisePropertyAccess;
    procedure RecogniseRequiresClause;
    procedure RecogniseInterfaceGuid;
    procedure RecogniseClassHeritage;
    procedure RecogniseClassVisibility;
    procedure RecogniseMethodName(const pbClassNameCompulsory: boolean);

    procedure RecogniseAsmBlock;
    procedure RecogniseAsmParam;
    procedure RecogniseAsmStatement;
    procedure RecogniseAsmExpr;
    procedure RecogniseAsmOperator;
    procedure RecogniseAsmFactor;

    procedure RecogniseAsmIdent;
    procedure RecogniseAsmOpcode;
    procedure RecogniseAsmLabel(const pbColon: boolean);
    procedure RecogniseWhiteSpace;

    procedure RecogniseHintDirectives;
    procedure RecognisePropertyDirectives;
    procedure RecogniseExternalProcDirective;

    procedure Recognise(const peTokenTypes: TTokenTypeSet); overload;
    procedure Recognise(const peTokenType: TTokenType); overload;

    function PushNode(const peNodeType: TParseTreeNodeType): TParseTreeNode;
    function PopNode: TParseTreeNode;
    function TopNode: TParseTreeNode;
    function IdentifierNext: boolean;
    function ArrayConstantNext: boolean;
  Protected
  Public
    constructor Create;
    destructor Destroy; override;
    procedure BuildParseTree;
    procedure Clear;
    property Root: TParseTreeNode Read fcRoot;
    property TokenList: TSourceTokenList Read fcTokenList Write fcTokenList;
  end;

implementation

uses
  { delphi }
  SysUtils,
  Forms,
  { jcl }
  JclStrings,
  { local }
  TokenUtils;

const
  UPDATE_INTERVAL = 512;

  {------------------------------------------------------------------------------
    standard overrides }

constructor TBuildParseTree.Create;
begin
  inherited;
  fcStack := TStack.Create;
  fcRoot  := nil;
  fiTokenCount := 0;
end;

destructor TBuildParseTree.Destroy;
begin
  Clear;
  fcStack.Free; {FreeAndNil is pointless here}
  inherited;
end;

procedure TBuildParseTree.Clear;
begin
  while fcStack.Count > 0 do
    fcStack.Pop;
  FreeAndNil(fcRoot);
end;

procedure TBuildParseTree.BuildParseTree;
begin
  Assert(fcTokenList <> nil);
  Clear;
  { read to end of file necessary?
  liIndex := 0;
  while BufferTokens(liIndex).TokenType <> ttEOF do
  begin
    BufferTokens(liIndex);
    inc(liIndex);
  end; }
  fiTokenIndex := 0;
  RecogniseGoal;

  { should not have any sections started but not finished }
  Assert(fcStack.Count = 0);

  { all tokens should have been processed }
  Assert(fcTokenList.Count = fcTokenList.CurrentTokenIndex);
  fcTokenList.Clear;


  fbMadeTree := true;
end;

{-------------------------------------------------------------------------------
  recogniser support }

procedure TBuildParseTree.Recognise(const peTokenTypes: TTokenTypeSet);

  function DescribeTarget: string;
  begin
    Result := '"';
    if peTokenTypes <> [] then
      Result := Result + TokenTypesToString(peTokenTypes);
    Result := Result + '"';
  end;

var
  lcCurrentToken: TSourceToken;
  liCurrentTokenIndex:   integer;
begin
  // must accept something
  Assert(peTokenTypes <> []);

  { read tokens up to and including the specified one.
    Add them to the parse tree at the current growing point  }
  liCurrentTokenIndex := fcTokenList.CurrentTokenIndex;
  while liCurrentTokenIndex < fcTokenList.Count do
  begin
    lcCurrentToken := fcTokenList.Extract(liCurrentTokenIndex);
    Assert(lcCurrentToken <> nil);

    TopNode.AddChild(lcCurrentToken);
    // the the match must be the first solid token
    if lcCurrentToken.TokenType in peTokenTypes then
      Break
    else if not (lcCurrentToken.TokenType in NotSolidTokens) then
      raise TEParseError.Create('Unexpected token, expected ' +
        DescribeTarget, lcCurrentToken);
    Inc(liCurrentTokenIndex);
  end;

  Inc(fiTokenCount);
  if (fiTokenCount mod UPDATE_INTERVAL) = 0 then
    Application.ProcessMessages;
end;

procedure TBuildParseTree.Recognise(const peTokenType: TTokenType);
begin
  Recognise([peTokenType]);
end;

function TBuildParseTree.PushNode(const peNodeType: TParseTreeNodeType): TParseTreeNode;
begin
  Result := TParseTreeNode.Create;
  Result.NodeType := peNodeType;

  if fcStack.Count > 0 then
  begin
    TopNode.AddChild(Result);
    Result.Parent := TopNode;
  end
  else
    fcRoot := Result;

  fcStack.Push(Result);
end;

function TBuildParseTree.PopNode: TParseTreeNode;
begin
  Result := fcStack.Pop;
end;

function TBuildParseTree.TopNode: TParseTreeNode;
begin
  Result := fcStack.Peek;
end;

{a unit / type/var name }

function TBuildParseTree.IdentifierNext: boolean;
var
  lc: TSourceToken;
begin
  lc     := fcTokenList.FirstSolidToken;
  { We have to admit directives and type names as identifiers. see TestBogusDirectives.pas for the reasons why }
  Result := IsIdentifierToken(lc);
end;

{-------------------------------------------------------------------------------
  recognisers for the parse tree  top to bottom

  These procs are based on the "Appendix A Object Pascal grammar"
  Found on the Borland Web site
  All the productions should be here, in the same order
}

procedure TBuildParseTree.RecogniseGoal;
var
  lc: TSourceToken;
begin
  // Goal -> (Program | Package  | Library  | Unit)

  if fcTokenList.Count < 1 then
    raise TEParseError.Create('No source to parse', nil);

  lc := fcTokenList.FirstSolidToken;
  Assert(lc <> nil);

  case lc.TokenType of
    ttProgram:
      RecogniseProgram;
    ttPackage:
      RecognisePackage;
    ttLibrary:
      RecogniseLibrary;
    ttUnit:
      RecogniseUnit;
    else
      raise TEParseError.Create('Expected program, package, library, unit', lc);
  end
end;

procedure TBuildParseTree.RecogniseProgram;
begin
  // Program -> [PROGRAM Ident ['(' IdentList ')'] ';']  ProgramBlock '.'
  PushNode(nProgram);

  PushNode(nUnitHeader);
  Recognise(ttProgram);

  PushNode(nUnitName);
  RecogniseIdentifier(false);
  PopNode;

  if fcTokenList.FirstSolidTokenType = ttOpenBracket then
  begin
    Recognise(ttOpenBracket);
    RecogniseIdentList(false);
    Recognise(ttCloseBracket);
  end;

  if fcTokenList.FirstSolidTokenType = ttSemiColon then
    Recognise(ttSemicolon);

  PopNode;

  RecogniseProgramBlock;
  RecogniseFileEnd;

  PopNode;
end;

procedure TBuildParseTree.RecogniseUnit;
begin
  // Unit -> UNIT Ident ';' InterfaceSection ImplementationSection InitSection '.'
  PushNode(nUnit);

  PushNode(nUnitHeader);
  Recognise(ttUnit);

  PushNode(nUnitName);
  RecogniseIdentifier(false);
  PopNode;

  { unit can be "deprecated platform library" }
  if fcTokenList.FirstSolidTokenType in HintDirectives then
  begin
    PushNode(nHintDirectives);

    while fcTokenList.FirstSolidTokenType in HintDirectives do
      Recognise(HintDirectives);

    PopNode;
  end;

  { or platform }
  if fcTokenList.FirstSolidTokenType = ttPlatform then
    Recognise(ttPlatform);

  Recognise(ttSemicolon);

  PopNode;

  RecogniseInterfaceSection;
  RecogniseImplementationSection;
  RecogniseInitSection;
  RecogniseFileEnd;

  PopNode;
end;

procedure TBuildParseTree.RecognisePackage;
begin
  // Package -> PACKAGE Ident ';' [RequiresClause] [ContainsClause] END '.'
  PushNode(nPackage);

  PushNode(nUnitHeader);
  Recognise(ttPackage);

  PushNode(nUnitName);
  RecogniseIdentifier(false);
  PopNode;
  Recognise(ttSemicolon);
  PopNode;

  if fcTokenList.FirstSolidTokenType = ttRequires then
    RecogniseRequiresClause;

  if fcTokenList.FirstSolidTokenType = ttContains then
    RecogniseContainsClause;

  Recognise(ttEnd);
  RecogniseFileEnd;

  PopNode;
end;

procedure TBuildParseTree.RecogniseLibrary;
begin
  // Library -> LIBRARY Ident ';' ProgramBlock '.'
  PushNode(nLibrary);

  PushNode(nUnitHeader);
  Recognise(ttLibrary);

  PushNode(nUnitName);
  RecogniseIdentifier(false);
  PopNode;
  Recognise(ttSemicolon);
  PopNode;

  RecogniseProgramBlock;
  RecogniseFileEnd;

  PopNode;
end;

procedure TBuildParseTree.RecogniseFileEnd;
var
  liCurrentIndex: integer;
  lcCurrentToken: TSourceToken;
begin
  Recognise(ttDot);

  { delphi accepts anything after the final end }
  liCurrentIndex := fcTokenList.CurrentTokenIndex; 
  while liCurrentIndex < fcTokenList.Count do
  begin
    lcCurrentToken := fcTokenList.Extract(liCurrentIndex);
    TopNode.AddChild(lcCurrentToken);
    Inc(liCurrentIndex);
  end;
end;

procedure TBuildParseTree.RecogniseProgramBlock;
var
  lc: TSourceToken;
begin
  // ProgramBlock -> [UsesClause] Block

  lc := fcTokenList.FirstSolidToken;

  if lc.TokenType = ttUses then
    RecogniseUsesClause(true);

  RecogniseBlock;
end;

procedure TBuildParseTree.RecogniseUsesClause(const pbInFiles: boolean);
begin
  // UsesClause -> USES IdentList ';'
  PushNode(nUses);

  Recognise(ttUses);

  // IdentList -> Ident/','...
  PushNode(nIdentList);

  RecogniseUsesItem(pbInFiles);

  while fcTokenList.FirstSolidTokenType = ttComma do
  begin
    Recognise(ttComma);
    RecogniseUsesItem(pbInFiles);
  end;

  PopNode;

  Recognise(ttSemicolon);

  PopNode;
end;

procedure TBuildParseTree.RecogniseUsesItem(const pbInFiles: boolean);
begin
  PushNode(nUsesItem);

  RecogniseIdentifier(false);

  if pbInFiles and (fcTokenList.FirstSolidTokenType = ttIn) then
  begin
    Recognise(ttIn);
    Recognise(ttLiteralString);
  end;

  PopNode;
end;

procedure TBuildParseTree.RecogniseInterfaceSection;
begin
  // InterfaceSection -> INTERFACE [UsesClause] [InterfaceDecl]...

  PushNode(nInterfaceSection);

  Recognise(ttInterface);

  if fcTokenList.FirstSolidTokenType = ttUses then
    RecogniseUsesClause(false);

  RecogniseInterfaceDecls;

  PopNode;
end;

procedure TBuildParseTree.RecogniseInterfaceDecls;
begin
  { a list of InterfaceDecl sections
    e.g.

      var a,b: integer;
      const b = 3;
      type foo = integer;
      procedure fred;

      NB also threadvar

  }
  while fcTokenList.FirstSolidTokenType in [ttConst, ttResourceString,
      ttType, ttVar, ttThreadVar] + ProcedureWords do
    RecogniseInterfaceDecl;
end;

procedure TBuildParseTree.RecogniseInterfaceDecl;
var
  lc: TSourceToken;
  lt: TTokenType;
begin
  {
   InterfaceDecl -> ConstSection
       -> TypeSection
       -> VarSection
       -> ExportedHeading
   }
  PushNode(nDeclSection);

  lc := fcTokenList.FirstSolidToken;
  lt := fcTokenList.FirstSolidTokenType;

  case lt of
    ttConst, ttResourceString:
      RecogniseConstSection;
    ttType:
      RecogniseTypeSection;
    ttVar, ttThreadvar:
      RecogniseVarSection;
    ttProcedure, ttFunction:
      RecogniseExportedHeading;
    else
      raise TEParseError.Create('Expected const, type, var, procedure or function', lc);
  end;

  PopNode;
end;

procedure TBuildParseTree.RecogniseExportedHeading;
var
  lc: TSourceToken;
  lt: TTokenType;
begin
  { ExportedHeading
     -> ProcedureHeading ';' [Directive]
     -> FunctionHeading ';' [Directive] }

  lc := fcTokenList.FirstSolidToken;
  lt := lc.TokenType;

  case lt of
    ttProcedure:
    begin
      RecogniseProcedureHeading(false, false);
    end;
    ttFunction:
    begin
      RecogniseFunctionHeading(false, false);
    end;
    else
      raise TEParseError.Create('Expected function or procedure', lc);
  end;

  { the ';' is ommited by lazy programmers in some rare occasions}
  if fcTokenList.FirstSolidTokenType = ttSemicolon then
    Recognise(ttSemicolon);
end;

procedure TBuildParseTree.RecogniseImplementationSection;
begin
  {
    ImplementationSection -> IMPLEMENTATION
         [UsesClause]
         [DeclSection]...
  }
  PushNode(nImplementationSection);

  Recognise(ttImplementation);

  if fcTokenList.FirstSolidTokenType = ttUses then
    RecogniseUsesClause(false);

  RecogniseDeclSections;

  PopNode;
end;

procedure TBuildParseTree.RecogniseBlock;
var
  lc: TSourceToken;
  lt: TTokenType;
begin
  { Block -> [DeclSection] CompoundStmt }

  lc := fcTokenList.FirstSolidToken;
  lt := lc.TokenType;

  PushNode(nBlock);

  // [DeclSection]

  if lt in (Declarations + ProcedureWords) then
    RecogniseDeclSections;

  if fcTokenList.FirstSolidTokenType = ttAsm then
    RecogniseAsmBlock
  else
    RecogniseCompoundStmnt;

  PopNode;
end;

procedure TBuildParseTree.RecogniseDeclSections;
begin
  { a list of Decl sections
    e.g.

      label b;
      var a: integer;
      const b = 3;
      type foo = integer;
      procedure fred;
      class procedure TFoo.bar;

  }
  while fcTokenList.FirstSolidTokenType in
    [ttClass] + Declarations + ProcedureWords do
    RecogniseDeclSection;
end;

procedure TBuildParseTree.RecogniseDeclSection;
var
  lc: TSourceToken;
  lt: TTokenType;
begin
  PushNode(nDeclSection);
  {
   DeclSection
     -> LabelDeclSection
     -> ConstSection
     -> TypeSection
     -> VarSection
     -> ProcedureDeclSection
   }

  lc := fcTokenList.FirstSolidToken;
  lt := fcTokenList.FirstSolidTokenType;

  case lt of
    ttLabel:
      RecogniseLabelDeclSection;
    ttConst, ttResourceString:
      RecogniseConstSection;
    ttType:
      RecogniseTypeSection;
    ttVar, ttThreadvar:
      RecogniseVarSection;
    ttProcedure, ttFunction, ttConstructor, ttDestructor, ttClass:
      RecogniseProcedureDeclSection;
    ttExports:
      RecogniseExportsSection;
    else
      raise TEParseError.Create(
        'Expected label, const, type, var, procedure or function', lc);
  end;

  PopNode;
end;

procedure TBuildParseTree.RecogniseLabelDeclSection;
begin
  {
    LabelDeclSection -> LABEL LabelId
    this grammer can't be right. Can be mutiple labels and must have semicolon

    e.g.
      Label foo, bar, fish;

    code below is more flexible
  }

  PushNode(nLabelDeclSection);
  Recognise(ttLabel);

  // almost a RecogniseIdentList, but not quite. also numbers allowed
  PushNode(nIdentList);

  RecogniseLabel;

  while fcTokenList.FirstSolidTokenType = ttComma do
  begin
    Recognise(ttComma);
    RecogniseLabel;
  end;

  PopNode;

  Recognise(ttSemicolon);

  PopNode;
end;

procedure TBuildParseTree.RecogniseLabel;
begin
  if fcTokenList.FirstSolidTokenType = ttNumber then
    Recognise(ttNumber)
  else
    // no unit qualifier
    RecogniseIdentifier(false);
end;

procedure TBuildParseTree.RecogniseConstSection;
begin
  {
    ConstSection -> CONST (ConstantDecl ';')...
  }
  PushNode(nConstSection);
  Recognise([ttConst, ttResourceString]);

  while fcTokenList.FirstSolidWordType in IdentifierTypes do
  begin
    RecogniseConstantDecl;
    Recognise(ttSemicolon);
  end;

  PopNode;
end;

procedure TBuildParseTree.RecogniseConstantDecl;
var
  lc: TSourceToken;
begin
  {
  ConstantDecl
    -> Ident '=' ConstExpr
    -> Ident ':' TypeId '=' TypedConstant

    TypeID is too simplistic -
    can be, for e.g.

    "const foo: array[1..3] of integer = (1,2,3);"
    or "const recs: array[1..3] of TSomeRecord = ( (... "
  }

  PushNode(nConstDecl);

  RecogniseIdentifier(false);

  lc := fcTokenList.FirstSolidToken;

  if lc.TokenType = ttEquals then
  begin
    Recognise(ttEquals);
    RecogniseConstantExpression;
  end
  else if lc.TokenType = ttColon then
  begin
    Recognise(ttColon);
    //RecogniseTypeId;
    RecogniseType;
    Recognise(ttEquals);
    RecogniseTypedConstant;
  end
  else
    raise TEParseError.Create('Expected equals or colon', lc);

  { can be deprecated library platform }
  RecogniseHintDirectives;

  PopNode;
end;

procedure TBuildParseTree.RecogniseTypeSection;
begin
  {
  TypeSection -> TYPE (TypeDecl ';')...
  }
  PushNode(nTypeSection);
  Recognise(ttType);

  while fcTokenList.FirstSolidWordType in IdentifierTypes do
  begin
    RecogniseTypeDecl;
  end;

  PopNode;
end;

procedure TBuildParseTree.RecogniseTypeDecl;
begin
  {
  TypeDecl -> Ident '=' Type
     -> Ident '=' RestrictedType

  Need a semicolon
  }

  PushNode(nTypeDecl);

  RecogniseIdentifier(false);
  Recognise(ttEquals);

  // type or restricted type
  if (fcTokenList.FirstSolidTokenType in [ttObject, ttClass, ttInterface,
    ttDispInterface]) then
    RecogniseRestrictedType
  else
    RecogniseType;

  Recognise(ttSemicolon);

  PopNode;
end;

{ helper proc for RecogniseTypedConstant
  need to distinguish
  "expr" from "(expr, expr)"
  note that expr can -> (expr)
  so we need to notice the comma
  is there a semicolon first or a comma

  Array of records can be "((f: 1), (f: 2))"
  and if it is an array with one element then it is "((f: x))"
}

function TBuildParseTree.ArrayConstantNext: boolean;
var
  liIndex: integer;
  liBracketLevel: integer;
  tt:      TTokenType;
begin
  Result := false;

  if fcTokenList.FirstSolidTokenType <> ttOpenBracket then
    exit;

  liBracketLevel := 0;
  liIndex := fcTokenList.CurrentTokenIndex;
  // scan past the open bracket
  while fcTokenList.SourceTokens[liIndex].TokenType <> ttOpenBracket do
    Inc(liIndex);

  Inc(liIndex);

  // look forward to find the first comma or semicolon
  while true do
  begin
    if liIndex >= fcTokenList.Count then
      break;

    tt := fcTokenList.SourceTokens[liIndex].TokenType;

    if tt = ttOpenBracket then
      Inc(liBracketLevel)
    else if tt = ttCloseBracket then
      Dec(liBracketLevel)
    else if (tt = ttComma) and (liBracketLevel = 0) then
    begin
      Result := true;
      break;
    end
    else if (tt = ttSemicolon) and (liBracketLevel = 0) then
    begin
      Result := false;
      break;
    end
      { if we get an semicolon at bracket level 2, it means an array of records
        e.g.
          Const MyFooRecArray = ((x: 2; y:3), (x: 5; y: 6)); }
    else if (tt = ttSemicolon) and (liBracketLevel = 1) then
    begin
      Result := true;
      break;
    end;

    Inc(liIndex);
  end;

end;

procedure TBuildParseTree.RecogniseTypedConstant;
begin
  { TypedConstant -> (ConstExpr | ArrayConstant | RecordConstant)

   How to tell these apart?

   The record constant must start with open brackets, a field name followed by a colon,
   e.g.   "AREC: TMap = (s1: 'Foo'; i1: 1; i2: 4);"
    No complexity is permitted here. All that can vary is the names

    Array and normal constants are trickier, as both can start with an
    arbitrary number of open brackets
    a normal constant is an expression, and an array constant is a
    bracketed comma-sperated list of them
    You can't look for the word 'array' in the just-parsed text
    as an alias type could be used
   }
  if (fcTokenList.FirstSolidTokenType = ttOpenBracket) and
    (fcTokenList.SolidWordType(2) in IdentifierTypes) and
    (fcTokenList.SolidTokenType(3) = ttColon) then
  begin
    RecogniseRecordConstant;
  end
  else if (ArrayConstantNext) then
  begin
    RecogniseArrayConstant
  end
  else
    RecogniseConstantExpression;
end;

procedure TBuildParseTree.RecogniseArrayConstant;
begin
  // ArrayConstant -> '(' TypedConstant/','... ')'

  PushNode(nArrayConstant);

  Recognise(ttOpenBracket);

  RecogniseTypedConstant;
  while (fcTokenList.FirstSolidTokenType = ttComma) do
  begin
    Recognise(ttComma);
    RecogniseTypedConstant;
  end;

  Recognise(ttCloseBracket);

  PopNode;
end;

procedure TBuildParseTree.RecogniseRecordConstant;
begin
  // RecordConstant -> '(' RecordFieldConstant/';'... ')'

  PushNode(nRecordConstant);

  Recognise(ttOpenBracket);

  RecogniseRecordFieldConstant;
  while (fcTokenList.FirstSolidTokenType = ttSemicolon) do
  begin
    Recognise(ttSemicolon);

    if fcTokenList.FirstSolidTokenType = ttCloseBracket then
      break;

    RecogniseRecordFieldConstant;
  end;

  Recognise(ttCloseBracket);
  PopNode;
end;

procedure TBuildParseTree.RecogniseRecordFieldConstant;
begin
  // RecordFieldConstant -> Ident ':' TypedConstant

  PushNode(nRecordFieldConstant);

  RecogniseIdentifier(false);
  Recognise(ttColon);
  RecogniseTypedConstant;

  PopNode;
end;

procedure TBuildParseTree.RecogniseType;
var
  lc, lc2: TSourceToken;
begin
  {
  Type
    -> TypeId
    -> SimpleType
    -> StrucType
    -> PointerType
    -> StringType
    -> ProcedureType
    -> VariantType
    -> ClassRefType

    NB: const can be a psuedo-type in params
    e.g. "procedure fred(foo: const);"
  }

  PushNode(nType);

  lc  := fcTokenList.FirstSolidToken;
  lc2 := fcTokenList.SolidToken(2);

  { type can be prefixed with a unit name, e.g.
    Classes.TList; }
  if lc2.TokenType = ttDot then
  begin
    RecogniseIdentifier(false);
    Recognise(ttDot);
  end;

  if (lc.TokenType = ttType) then
  begin
    { this can be a prefix. See help under "Declaring types".
      an e.g. is in TestDeclarations.pas }
    Recognise(ttType);
  end;
  (*
  If (lc.TokenType = ttConst) Then Recognise(ttConst)
  Else If (lc.TokenType In RealTypes + OrdTypes) Then RecogniseSimpleType
  Else If (lc.TokenType = ttOpenBracket) Then RecogniseSimpleType // for enumerated types
  Else If (lc.TokenType In [ttPacked, ttArray, ttSet, ttFile, ttRecord]) Then RecogniseStrucType
  Else If (lc.TokenType = ttHat) Then RecognisePointerType
  Else If (lc.TokenType In StringWords) Then RecogniseStringType
  Else If (lc.TokenType In [ttProcedure, ttFunction]) Then RecogniseProcedureType
  Else If lc.TokenType In VariantTypes Then RecogniseVariantType
  Else If (lc.TokenType = ttClass) And (lc2.TokenType = ttOf) Then RecogniseClassRefType
  Else If (lc.WordType In IdentifierTypes) Then Begin
    { could be a subrange on an enum, e.g. "clBlue .. clBlack". NB: this can also be Low(Integer) .. High(Integer) }
    If (AnsiSameText(lc.SourceCode, 'Low')) Or (fcTokenList.SolidTokenType(2) = ttDoubleDot) Then RecogniseSubRangeType
    Else RecogniseTypeId; // some previously declared type that this simple prog does not know of
  End Else RecogniseSimpleType;
  *)

  case lc.TokenType of
  {I am not sure this is faster. But it sure avoids mixing tokentypes in the conditionals}
    ttConst: Recognise(ttConst);
    ttReal48,
    ttReal,
    ttSingle,
    ttDouble,
    ttExtended,
    ttCurrency,
    ttComp,
    ttShortInt,
    ttSmallInt,
    ttInteger,
    ttByte,
    ttLongInt,
    ttInt64,
    ttWord,
    ttBoolean,
    ttByteBool,
    ttWordBool,
    ttLongBool,
    ttChar,
    ttWideChar,
    ttLongWord,
    ttPChar: RecogniseSimpleType; {RealTypes + OrdTypes}
    ttOpenBracket: RecogniseSimpleType; {enumerated types}
    ttPacked,
    ttArray,
    ttSet,
    ttFile,
    ttRecord: RecogniseStrucType;
    ttHat: RecognisePointerType;
    ttString,
    ttAnsiString,
    ttWideString: RecogniseStringType; {StringWords}
    ttProcedure,
    ttFunction: RecogniseProcedureType;
    ttVariant,
    ttOleVariant: RecogniseVariantType; {VariantTypes}
    else
      if (lc.TokenType = ttClass) and (lc2.TokenType = ttOf) then
        RecogniseClassRefType
      else if (lc.WordType in IdentifierTypes) then
      begin
      { could be a subrange on an enum, e.g. "clBlue .. clBlack". NB: this can also be Low(Integer) .. High(Integer) }
        if (AnsiSameText(lc.SourceCode, 'Low')) or
          (fcTokenList.SolidTokenType(2) = ttDoubleDot) then
          RecogniseSubRangeType
        else
          RecogniseTypeId; // some previously declared type that this simple prog does not know of
      end
      else
        RecogniseSimpleType;
  end;

  PopNode;
end;

procedure TBuildParseTree.RecogniseRestrictedType;
var
  lc: TSourceToken;
begin
  {
  RestrictedType
    -> ObjectType
    -> ClassType
    -> InterfaceType
  }

  PushNode(nRestrictedType);

  lc := fcTokenList.FirstSolidToken;
  case lc.TokenType of
    ttObject:
      RecogniseObjectType;
    ttClass:
      RecogniseClassType;
    ttInterface, ttDispInterface:
      RecogniseInterfaceType;
    else
      raise TEParseError.Create('Expected object, class or interface', lc);
  end;

  PopNode;
end;

procedure TBuildParseTree.RecogniseClassRefType;
begin
  // ClassRefType -> CLASS OF TypeId

  Recognise(ttClass);
  Recognise(ttOf);
  RecogniseTypeId;
end;

procedure TBuildParseTree.RecogniseSimpleType;
var
  lc: TSourceToken;
begin
  // SimpleType -> (OrdinalType | RealType)

  lc := fcTokenList.FirstSolidToken;

  if lc.TokenType in RealTypes then
    RecogniseRealType
  else
    RecogniseOrdinalType;
end;

procedure TBuildParseTree.RecogniseRealType;
begin
  { RealType
     -> REAL48
     -> REAL
     -> SINGLE
     -> DOUBLE
     -> EXTENDED
     -> CURRENCY
     -> COMP
  }
  Recognise(RealTypes);
end;

procedure TBuildParseTree.RecogniseOrdinalType;
var
  lc: TSourceToken;
begin
  // OrdinalType -> (SubrangeType | EnumeratedType | OrdIdent)

  lc := fcTokenList.FirstSolidToken;

  if lc.TokenType = ttOpenBracket then
    RecogniseEnumeratedType
  else if lc.TokenType in OrdTypes then
    RecogniseOrdIdent
  else
    RecogniseSubRangeType;
end;

procedure TBuildParseTree.RecogniseOrdIdent;
begin
  {
   OrdIdent
     -> SHORTINT
     -> SMALLINT
     -> INTEGER
     -> BYTE
     -> LONGINT
     -> INT64
     -> WORD
     -> BOOLEAN
     -> CHAR
     -> WIDECHAR
     -> LONGWORD
     -> PCHAR
    }
  Recognise(OrdTypes);
end;

procedure TBuildParseTree.RecogniseVariantType;
begin
  {
    VariantType
      -> VARIANT
      -> OLEVARIANT
  }

  Recognise(VariantTypes);

end;

procedure TBuildParseTree.RecogniseSubrangeType;
begin
  { SubrangeType -> ConstExpr '..' ConstExpr
    this fails when an array is indexed on an entire type, eg
    'BoolArray: array[Boolean] of Boolean;'
  }
  PushNode(nSubrangeType);

  RecogniseConstantExpression;
  if fcTokenList.FirstSolidTokenType = ttDoubleDot then
  begin
    Recognise(ttDoubleDot);

    { recognising any expr is a bad idea here, as "a = 3" is an expression
      and we want this to end with a '='
      this could be "const ValidCharSet: set of 'A'..'z' = ['A'..'Z','a'..'z'];"

       }
    RecogniseExpr(false);
  end;

  PopNode;
end;

procedure TBuildParseTree.RecogniseEnumeratedType;
begin
  // EnumeratedType -> '(' IdentList ')'
  PushNode(nEnumeratedType);

  Recognise(ttOpenBracket);
  RecogniseIdentList(false);
  Recognise(ttCloseBracket);

  PopNode;
end;

procedure TBuildParseTree.RecogniseStringType;
begin
  {
    StringType
      -> STRING
       -> ANSISTRING
       -> WIDESTRING
       -> STRING '[' ConstExpr ']'
   }

  if fcTokenList.FirstSolidTokenType = ttString then
  begin
    Recognise(ttString);
    if fcTokenList.FirstSolidTokenType = ttOpenSquareBracket then
    begin
      // e.g. var f = String[30];
      Recognise(ttOpenSquareBracket);
      RecogniseConstantExpression;
      Recognise(ttCloseSquareBracket);

    end;
  end
  else
    Recognise([ttAnsiString, ttWideString]);
end;

procedure TBuildParseTree.RecogniseStrucType;
var
  lc: TSourceToken;
begin
  // StrucType -> [PACKED] (ArrayType | SetType | FileType | RecType)

  if fcTokenList.FirstSolidTokenType = ttPacked then
    Recognise(ttPacked);

  lc := fcTokenList.FirstSolidToken;

  case lc.TokenType of
    ttArray:
      RecogniseArrayType;
    ttSet:
      RecogniseSetType;
    ttFile:
      RecogniseFileType;
    ttRecord:
      RecogniseRecordType;
    else
      raise TEParseError.Create('Expected array, set, file or record type', lc);
  end;
end;

procedure TBuildParseTree.RecogniseArrayType;
begin
  // ArrayType -> ARRAY ['[' OrdinalType/','... ']'] OF Type
  PushNode(nArrayType);

  Recognise(ttArray);

  if fcTokenList.FirstSolidTokenType = ttOpenSquarebracket then
  begin
    Recognise(ttOpenSquareBracket);

    RecogniseOrdinalType;
    while fcTokenList.FirstSolidTokenType = ttComma do
    begin
      Recognise(ttComma);
      RecogniseOrdinalType;
    end;

    Recognise(ttCloseSquareBracket);
  end;
  Recognise(ttOf);
  RecogniseType;

  PopNode;
end;

procedure TBuildParseTree.RecogniseRecordType;
begin
  // RecType -> RECORD [FieldList] END

  PushNode(nRecordType);

  Recognise(ttRecord);
  if fcTokenList.FirstSolidTokenType <> ttEnd then
    RecogniseFieldList;
  Recognise(ttEnd);

  RecogniseHintDirectives;

  PopNode;
end;

procedure TBuildParseTree.RecogniseFieldList;
begin
  // FieldList ->  FieldDecl/';'... [VariantSection] [';']

  while ( not (fcTokenList.FirstSolidTokenType in [ttEnd, ttCase])) and
    ( not (fcTokenList.FirstSolidTokenType = ttCloseBracket)) do
  begin
    RecogniseFieldDecl;
    if fcTokenList.FirstSolidTokenType = ttSemicolon then
      Recognise(ttSemicolon)
    else
      Break;
  end;

  if fcTokenList.FirstSolidTokenType = ttCase then
    RecogniseVariantSection;

  if fcTokenList.FirstSolidTokenType = ttSemicolon then
    Recognise(ttSemicolon);
end;

procedure TBuildParseTree.RecogniseFieldDecl;
begin
  // FieldDecl -> IdentList ':' Type
  PushNode(nFieldDeclaration);

  RecogniseIdentList(false);
  Recognise(ttColon);
  RecogniseType;

  RecogniseHintDirectives;

  PopNode;
end;

procedure TBuildParseTree.RecogniseVariantSection;
begin
  PushNode(nRecordVariantSection);

  // VariantSection -> CASE [Ident ':'] TypeId OF RecVariant/';'...
  Recognise(ttCase);

  // is there an 'of' 2 tokens hence? If not, must be 'ident:' first
  if not (fcTokenList.SolidTokenType(2) = ttOf) then
  begin
    RecogniseIdentifier(false);
    Recognise(ttColon);
  end;

  RecogniseTypeId;
  Recognise(ttOf);

  // I have tested and that there must be at least 1 case in a var section
  repeat
    RecogniseRecVariant;

    // semicolon is optional on the last one
    if fcTokenList.FirstSolidTokenType = ttSemicolon then
      Recognise(ttSemicolon)
    else
      break;

  until (fcTokenList.FirstSolidTokenType in [ttEnd, ttCloseBracket]);

  PopNode;
end;

procedure TBuildParseTree.RecogniseRecVariant;
begin
  // RecVariant -> ConstExpr/','...  ':' '(' [FieldList] ')'

  PushNode(nRecordVariant);

  RecogniseConstantExpression;
  while fcTokenList.FirstSolidTokenType = ttComma do
  begin
    Recognise(ttComma);
    RecogniseConstantExpression;
  end;

  Recognise(ttColon);
  Recognise(ttOpenBracket);

  if fcTokenList.FirstSolidTokenType <> ttCloseBracket then
    RecogniseFieldList;

  Recognise(ttCloseBracket);

  PopNode;
end;

procedure TBuildParseTree.RecogniseSetType;
begin
  { SetType -> SET OF OrdinalType

  cannot limit it to ord types, as this will not parse the below:

  e.g.
    type
    TFoo = 1..20;
    TBars = (monkey, williamshatnir, soy);

    TFooSet = set of TFoo;
    TBarSet = set of TBar;
  }

  PushNode(nSetType);

  Recognise(ttSet);
  Recognise(ttOf);

  //RecogniseOrdinalType;
  RecogniseType;

  PopNode;
end;

procedure TBuildParseTree.RecogniseFileType;
begin
  {
   FileType -> FILE OF TypeId

   also just plain 'file'
  }

  Recognise(ttFile);
  if fcTokenList.FirstSolidTokenType = ttOf then
  begin
    Recognise(ttOf);
    RecogniseTypeId;
  end;
end;

procedure TBuildParseTree.RecognisePointerType;
begin
  // PointerType -> '^' TypeId
  Recognise(ttHat);
  RecogniseTypeId;
end;

procedure TBuildParseTree.RecogniseProcedureType;
begin
  PushNode(nProcedureType);

  // ProcedureType -> (ProcedureHeading | FunctionHeading) [OF OBJECT]
  if fcTokenList.FirstSolidTokenType = ttProcedure then
    RecogniseProcedureHeading(true, false)
  else if fcTokenList.FirstSolidTokenType = ttFunction then
    RecogniseFunctionHeading(true, false)
  else
    raise TEParseError.Create('Expected procedure or function type',
      fcTokenList.FirstSolidToken);

  if fcTokenList.FirstSolidTokenType = ttOf then
  begin
    Recognise(ttOf);
    Recognise(ttObject);
  end;

  RecogniseProcedureDirectives;

  PopNode;
end;

procedure TBuildParseTree.RecogniseVarSection;
const
  END_VAR_SECTION: TTokenTypeSet =
    [ttVar, ttThreadVar, ttConst, ttLabel, ttResourceString, ttType,
    ttBegin, ttEnd, ttImplementation, ttInitialization,
    ttProcedure, ttFunction, ttConstructor, ttDestructor, ttClass, ttAsm];
begin
  PushNode(nVarSection);

  // VarSection -> VAR (VarDecl ';')...
  Recognise([ttVar, ttThreadvar]);

  repeat
    RecogniseVarDecl;
    Recognise(ttSemicolon);
  until (fcTokenList.FirstSolidTokenType in END_VAR_SECTION);

  PopNode;
end;

procedure TBuildParseTree.RecogniseVarDecl;
var
  lc: TSourceToken;
begin
  // VarDecl -> IdentList ':' Type [(ABSOLUTE (Ident | ConstExpr)) | '=' ConstExpr]

  PushNode(nVarDecl);

  RecogniseIdentList(false);
  Recognise(ttColon);
  RecogniseType;

  lc := fcTokenList.FirstSolidToken;

  if lc.TokenType = ttAbsolute then
  begin
    PushNode(nAbsoluteVar);
    Recognise(ttAbsolute);

    if (fcTokenList.FirstSolidWordType in IdentifierTypes) then
      RecogniseIdentifier(false)
    else
      RecogniseConstantExpression;

    PopNode;
  end
  else
  begin
    RecogniseHintDirectives;

    if fcTokenList.FirstSolidTokenType = ttEquals then
    begin
      PushNode(nVariableInit);

      Recognise(ttEquals);

      { not just an expr - can be an array, record or the like
        reuse the code from typed constant declaration as it works the same
      }
      RecogniseTypedConstant;

      PopNode;
    end;
  end;

  { yes, they can occur here too }
  RecogniseHintDirectives;

  PopNode;
end;

procedure TBuildParseTree.RecogniseExpr(const pbAllowRelop: boolean);
begin
  { Expression -> SimpleExpression [RelOp SimpleExpression]...

    nb this doesn't parse
    lb := foo.Owner;
  }

  PushNode(nExpression);

  RecogniseSimpleExpression;

  if pbAllowRelop then
  begin
    while fcTokenList.FirstSolidTokenType in RelationalOperators do
    begin
      RecogniseRelop;
      RecogniseSimpleExpression;
    end;
  end;

  // added this to cope with real usage - see TestCastSimple
  if fcTokenList.FirstSolidTokenType = ttDot then
  begin
    Recognise(ttDot);
    RecogniseExpr(true);
  end;

  //likewise need to cope with pchar(foo)^
  if fcTokenList.FirstSolidTokenType = ttHat then
  begin
    Recognise(ttHat);
  end;

  PopNode;
end;

procedure TBuildParseTree.RecogniseSimpleExpression;
{var
  lc: TSourceToken;}
begin
  { SimpleExpression -> ['+' | '-'] Term [AddOp Term]...

    the plus/minus prefix is a red herring
    RecogniseFactor does that with a unary operator
  }

{
  lc := fcTokenList.FirstSolidToken;

  if lc.TokenType = wMinus then
    Recognise(wMinus)
  else if lc.TokenType = wPlus then
    Recognise(wPlus);
 }
  RecogniseTerm;
  while fcTokenList.FirstSolidTokenType in AddOperators do
  begin
    RecogniseAddOp;
    RecogniseTerm;
  end;
end;

procedure TBuildParseTree.RecogniseTerm;
begin
  // Term -> Factor [MulOp Factor]...

  PushNode(nTerm);

  RecogniseFactor;

  while fcTokenList.FirstSolidTokenType in MulOperators do
  begin
    RecogniseMulOp;
    RecogniseFactor;
  end;

  PopNode;
end;

procedure TBuildParseTree.RecogniseFactor;
var
  lc: TSourceToken;
begin
  {
  Factor
    -> Designator ['(' ExprList ')']
    -> '' Designator
    -> Number
    -> String
    -> NIL
    -> '(' Expression ')'
    -> NOT Factor
    -> SetConstructor
    -> TypeId '(' Expression ')'

    What is that second line??

    What about unary operators other than not,
    e.g. b := b * -2;
    PossiblyUnarySymbolOperators

    Can also be fn call with no params but with the optional braces,
      e.g. "Foo();"

      or a call to an inherited fucntion, e.g. "inherited foo();
      Note that the function name can be omitted "
   }
  lc := fcTokenList.FirstSolidToken;

  if lc.TokenType = ttInherited then
  begin
    Recognise(ttInherited);

    RecogniseDesignator;
    if fcTokenList.FirstSolidTokenType = ttOpenBracket then
    begin
      RecogniseActualParams;
    end;
  end
  else if (IdentifierNext) then
  begin
    RecogniseDesignator;
    if fcTokenList.FirstSolidTokenType = ttOpenBracket then
    begin
      RecogniseActualParams;
    end;
  end
  else if (fcTokenList.FirstSolidTokenType = ttNumber) then
  begin
    Recognise(ttNumber);
  end
  else if (fcTokenList.FirstSolidTokenType = ttLiteralString) then
  begin
    Recognise(ttLiteralString);
  end
  else if (fcTokenList.FirstSolidTokenType in BuiltInConstants) then
  begin
    // nil, true, false
    Recognise(BuiltInConstants);
  end
  else if (fcTokenList.FirstSolidTokenType = ttOpenBracket) then
  begin
    Recognise(ttOpenBracket);

    { can be empty brackets }
    if fcTokenList.FirstSolidTokenType <> ttCloseBracket then
      RecogniseExpr(true);
    Recognise(ttCloseBracket);

    //!!! recognise expressions like (Foo.Stuff['x'].Pointer)^.MyIndex
  end
  else if (fcTokenList.FirstSolidTokenType = ttNot) then
  begin
    Recognise(ttNot);
    RecogniseFactor;
  end
  else if fcTokenList.FirstSolidTokenType in PossiblyUnarySymbolOperators then
  begin
    PushNode(nUnaryOp);
    Recognise(PossiblyUnarySymbolOperators);
    RecogniseFactor;

    PopNode;
  end
  else if (fcTokenList.FirstSolidTokenType = ttOpenSquareBracket) then
  begin
    RecogniseSetConstructor;
  end
  else
    raise TEParseError.Create('unexpected token in factor', lc);

  if fcTokenList.FirstSolidTokenType in [ttHat, ttDot, ttOpenSquareBracket] then
    RecogniseDesignatorTail;
end;

procedure TBuildParseTree.RecogniseRelOp;
var
  lc: TSourceToken;
begin
  {RelOp
  -> '>'
  -> '<'
  -> '<='
  -> '>='
  -> '<>'
  -> IN
  -> IS
  -> AS
  }

  lc := fcTokenList.FirstSolidToken;

  if lc.TokenType in RelationalOperators then
    Recognise(RelationalOperators)
  else
    raise TEParseError.Create('unexpected token in rel op', lc);
end;

procedure TBuildParseTree.RecogniseAddOp;
var
  lc: TSourceToken;
begin
  lc := fcTokenList.FirstSolidToken;

  if lc.TokenType in AddOperators then
    Recognise(AddOperators)
  else
    raise TEParseError.Create('unexpected token in add op', lc);
end;

procedure TBuildParseTree.RecogniseMulOp;
var
  lc: TSourceToken;
begin
  {
  MulOp
    -> '*'
    -> '/'
    -> DIV
    -> MOD
    -> AND
    -> SHL
    -> SHR

  }
  lc := fcTokenList.FirstSolidToken;

  if lc.TokenType in MulOperators then
    Recognise(MulOperators)
  else
    raise TEParseError.Create('unexpected token in mul op', lc);
end;

procedure TBuildParseTree.RecogniseDesignator;
begin
  { Designator -> QualId ['.' Ident | '[' ExprList ']' | '^']...

    Need brackets here too for hard typecasts like
      pointer(foo)
  }
  PushNode(nDesignator);

  if fcTokenList.FirstSolidTokenType = ttAtSign then
    Recognise(ttAtSign);

  RecogniseQualId;

  RecogniseDesignatorTail;

  PopNode;
end;

procedure TBuildParseTree.RecogniseDesignatorTail;
const
  DESIGNATOR_TAIL_TOKENS = [ttDot, ttOpenBracket, ttOpenSquareBracket, ttHat,
    ttPlus, ttMinus];
begin

  while (fcTokenList.FirstSolidTokenType in DESIGNATOR_TAIL_TOKENS) do
  begin
    case fcTokenList.FirstSolidTokenType of
      ttDot:
      begin
        Recognise(ttDot);
        RecogniseIdentifier(false);
      end;
      ttHat:
      begin
        Recognise(ttHat);
          // and after the deref operator ?
      end;
      ttOpenSquareBracket:
      begin
        Recognise(ttOpenSquareBracket);
        RecogniseExprList;
        Recognise(ttCloseSquareBracket);
      end;
      ttOpenBracket:
      begin
        RecogniseActualParams;
      end;
      ttPlus, ttMinus:
      begin
        Recognise([ttPlus, ttMinus]);
        RecogniseExpr(true);
      end;
      else
        Assert(false, 'Should not be here - bad token type');
    end;
  end;
end;

procedure TBuildParseTree.RecogniseSetConstructor;
begin
  // SetConstructor -> '[' [SetElement/','...] ']'

  Recognise(ttOpenSquareBracket);

  while fcTokenList.FirstSolidTokenType <> ttCloseSquareBracket do
  begin
    RecogniseSetElement;
    if fcTokenList.FirstSolidTokenType = ttComma then
      Recognise(ttComma)
    else
      break; // no comma -> no more items
  end;

  Recognise(ttCloseSquareBracket);
end;

procedure TBuildParseTree.RecogniseSetElement;
begin
  // SetElement -> Expression ['..' Expression]

  RecogniseExpr(true);
  if fcTokenList.FirstSolidTokenType = ttDoubleDot then
  begin
    Recognise(ttDoubleDot);
    RecogniseExpr(false);
  end;
end;

procedure TBuildParseTree.RecogniseExprList;
begin
  // ExprList -> Expression/','...

  RecogniseExpr(true);
  while fcTokenList.FirstSolidTokenType = ttComma do
  begin
    Recognise(ttComma);
    RecogniseExpr(true);
  end;
end;

procedure TBuildParseTree.RecogniseStatement;
const
  BLOCK_END: TTokenTypeSet = [ttEnd, ttFinally, ttExcept];
var
  lc: TSourceToken;
begin
  // Statement -> [LabelId ':'] [SimpleStatement | StructStmt]

  PushNode(nStatement);

  // empty statement
  if fcTokenList.FirstSolidTokenType = ttSemicolon then
  begin
    PopNode;
    Exit;
  end;

  if fcTokenList.FirstSolidTokenType = ttEnd then
  begin
    PopNode;
    Exit;
  end;

  CheckLabelPrefix;

  lc := fcTokenList.FirstSolidToken;

  { anything more? can just be a label at the end of the proc/block }
  if not (lc.TokenType in BLOCK_END) then
  begin

    if lc.TokenType in StructStatementWords then
      RecogniseStructStmnt
    else
      RecogniseSimpleStmnt;
  end;

  PopNode;
end;

procedure TBuildParseTree.CheckLabelPrefix;
var
  lc2: TSourceToken;
  lbColonSecond: boolean;
begin
  lc2 := fcTokenList.SolidToken(2);
  lbColonSecond := (lc2.TokenType = ttColon);
  if (lbColonSecond) then
  begin
    PushNode(nStatementLabel);
    RecogniseLabel;
    Recognise(ttColon);
    PopNode;

    { can be followed by another label  }
    CheckLabelPrefix
  end

end;

procedure TBuildParseTree.RecogniseStatementList(const peEndTokens: TTokenTypeSet);
begin
  // StmtList -> Statement/';'...
  PushNode(nStatementList);

  while not (fcTokenList.FirstSolidTokenType in peEndTokens) do
  begin
    RecogniseStatement;

    // last semicolon is optional
    if fcTokenList.FirstSolidTokenType = ttSemicolon then
      Recognise(ttSemicolon)
    else
      break;
  end;

  PopNode;
end;

procedure TBuildParseTree.RecogniseSimpleStmnt;
var
  lc: TSourceToken;
begin
  {
  SimpleStatement
    -> Designator ['(' ExprList ')']
    -> Designator ':=' Expression
    -> INHERITED
    -> GOTO LabelId

    argh this doesn't take brackets into account
    as far as I can tell, typecasts like "(lcFoo as TComponent)" is a designator

    so is "Pointer(lcFoo)" so that you can do
    " Pointer(lcFoo) := Pointer(lcFoo) + 1;

    Niether does it take into account using property on returned object, e.g.
    qry.fieldbyname('line').AsInteger := 1;

    These can be chained indefinitely, as in
   foo.GetBar(1).Stuff['fish'].MyFudgeFactor.Default(2).Name := 'Jiim';
}

  lc := fcTokenList.FirstSolidToken;

  if (IdentifierNext) or (lc.TokenType in [ttOpenBracket, ttAtSign]) then
  begin
    // should be fullblown expression?
    RecogniseDesignator;

    if fcTokenList.FirstSolidTokenType = ttOpenBracket then
    begin
      RecogniseActualParams;
    end;

    // can be a hat after the close backets to deref the return value
    if fcTokenList.FirstSolidTokenType = ttHat then
      Recognise(ttHat);

    // dot next ?
    if fcTokenList.FirstSolidTokenType = ttDot then
    begin
      Recognise(ttDot);
      RecogniseSimpleStmnt;
    end

    else if fcTokenList.FirstSolidTokenType = ttAssign then
    begin
      PushNode(nAssignment);

      Recognise(ttAssign);
      RecogniseExpr(true);

      PopNode;
    end;

    // else nothing at all is also ok. i.e. procedure call with no params
  end
  else if lc.TokenType = ttInherited then
  begin
    { can be one of
      "inherited;
      inherited Foo;
      inherited Foo(bar);
      inherited FooProp := bar;
      inherited FooProp[Bar] := Fish;
      bar :=  inherited FooProp[Bar];
      }

    Recognise(ttInherited);
    if IdentifierNext then
      RecogniseSimpleStmnt;
  end
  else if lc.TokenType = ttGoto then
  begin
    Recognise(ttGoto);
    RecogniseLabel;
  end
  else if lc.TokenType = ttRaise then
  begin
    RecogniseRaise;
  end
  else if lc.TokenType = ttSemicolon then
  begin
    // empty statement
    // this gets doen later in common code Recognise(ttSemicolon);
  end
  else
    raise TEParseError.Create('expected simple statement', lc);

end;

procedure TBuildParseTree.RecogniseRaise;
begin
  // another omission - raise expr  or just raise (in except block)
  Recognise(ttRaise);
  if not (fcTokenList.FirstSolidTokenType in [ttSemicolon, ttEnd, ttElse]) then
    RecogniseExpr(true);

  // can be at addr
  if fcTokenList.FirstSolidTokenType = ttAt then
  begin
    Recognise(ttAt);
    RecogniseExpr(true);
  end;
end;

procedure TBuildParseTree.RecogniseStructStmnt;
var
  lc: TSourceToken;
begin
  {
    StructStmt
      -> CompoundStmt
       -> ConditionalStmt
       -> LoopStmt
       -> WithStmt
    }

    { ConditionalStmt
        -> IfStmt
        -> CaseStmt
    }

   {
    LoopStmt
      -> RepeatStmt
     -> WhileStmt
     -> ForStmt
    }

    { they completely left out try blocks !}

  lc := fcTokenList.FirstSolidToken;

  case lc.TokenType of
    ttBegin:
      RecogniseCompoundStmnt;
    ttAsm:
      RecogniseAsmBlock;
    ttIf:
      RecogniseIfStmnt;
    ttCase:
      RecogniseCaseStmnt;
    ttRepeat:
      RecogniseRepeatStmnt;
    ttWhile:
      RecogniseWhileStmnt;
    ttFor:
      RecogniseForStmnt;
    ttWith:
      RecogniseWithStmnt;
    ttTry:
      RecogniseTryStatement;
    else
      raise TEParseError.Create('expected structured statement', lc);
  end;

end;

procedure TBuildParseTree.RecogniseCompoundStmnt;
begin
  { CompoundStmt -> BEGIN StmtList END }
  PushNode(nCompoundStatement);
  Recognise(ttBegin);
  RecogniseStatementList([ttEnd]);
  Recognise(ttEnd);
  PopNode;
end;

procedure TBuildParseTree.RecogniseIfStmnt;
begin
  // IfStmt -> IF Expression THEN Statement [ELSE Statement]

  Recognise(ttIf);

  PushNode(nIfCondition);
  RecogniseExpr(true);
  PopNode;

  Recognise(ttThen);

  PushNode(nIfBlock);

  { if body can be completely missing - go straight to else }
  if fcTokenList.FirstSolidTokenType <> ttElse then
    RecogniseStatement;
  PopNode;

  if fcTokenList.FirstSolidTokenType = ttElse then
  begin
    Recognise(ttElse);
    PushNode(nElseBlock);
    if not (fcTokenList.FirstSolidTokenType in [ttElse, ttEnd]) then
      RecogniseStatement;
    PopNode;
  end;
end;

procedure TBuildParseTree.RecogniseCaseStmnt;
begin
  // CaseStmt -> CASE Expression OF CaseSelector/';'... [ELSE Statement] [';'] END
  PushNode(nCaseStatement);

  Recognise(ttCase);

  PushNode(nBlockHeaderExpr);
  RecogniseExpr(true);
  PopNode;

  Recognise(ttOf);

  while not (fcTokenList.FirstSolidTokenType in [ttElse, ttEnd]) do
    RecogniseCaseSelector;

  if fcTokenList.FirstSolidTokenType = ttElse then
  begin
    PushNode(nElseCase);
    Recognise(ttElse);
    RecogniseStatementList([ttEnd]);
    PopNode;
  end;

  if fcTokenList.FirstSolidTokenType = ttSemicolon then
    Recognise(ttSemicolon);

  Recognise(ttEnd);

  PopNode;
end;

procedure TBuildParseTree.RecogniseCaseSelector;
begin
  // CaseSelector -> CaseLabel/','... ':' Statement ';'

  PushNode(nCaseSelector);

  PushNode(nCaseLabels);
  RecogniseCaseLabel;

  while (fcTokenList.FirstSolidTokenType = ttComma) do
  begin
    Recognise(ttComma);
    RecogniseCaseLabel;
  end;

  Recognise(ttColon);
  PopNode;

  { semicolon is optional in the last case before the else }
  if not (fcTokenList.FirstSolidTokenType in [ttElse, ttEnd]) then
  begin
    RecogniseStatement;

    if fcTokenList.FirstSolidTokenType = ttSemicolon then
      Recognise(ttSemicolon);
  end;

  PopNode;
end;

procedure TBuildParseTree.RecogniseCaseLabel;
begin
  // CaseLabel -> ConstExpr ['..' ConstExpr]

  PushNode(nCaseLabel);

  RecogniseConstantExpression;
  if (fcTokenList.FirstSolidTokenType = ttDoubleDot) then
  begin
    Recognise(ttDoubleDot);
    RecogniseConstantExpression;
  end;

  PopNode;
end;

procedure TBuildParseTree.RecogniseRepeatStmnt;
begin
  { RepeatStmt -> REPEAT Statement UNTIL Expression

   Incorect - it is a statement list
  }
  PushNode(nRepeatStatement);

  Recognise(ttRepeat);
  RecogniseStatementList([ttUntil]);
  Recognise(ttUntil);

  PushNode(nLoopHeaderExpr);
  RecogniseExpr(true);
  PopNode;

  PopNode;
end;

procedure TBuildParseTree.RecogniseWhileStmnt;
begin
  // WhileStmt -> WHILE Expression DO Statement
  PushNode(nWhileStatement);

  Recognise(ttWhile);

  PushNode(nLoopHeaderExpr);
  RecogniseExpr(true);
  PopNode;

  Recognise(ttDo);
  RecogniseStatement;

  PopNode;
end;

procedure TBuildParseTree.RecogniseForStmnt;
begin
  // ForStmt -> FOR QualId ':=' Expression (TO | DOWNTO) Expression DO Statement
  PushNode(nForStatement);

  Recognise(ttFor);
  RecogniseQualId;
  Recognise(ttAssign);

  PushNode(nLoopHeaderExpr);
  RecogniseExpr(true);
  PopNode;

  Recognise([ttTo, ttDownto]);

  PushNode(nLoopHeaderExpr);
  RecogniseExpr(true);
  PopNode;

  Recognise([ttDo]);
  RecogniseStatement;

  PopNode;
end;

procedure TBuildParseTree.RecogniseWithStmnt;
begin
  { WithStmt -> WITH IdentList DO Statement

   it's not an identlist, but an expression list
  }
  PushNode(nWithStatement);

  Recognise([ttWith]);

  //RecogniseIdentList;
  PushNode(nBlockHeaderExpr);
  RecogniseExprList;
  PopNode;

  Recognise([ttDo]);
  RecogniseStatement;

  PopNode;
end;

procedure TBuildParseTree.RecogniseTryStatement;
var
  lc: TSourceToken;
begin
  { um. right, I'll have to wing this one
    as borland neglected to mention it at all

    TryStatement -> 'try' StatementList TryEnd

    TryEnd
      -> 'finally' StatementList 'end'
      -> except ExceptionHandlers 'end'
  }

  PushNode(nTryAndHandlerBlock);

  PushNode(nTryBlock);

  Recognise(ttTry);
  RecogniseStatementList([ttEnd, ttFinally, ttExcept]);

  PopNode;

  lc := fcTokenList.FirstSolidToken;
  case lc.TokenType of
    ttFinally:
    begin
      PushNode(nFinallyBlock);

      Recognise(ttFinally);
      RecogniseStatementList([ttEnd]);
      Recognise(ttEnd);

      PopNode;
    end;
    ttExcept:
    begin
      PushNode(nExceptBlock);

      Recognise(ttExcept);
      RecogniseExceptionHandlerBlock;
      Recognise(ttEnd);

      PopNode;
    end
    else
      raise TEParseError.Create('expected except or finally', lc);

  end;

  PopNode;
end;

procedure TBuildParseTree.RecogniseExceptionHandlerBlock;
begin
  { um. Double-um
    can be a statement list
     or those 'on Excepttype' thingies
    ie

    try
      ...
    except
      ShowMessage('Foo');
    end

    or

    try
      ...
    except
      on TFooException do
        ShowMessage('Foo');
      on E: TBarException do
        ShowMessage('Bar');
      else
        ShowMessage('Else');
    end;

    here's the grammar

    ExceptionHandlers -> Statement
    ExceptionHandlers -> ExceptionSpecifier

  }
  PushNode(nExceptionHandlers);

  if fcTokenList.FirstSolidTokenType in [ttOn, ttElse] then
  begin
    while fcTokenList.FirstSolidTokenType in [ttOn, ttElse] do
      RecogniseExceptionHandler;
  end
  else
  begin
    // can be 0 or more statements
    RecogniseStatementList([ttEnd]);
  end;

  PopNode;
end;

procedure TBuildParseTree.RecogniseExceptionHandler;
begin
  {
    ExceptionSpecifier
        -> 'on' [ident ':'] ExceptType 'do' Statement
        -> 'else' Statement
  }
  PushNode(nOnExceptionHandler);

  if fcTokenList.FirstSolidTokenType = ttElse then
  begin
    Recognise(ttElse);
    RecogniseStatement;
  end
  else if fcTokenList.FirstSolidTokenType = ttOn then
  begin
    Recognise(ttOn);
    if fcTokenList.SolidTokenType(2) = ttColon then
    begin
      RecogniseIdentifier(false);
      Recognise(ttColon);
    end;

    RecogniseIdentifier(true);
    Recognise(ttDo);

    { special case - empty statement block, go straight on to the else }
    if fcTokenList.FirstSolidTokenType <> ttElse then
      RecogniseStatement;
  end
  else
    RecogniseStatement;

  if fcTokenList.FirstSolidTokenType = ttSemicolon then
    Recognise(ttSemicolon);

  PopNode;
end;

procedure TBuildParseTree.RecogniseProcedureDeclSection;
var
  lc: TSourceToken;
begin
  {
  ProcedureDeclSection
    -> ProcedureDecl
    -> FunctionDecl
  }

  lc := fcTokenList.FirstSolidToken;

  case lc.TokenType of
    ttProcedure:
      RecogniseProcedureDecl;
    ttFunction:
      RecogniseFunctionDecl;
    ttConstructor:
      RecogniseConstructorDecl;
    ttDestructor:
      RecogniseDestructorDecl;

    ttClass:
    begin
        // class proc or function
      case fcTokenList.SolidTokenType(2) of
        ttProcedure:
          RecogniseProcedureDecl;
        ttFunction:
          RecogniseFunctionDecl;
        else
          raise TEParseError.Create('expected class procedure or class function', lc);
      end;
    end;
    else
      raise TEParseError.Create('expected procedure or function', lc);
  end;

end;

{ the proc/function is forward or extern (ie has no body)
  if the word 'forward' or 'extern' is in the directives
  these are also valid param names }

function IsForwardExtern(pt: TParseTreeNode): boolean;
var
  lcDirectives: TParseTreeNode;
begin
  Assert(pt <> nil);

  if pt.NodeType in ProcedureNodes then
    pt := pt.GetImmediateChild(ProcedureHeadings);

  Assert(pt <> nil);

  lcDirectives := pt.GetImmediateChild(nProcedureDirectives);

  Result := (lcDirectives <> nil) and lcDirectives.HasChildNode([ttExternal, ttForward])
end;

procedure TBuildParseTree.RecogniseProcedureDecl;
var
  lcTop: TParseTreeNode;
begin
  { ProcedureDecl -> ProcedureHeading ';' [Directive] Block ';'

    NB: the block is omitted if there is a 'forward' or external' directive

  }
  PushNode(nProcedureDecl);

  RecogniseProcedureHeading(false, false);

  { the ';' is ommited by lazy programmers in some rare occasions}
  if fcTokenList.FirstSolidTokenType = ttSemicolon then
    Recognise(ttSemicolon);

  { if the proc declaration has the directive external or forward,
    it will not have a body
    note that though 'forward' is a spectacularly unfortunate variable name,
    it has happened, e.g. in ActnMenus.pas }
  lcTop := TParseTreeNode(fcStack.Peek);
  if not IsForwardExtern(lcTop) then
  begin
    RecogniseBlock;
    Recognise(ttSemicolon);
  end;

  PopNode;
end;

procedure TBuildParseTree.RecogniseFunctionDecl;
var
  lcTop: TParseTreeNode;
begin
  // ProcedureDecl -> FunctionHeading ';' [Directive] Block ';'

  PushNode(nFunctionDecl);

  RecogniseFunctionHeading(false, false);
  { the ';' is ommited by lazy programmers in some rare occasions}
  if fcTokenList.FirstSolidTokenType = ttSemicolon then
    Recognise(ttSemicolon);

  //opt
  if fcTokenList.FirstSolidTokenType in ProcedureDirectives then
    RecogniseProcedureDirectives;

  { if the proc declaration has the directive external or forward,
    it will not have a body }
  lcTop := TParseTreeNode(fcStack.Peek);
  if not IsForwardExtern(lcTop) then
  begin
    RecogniseBlock;
    Recognise(ttSemicolon);
  end;

  PopNode;
end;

procedure TBuildParseTree.RecogniseConstructorDecl;
begin
  // ProcedureDecl -> ProcedureHeading ';' [Directive] Block ';'

  PushNode(nConstructorDecl);

  RecogniseConstructorHeading(false);
  Recognise(ttSemicolon);

  if fcTokenList.FirstSolidTokenType in ProcedureDirectives then
    RecogniseProcedureDirectives;
  RecogniseBlock;
  Recognise(ttSemicolon);

  PopNode;
end;

procedure TBuildParseTree.RecogniseDestructorDecl;
begin
  // ProcedureDecl -> ProcedureHeading ';' [Directive] Block ';'

  PushNode(nDestructorDecl);

  RecogniseDestructorHeading(false);
  Recognise(ttSemicolon);

  if fcTokenList.FirstSolidTokenType in ProcedureDirectives then
    RecogniseProcedureDirectives;
  RecogniseBlock;
  Recognise(ttSemicolon);

  PopNode;
end;

procedure TBuildParseTree.RecogniseFunctionHeading(
  const pbAnon, pbCanInterfaceMap: boolean);
begin
  // FunctionHeading -> FUNCTION Ident [FormalParameters] ':' (SimpleType | STRING)
  PushNode(nFunctionHeading);

  // class procs
  if fcTokenList.FirstSolidTokenType = ttClass then
    Recognise(ttClass);

  Recognise(ttFunction);
  if not pbAnon then
    RecogniseMethodName(false);

  if fcTokenList.FirstSolidTokenType = ttOpenBracket then
    RecogniseFormalParameters;

  { the colon and type is in fact optional in
    - external fns
    - when making good on a forward }
  if fcTokenList.FirstSolidTokenType = ttColon then
  begin
    Recognise(ttColon);
    PushNode(nFunctionReturnType);
    RecogniseType;
    PopNode;
  end;

  RecogniseProcedureDirectives;

  if pbCanInterfaceMap and (fcTokenList.FirstSolidTokenType = ttEquals) then
  begin
    Recognise(ttEquals);
    RecogniseIdentifier(false);
  end;

  PopNode;
end;

procedure TBuildParseTree.RecogniseProcedureHeading(
  const pbAnon, pbCanInterfaceMap: boolean);
begin
  { ProcedureHeading -> PROCEDURE Ident [FormalParameters]

    can also map to an interface name
    e.g.
      type
        TFoo = class(TObject, IFoo)
          public
            procedure IFoo.P1 = MyP1;
            Procedure MyP1;
        end;

        Or a constant
  }

  PushNode(nProcedureHeading);

  if fcTokenList.FirstSolidTokenType = ttClass then
    Recognise(ttClass);

  Recognise(ttProcedure);
  if not pbAnon then
    RecogniseMethodName(false);

  if fcTokenList.FirstSolidTokenType = ttOpenBracket then
    RecogniseFormalParameters;

  RecogniseProcedureDirectives;

  if pbCanInterfaceMap and (fcTokenList.FirstSolidTokenType = ttEquals) then
  begin
    Recognise(ttEquals);
    RecogniseIdentifier(false);
  end;

  PopNode;
end;

procedure TBuildParseTree.RecogniseFormalParameters;
begin
  // FormalParameters -> '(' FormalParm/';'... ')'

  PushNode(nFormalParams);

  Recognise(ttOpenBracket);

  { funciton Foo(); is accepted so must allow empty brackets }

  if fcTokenList.FirstSolidTokenType <> ttCloseBracket then
  begin
    RecogniseFormalParam;
    while fcTokenList.FirstSolidTokenType = ttSemicolon do
    begin
      Recognise(ttSemicolon);
      RecogniseFormalParam;
    end;
  end;

  Recognise(ttCloseBracket);

  PopNode;
end;

procedure TBuildParseTree.RecogniseFormalParam;
const
  PARAM_PREFIXES: TTokenTypeSet = [ttVar, ttConst];
begin
  PushNode(nFormalParam);

  { FormalParm -> [VAR | CONST | OUT] Parameter

    'out' is different as it is also a param name so this is legal
    procedure Foo(out out: integer);

    'out' with a comma, colon or ')' directly after is not a prefix, it is a param name
    if another name follows it is a prefix
  }

  if fcTokenList.FirstSolidTokenType in PARAM_PREFIXES then
    Recognise(PARAM_PREFIXES)
  else if fcTokenList.FirstSolidTokenType = ttOut then
  begin
    if IsIdentifierToken(fcTokenList.SolidToken(2)) then
      Recognise(ttOut);
  end;

  RecogniseParameter;

  PopNode;
end;

procedure TBuildParseTree.RecogniseParameter;
var
  lbArray: boolean;
begin
  { Parameter
    -> IdentList  [':' ([ARRAY OF] SimpleType | STRING | FILE)]
    -> Ident ':' SimpleType '=' ConstExpr

    hard to distinguish these two productions
    will go for the superset

    -> IdentList  [':' ([ARRAY OF] Type) ['=' ConstExpr] ]

    Also I think that's broken as the following are legal:

    procedure foo(bar: array of file);
    procedure foo(bar: array of TMyRecord);

  }
  lbArray := false;
  RecogniseIdentList(false);
  if fcTokenList.FirstSolidTokenType = ttColon then
  begin
    Recognise(ttColon);

    if fcTokenList.FirstSolidTokenType = ttArray then
    begin
      Recognise(ttArray);
      Recognise(ttOf);
      lbArray := true;
    end;

    // type is optional in params ie procedure foo(var pp);
    if (lbArray) or ( not (fcTokenList.FirstSolidTokenType in
      [ttSemicolon, ttCloseBracket])) then
      RecogniseType;

    if fcTokenList.FirstSolidTokenType = ttEquals then
    begin
      Recognise(ttEquals);
      RecogniseConstantExpression;
    end;
  end;
end;

procedure TBuildParseTree.RecogniseProcedureDirectives;
var
  lbFirstPass: boolean;
begin
  { these are semi-colon seperated

    want to leave 'Function foo;' as is,
    but strip off the '; safecall' off 'Function bar; safecall;'

    external is more complex
  }

  if (fcTokenList.FirstSolidTokenType in ProcedureDirectives) or
    ((fcTokenList.FirstSolidTokenType = ttSemicolon) and
    (fcTokenList.SolidTokenType(2) in ProcedureDirectives)) then
  begin
    PushNode(nProcedureDirectives);

    if fcTokenList.FirstSolidTokenType = ttSemiColon then
      Recognise(ttSemiColon);
    lbFirstPass := true;

    while (fcTokenList.FirstSolidTokenType in ProcedureDirectives) or
      ((fcTokenList.FirstSolidTokenType = ttSemicolon) and
        (fcTokenList.SolidTokenType(2) in ProcedureDirectives)) do
    begin
      if ( not lbFirstPass) and (fcTokenList.FirstSolidTokenType = ttSemiColon) then
        Recognise(ttSemiColon);

      case fcTokenList.FirstSolidTokenType of
        ttExternal:
        begin
          RecogniseExternalProcDirective;
        end;
        ttDispId:
        begin
          Recognise(ttDispId);
          RecogniseConstantExpression;
        end;
        ttMessage:
        begin
          Recognise(ttMessage);
          RecogniseConstantExpression;
        end;
        else
          Recognise(ProcedureDirectives);
      end;

      lbFirstPass := false;
    end;

    PopNode;
  end;
end;

procedure TBuildParseTree.RecogniseExternalProcDirective;
begin
  { right, i'll fake this one

    ExternalProcDirective ->
      External ["'" libname "'" ["name" "'" procname "'"]]
  }
  PushNode(nExternalDirective);

  Recognise(ttExternal);
  if fcTokenList.FirstSolidTokenType in (IdentiferTokens + [ttLiteralString]) then
  begin
    Recognise((IdentiferTokens + [ttLiteralString]));

    if fcTokenList.FirstSolidTokenType = ttName then
    begin
      Recognise(ttName);
      RecogniseConstantExpression;
    end;
  end;

  PopNode;
end;

procedure TBuildParseTree.RecogniseObjectType;
begin
  { ObjectType -> OBJECT [ObjHeritage] [ObjFieldList] [MethodList] END

      arg this is badly broken, need to
  }

  PushNode(nObjectType);

  Recognise(ttObject);

  if fcTokenList.FirstSolidTokenType = ttOpenBracket then
    RecogniseObjHeritage;

  // swiped this from the delphi object defs
  RecogniseClassBody;

  Recognise(ttEnd);

  PopNode;
end;

procedure TBuildParseTree.RecogniseObjHeritage;
begin
  // ObjHeritage -> '(' QualId ')'

  Recognise(ttOpenBracket);
  RecogniseQualId;
  Recognise(ttCloseBracket);
end;

procedure TBuildParseTree.RecogniseConstructorHeading(const pbDeclaration: boolean);
begin
  //ConstructorHeading -> CONSTRUCTOR Ident [FormalParameters]
  PushNode(nConstructorHeading);

  Recognise(ttConstructor);
  RecogniseMethodName( not pbDeclaration);
  if fcTokenList.FirstSolidTokenType = ttOpenBracket then
    RecogniseFormalParameters;

  RecogniseProcedureDirectives;

  PopNode;
end;

procedure TBuildParseTree.RecogniseDestructorHeading(const pbDeclaration: boolean);
begin
  //DestructorHeading -> DESTRUCTOR Ident [FormalParameters]
  PushNode(nDestructorHeading);

  Recognise(ttDestructor);
  RecogniseMethodName( not pbDeclaration);
  if fcTokenList.FirstSolidTokenType = ttOpenBracket then
    RecogniseFormalParameters;

  RecogniseProcedureDirectives;

  PopNode;
end;

procedure TBuildParseTree.RecogniseInitSection;
var
  lc: TSourceToken;
begin
  {
    InitSection
      -> INITIALIZATION StmtList [FINALIZATION StmtList] END
      -> BEGIN StmtList END
      -> END
  }

  lc := fcTokenList.FirstSolidToken;

  PushNode(nInitSection);

  case lc.TokenType of
    ttInitialization:
    begin
      Recognise(ttInitialization);
      RecogniseStatementList([ttEnd, ttFinalization]);

      if fcTokenList.FirstSolidTokenType = ttFinalization then
      begin
        Recognise(ttFinalization);
        RecogniseStatementList([ttEnd]);
      end;

      Recognise(ttEnd);
    end;
    ttBegin:
    begin
      Recognise(ttBegin);
      RecogniseStatementList([ttEnd]);
      Recognise(ttEnd);
    end;
    ttEnd:
    begin
      Recognise(ttEnd);
    end
    else
      raise TEParseError.Create('expected initialisation, begin or end', lc);
  end;

  PopNode;
end;

procedure TBuildParseTree.RecogniseClassType;
begin
  {
  ClassType -> CLASS [ClassHeritage]
       [ClassFieldList]
       [ClassMethodList]
       [ClassPropertyList]
       END

  This is not right - these can repeat

  My own take on this is as follows:

  class -> ident '=' 'class' [Classheritage] classbody 'end'
  classbody -> clasdeclarations (ClassVisibility clasdeclarations) ...
  ClassVisibility -> 'private' | 'protected' | 'public' | 'published' | 'automated'
  classdeclarations -> (procheader|fnheader|constructor|destructor|vars|property|) [';'] ...

  can also be a forward declaration, e.g.
    TFred = class;

  or a class ref type
    TFoo = class of TBar;
  }

  PushNode(nClassType);

  Recognise(ttClass);

  if fcTokenList.FirstSolidTokenType = ttSemicolon then
  begin
    PopNode;
    exit;
  end;

  if fcTokenList.FirstSolidTokenType = ttOf then
  begin
    Recognise(ttOf);
    RecogniseIdentifier(false);
    PopNode;
    exit;
  end;

  if fcTokenList.FirstSolidTokenType = ttOpenBracket then
    RecogniseClassHeritage;

  // can end here
  if fcTokenList.FirstSolidTokenType = ttSemicolon then
  begin
    PopNode;
    exit;
  end;

  RecogniseClassBody;
  Recognise(ttEnd);

  RecogniseHintDirectives;

  PopNode;
end;

procedure TBuildParseTree.RecogniseClassHeritage;
begin
  PushNode(nClassHeritage);

  // ClassHeritage -> '(' IdentList ')'
  Recognise(ttOpenBracket);
  RecogniseIdentList(true);
  Recognise(ttCloseBracket);

  PopNode;
end;

procedure TBuildParseTree.RecogniseClassVisibility;
begin
  // ClassVisibility -> [PUBLIC | PROTECTED | PRIVATE | PUBLISHED]

  Recognise(ClassVisibility);
end;

procedure TBuildParseTree.RecogniseClassBody;
begin
  //ClassBody -> classdeclarations (access classdeclarations) ...
  PushNode(nClassBody);

  RecogniseClassDeclarations(false);

  while (fcTokenList.FirstSolidTokenType in ClassVisibility) do
  begin
    PushNode(nClassVisibility);
    RecogniseClassVisibility;
    RecogniseClassDeclarations(false);
    PopNode;
  end;

  PopNode;
end;

procedure TBuildParseTree.RecogniseClassDeclarations(const pbInterface: boolean);
const
  // can declare thse things in a class
  CLASS_DECL_WORDS = [ttProcedure, ttFunction,
    ttConstructor, ttDestructor, ttProperty, ttClass];
var
  lc: TSourceToken;
  lbStarted: boolean;
begin
  { this is a superset of delphi.
    in dcc these must be ordered vars, then fns then properties

    nb this can be empty  as in
      class TFoo(Tobject)
        private
        public
        end;

    or even
      class TBar(TObject) end;

   classdeclarations -> (procheader|fnheader|constructor|destructor|vars|property|) [';'] ...

   This is all the stuff in a class def between different visibility sections

    could a procedure, fuction, constructor, destructor, or property
     all of which start with the requite word
     or it could just be a varaible declaration, which starts with a new var name

     addition: must also do class fns and procs,
     eg
      " class function ClassName: ShortString; "
   }
  lbStarted := false;

  while (fcTokenList.FirstSolidTokenType in CLASS_DECL_WORDS) or
    (fcTokenList.FirstSolidWordType in IdentifierTypes) do
  begin
    // only make this node if it will have children
    if not lbStarted then
      PushNode(nClassDeclarations);
    lbStarted := true;

    lc := fcTokenList.FirstSolidToken;

    // these end the visibility section
    if fcTokenList.FirstSolidTokenType in (ClassVisibility + [ttEnd]) then
      break;

    case lc.TokenType of
      ttProcedure:
        RecogniseProcedureHeading(false, true);
      ttFunction:
        RecogniseFunctionHeading(false, true);
      ttClass:
      begin
          // must be followed by 'procedure' or 'function'
        case fcTokenList.SolidTokenType(2) of
          ttProcedure:
            RecogniseProcedureHeading(false, true);
          ttFunction:
            RecogniseFunctionHeading(false, true);
          else
            raise TEParseError.Create('Expected class procedure or class function', lc);
        end;

      end;
      ttConstructor:
      begin
          // no constructor on interface
        if pbInterface then
          raise TEParseError.Create('unexpected token', lc);
        RecogniseConstructorHeading(true);
      end;
      ttDestructor:
      begin
          // no constructor on interface
        if pbInterface then
          raise TEParseError.Create('unexpected token', lc);
        RecogniseDestructorHeading(true);
      end;
      ttProperty:
        RecogniseProperty;
      else
      begin
        // end of this list with next visibility section or class end?
        if lc.TokenType in CLASS_DECL_WORDS + [ttEnd] then
        begin
          break;
        end
          // vars start with an identifier
        else if lc.TokenType in IdentiferTokens then
        begin
          // no vars on interface
          if pbInterface then
            raise TEParseError.Create('unexpected token', lc);

          RecogniseVarDecl;
        end
        else
          raise TEParseError.Create('unexpected token', lc);
      end;
    end;

    // semicolon after each def.
    if fcTokenList.FirstSolidTokenType = ttSemicolon then
      Recognise(ttSemicolon)
    else
      Break; // except the last

  end;

  if lbStarted then
    PopNode;
end;

procedure TBuildParseTree.RecogniseProperty;
begin
  {PropertyList -> PROPERTY  Ident [PropertyInterface]  PropertySpecifiers

  There is also the syntax of reclaring properties to raise visibility
    -> Property Ident;
  }
  PushNode(nProperty);

  Recognise(ttProperty);

  RecogniseIdentifier(false);

  { this is omitted if it is a property redeclaration for visibility raising
    in that case it may still have directives and hints }
  if fcTokenList.FirstSolidTokenType in [ttColon, ttOpenSquareBracket] then
  begin
    RecognisePropertyInterface;
  end;

  RecognisePropertySpecifiers;

  RecognisePropertyDirectives;
  RecogniseHintDirectives;

  PopNode;
end;

procedure TBuildParseTree.RecognisePropertyInterface;
begin
  // PropertyInterface -> [PropertyParameterList] ':' Ident

  if fcTokenList.FirstSolidTokenType <> ttColon then
    RecognisePropertyParameterList;

  Recognise(ttColon);

  // recongising any type is overkill but hey
  RecogniseType;
end;

procedure TBuildParseTree.RecognisePropertyParameterList;
begin
  { PropertyParameterList -> '[' (IdentList ':' TypeId)/';'... ']'

   this forgets const and var, e.g.

   property ComplexArrayProp[const piIndex: integer; var pcsString: string]: boolean read GetComplexArrayProp ;

  }
  PushNode(nPropertyParameterList);

  Recognise(ttOpenSquareBracket);
  repeat
    if (fcTokenList.FirstSolidTokenType in [ttConst, ttVar, ttOut]) then
      Recognise([ttConst, ttVar, ttOut]);

    RecogniseIdentList(false);
    Recognise(ttColon);
    RecogniseTypeId;

    if fcTokenList.FirstSolidTokenType = ttSemicolon then
      Recognise(ttSemicolon)
    else
      break;

  until fcTokenList.FirstSolidTokenType = ttCloseSquareBracket;

  Recognise(ttCloseSquareBracket);

  PopNode;
end;

procedure TBuildParseTree.RecognisePropertySpecifiers;
var
  lc: TSourceToken;
const
  PROPERTY_SPECIFIERS: TTokenTypeSet = [ttIndex, ttRead, ttWrite, ttStored,
    ttDefault, ttNoDefault, ttImplements, ttDispId, ttReadOnly, ttWriteOnly];
begin
  {
   PropertySpecifiers ->
     [INDEX ConstExpr]
     [READ Ident]
     [WRITE Ident]
     [STORED (Ident | Constant)]
     [(DEFAULT ConstExpr) | NODEFAULT]
     [IMPLEMENTS TypeId]

     This is broken in that
       - can be more than one of them (and usually are for read and write)
       - left out dispid
       - left out readonly
   }
  lc := fcTokenList.FirstSolidToken;

  while lc.TokenType in PROPERTY_SPECIFIERS do
  begin
    PushNode(nPropertySpecifier);

    case lc.TokenType of
      ttIndex:
      begin
        Recognise(ttIndex);
        RecogniseConstantExpression;
      end;
      ttRead:
      begin
        Recognise(ttRead);
        RecognisePropertyAccess;
      end;
      ttWrite:
      begin
        Recognise(ttWrite);
        RecognisePropertyAccess;
      end;
      ttStored:
      begin
        Recognise(ttStored);
        RecogniseConstantExpression;
      end;
      ttDefault:
      begin
        Recognise(ttDefault);
        RecogniseConstantExpression;
      end;
      ttNoDefault:
      begin
        Recognise(ttNoDefault);
      end;
      ttImplements:
      begin
        Recognise(ttImplements);
        RecogniseTypeId;

          { can be a lost of them, e.g. "implements foo, bar" }
        while fcTokenList.FirstSolidTokenType = ttComma do
        begin
          Recognise(ttComma);
          RecogniseTypeId;
        end;
      end;
      ttDispId:
      begin
        Recognise(ttDispId);
        RecogniseConstantExpression;
      end;
      ttReadOnly:
      begin
        Recognise(ttReadOnly);
      end;
      ttWriteOnly:
      begin
        Recognise(ttWriteOnly);
      end;
      else
        raise TEParseError.Create('expected proeprty specifier',
          fcTokenList.FirstSolidToken);
    end;

    PopNode;
    lc := fcTokenList.FirstSolidToken;

  end;
end;

procedure TBuildParseTree.RecognisePropertyAccess;
begin
  { property access is the bit after the "read" or "write" in a property declaration
    This is usually just a procedure, function or simple var
    but sometimes it is a record or array field, .. or both e.g. "FDummy[0].ERX" }

  RecogniseIdentifier(false);

  { array access }
  if fcTokenList.FirstSolidTokenType = ttOpenSquareBracket then
  begin
    Recognise(ttOpenSquareBracket);
    // this is evaluated at compile-time, so we expect a constant subscript, e.g. "FDummy[0]"
    RecogniseConstantExpression;
    Recognise(ttCloseSquareBracket);
  end;

  { record field }
  if fcTokenList.FirstSolidTokenType = ttDot then
  begin
    Recognise(ttDot);
    // after the dot can be more structure, so recurse
    RecognisePropertyAccess;
  end

end;

procedure TBuildParseTree.RecogniseInterfaceType;
begin
  {
    InterfaceType -> INTERFACE [InterfaceHeritage]
         [ClassMethodList]
         [ClassPropertyList]
         END

    This is broken
      - left out Dispinterface
      - left out possible guid
      - left out forward declaration e.g. "IFoo = interface; "
  }
  PushNode(nInterfaceType);
  Recognise(InterfaceWords);

  if fcTokenList.FirstSolidTokenType = ttSemicolon then
  begin
    PopNode;
    exit;
  end;

  if fcTokenList.FirstSolidTokenType = ttOpenBracket then
    RecogniseInterfaceHeritage;

  if fcTokenList.FirstSolidTokenType = ttOpenSquareBracket then
    RecogniseInterfaceGuid;

  if fcTokenList.FirstSolidTokenType <> ttEnd then
  begin
    PushNode(nInterfaceBody);
    RecogniseClassDeclarations(true);
    PopNode;
  end;

  Recognise(ttEnd);

  PopNode;
end;

procedure TBuildParseTree.RecogniseInterfaceGuid;
begin
  // interface guid can be a litteral string, or occasionally a string constant
  PushNode(nInterfaceTypeGuid);

  Recognise(ttOpenSquareBracket);
  if fcTokenList.FirstSolidTokenType = ttLiteralString then
    Recognise(ttLiteralString)
  else
    RecogniseIdentifier(false);

  Recognise(ttCloseSquareBracket);

  PopNode;
end;

procedure TBuildParseTree.RecogniseInterfaceHeritage;
begin
  // InterfaceHeritage -> '(' IdentList ')'
  PushNode(nInterfaceHeritage);

  Recognise(ttOpenBracket);
  RecogniseIdentList(true);
  Recognise(ttCloseBracket);

  PopNode;
end;

procedure TBuildParseTree.RecogniseRequiresClause;
begin
  // RequiresClause -> REQUIRES IdentList... ';'

  PushNode(nRequires);

  Recognise(ttRequires);
  RecogniseIdentList(false);
  Recognise(ttSemicolon);

  PopNode;
end;

procedure TBuildParseTree.RecogniseContainsClause;
begin
  // ContainsClause -> CONTAINS IdentList... ';'

  { it's not an ident list it's a unit list can be
  "ident1, indent2" etc

  or more usually
  "ident1 in 'file1.pas',
  ident2 in 'file2.pas' " etc}

  PushNode(nContains);

  Recognise(ttContains);

  PushNode(nIdentList);

  RecogniseUsesItem(true);
  while fcTokenList.FirstSolidTokenType = ttComma do
  begin
    Recognise(ttComma);
    RecogniseUsesItem(true);
  end;
  PopNode;

  Recognise(ttSemicolon);

  PopNode;
end;

{ worker for RecogniseIdentList }

procedure TBuildParseTree.RecogniseIdentValue;
begin
  if fcTokenList.FirstSolidTokenType = ttEquals then
  begin
    Recognise(ttEquals);
    RecogniseExpr(true);
  end;
end;

procedure TBuildParseTree.RecogniseIdentList(const pbCanHaveUnitQualifier: boolean);
begin
  { IdentList -> Ident/','...

    now in D6 enum types can have numeric values
     e.g. (foo, bar = 3, baz)
  }
  PushNode(nIdentList);

  RecogniseIdentifier(pbCanHaveUnitQualifier);
  RecogniseIdentValue;

  while fcTokenList.FirstSolidTokenType = ttComma do
  begin
    Recognise(ttComma);
    RecogniseIdentifier(pbCanHaveUnitQualifier);
    RecogniseIdentValue;
  end;

  PopNode;
end;

procedure TBuildParseTree.RecogniseConstantExpression;
begin
  RecogniseExpr(true);
end;

procedure TBuildParseTree.RecogniseQualId;
begin
  { typecast, e.g. "(x as Ty)"
     or just bracketed, as in (x).y();

     See TestCastSimple.pas for the heinous examples

     QualID ->
      -> (Designator)
      -> (Designator as type)
      -> ident
      ->(pointervar + expr)
  }
  if (fcTokenList.FirstSolidTokenType = ttOpenBracket) then
  begin
    PushNode(nBracketedQual);
    Recognise(ttOpenBracket);
    RecogniseDesignator;

    if (fcTokenList.FirstSolidTokenType = ttAs) then
    begin
      Recognise(ttAs);
      RecogniseIdentifier(true);
    end;
    Recognise(ttCloseBracket);
    PopNode;
  end
  else
    // a simple ident - e.g. "x"
    RecogniseIdentifier(true);
end;

procedure TBuildParseTree.RecogniseIdentifier(const pbCanHaveUnitQualifier: boolean);
var
  lc: TSourceToken;
begin
  lc := fcTokenList.FirstSolidToken;

  if not (IdentifierNext) then
    raise TEParseError.Create('Expected identifer', lc);

  PushNode(nIdentifier);
  Recognise(IdentiferTokens);

  { tokens can be qualified by a unit name }
  if pbCanHaveUnitQualifier and (fcTokenList.FirstSolidTokenType = ttDot) then
  begin
    Recognise(ttDot);
    Recognise(IdentiferTokens);
  end;

  PopNode;
end;

{ the name of a procedure/function/constructor can be
  a plain name or classname.methodname }

procedure TBuildParseTree.RecogniseMethodName(const pbClassNameCompulsory: boolean);
begin
  if not (IdentifierNext) then
    raise TEParseError.Create('Expected identifer', fcTokenList.FirstSolidToken);

  Recognise(IdentiferTokens);

  if (fcTokenList.FirstSolidTokenType = ttDot) or pbClassNameCompulsory then
  begin
    Recognise(ttDot);
    RecogniseIdentifier(false);
  end
end;

procedure TBuildParseTree.RecogniseTypeId;
var
  lc: TSourceToken;
begin
  lc := fcTokenList.FirstSolidToken;

  { a type is an identifier. Or a file or other Reserved word }
  if lc.TokenType in BuiltInTypes then
    Recognise(BuiltInTypes)
  else if lc.TokenType = ttFile then
    Recognise(ttFile)
  else
    RecogniseIdentifier(true);
end;

procedure TBuildParseTree.RecogniseAsmBlock;
begin
  PushNode(nAsm);

  Recognise(ttAsm);
  while fcTokenList.FirstSolidTokenType <> ttEnd do
    RecogniseAsmStatement;

  Recognise(ttEnd);

  PopNode;
end;

procedure TBuildParseTree.RecogniseAsmStatement;
begin
  { um.

    AsmStatement
     -> [AsmLabel]
     -> Opcode [AsmParam] [',' AsmParam]...

     NB whitespace is significant, i.e. returns can seperate statement
     Help says ' semicolons, end-of-line characters, or Delphi comments.'

     I know that the help claims that a label is a prefix on a statement,
     but a label can be the last thing in an asm block
     so that would require a complete statement to consist of
     an optional label followed by an optional opcode

     Anyway labels are usually placed on a seperate line

     RET is opcode with no params
  }

  PushNode(nAsmStatement);

  if fcTokenList.FirstSolidTokenType = ttAtSign then
  begin
    RecogniseAsmLabel(true);
  end
  else
  begin
    RecogniseAsmOpcode;

    RecogniseWhiteSpace;

    while not (fcTokenList.FirstTokenType in [ttSemicolon, ttReturn,
        ttComment, ttEnd]) do
    begin
      if fcTokenList.FirstSolidTokenType = ttComma then
        Recognise(ttComma);
      RecogniseAsmParam;

      RecogniseWhiteSpace;

      if fcTokenList.FirstSolidTokenType = ttEnd then
        Break;

      if fcTokenList.FirstSolidTokenType = ttSemiColon then
      begin
        Recognise(ttSemiColon);
        break;
      end;
    end;

  end;

  PopNode;
end;

{ purpose: to consume white space
  make sure that buffertokens(0)
  contains a retunr, comment or solid token }

procedure TBuildParseTree.RecogniseWhiteSpace;
begin
  while fcTokenList.FirstTokenType = ttWhiteSpace do
    Recognise(ttWhiteSpace);
end;

procedure TBuildParseTree.RecogniseAsmIdent;
var
  lc: TSourceToken;
begin
  PushNode(nAsmIdent);

  { can contain '@' signs }
  lc := fcTokenList.FirstSolidToken;

  if not (lc.TokenType in IdentiferTokens + [ttAtSign]) then
    raise TEParseError.Create('Expected asm identifer', lc);

  while (lc.TokenType in IdentiferTokens + [ttAtSign]) do
  begin
    Recognise(IdentiferTokens + [ttAtSign]);
    { whitespace ends this so no fcTokenList.FirstSolidToken }
    lc := fcTokenList.First;
  end;

  PopNode;
end;

procedure TBuildParseTree.RecogniseAsmOpcode;
begin
  { these are all short (3 chars? 4 chars)

    but it's too large a cast and varies from CPU to CPU
    so I will not enumerate them all

    some overlap with Delphi reserved words
    e.g. SHL
   }
  PushNode(nASMOpcode);
  if IdentifierNext then
    RecogniseIdentifier(false)
  else if WordTypeOfToken(fcTokenList.FirstSolidTokenType) in TextualWordTypes then
    // match anything
    Recognise(fcTokenList.FirstSolidTokenType)
  else
    raise TEParseError.Create('Expected asm opcode', fcTokenList.FirstSolidToken);

  PopNode;
end;

function IsAsmLabel(const pt: TSourceToken): boolean;
begin
  Result := false;
  if pt = nil then
    exit;
  Result := (pt.TokenType in [ttNumber, ttIdentifier, ttAtSign]) or
    (pt.WordType in [wtReservedWord, wtReservedWordDirective,
    wtBuiltInConstant, wtBuiltInType]);
end;

procedure TBuildParseTree.RecogniseAsmLabel(const pbColon: boolean);
begin
  PushNode(nAsmLabel);

  Recognise(ttAtSign);
  if fcTokenList.FirstSolidTokenType = ttAtSign then
    Recognise(ttAtSign);

  { label can be a number, eg "@@1:"
    or an identifier that starts with a number, eg "@@2a"

    can also be a delphi keyword, e.g. "@@repeat:"
  }

  while IsAsmLabel(fcTokenList.First) do
  begin
    Recognise(fcTokenList.FirstTokenType);
  end;

  if pbColon then
    Recognise(ttColon);

  PopNode;
end;

procedure TBuildParseTree.RecogniseAsmParam;
const
  ASM_EXPRESSION_START = [ttOpenBracket, ttOpenSquareBracket, ttNumber,
    ttNot, ttLiteralString,
    ttTrue, ttFalse, ttPlus, ttMinus, ttType, ttOffset];
var
  lc: TSourceToken;
  lbHasLabel: boolean;
begin
  { um.

  AsmParam
    -> Ident
    -> '@' ident
    -> '[' AsmExpr ']'
    -> Ident(number)
  }

  lbHasLabel := false;
  PushNode(nAsmParam);

  lc := fcTokenList.FirstSolidToken;

  if lc.TokenType = ttAtSign then
  begin
    RecogniseAsmLabel(false);
    lbHasLabel := true;

    if fcTokenList.FirstSolidTokenType = ttDot then
      Recognise(ttDot);
  end;

  if IdentifierNext or (lc.TokenType in ASM_EXPRESSION_START) then
  begin
    RecogniseAsmExpr;
  end
  else
  begin
    if not lbHasLabel then
      raise TEParseError.Create('Expected asm param', lc);
  end;

  PopNode;
end;

const
  ASM_OPERATORS = [ttPlus, ttMinus, ttAnd, ttOr, ttTimes, ttFloatDiv, ttPtr, ttColon];

  { having to wing this one. it is like expressions, but different }

procedure TBuildParseTree.RecogniseAsmExpr;
var
  lc: TSourceToken;
begin
  RecogniseAsmFactor;

  { can't go past returns }
  lc := fcTokenList.FirstTokenWithExclusion([ttWhiteSpace]);
  while lc.TokenType in ASM_OPERATORS do
  begin
    RecogniseAsmOperator;
    RecogniseAsmFactor;
    lc := fcTokenList.FirstTokenWithExclusion([ttWhiteSpace]);
  end;
end;

procedure TBuildParseTree.RecogniseAsmOperator;
begin
  Recognise(ASM_OPERATORS);
end;

procedure TBuildParseTree.RecogniseAsmFactor;
begin
  if fcTokenList.FirstSolidTokenType = ttNot then
    Recognise(ttNot);

  if fcTokenList.FirstSolidTokenType = ttMinus then
    Recognise(ttMinus);

  if fcTokenList.FirstSolidTokenType = ttAt then
    Recognise(ttAt);

  if fcTokenList.FirstSolidTokenType = ttType then
    Recognise(ttType);

  if fcTokenList.FirstSolidTokenType = ttOffset then
    Recognise(ttOffset);

  case fcTokenList.FirstSolidTokenType of
    ttNumber:
      Recognise(ttNumber);
    ttLiteralString:
      Recognise(ttLiteralString);
    ttTrue:
      Recognise(ttTrue);
    ttFalse:
      Recognise(ttFalse);
    ttOpenBracket:
    begin
      Recognise(ttOpenBracket);
      RecogniseAsmExpr;
      Recognise(ttCloseBracket);
    end;
    ttOpenSquareBracket:
    begin
      Recognise(ttOpenSquareBracket);
      RecogniseAsmExpr;
      Recognise(ttCloseSquareBracket);
    end
    else
    begin
      RecogniseAsmIdent;
    end
  end;

  while fcTokenList.FirstSolidTokenType in [ttDot] do
  begin
    Recognise([ttDot]);

    if fcTokenList.FirstSolidTokenType = ttAtSign then
      Recognise(ttAtSign);
    RecogniseAsmIdent;
  end;

  if fcTokenList.FirstSolidTokenType = ttOpenBracket then
  begin
    Recognise(ttOpenBracket);
    RecogniseAsmFactor;
    Recognise(ttCloseBracket);
  end;

end;

procedure TBuildParseTree.RecogniseHintDirectives;
begin
  if ((fcTokenList.FirstSolidTokenType = ttSemicolon) and
    (fcTokenList.SolidTokenType(2) in HintDirectives)) or
    (fcTokenList.FirstSolidTokenType in HintDirectives) then
  begin
    if fcTokenList.FirstSolidTokenType = ttSemicolon then
      Recognise(ttSemicolon);

    PushNode(nHintDirectives);

    while (fcTokenList.FirstSolidTokenType in HintDirectives) do
    begin
      Recognise(HintDirectives);
    end;

    PopNode;
  end;
end;

procedure TBuildParseTree.RecognisePropertyDirectives;
const
  { this can be specified at the end after a semicolon
  so it's not just in the specifiers

  the default directive works differently for array and not-array properties

  for non-array properties it is followed by an identifier
  }
  PropertyDirectives = [ttDefault, ttNoDefault, ttStored];
begin
  if ((fcTokenList.FirstSolidTokenType = ttSemicolon) and
    (fcTokenList.SolidTokenType(2) in PropertyDirectives)) or
    (fcTokenList.FirstSolidTokenType in PropertyDirectives) then
  begin
    if fcTokenList.FirstSolidTokenType = ttSemicolon then
      Recognise(ttSemicolon);

    while fcTokenList.FirstSolidTokenType in PropertyDirectives do
    begin
      PushNode(nPropertyDirective);

      case fcTokenList.FirstSolidTokenType of
        ttDefault:
        begin
          Recognise(ttDefault);
          if fcTokenList.FirstSolidTokenType <> ttSemicolon then
            RecogniseConstantExpression;
        end;
        ttNoDefault:
        begin
          Recognise(ttNoDefault);
        end;
        ttStored:
        begin
          Recognise(ttStored);
          if fcTokenList.FirstSolidTokenType <> ttSemicolon then
            RecogniseConstantExpression;
        end;
      end;

      PopNode;
    end;

  end;

end;

procedure TBuildParseTree.RecogniseExportsSection;
begin
  PushNode(nExports);

  Recognise(ttExports);
  RecogniseExportedProc;

  // more to come?
  while fcTokenList.FirstTokenType <> ttSemicolon do
  begin
    Recognise(ttComma);
    RecogniseExportedProc;
  end;

  Recognise(ttSemicolon);

  PopNode;
end;

procedure TBuildParseTree.RecogniseExportedProc;
const
  ExportedDirectives: TTokenTypeSet = [ttName, ttIndex, ttResident];
var
  lc: TSourceToken;
begin
  PushNode(nExportedProc);

  RecogniseIdentifier(true);

  if fcTokenList.FirstSolidTokenType = ttOpenBracket then
    RecogniseFormalParameters;

  while fcTokenList.FirstSolidTokenType in ExportedDirectives do
  begin
    lc := fcTokenList.FirstSolidToken;

    case lc.TokenType of
      ttName:
      begin
        Recognise(ttName);
        Recognise(IdentiferTokens + [ttLiteralString]);
      end;
      ttIndex:
      begin
        Recognise(ttIndex);
        Recognise(ttNumber);
      end;
      ttResident:
        Recognise(ttResident);
      else
        raise TEParseError.Create('Expected export directive', lc);
    end;
  end;

  PopNode;
end;

procedure TBuildParseTree.RecogniseActualParams;
var
  lbMore: boolean;
begin
  PushNode(nActualParams);

  Recognise(ttOpenBracket);

  if fcTokenList.FirstSolidTokenType <> ttCloseBracket then
  begin
    //RecogniseExprList;

    repeat
      RecogniseActualParam;

      lbMore := fcTokenList.FirstSolidTokenType = ttComma;
      if lbMore then
        Recognise(ttComma);

    until not lbMore;

  end;

  Recognise(ttCloseBracket);

  PopNode;
end;

procedure TBuildParseTree.RecogniseActualParam;
const
  EXPR_TYPES = [ttNumber, ttIdentifier, ttLiteralString,
    ttPlus, ttMinus, ttOpenBracket, ttOpenSquareBracket, ttNot, ttInherited];
var
  lc: TSourceToken;
begin
  lc := fcTokenList.FirstSolidToken;

  { all kinds of reserved words can sometimes be param names
    thanks to COM and named params
    See LittleTest43.pas }
  if ( not (lc.TokenType in EXPR_TYPES)) and StrIsAlphaNum(lc.SourceCode) and
    ( not IsIdentifierToken(lc)) then
  begin
    { quick surgery. Perhaps even a hack -
      reclasify the token, as it isn't what it thinks it is
      e.g. if this word is 'then', then
      we don't want a linbreak after it like in if statements }
    lc.TokenType := ttIdentifier;
    Recognise(ttIdentifier);

    { this must be a named value, e.g. "end = 3". See LittleTest43.pas for e.g.s }
    Recognise(ttAssign);
    RecogniseExpr(true);
  end
  else
  begin
    RecogniseExpr(true);

    { ole named param syntax, e.g.
      " MSWord.TextToTable(ConvertFrom := 2, NumColumns := 3);"
    }

    if fcTokenList.FirstSolidTokenType = ttAssign then
    begin
      Recognise(ttAssign);
      RecogniseExpr(true);
    end

      { str width specifiers e.g. " Str(val:0, S);" this is an odd wart on the syntax }
    else if fcTokenList.FirstSolidTokenType = ttColon then
    begin
      { can be more than one of them }
      while fcTokenList.FirstSolidTokenType = ttColon do
      begin
        Recognise(ttColon);
        RecogniseExpr(true);
      end;
    end;
  end;
end;

end.

