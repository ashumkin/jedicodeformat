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
  { delphi }
  Contnrs,
  { local } ParseTreeNode, ParseTreeNodeType,
  SourceToken, SourceTokenList, Tokens;



type
  TBuildParseTree = class(TObject)
  private
    fbMadeTree: Boolean;
    fbParseError: boolean;
    fsParseErrorMessage: string;
    fiTokenIndex: integer;

    fcRoot: TParseTreeNode;
    fcStack: TStack;
    fcTokenList: TSourceTokenList;

    procedure RecogniseGoal;
    procedure RecogniseUnit;
    procedure RecogniseProgram;
    procedure RecognisePackage;
    procedure RecogniseLibrary;

    procedure RecogniseProgramBlock;
    procedure RecogniseUsesClause(const pbInFiles: Boolean);
    procedure RecogniseUsesItem(const pbInFiles: Boolean);

    procedure RecogniseInterfaceSection;
    procedure RecogniseInterfaceDecls;
    procedure RecogniseInterfaceDecl;
    procedure RecogniseExportedHeading;

    procedure RecogniseIdentifier(const pbCanHaveUnitQualifier: Boolean);
    procedure RecogniseImplementationSection;
    procedure RecogniseDeclSections;
    procedure RecogniseDeclSection;
    procedure RecogniseInitSection;
    procedure RecogniseBlock;
    procedure RecogniseIdentList(const pbCanHaveUnitQualifier: Boolean);
    procedure RecogniseIdentValue;

    procedure RecogniseLabelDeclSection;
    procedure RecogniseConstSection;
    procedure RecogniseConstantDecl;

    procedure RecogniseTypeSection;
    procedure RecogniseVarSection;
    procedure RecogniseProcedureDeclSection;

    // set pbAnon = true if the proc has no name
    procedure RecogniseProcedureHeading(const pbAnon, pbCanInterfaceMap: Boolean);
    procedure RecogniseFunctionHeading(const pbAnon, pbCanInterfaceMap: Boolean);
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
    procedure RecogniseClassDeclarations(const pbInterface: Boolean);

    procedure RecogniseInterfaceType;
    procedure RecogniseObjectType;
    procedure RecogniseVariantSection;
    procedure RecogniseVarDecl;
    procedure RecogniseAddOp;
    procedure RecogniseDesignator;
    procedure RecogniseExpr;
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

    procedure RecogniseFunctionDecl;
    procedure RecogniseProcedureDecl;
    procedure RecogniseConstructorDecl;
    procedure RecogniseDestructorDecl;

    procedure RecogniseFormalParameters;
    procedure RecogniseFormalParam;
    procedure RecogniseParameter;
    procedure RecogniseActualParams;

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
    procedure RecogniseRequiresClause;
    procedure RecogniseInterfaceGuid;
    procedure RecogniseClassHeritage;
    procedure RecogniseClassVisibility;
    procedure RecogniseMethodName(const pbClassNameCompulsory: Boolean);

    procedure RecogniseAsmBlock;
    procedure RecogniseAsmParam;
    procedure RecogniseAsmStatement;
    procedure RecogniseAsmExpr;
    procedure RecogniseAsmIdent;
    procedure RecogniseAsmOpcode;
    procedure RecogniseWhiteSpace;

    procedure RecogniseHintDirectives;
    procedure RecognisePropertyDirectives;
    procedure RecogniseExternalProcDirective;

    procedure Recognise(const peTokenTypes: TTokenTypeSet); overload;
    procedure Recognise(const peTokenType: TTokenType); overload;

    function PushNode(const peNodeType: TParseTreeNodeType): TParseTreeNode;
    function PopNode: TParseTreeNode;
    function TopNode: TParseTreeNode;
    function IdentifierNext: Boolean;
    function ArrayConstantNext: boolean;

  protected
  public
    constructor Create;
    destructor Destroy; override;

    procedure BuildParseTree;
    procedure Clear;

    property ParseError: boolean read fbParseError;
    property ParseErrorMessage: String read fsParseErrorMessage;
    property Root: TParseTreeNode read fcRoot;


    property TokenList: TSourceTokenList read fcTokenList write fcTokenList;
  end;

implementation

uses
  { delphi } SysUtils,  Dialogs,
  { local } ParseError;


{------------------------------------------------------------------------------
  standard overrides }

constructor TBuildParseTree.Create;
begin
  inherited;
  fcStack := TStack.Create;
  fcRoot := nil;
end;

destructor TBuildParseTree.Destroy;
begin
  freeAndNil(fcStack);
  inherited;
end;

procedure TBuildParseTree.Clear;
begin
  while fcStack.Count > 0 do
    fcStack.Pop;

  FreeAndNil(fcRoot);
end;


procedure TBuildParseTree.BuildParseTree;
var
  lsTokenMessage: string;
begin
  Assert(TokenList <> nil);

  Clear;

  { read to end of file necessary?
  liIndex := 0;
  while BufferTokens(liIndex).TokenType <> ttEOF do
  begin
    BufferTokens(liIndex);
    inc(liIndex);
  end; }

  fiTokenIndex := 0;
  fbParseError := False;
  fsParseErrorMessage := '';

  try
    RecogniseGoal;

    Assert(fcStack.Count = 0);
  except
    on E: TEParseError do
    begin
      fbParseError := True;
      fsParseErrorMessage := E.Message;
      lsTokenMessage := E.TokenMessage;
      if lsTokenMessage <> '' then
        fsParseErrorMessage :=  fsParseErrorMessage + ' near ' + lsTokenMessage;
    end;
    on E: exception do
    begin
      fbParseError := True;
      fsParseErrorMessage := E.Message;
    end;
  end;

  fbMadeTree := True;
end;


{-------------------------------------------------------------------------------
  recogniser support }

procedure TBuildParseTree.Recognise(const peTokenTypes: TTokenTypeSet);
var
  lcCurrentToken: TSourceToken;

  function Matched: Boolean;
  begin
    Result := (lcCurrentToken.TokenType in peTokenTypes);
  end;

  function DescribeTarget: string;
  begin
    Result := '" ';

    if peTokenTypes <> [] then
      Result := Result + TokenTypesToString(peTokenTypes);

    Result := Result + '"';
  end;

begin

  // must accept something
  Assert(peTokenTypes <> []);

  { read tokens up to and including the specified one.
    Add them to the parse tree at the current growing point  }
  repeat
    lcCurrentToken := TokenList.ExtractFirst;

    TopNode.AddChild(lcCurrentToken);

    // the the match must be the first solid token
    if (not (lcCurrentToken.TokenType in [ttReturn, ttWhiteSpace, ttComment])) and
      (not Matched) then
    begin
        Raise TEParseError.Create('Unexpected token, expected ' + DescribeTarget, lcCurrentToken);
    end;

  until Matched;
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
function TBuildParseTree.IdentifierNext: Boolean;
var
  lc: TSourceToken;
begin
  lc := TokenList.FirstSolidToken;

  { We have to admit directives and type names as identifiers.
    see TestBogusDirectives.pas for the reasons why }
  Result := (lc.WordType in IdentifierTypes);
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

  if TokenList.Count < 1 then
    Raise TEParseError.Create('No source to parse', nil);

 lc := TokenList.FirstSolidToken;
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
      Raise TEParseError.Create('Expected program, package, library, unit', lc);
  end
end;

procedure TBuildParseTree.RecogniseProgram;
begin
  // Program -> [PROGRAM Ident ['(' IdentList ')'] ';']  ProgramBlock '.'
  PushNode(nProgram);

  PushNode(nUnitHeader);
  Recognise(ttProgram);

  PushNode(nUnitName);
  RecogniseIdentifier(False);
  PopNode;

  if TokenList.FirstSolidTokenType = ttOpenBracket then
  begin
    Recognise(ttOpenBracket);
    RecogniseIdentList(False);
    Recognise(ttCloseBracket);
  end;

  if TokenList.FirstSolidTokenType = ttSemiColon then
    Recognise(ttSemicolon);

  PopNode;

  RecogniseProgramBlock;
  Recognise(ttDot);

  PopNode;
end;

procedure TBuildParseTree.RecogniseUnit;
begin
  // Unit -> UNIT Ident ';' InterfaceSection ImplementationSection InitSection '.'
  PushNode(nUnit);

  PushNode(nUnitHeader);
  Recognise(ttUnit);

  PushNode(nUnitName);
  RecogniseIdentifier(False);
  PopNode;
  Recognise(ttSemicolon);

  PopNode;

  RecogniseInterfaceSection;
  RecogniseImplementationSection;
  RecogniseInitSection;
  Recognise(ttDot);

  PopNode;
end;

procedure TBuildParseTree.RecognisePackage;
begin
  // Package -> PACKAGE Ident ';' [RequiresClause] [ContainsClause] END '.'
  PushNode(nPackage);

  PushNode(nUnitHeader);
  Recognise(ttPackage);

  PushNode(nUnitName);
  RecogniseIdentifier(False);
  PopNode;
  Recognise(ttSemicolon);
  PopNode;

  if TokenList.FirstSolidTokenType = ttRequires then
    RecogniseRequiresClause;

  if TokenList.FirstSolidTokenType = ttContains then
    RecogniseContainsClause;

  Recognise(ttEnd);
  Recognise(ttDot);

  PopNode;
end;

procedure TBuildParseTree.RecogniseLibrary;
begin
  // Library -> LIBRARY Ident ';' ProgramBlock '.'
  PushNode(nLibrary);

  PushNode(nUnitHeader);
  Recognise(ttLibrary);

  PushNode(nUnitName);
  RecogniseIdentifier(False);
  PopNode;
  Recognise(ttSemicolon);
  PopNode;

  RecogniseProgramBlock;
  Recognise(ttDot);

  PopNode;
end;

procedure TBuildParseTree.RecogniseProgramBlock;
var
  lc: TSourceToken;
begin
  // ProgramBlock -> [UsesClause] Block

 lc := TokenList.FirstSolidToken;

  if lc.TokenType = ttUses then
    RecogniseUsesClause(True);

 RecogniseBlock;
end;

procedure TBuildParseTree.RecogniseUsesClause(const pbInFiles: Boolean);
begin
  // UsesClause -> USES IdentList ';'
  PushNode(nUses);

  Recognise(ttUses);

  // IdentList -> Ident/','...
  PushNode(nIdentList);

  RecogniseUsesItem(pbInFiles);

  while TokenList.FirstSolidTokenType = ttComma do
  begin
    Recognise(ttComma);
    RecogniseUsesItem(pbInFiles);
  end;

  PopNode;

  Recognise(ttSemicolon);

  PopNode;
end;

procedure TBuildParseTree.RecogniseUsesItem(const pbInFiles: Boolean);
begin
  PushNode(nUsesItem);

  RecogniseIdentifier(False);

  if pbInFiles and (TokenList.FirstSolidTokenType  = ttIn) then
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

  if TokenList.FirstSolidTokenType = ttUses then
    RecogniseUsesClause(False);

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
  while TokenList.FirstSolidTokenType in [ttConst, ttResourceString, ttType, ttVar, ttThreadVar] + ProcedureWords do
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

  lc := TokenList.FirstSolidToken;
  lt := TokenList.FirstSolidTokenType;

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
      Raise TEParseError.Create('Expected const, type, var, procedure or function', lc);
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

  lc := TokenList.FirstSolidToken;
  lt := lc.TokenType;

  case lt of
    ttProcedure:
    begin
      RecogniseProcedureHeading(False, False);
    end;
    ttFunction:
    begin
      RecogniseFunctionHeading(False, False);
    end;
    else
      Raise TEParseError.Create('Expected function or procedure', lc);
  end;

  Recognise(ttSemicolon);
  RecogniseProcedureDirectives;
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

  if TokenList.FirstSolidTokenType = ttUses then
    RecogniseUsesClause(False);

  RecogniseDeclSections;

  PopNode;
end;

procedure TBuildParseTree.RecogniseBlock;
var
  lc: TSourceToken;
  lt: TTokenType;
begin
  { Block -> [DeclSection] CompoundStmt }

  lc := TokenList.FirstSolidToken;
  lt := lc.TokenType;


  PushNode(nBlock);

  // [DeclSection]

  if lt in (Declarations + ProcedureWords) then
    RecogniseDeclSections;

  if TokenList.FirstSolidTokenType = ttAsm then
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
  while TokenList.FirstSolidTokenType in
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

  lc := TokenList.FirstSolidToken;
  lt := TokenList.FirstSolidTokenType;

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
      Raise TEParseError.Create('Expected label, const, type, var, procedure or function', lc);
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
  RecogniseIdentList(False);
  Recognise(ttSemicolon);

  PopNode;
end;

procedure TBuildParseTree.RecogniseConstSection;
begin
  {
    ConstSection -> CONST (ConstantDecl ';')...
  }
  PushNode(nConstSection);
  Recognise([ttConst, ttResourceString]);

  while TokenList.FirstSolidWordType in IdentifierTypes do
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

  RecogniseIdentifier(False);

  lc := TokenList.FirstSolidToken;

  if lc.TokenType = ttEquals then
  begin
    Recognise(ttEquals);
    RecogniseConstantExpression;
  end
  else
  if lc.TokenType = ttColon then
  begin
    Recognise(ttColon);
    //RecogniseTypeId;
    RecogniseType;
    Recognise(ttEquals);
    RecogniseTypedConstant;
  end
  else
    Raise TEParseError.Create('Expected equals or colon', lc);

  PopNode;
end;

procedure TBuildParseTree.RecogniseTypeSection;
begin
  {
  TypeSection -> TYPE (TypeDecl ';')...
  }
  PushNode(nTypeSection);
  Recognise(ttType);

  while TokenList.FirstSolidWordType in IdentifierTypes do
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

  RecogniseIdentifier(False);
  Recognise(ttEquals);

  // type or restricted type
  if (TokenList.FirstSolidTokenType in [ttObject, ttClass, ttInterface, ttDispInterface]) then
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
}
function TBuildParseTree.ArrayConstantNext: boolean;
var
  liIndex: integer;
  liBracketLevel: integer;
  tt: TTokenType;
begin
  Result := False;

  if TokenList.FirstSolidTokenType <> ttOpenBracket then
    exit;

  liIndex := 0;
  liBracketLevel := 0;

  // scan past the open bracket
  while TokenList.SourceTokens[liIndex].TokenType <> ttOpenBracket do
    inc(liIndex);

  inc(liIndex);

  // look forward to find the first comma or semicolon
  while True do
  begin
    if liIndex >= TokenList.Count then
      break;

    tt := TokenList.SourceTokens[liIndex].TokenType;

    if tt = ttOpenBracket then
      inc(liBracketLevel)
    else if tt = ttCloseBracket then
      dec(liBracketLevel)
    else if (tt = ttComma) and (liBracketLevel = 0) then
    begin
      Result := True;
      break;
    end
    else if (tt = ttSemicolon) and (liBracketLevel = 0) then
    begin
      Result := False;
      break;
    end;

    inc(liIndex);
  end;

end;

procedure TBuildParseTree.RecogniseTypedConstant;
begin
   { TypedConstant -> (ConstExpr | ArrayConstant | RecordConstant)

    How to tell these apart?

    The record constant must start with open brackets, a field name followed by a colon,
    e.g.   "AREC: TMap = (s1: 'Foo'; i1: 1; i2: 4);"
     No complexity is permitted here. All that can vary is the name

     Array and normal constants are trickier, as both can start with an
     arbitrary number of open brackets
     a normal constant is an expression, and an array constant is a
     bracketed comma-sperated list of them
     You can't look for the word 'array' in the just-parsed text
     as an alias type could be used
    }
    if (TokenList.FirstSolidTokenType = ttOpenBracket) and
      (TokenList.SolidWordType(2) in IdentifierTypes) and
      (TokenList.SolidTokenType(3) = ttColon) then
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
  while (TokenList.FirstSolidTokenType = ttComma) do
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
  while (TokenList.FirstSolidTokenType = ttSemicolon) do
  begin
    Recognise(ttSemicolon);
    RecogniseRecordFieldConstant;
  end;

  Recognise(ttCloseBracket);
  PopNode;
end;

procedure TBuildParseTree.RecogniseRecordFieldConstant;
begin
  // RecordFieldConstant -> Ident ':' TypedConstant

  PushNode(nRecordFieldConstant);

  RecogniseIdentifier(False);
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

  lc := TokenList.FirstSolidToken;
  lc2 := TokenList.SolidToken(2);

  { type can be prefixed with a unit name, e.g.
    Classes.TList; }
  if lc2.TokenType = ttDot then
  begin
    RecogniseIdentifier(False);
    Recognise(ttDot);
  end;

  if (lc.TokenType = ttType) then
  begin
    { this can be a prefix. See help under "Declaring types".
      an e.g. is in TestDeclarations.pas }
    Recognise(ttType);
  end;

  if (lc.TokenType = ttConst) then
    Recognise(ttConst)
  else if (lc.TokenType in RealTypes + OrdTypes) then
    RecogniseSimpleType
  else if (lc.TokenType = ttOpenBracket) then
    RecogniseSimpleType // for enumerated types
  else if (lc.TokenType in [ttPacked, ttArray, ttSet, ttFile, ttRecord]) then
    RecogniseStrucType
  else if (lc.TokenType = ttHat) then
    RecognisePointerType
  else if (lc.TokenType in StringWords) then
    RecogniseStringType
  else if (lc.TokenType in [ttProcedure, ttFunction]) then
    RecogniseProcedureType
  else if lc.TokenType in VariantTypes then
    RecogniseVariantType
  else if (lc.TokenType = ttClass) and (lc2.TokenType = ttOf) then
    RecogniseClassRefType
  else if (lc.WordType in IdentifierTypes) then
  begin
    { could be a subrange on an enum, e.g. "clBlue .. clBlack" }
    if TokenList.SolidTokenType(2) = ttDoubleDot then
      RecogniseSubRangeType
    else
      // some previously declared type that this simple prog does not know of
      RecogniseTypeId;
  end
  else
    RecogniseSimpleType;
    //Raise TEParseError.Create('Expected type definition', lc);

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

  lc := TokenList.FirstSolidToken;
  case lc.TokenType of
    ttObject:
      RecogniseObjectType;
    ttClass:
      RecogniseClassType;
    ttInterface, ttDispInterface:
      RecogniseInterfaceType;
    else
      Raise TEParseError.Create('Expected object, class or interface', lc);
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

    lc := TokenList.FirstSolidToken;

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

  lc := TokenList.FirstSolidToken;

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
  if TokenList.FirstSolidTokenType = ttDoubleDot then
  begin
    Recognise(ttDoubleDot);
    RecogniseConstantExpression;
  end;

  PopNode;
end;

procedure TBuildParseTree.RecogniseEnumeratedType;
begin
  // EnumeratedType -> '(' IdentList ')'
  PushNode(nEnumeratedType);

  Recognise(ttOpenBracket);
  RecogniseIdentList(False);
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

 if TokenList.FirstSolidTokenType = ttString then
 begin
  Recognise(ttString);
  if TokenList.FirstSolidTokenType = ttOpenSquareBracket then
  begin
    // e.g. var f = String[30];
    Recognise(ttOpenSquareBracket);
    Recognise(ttNumber);
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

  if TokenList.FirstSolidTokenType = ttPacked then
    Recognise(ttPacked);

  lc := TokenList.FirstSolidToken;

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
      Raise TEParseError.Create('Expected array, set, file or record type', lc);
  end;
end;

procedure TBuildParseTree.RecogniseArrayType;
begin
  // ArrayType -> ARRAY ['[' OrdinalType/','... ']'] OF Type
  PushNode(nArrayType);

  Recognise(ttArray);

  if TokenList.FirstSolidTokenType = ttOpenSquarebracket then
  begin
    Recognise(ttOpenSquareBracket);

    RecogniseOrdinalType;
    while TokenList.FirstSolidTokenType = ttComma do
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
  if TokenList.FirstSolidTokenType <> ttEnd then
    RecogniseFieldList;
  Recognise(ttEnd);

  RecogniseHintDirectives;

  PopNode;
end;

procedure TBuildParseTree.RecogniseFieldList;
begin
  // FieldList ->  FieldDecl/';'... [VariantSection] [';']

  while (not (TokenList.FirstSolidTokenType in [ttEnd, ttCase]))
    and (not (TokenList.FirstSolidTokenType = ttCloseBracket))  do
  begin
    RecogniseFieldDecl;
    if TokenList.FirstSolidTokenType = ttSemicolon then
      Recognise(ttSemicolon)
    else
      Break;
  end;

  if TokenList.FirstSolidTokenType = ttCase then
    RecogniseVariantSection;

  if TokenList.FirstSolidTokenType = ttSemicolon then
    Recognise(ttSemicolon);
end;

procedure TBuildParseTree.RecogniseFieldDecl;
begin
  // FieldDecl -> IdentList ':' Type
  PushNode(nFieldDeclaration);

  RecogniseIdentList(False);
  Recognise(ttColon);
  RecogniseType;

  PopNode;
end;

procedure TBuildParseTree.RecogniseVariantSection;
begin
  PushNode(nRecordVariantSection);

  // VariantSection -> CASE [Ident ':'] TypeId OF RecVariant/';'...
  Recognise(ttCase);

  // is there an 'of' 2 tokens hence? If not, must be 'ident:' first
  if not (TokenList.SolidTokenType(2) = ttOf) then
  begin
    RecogniseIdentifier(False);
    Recognise(ttColon);
  end;

  RecogniseTypeId;
  Recognise(ttOf);

  // I have tested and that there must be at least 1 case in a var section
  repeat
    RecogniseRecVariant;

    // semicolon is optional on the last one
    if TokenList.FirstSolidTokenType = ttSemicolon then
      Recognise(ttSemicolon)
    else
      break;

  until (TokenList.FirstSolidTokenType in [ttEnd, ttCloseBracket]);

  PopNode;
end;

procedure TBuildParseTree.RecogniseRecVariant;
begin
 // RecVariant -> ConstExpr/','...  ':' '(' [FieldList] ')'

 PushNode(nRecordVariant);

  RecogniseConstantExpression;
  while TokenList.FirstSolidTokenType = ttComma do
  begin
    Recognise(ttComma);
    RecogniseConstantExpression;
  end;

  Recognise(ttColon);
  Recognise(ttOpenBracket);

  if TokenList.FirstSolidTokenType <> ttCloseBracket then
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
  if TokenList.FirstSolidTokenType = ttOf then
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
  if TokenList.FirstSolidTokenType = ttProcedure then
    RecogniseProcedureHeading(True, False)
  else if TokenList.FirstSolidTokenType = ttFunction then
    RecogniseFunctionHeading(True, False)
  else
    Raise TEParseError.Create('Expected procedure or function type', TokenList.FirstSolidToken);

  if TokenList.FirstSolidTokenType = ttOf then
  begin
    Recognise(ttOf);
    Recognise(ttObject);
  end;

  PopNode;
end;

procedure TBuildParseTree.RecogniseVarSection;
const
  END_VAR_SECTION: TTokenTypeSet =
    [ttVar, ttThreadVar, ttConst, ttLabel, ttResourceString, ttType,
      ttBegin, ttEnd, ttImplementation, ttInitialization,
      ttProcedure, ttFunction, ttConstructor, ttDestructor, ttClass];
begin
  PushNode(nVarSection);

  // VarSection -> VAR (VarDecl ';')...
  Recognise([ttVar, ttThreadvar]);

  repeat
    RecogniseVarDecl;
    Recognise(ttSemicolon);
  until (TokenList.FirstSolidTokenType in END_VAR_SECTION);

  PopNode;
end;


procedure TBuildParseTree.RecogniseVarDecl;
var
  lc: TSourceToken;
begin
  // VarDecl -> IdentList ':' Type [(ABSOLUTE (Ident | ConstExpr)) | '=' ConstExpr]

  PushNode(nVarDecl);

  RecogniseIdentList(False);
  Recognise(ttColon);
  RecogniseType;

  lc := TokenList.FirstSolidToken;

  if lc.TokenType = ttAbsolute then
  begin
    PushNode(nAbsoluteVar);
    Recognise(ttAbsolute);

    if (TokenList.FirstSolidWordType in IdentifierTypes) then
      RecogniseIdentifier(False)
    else
      RecogniseConstantExpression;

    PopNode;
  end
  else if lc.TokenType = ttEquals then
  begin
    PushNode(nVariableInit);

    Recognise(ttEquals);

    { not just an expr - can be an array, record or the like
      reuse the code from typed constant declaration as it works the same
    }
    RecogniseTypedConstant;

    PopNode;
  end;

  RecogniseHintDirectives;

  PopNode;
end;

procedure TBuildParseTree.RecogniseExpr;
begin
  { Expression -> SimpleExpression [RelOp SimpleExpression]...

    nb this doesn't parse
    lb := foo.Owner;
  }

  PushNode(nExpression);

  RecogniseSimpleExpression;

  while TokenList.FirstSolidTokenType in RelationalOperators do
  begin
    RecogniseRelop;
    RecogniseSimpleExpression;
  end;

  // added this to cope with real usage - see TestCastSimple
  if TokenList.FirstSolidTokenType = ttDot then
  begin
    Recognise(ttDot);
    RecogniseExpr;
  end;

  //likewise need to cope with pchar(foo)^
  if TokenList.FirstSolidTokenType = ttHat then
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
  lc := TokenList.FirstSolidToken;

  if lc.TokenType = wMinus then
    Recognise(wMinus)
  else if lc.TokenType = wPlus then
    Recognise(wPlus);
 }
  RecogniseTerm;
  while TokenList.FirstSolidTokenType in AddOperators do
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

  while TokenList.FirstSolidTokenType in MulOperators do
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
  lc := TokenList.FirstSolidToken;

  if lc.TokenType = ttInherited then
  begin
    Recognise(ttInherited);
    RecogniseDesignator;
    if TokenList.FirstSolidTokenType = ttOpenBracket then
    begin
      RecogniseActualParams;
    end;
  end
  else if (IdentifierNext) then
  begin
    RecogniseDesignator;
    if TokenList.FirstSolidTokenType = ttOpenBracket then
    begin
      RecogniseActualParams;
    end;
  end
  else if (TokenList.FirstSolidTokenType = ttNumber) then
  begin
    Recognise(ttNumber);
  end
  else if (TokenList.FirstSolidTokenType = ttLiteralString) then
  begin
    Recognise(ttLiteralString);
  end
  else if (TokenList.FirstSolidTokenType in BuiltInConstants) then
  begin
    // nil, true, false
    Recognise(BuiltInConstants);
  end
  else if (TokenList.FirstSolidTokenType = ttOpenBracket) then
  begin
    Recognise(ttOpenBracket);
    RecogniseExpr;
    Recognise(ttCloseBracket);
  end
  else if (TokenList.FirstSolidTokenType = ttNot) then
  begin
    Recognise(ttNot);
    RecogniseFactor;
  end
  else if TokenList.FirstSolidTokenType in PossiblyUnarySymbolOperators then
  begin
    PushNode(nUnaryOp);
    Recognise(PossiblyUnarySymbolOperators);
    RecogniseFactor;

    PopNode;
  end
  else if (TokenList.FirstSolidTokenType = ttOpenSquareBracket) then
  begin
    RecogniseSetConstructor;
  end
  else
    Raise TEParseError.Create('unexpected token in factor', lc);
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

  lc := TokenList.FirstSolidToken;

  if lc.TokenType in RelationalOperators then
    Recognise(RelationalOperators)
  else
    Raise TEParseError.Create('unexpected token in rel op', lc);
end;

procedure TBuildParseTree.RecogniseAddOp;
var
  lc: TSourceToken;
begin
  lc := TokenList.FirstSolidToken;

  if lc.TokenType in AddOperators then
    Recognise(AddOperators)
  else
    Raise TEParseError.Create('unexpected token in add op', lc);
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
  lc := TokenList.FirstSolidToken;

  if lc.TokenType in MulOperators then
    Recognise(MulOperators)
  else
    Raise TEParseError.Create('unexpected token in mul op', lc);
end;


procedure TBuildParseTree.RecogniseDesignator;
begin
  { Designator -> QualId ['.' Ident | '[' ExprList ']' | '^']...


    Need brackets here too for hard typecasts like
      pointer(foo)
  }
  PushNode(nDesignator);

  RecogniseQualId;

  while (TokenList.FirstSolidTokenType in [ttDot, ttOpenBracket, ttOpenSquareBracket, ttHat]) do
  begin
    if TokenList.FirstSolidTokenType = ttDot then
    begin
      Recognise(ttDot);
      RecogniseIdentifier(False);
    end
    else if TokenList.FirstSolidTokenType = ttHat then
    begin
      Recognise(ttHat);
      // and after the deref operator ?
    end
    else if TokenList.FirstSolidTokenType = ttOpenSquareBracket then
    begin
      Recognise(ttOpenSquareBracket);
      RecogniseExprList;
      Recognise(ttCloseSquareBracket);
    end
    else if TokenList.FirstSolidTokenType = ttOpenBracket then
    begin
      RecogniseActualParams;
    end
    else
      Assert(False, 'Should not be here - bad token type');
  end;

  PopNode;
end;

procedure TBuildParseTree.RecogniseSetConstructor;
begin
  // SetConstructor -> '[' [SetElement/','...] ']'

  Recognise(ttOpenSquareBracket);

   while TokenList.FirstSolidTokenType <> ttCloseSquareBracket do
   begin
    RecogniseSetElement;
    if TokenList.FirstSolidTokenType = ttComma then
      Recognise(ttComma)
    else
      break; // no comma -> no more items
   end;

  Recognise(ttCloseSquareBracket);
end;

procedure TBuildParseTree.RecogniseSetElement;
begin
  // SetElement -> Expression ['..' Expression]

  RecogniseExpr;
  if TokenList.FirstSolidTokenType = ttDoubleDot then
  begin
    Recognise(ttDoubleDot);
    RecogniseExpr;
  end;
end;


procedure TBuildParseTree.RecogniseExprList;
begin
 // ExprList -> Expression/','...

  RecogniseExpr;
  while TokenList.FirstSolidTokenType = ttComma do
  begin
    Recognise(ttComma);
    RecogniseExpr;
  end;
end;

procedure TBuildParseTree.RecogniseStatement;
var
  lc, lc2: TSourceToken;
  lbColonSecond: boolean;
begin
  // Statement -> [LabelId ':'] [SimpleStatement | StructStmt]

  PushNode(nStatement);

  // empty statement
  if TokenList.FirstSolidTokenType = ttSemicolon then
  begin
    PopNode;
    Exit;
  end;

  if TokenList.FirstSolidTokenType = ttEnd then
  begin
    PopNode;
    Exit;
  end;

  lc2 := TokenList.SolidToken(2);
  lbColonSecond := (lc2.TokenType = ttColon);
  if (lbColonSecond) then
  begin
    PushNode(nStatementLabel);
    RecogniseIdentifier(True);
    Recognise(ttColon);
    PopNode;
  end;

  lc := TokenList.FirstSolidToken;
  if lc.TokenType in StructStatementWords then
    RecogniseStructStmnt
  else
    RecogniseSimpleStmnt;

  PopNode;
end;

procedure TBuildParseTree.RecogniseStatementList(const peEndTokens: TTokenTypeSet);
begin
  // StmtList -> Statement/';'...
  PushNode(nStatementList);

  while not (TokenList.FirstSolidTokenType in peEndTokens) do
  begin
    RecogniseStatement;

    // last semicolon is optional
    if TokenList.FirstSolidTokenType = ttSemicolon then
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

  lc := TokenList.FirstSolidToken;

  if (IdentifierNext) or (lc.TokenType = ttOpenBracket) then
  begin
    RecogniseDesignator;

    if TokenList.FirstSolidTokenType = ttOpenBracket then
    begin
      RecogniseActualParams;
    end;

    // can be a hat after the close backets to deref the return value
    if TokenList.FirstSolidTokenType = ttHat then
      Recognise(ttHat);

    // dot next ?
    if TokenList.FirstSolidTokenType = ttDot then
    begin
      Recognise(ttDot);
      RecogniseSimpleStmnt;
    end

    else if TokenList.FirstSolidTokenType = ttAssign then
    begin
      PushNode(nAssignment);

      Recognise(ttAssign);
      RecogniseExpr;

      PopNode;
    end;

    // else nothing at all is also ok. i.e. procedure call with no params
  end
  else if lc.TokenType = ttInherited then
  begin
     Recognise(ttInherited);
     // can be followed by a method name with or without params
     if IdentifierNext then
     begin
      RecogniseIdentifier(False);

      if TokenList.FirstSolidTokenType = ttOpenBracket then
        RecogniseActualParams;
     end;

  end
  else if lc.TokenType = ttGoto then
  begin
     Recognise(ttGoto);
     RecogniseIdentifier(False);
  end
  else if lc.TokenType = ttRaise then
  begin
    // another omission - raise expr  or just raise (in except block)
    Recognise(ttRaise);
    if not (TokenList.FirstSolidTokenType in [ttSemicolon, ttEnd]) then
      RecogniseExpr;
  end
  else if lc.TokenType = ttSemicolon then
  begin
    // empty statement
    // this gets doen later in common code Recognise(ttSemicolon);
  end
  else
    Raise TEParseError.Create('expected simple statement', lc);

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


  lc := TokenList.FirstSolidToken;

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
      Raise TEParseError.Create('expected structured statement', lc);
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
  RecogniseExpr;
  PopNode;

  Recognise(ttThen);

  PushNode(nIfBlock);
  RecogniseStatement;
  PopNode;

  if TokenList.FirstSolidTokenType = ttElse then
  begin
    Recognise(ttElse);
    PushNode(nElseBlock);
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
  RecogniseExpr;
  PopNode;

  Recognise(ttOf);

  while not (TokenList.FirstSolidTokenType in [ttElse, ttEnd]) do
    RecogniseCaseSelector;

  if TokenList.FirstSolidTokenType = ttElse then
  begin
    PushNode(nElseCase);
    Recognise(ttElse);
    RecogniseStatementList([ttEnd]);
    PopNode;
  end;

  if TokenList.FirstSolidTokenType = ttSemicolon then
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

  while (TokenList.FirstSolidTokenType = ttComma) do
  begin
    Recognise(ttComma);
    RecogniseCaseLabel;
  end;

  Recognise(ttColon);
  PopNode;

  RecogniseStatement;

  if TokenList.FirstSolidTokenType = ttSemicolon then
    Recognise(ttSemicolon);

  PopNode;
end;

procedure TBuildParseTree.RecogniseCaseLabel;
begin
  // CaseLabel -> ConstExpr ['..' ConstExpr]

  PushNode(nCaseLabel);

  RecogniseConstantExpression;
  if (TokenList.FirstSolidTokenType = ttDoubleDot) then
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
  RecogniseExpr;
  PopNode;

  PopNode;
end;

procedure TBuildParseTree.RecogniseWhileStmnt;
begin
  // WhileStmt -> WHILE Expression DO Statement
  PushNode(nWhileStatement);

  Recognise(ttWhile);

  PushNode(nLoopHeaderExpr);
  RecogniseExpr;
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
  RecogniseExpr;
  PopNode;

  Recognise([ttTo, ttDownto]);

  PushNode(nLoopHeaderExpr);
  RecogniseExpr;
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

  lc := TokenList.FirstSolidToken;
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
       Raise TEParseError.Create('expected except or finally', lc);

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

  if TokenList.FirstSolidTokenType in [ttOn, ttElse]  then
  begin
    while TokenList.FirstSolidTokenType in [ttOn, ttElse] do
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

  if TokenList.FirstSolidTokenType = ttElse then
  begin
    Recognise(ttElse);
    RecogniseStatement;
  end
  else if TokenList.FirstSolidTokenType = ttOn then
  begin
    Recognise(ttOn);
    if TokenList.SolidTokenType(2) = ttColon then
    begin
      RecogniseIdentifier(False);
      Recognise(ttColon);
    end;

    RecogniseIdentifier(True);
    Recognise(ttDo);

    RecogniseStatement;
  end
  else
    RecogniseStatement;

  if TokenList.FirstSolidTokenType = ttSemicolon then
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

  lc := TokenList.FirstSolidToken;

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
      case TokenList.SolidTokenType(2) of
        ttProcedure:
          RecogniseProcedureDecl;
        ttFunction:
          RecogniseFunctionDecl;
        else
           Raise TEParseError.Create('expected class procedure or class function', lc);
        end;
    end;
    else
      Raise TEParseError.Create('expected procedure or function', lc);
  end;

end;

procedure TBuildParseTree.RecogniseProcedureDecl;
var
  lcTop: TParseTreeNode;
begin
  { ProcedureDecl -> ProcedureHeading ';' [Directive] Block ';'

    NB: the block is omitted if there is a 'forward' or external' directive

  }
  PushNode(nProcedureDecl);

  RecogniseProcedureHeading(False, False);
  Recognise(ttSemicolon);

  { if the proc declaration has the directive external or forward,
    it will not have a body }
  lcTop := TParseTreeNode(fcStack.Peek);
  if not lcTop.HasChildNode([ttExternal, ttForward]) then
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

  RecogniseFunctionHeading(False, False);
  Recognise(ttSemicolon);
  //opt
  if TokenList.FirstSolidTokenType in ProcedureDirectives then
    RecogniseProcedureDirectives;

  { if the proc declaration has the directive external or forward,
    it will not have a body }
  lcTop := TParseTreeNode(fcStack.Peek);
  if not lcTop.HasChildNode([ttExternal, ttForward]) then
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

  RecogniseConstructorHeading(False);
  Recognise(ttSemicolon);

  if TokenList.FirstSolidTokenType in ProcedureDirectives then
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

  if TokenList.FirstSolidTokenType in ProcedureDirectives then
    RecogniseProcedureDirectives;
  RecogniseBlock;
  Recognise(ttSemicolon);

  PopNode;
end;

procedure TBuildParseTree.RecogniseFunctionHeading(const pbAnon, pbCanInterfaceMap: Boolean);
begin
  // FunctionHeading -> FUNCTION Ident [FormalParameters] ':' (SimpleType | STRING)
  PushNode(nFunctionHeading);

  // class procs
  if TokenList.FirstSolidTokenType = ttClass then
    Recognise(ttClass);

  Recognise(ttFunction);
  if not pbAnon then
    RecogniseMethodName(False);

  if TokenList.FirstSolidTokenType = ttOpenBracket then
    RecogniseFormalParameters;

  { the colon and type is in fact optional in
    - external fns
    - when making good on a forward }
  if TokenList.FirstSolidTokenType = ttColon then
  begin
    Recognise(ttColon);
    PushNode(nFunctionReturnType);
    RecogniseType;
    PopNode;
  end;

  RecogniseProcedureDirectives;

  if pbCanInterfaceMap and (TokenList.FirstSolidTokenType = ttEquals) then
  begin
    Recognise(ttEquals);
    RecogniseIdentifier(False);
  end;

  PopNode;
end;

procedure TBuildParseTree.RecogniseProcedureHeading(const pbAnon, pbCanInterfaceMap: Boolean);
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

  if TokenList.FirstSolidTokenType = ttClass then
    Recognise(ttClass);

  Recognise(ttProcedure);
  if not pbAnon then
    RecogniseMethodName(False);

  if TokenList.FirstSolidTokenType = ttOpenBracket then
    RecogniseFormalParameters;

  RecogniseProcedureDirectives;

  if pbCanInterfaceMap and (TokenList.FirstSolidTokenType = ttEquals) then
  begin
    Recognise(ttEquals);
    RecogniseIdentifier(False);
  end;

  PopNode;
end;

procedure TBuildParseTree.RecogniseFormalParameters;
begin
  // FormalParameters -> '(' FormalParm/';'... ')'

  PushNode(nFormalParams);

  Recognise(ttOpenBracket);

  { funciton Foo(); is accepted so must allow empty brackets }

  if TokenList.FirstSolidTokenType <> ttCloseBracket then
  begin
    RecogniseFormalParam;
    while TokenList.FirstSolidTokenType = ttSemicolon do
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
  PARAM_PREFIXES: TTokenTypeSet = [ttVar, ttConst, ttOut];
begin
  PushNode(nFormalParam);

  // FormalParm -> [VAR | CONST | OUT] Parameter

  if TokenList.FirstSolidTokenType in PARAM_PREFIXES then
    Recognise(PARAM_PREFIXES);

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
  lbArray := False;
  RecogniseIdentList(False);
  if TokenList.FirstSolidTokenType = ttColon then
  begin
    Recognise(ttColon);

    if TokenList.FirstSolidTokenType = ttArray then
    begin
      Recognise(ttArray);
      Recognise(ttOf);
      lbArray := True;
    end;

    // type is optional in params ie procedure foo(var pp);
    if (lbArray) or (not (TokenList.FirstSolidTokenType in [ttSemicolon, ttCloseBracket])) then
      RecogniseType;

   if TokenList.FirstSolidTokenType = ttEquals then
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

  if (TokenList.FirstSolidTokenType in ProcedureDirectives) or
    ((TokenList.FirstSolidTokenType = ttSemicolon) and (TokenList.SolidTokenType(2) in ProcedureDirectives)) then
  begin
    PushNode(nProcedureDirectives);

    if TokenList.FirstSolidTokenType = ttSemiColon then
      Recognise(ttSemiColon);
    lbFirstPass := True;


    while (TokenList.FirstSolidTokenType in ProcedureDirectives) or
      ((TokenList.FirstSolidTokenType = ttSemicolon) and (TokenList.SolidTokenType(2) in ProcedureDirectives)) do
    begin
      if (not lbFirstPass) and (TokenList.FirstSolidTokenType = ttSemiColon) then
        Recognise(ttSemiColon);

      case TokenList.FirstSolidTokenType of
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
          RecogniseIdentifier(False);
        end;
        else
          Recognise(ProcedureDirectives);
      end;

      lbFirstPass := False;
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
  if TokenList.FirstSolidTokenType in (IdentiferTokens + [ttLiteralString]) then
  begin
    Recognise((IdentiferTokens + [ttLiteralString]));

    if TokenList.FirstSolidTokenType = ttName then
    begin
      Recognise(ttName);
      Recognise((IdentiferTokens + [ttLiteralString]));
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

  if TokenList.FirstSolidTokenType = ttOpenBracket then
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
  RecogniseMethodName(not pbDeclaration);
  if TokenList.FirstSolidTokenType = ttOpenBracket then
    RecogniseFormalParameters;

  RecogniseProcedureDirectives;

  PopNode;
end;

procedure TBuildParseTree.RecogniseDestructorHeading(const pbDeclaration: boolean);
begin
  //DestructorHeading -> DESTRUCTOR Ident [FormalParameters]
  PushNode(nDestructorHeading);

  Recognise(ttDestructor);
  RecogniseMethodName(not pbDeclaration);
  if TokenList.FirstSolidTokenType = ttOpenBracket then
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

 lc := TokenList.FirstSolidToken;

 PushNode(nInitSection);

  case lc.TokenType of
    ttInitialization:
    begin
      Recognise(ttInitialization);
      RecogniseStatementList([ttEnd, ttFinalization]);

      if TokenList.FirstSolidTokenType = ttFinalization then
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
       Raise TEParseError.Create('expected initialisation, begin or end', lc);
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

  if TokenList.FirstSolidTokenType = ttSemicolon then
  begin
    PopNode;
    exit;
  end;

  if TokenList.FirstSolidTokenType = ttOf then
  begin
    Recognise(ttOf);
    RecogniseIdentifier(False);
    PopNode;
    exit;
  end;


  if TokenList.FirstSolidTokenType = ttOpenBracket then
    RecogniseClassHeritage;

  // can end here
  if TokenList.FirstSolidTokenType = ttSemicolon then
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
  RecogniseIdentList(True);
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

  RecogniseClassDeclarations(False);

  while(TokenList.FirstSolidTokenType in ClassVisibility) do
  begin
    PushNode(nClassVisibility);
    RecogniseClassVisibility;
    RecogniseClassDeclarations(False);
    PopNode;
  end;

  PopNode;
end;

procedure TBuildParseTree.RecogniseClassDeclarations(const pbInterface: Boolean);
const
  // can declare thse things in a class
  CLASS_DECL_WORDS = [ttProcedure, ttFunction,
    ttConstructor, ttDestructor, ttProperty, ttClass];
var
  lc: TSourceToken;
  lbStarted: Boolean;
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
  lbStarted := False;

  while (TokenList.FirstSolidTokenType in CLASS_DECL_WORDS) or
    (TokenList.FirstSolidWordType in IdentifierTypes) do
  begin
    // only make this node if it will have children
    if not lbStarted then
      PushNode(nClassDeclarations);
    lbStarted := True;

    lc := TokenList.FirstSolidToken;

    // these end the visibility section
    if TokenList.FirstSolidTokenType in (ClassVisibility + [ttEnd]) then
      break;

    case lc.TokenType of
      ttProcedure:
        RecogniseProcedureHeading(False, True);
      ttFunction:
        RecogniseFunctionHeading(False, True);
      ttClass:
      begin
        // must be followed by 'procedure' or 'function'
        case TokenList.SolidTokenType(2) of
          ttProcedure:
            RecogniseProcedureHeading(False, True);
          ttFunction:
            RecogniseFunctionHeading(False, True);
          else
            Raise TEParseError.Create('Expected class procedure or class function', lc);
        end;

      end;
      ttConstructor:
      begin
        // no constructor on interface
        if pbInterface then
          Raise TEParseError.Create('unexpected token', lc);
        RecogniseConstructorHeading(True);
      end;
      ttDestructor:
      begin
        // no constructor on interface
        if pbInterface then
          Raise TEParseError.Create('unexpected token', lc);
        RecogniseDestructorHeading(True);
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
            Raise TEParseError.Create('unexpected token', lc);

          RecogniseVarDecl;
        end
        else
          Raise TEParseError.Create('unexpected token', lc);
      end;
    end;

    // semicolon after each def.
    if TokenList.FirstSolidTokenType = ttSemicolon then
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

  RecogniseIdentifier(False);

  { this is omitted if it is a property redeclaration for visibility raising
    in that case it may still have directives and hints }
  if TokenList.FirstSolidTokenType in [ttColon, ttOpenSquareBracket] then
  begin
    RecognisePropertyInterface;
    RecognisePropertySpecifiers;
  end;

  RecognisePropertyDirectives;
  RecogniseHintDirectives;

  PopNode;
end;

procedure TBuildParseTree.RecognisePropertyInterface;
begin
  // PropertyInterface -> [PropertyParameterList] ':' Ident

  if TokenList.FirstSolidTokenType <> ttColon then
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
    if (TokenList.FirstSolidTokenType in [ttConst, ttVar]) then
      Recognise([ttConst, ttVar]);

    RecogniseIdentList(False);
    Recognise(ttColon);
    RecogniseTypeId;

    if TokenList.FirstSolidTokenType = ttSemicolon then
      Recognise(ttSemicolon)
    else
      break;

  until TokenList.FirstSolidTokenType = ttCloseSquareBracket;

  Recognise(ttCloseSquareBracket);

  PopNode;
end;

procedure TBuildParseTree.RecognisePropertySpecifiers;
var
  lc: TSourceToken;
const
  PROPERTY_SPECIFIERS: TTokenTypeSet = [ttIndex, ttRead, ttWrite, ttStored,
    ttDefault, ttNoDefault, ttImplements, ttDispId, ttReadOnly];
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
  lc := TokenList.FirstSolidToken;

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
        RecogniseIdentifier(False);
      end;
      ttWrite:
      begin
        Recognise(ttWrite);
        RecogniseIdentifier(False);
      end;
      ttStored:
      begin
        Recognise(ttStored);
        if TokenList.FirstSolidWordType in IdentifierTypes then
          RecogniseIdentifier(False)
         else
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
      else
        Raise TEParseError.Create('expected proeprty specifier', TokenList.FirstSolidToken);
    end;

    PopNode;
    lc := TokenList.FirstSolidToken;

  end;
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

  if TokenList.FirstSolidTokenType = ttSemicolon then
  begin
    PopNode;
    exit;
  end;

  if TokenList.FirstSolidTokenType = ttOpenBracket then
    RecogniseInterfaceHeritage;

  if TokenList.FirstSolidTokenType = ttOpenSquareBracket then
    RecogniseInterfaceGuid;

  if TokenList.FirstSolidTokenType <> ttEnd then
  begin
    PushNode(nInterfaceBody);
    RecogniseClassDeclarations(True);
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
  if TokenList.FirstSolidTokenType = ttLiteralString then
    Recognise(ttLiteralString)
  else
    RecogniseIdentifier(False);

  Recognise(ttCloseSquareBracket);

  PopNode;
end;

procedure TBuildParseTree.RecogniseInterfaceHeritage;
begin
  // InterfaceHeritage -> '(' IdentList ')'
  PushNode(nInterfaceHeritage);

  Recognise(ttOpenBracket);
  RecogniseIdentList(True);
  Recognise(ttCloseBracket);

  PopNode;
end;

procedure TBuildParseTree.RecogniseRequiresClause;
begin
  // RequiresClause -> REQUIRES IdentList... ';'

  PushNode(nRequires);

  Recognise(ttRequires);
  RecogniseIdentList(False);
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

  RecogniseUsesItem(True);
  while TokenList.FirstSolidTokenType = ttComma do
  begin
    Recognise(ttComma);
    RecogniseUsesItem(True);
  end;
  PopNode;

  Recognise(ttSemicolon);

  PopNode;
end;

{ worker for RecogniseIdentList }
procedure TBuildParseTree.RecogniseIdentValue;
begin
  if TokenList.FirstSolidTokenType = ttEquals then
  begin
    Recognise(ttEquals);
    RecogniseExpr;
  end;
end;

procedure TBuildParseTree.RecogniseIdentList(const pbCanHaveUnitQualifier: Boolean);
begin
  { IdentList -> Ident/','...

    now in D6 enum types can have numeric values
     e.g. (foo, bar = 3, baz)
  }
  PushNode(nIdentList);

  RecogniseIdentifier(pbCanHaveUnitQualifier);
  RecogniseIdentValue;

  while TokenList.FirstSolidTokenType = ttComma do
  begin
    Recognise(ttComma);
    RecogniseIdentifier(pbCanHaveUnitQualifier);
    RecogniseIdentValue;
  end;

  PopNode;
end;

procedure TBuildParseTree.RecogniseConstantExpression;
begin
  RecogniseExpr;
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
  }
  if (TokenList.FirstSolidTokenType = ttOpenBracket) then
  begin
    PushNode(nBracketedQual);
    Recognise(ttOpenBracket);
    RecogniseDesignator;

    if (TokenList.FirstSolidTokenType = ttAs) then
    begin
      Recognise(ttAs);
      RecogniseIdentifier(True);
    end;
    Recognise(ttCloseBracket);
    PopNode;
  end
  else
    // a simple ident - e.g. "x"
    RecogniseIdentifier(True);
end;

procedure TBuildParseTree.RecogniseIdentifier(const pbCanHaveUnitQualifier: Boolean);
var
  lc: TSourceToken;
begin
  lc := TokenList.FirstSolidToken;

  if not(IdentifierNext) then
    Raise TEParseError.Create('Expected identifer', lc);

  PushNode(nIdentifier);
  Recognise(IdentiferTokens);

  { tokens can be qualified by a unit name }
  if pbCanHaveUnitQualifier and (TokenList.FirstSolidTokenType = ttDot) then
  begin
    Recognise(ttDot);
    Recognise(IdentiferTokens);
  end;

  PopNode;
end;

{ the name of a procedure/function/constructor can be
  a plain name or classname.methodname }
procedure TBuildParseTree.RecogniseMethodName(const pbClassNameCompulsory: Boolean);
begin
  if not(IdentifierNext) then
    Raise TEParseError.Create('Expected identifer', TokenList.FirstSolidToken);

  Recognise(IdentiferTokens);

  if (TokenList.FirstSolidTokenType = ttDot) or pbClassNameCompulsory then
  begin
    Recognise(ttDot);
    RecogniseIdentifier(False);
  end
end;


procedure TBuildParseTree.RecogniseTypeId;
begin
  RecogniseIdentifier(True);
end;

procedure TBuildParseTree.RecogniseAsmBlock;
begin
  PushNode(nAsm);

  Recognise(ttAsm);
  while TokenList.FirstSolidTokenType <> ttEnd do
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

  if TokenList.FirstSolidTokenType = ttAtSign then
  begin

    Recognise(ttAtSign);
    Recognise(ttAtSign);
    RecogniseAsmIdent;
    Recognise(ttColon);
  end
  else
  begin
    RecogniseAsmOpcode;

    RecogniseWhiteSpace;

    while not (TokenList.FirstTokenType in [ttSemicolon, ttReturn, ttComment]) do
    begin
      if TokenList.FirstSolidTokenType = ttComma then
        Recognise(ttComma);
      RecogniseAsmParam;

      RecogniseWhiteSpace;

      if TokenList.FirstSolidTokenType = ttEnd then
        Break;
    end;

  end;

  PopNode;
end;

{ purpose: to consume white space
  make sure that buffertokens(0)
  contains a retunr, comment or solid token }
procedure TBuildParseTree.RecogniseWhiteSpace;
begin
  while TokenList.FirstTokenType = ttWhiteSpace do
    Recognise(ttWhiteSpace);
end;

procedure TBuildParseTree.RecogniseAsmIdent;
var
  lc: TSourceToken;
begin
  PushNode(nAsmIdent);

  { can contain '@' signs }
  lc := TokenList.FirstSolidToken;

  if not (lc.TokenType in IdentiferTokens + [ttAt]) then
     Raise TEParseError.Create('Expected asm identifer', TokenList.FirstSolidToken);

  while (lc.TokenType in IdentiferTokens + [ttAt]) do
  begin
    Recognise(IdentiferTokens + [ttAt]);
    { whitespace ends this so no TokenList.FirstSolidToken }
    lc := TokenList.First;
  end;

  PopNode;
end;


procedure TBuildParseTree.RecogniseAsmOpcode;
begin
  { these are all short (3 chars? 4 chars)

    but it's too large a cast and varies from CPU to CPU
    so I will not enumerate them all
   }
  PushNode(nASMOpcode);
  RecogniseIdentifier(False);
  PopNode;
end;

procedure TBuildParseTree.RecogniseAsmParam;
var
  lc: TSourceToken;
begin
  { um.

  AsmParam
    -> Ident
    -> '@' ident
    -> '[' AsmExpr ']'
  }

  PushNode(nAsmParam);

  lc := TokenList.FirstSolidToken;

  if lc.TokenType = ttAtSign then
  begin
    Recognise(ttAtSign);
    if TokenList.FirstSolidToken.TokenType = ttAtSign then
      Recognise(ttAtSign);


    RecogniseAsmIdent;
  end
  else if lc.TokenType = ttOpenSquareBracket then
  begin             
    Recognise(ttOpenSquareBracket);
    RecogniseAsmExpr;
    Recognise(ttCloseSquareBracket);
  end
  else if (lc.TokenType = ttNumber) then
  begin
    RecogniseAsmExpr;
  end
  else if (lc.TokenType in IdentiferTokens) then
  begin
    RecogniseAsmIdent;
  end
  else
    Raise TEParseError.Create('Expected asm', lc);

  PopNode;
end;

procedure TBuildParseTree.RecogniseAsmExpr;
begin
  RecogniseExpr;
end;


procedure TBuildParseTree.RecogniseHintDirectives;
begin
  if ((TokenList.FirstSolidTokenType = ttSemicolon) and (TokenList.SolidTokenType(2) in HintDirectives)) or
    (TokenList.FirstSolidTokenType in HintDirectives) then
  begin
    if TokenList.FirstSolidTokenType = ttSemicolon then
      Recognise(ttSemicolon);

    PushNode(nHintDirectives);

    while (TokenList.FirstSolidTokenType in HintDirectives) do
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
  if ((TokenList.FirstSolidTokenType = ttSemicolon) and (TokenList.SolidTokenType(2) in PropertyDirectives)) or
    (TokenList.FirstSolidTokenType in PropertyDirectives) then
  begin
    if TokenList.FirstSolidTokenType = ttSemicolon then
      Recognise(ttSemicolon);

    while TokenList.FirstSolidTokenType in PropertyDirectives do
    begin
      PushNode(nPropertyDirective);

      case TokenList.FirstSolidTokenType of
        ttDefault:
        begin
          Recognise(ttDefault);
          if TokenList.FirstSolidTokenType <> ttSemicolon then
            RecogniseConstantExpression;
        end;
        ttNoDefault:
        begin
          Recognise(ttNoDefault);
        end;
        ttStored:
        begin
          Recognise(ttStored);
          if TokenList.FirstSolidTokenType <> ttSemicolon then
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
  while TokenList.FirstTokenType <> ttSemicolon do
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

  RecogniseIdentifier(False);
  if TokenList.FirstSolidTokenType = ttOpenBracket then
    RecogniseFormalParameters;

  while TokenList.FirstSolidTokenType in ExportedDirectives do
  begin
    lc := TokenList.FirstSolidToken;

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
         Raise TEParseError.Create('Expected export directive', lc);
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

  if TokenList.FirstSolidTokenType <> ttCloseBracket then
  begin
    //RecogniseExprList;

    repeat
      RecogniseExpr;

      { ole named param syntax, e.g.
        " MSWord.TextToTable(ConvertFrom := 2, NumColumns := 3);"
      }

      if TokenList.FirstSolidTokenType = ttAssign then
      begin
        Recognise(ttAssign);
        RecogniseExpr;
      end;

      lbMore := TokenList.FirstSolidTokenType = ttComma;
      if lbMore then
        Recognise(ttComma);

    until not lbMore;

  end;

  Recognise(ttCloseBracket);

  PopNode;
end;

end.