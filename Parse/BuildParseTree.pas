unit BuildParseTree;

{ AFS 27 October
 This unit will attempt to turn a token stream into a full parse tree
 using the Recursive Descent method

 The token stream will still exist, but as the leaves of a tree structure
 This will be a preliminary step to putting a lot of the smarts
 e.g. (how many begin..end blocks is the current toekn within)
 in the tree not the stream

 Hence it currently fits into the toekn processing pipeline
 and does not cause any fuss.

 The grammer is 'Appendix A Object Pascal grammar'
 As found on the borland Web site.
}

interface

uses
  { delphi }
  Contnrs,
  { local } ParseTreeNode, ParseTreeNodeType,
  SourceToken, SourceTokenList, TokenType, WordMap;



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

    procedure RecogniseIdent;
    procedure RecogniseImplementationSection;
    procedure RecogniseDeclSections;
    procedure RecogniseDeclSection;
    procedure RecogniseInitSection;
    procedure RecogniseBlock;
    procedure RecogniseIdentList;
    procedure RecogniseIdentValue;

    procedure RecogniseLabelDeclSection;
    procedure RecogniseConstSection;
    procedure RecogniseConstantDecl;

    procedure RecogniseTypeSection;
    procedure RecogniseVarSection;
    procedure RecogniseProcedureDeclSection;

    // set pbAnon = true if the proc has no name
    procedure RecogniseProcedureHeading(const pbAnon: Boolean);
    procedure RecogniseFunctionHeading(const pbAnon: Boolean);
    procedure RecogniseCompoundStmnt;
    procedure RecogniseStmntList(const peEndWords: TWordSet);
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
    procedure RecogniseConstExpr;

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

    procedure Recognise(const peTokenTypes: TTokenTypeSet; const peWords: TWordSet); overload;
    procedure Recognise(const peTokenTypes: TTokenTypeSet); overload;
    procedure Recognise(const peWords: TWordSet); overload;

    procedure Recognise(const peTokenType: TTokenType); overload;
    procedure Recognise(const peWord: TWord); overload;

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

procedure TBuildParseTree.Recognise(const peTokenTypes: TTokenTypeSet;
  const peWords: TWordSet);
var
  lcCurrentToken: TSourceToken;

  function Matched: Boolean;
  begin
    Result := (lcCurrentToken.TokenType in peTokenTypes) and
      (lcCurrentToken.Word in peWords);

    // match any directive word?
    if (ttReservedWordDirective in peTokenTypes) and
      ( lcCurrentToken.TokenType = ttReservedWordDirective) and
      (wUnknown in peWords) then
        Result := True;

    // match any type name?
    if (ttBuiltInType in peTokenTypes) and
      (lcCurrentToken.TokenType = ttBuiltInType) and
      (wUnknown in peWords) then
        Result := True;

  end;

  function DescribeTarget: string;
  begin
    Result := '" ';

    if peTokenTypes <> [] then
      Result := Result + TokenTypesToString(peTokenTypes);

    if peWords <> [] then
      Result := Result + ' ' + WordsToString(peWords);

    Result := Result + '"';
  end;

begin

  // must accept something
  Assert((peTokenTypes <> []) or (peWords <> []));

  { read tokens up to and including the specified one.
    Add them to the parse tree at the current growing point  }
  repeat
    lcCurrentToken := TokenList.ExtractFirst;

    if (not (ttEOF in peTokenTypes)) and (lcCurrentToken.TokenType = ttEOF) then
      Raise TEParseError.Create('Unexpected end of file', lcCurrentToken);

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
  Recognise([peTokenType], [wUnknown]);
end;

procedure TBuildParseTree.Recognise(const peWords: TWordSet);
begin
  Recognise(TextualTokens, peWords);
end;

procedure TBuildParseTree.Recognise(const peWord: TWord);
begin
  Recognise(TextualTokens, [peWord]);
end;

procedure TBuildParseTree.Recognise(const peTokenTypes: TTokenTypeSet);
var
  leWordTypes: TWordSet;
begin
  leWordTypes := [wUnknown];

  Recognise(peTokenTypes, leWordTypes)
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
  Result := (lc.TokenType = ttWord) and (lc.Word = wUnknown);

  { We have to admit directives as identifiers.
    see TestBogusDirectives.pas for the reasons why }
  if (not Result) then
    Result := (lc.TokenType in [ttReservedWordDirective, ttBuiltInType]);
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
  lw: TWord;
begin
  // Goal -> (Program | Package  | Library  | Unit)

  if TokenList.Count < 1 then
    Raise TEParseError.Create('No source to parse', nil);

 lc := TokenList.FirstSolidToken;
 Assert(lc <> nil);

  if lc.TokenType <> ttReservedWord then
   Raise TEParseError.Create('Expected reserved word - program, package, library, unit', lc);

 lw := lc.Word;

  case lw of
    wProgram:
      RecogniseProgram;
    wPackage:
      RecognisePackage;
    wLibrary:
      RecogniseLibrary;
    wUnit:
      RecogniseUnit;
    else
      Raise TEParseError.Create('Expected program, package, library, unit', lc);
  end
end;

procedure TBuildParseTree.RecogniseProgram;
begin
  // Program -> [PROGRAM Ident ['(' IdentList ')'] ';']  ProgramBlock '.'
  PushNode(nProgram);
  Recognise(wProgram);

  RecogniseIdent;
  if TokenList.FirstSolidTokenType = ttOpenBracket then
  begin
    Recognise(ttOpenBracket);
    RecogniseIdentList;
    Recognise(ttCloseBracket);
  end;

  if TokenList.FirstSolidTokenType = ttSemiColon then
    Recognise(ttSemicolon);

  RecogniseProgramBlock;
  Recognise(ttDot);

  PopNode;
end;

procedure TBuildParseTree.RecogniseUnit;
begin
  // Unit -> UNIT Ident ';' InterfaceSection ImplementationSection InitSection '.'
  PushNode(nUnit);

  PushNode(nUnitHeader);
  Recognise(wUnit);

  RecogniseIdent;
  Recognise(ttSemiColon);

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
  Recognise(wPackage);

  RecogniseIdent;
  Recognise(ttSemiColon);
  if TokenList.FirstSolidTokenWord = wRequires then
    RecogniseRequiresClause;

  if TokenList.FirstSolidTokenWord = wContains then
    RecogniseContainsClause;
    
  Recognise(wEnd);
  Recognise(ttDot);

  PopNode;
end;

procedure TBuildParseTree.RecogniseLibrary;
begin
  // Library -> LIBRARY Ident ';' ProgramBlock '.'
  PushNode(nLibrary);
  Recognise(wLibrary);

  RecogniseIdent;
  Recognise(ttSemiColon);
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

  if lc.Word = wUses then
    RecogniseUsesClause(True);

 RecogniseBlock;
end;

procedure TBuildParseTree.RecogniseUsesClause(const pbInFiles: Boolean);
begin
  // UsesClause -> USES IdentList ';'
  PushNode(nUses);

  Recognise(wUses);

  // IdentList -> Ident/','...
  PushNode(nIdentList);

  RecogniseUsesItem(pbInFiles);

  while TokenList.FirstSolidTokenType = ttComma do
  begin
    Recognise(ttComma);
    RecogniseUsesItem(pbInFiles);
  end;

  PopNode;

  Recognise(ttSemiColon);

  PopNode;
end;

procedure TBuildParseTree.RecogniseUsesItem(const pbInFiles: Boolean);
begin
  PushNode(nUsesItem);

  RecogniseIdent;

  if pbInFiles and (TokenList.FirstSolidTokenWord = wIn) then
  begin
    Recognise(wIn);
    Recognise(ttLiteralString);
  end;

  PopNode;

end;


procedure TBuildParseTree.RecogniseInterfaceSection;
begin
 // InterfaceSection -> INTERFACE [UsesClause] [InterfaceDecl]...

  PushNode(nInterfaceSection);

  Recognise(wInterface);

  if TokenList.FirstSolidTokenWord = wUses then
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
  while TokenList.FirstSolidTokenWord in [wConst, wResourceString, wType, wVar, wThreadVar] + ProcedureWords do
    RecogniseInterfaceDecl;
end;

procedure TBuildParseTree.RecogniseInterfaceDecl;
var
  lc: TSourceToken;
  lw: TWord;
begin
 {
  InterfaceDecl -> ConstSection
      -> TypeSection
      -> VarSection
      -> ExportedHeading
  }
  PushNode(nDeclSection);

  lc := TokenList.FirstSolidToken;
  lw := TokenList.FirstSolidTokenWord;

  case lw of
    wConst, wResourceString:
      RecogniseConstSection;
    wType:
      RecogniseTypeSection;
    wVar, wThreadvar:
      RecogniseVarSection;
    wProcedure, wFunction:
      RecogniseExportedHeading;
    else
      Raise TEParseError.Create('Expected const, type, var, procedure or function', lc);
  end;

  PopNode;
end;

procedure TBuildParseTree.RecogniseExportedHeading;
var
  lc: TSourceToken;
  lw: TWord;
begin
   { ExportedHeading
      -> ProcedureHeading ';' [Directive]
      -> FunctionHeading ';' [Directive] }

  lc := TokenList.FirstSolidToken;
  lw := lc.Word;

  case lw of
    wProcedure:
    begin
      RecogniseProcedureHeading(false);
    end;
    wFunction:
    begin
      RecogniseFunctionHeading(false);
    end;
    else
      Raise TEParseError.Create('Expected function or procedure', lc);
  end;

  Recognise(ttSemiColon);
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

  Recognise(wImplementation);

  if TokenList.FirstSolidTokenWord = wUses then
    RecogniseUsesClause(False);

  RecogniseDeclSections;

  PopNode;
end;

procedure TBuildParseTree.RecogniseBlock;
var
  lc: TSourceToken;
  lw: TWord;
begin
  { Block -> [DeclSection] CompoundStmt }

  lc := TokenList.FirstSolidToken;
  lw := lc.Word;


  PushNode(nBlock);

  // [DeclSection]

  if lw in (Declarations + ProcedureWords) then
    RecogniseDeclSections;

  if TokenList.FirstSolidTokenWord = wAsm then
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
  while TokenList.FirstSolidTokenWord in
    [wClass] + Declarations + ProcedureWords do
      RecogniseDeclSection;
end;

procedure TBuildParseTree.RecogniseDeclSection;
var
  lc: TSourceToken;
  lw: TWord;
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
  lw := TokenList.FirstSolidTokenWord;

  case lw of
    wLabel:
      RecogniseLabelDeclSection;
    wConst, wResourceString:
      RecogniseConstSection;
    wType:
      RecogniseTypeSection;
    wVar, wThreadvar:
      RecogniseVarSection;
    wProcedure, wFunction, wConstructor, wDestructor, wClass:
      RecogniseProcedureDeclSection;
    wExports:
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
  Recognise(wLabel);
  RecogniseIdentList;
  Recognise(ttSemiColon);

  PopNode;
end;

procedure TBuildParseTree.RecogniseConstSection;
begin
  {
    ConstSection -> CONST (ConstantDecl ';')...
  }
  PushNode(nConstSection);
  Recognise([wConst, wResourceString]);

  while TokenList.FirstSolidTokenType in IdentifierTypes do
  begin
    RecogniseConstantDecl;
    Recognise(ttSemiColon);
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

  RecogniseIdent;

  lc := TokenList.FirstSolidToken;

  if lc.Word = wEquals then
  begin
    Recognise(wEquals);
    RecogniseConstExpr;
  end
  else
  if lc.TokenType = ttColon then
  begin
    Recognise(ttColon);
    //RecogniseTypeId;
    RecogniseType;
    Recognise(wEquals);
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
  Recognise(wType);

  while TokenList.FirstSolidTokenType in IdentifierTypes do
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

  RecogniseIdent;
  Recognise(wEquals);

  // type or restricted type
  if (TokenList.FirstSolidTokenWord in [wObject, wClass, wInterface, wDispInterface]) then
    RecogniseRestrictedType
  else
    RecogniseType;

  Recognise(ttSemiColon);

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
    else if (tt = ttSemiColon) and (liBracketLevel = 0) then
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
      (TokenList.SolidTokenType(2) = ttWord) and
      (TokenList.SolidTokenType(3) = ttColon) then
    begin
      RecogniseRecordConstant;
    end
    else if (ArrayConstantNext) then
    begin
      RecogniseArrayConstant
    end
    else
      RecogniseConstExpr;
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
  while (TokenList.FirstSolidTokenType = ttSemiColon) do
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

  RecogniseIdent;
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

  if (lc.Word = wType) then
  begin
    { this can be a prefix. See help under "Declaring types".
      an e.g. is in TestDeclarations.pas }
    Recognise(wType);
  end;

  if (lc.Word = wConst) then
    Recognise(wConst)
  else if (lc.Word in REAL_TYPES + ORD_TYPES) then
    RecogniseSimpleType
  else if (lc.TokenType = ttOpenBracket) then
    RecogniseSimpleType // for enumerated types
  else if (lc.Word in [wPacked, wArray, wSet, wFile, wRecord]) then
    RecogniseStrucType
  else if (lc.Word = wHat) then
    RecognisePointerType
  else if (lc.Word in StringWords) then
    RecogniseStringType
  else if (lc.Word in [wProcedure, wFunction]) then
    RecogniseProcedureType
  else if lc.Word in VARIANT_TYPES then
    RecogniseVariantType
  else if (lc.Word = wClass) and (lc2.Word = wOf) then
    RecogniseClassRefType
  else if (lc.TokenType in IdentifierTypes) and (lc.Word = wUnknown) then
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
  case lc.Word of
    wObject:
      RecogniseObjectType;
    wClass:
      RecogniseClassType;
    wInterface, wDispInterface:
      RecogniseInterfaceType;
    else
      Raise TEParseError.Create('Expected object, class or interface', lc);
  end;

  PopNode;
end;

procedure TBuildParseTree.RecogniseClassRefType;
begin
  // ClassRefType -> CLASS OF TypeId

  Recognise(wClass);
  Recognise(wOf);
  RecogniseTypeId;
end;

procedure TBuildParseTree.RecogniseSimpleType;
var
  lc: TSourceToken;
begin
  // SimpleType -> (OrdinalType | RealType)

    lc := TokenList.FirstSolidToken;

    if lc.Word in REAL_TYPES then
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
  Recognise(REAL_TYPES);
end;

procedure TBuildParseTree.RecogniseOrdinalType;
var
  lc: TSourceToken;
begin
  // OrdinalType -> (SubrangeType | EnumeratedType | OrdIdent)

  lc := TokenList.FirstSolidToken;

  if lc.TokenType = ttOpenBracket then
    RecogniseEnumeratedType
  else if lc.Word in ORD_TYPES then
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
  Recognise(ORD_TYPES);
end;

procedure TBuildParseTree.RecogniseVariantType;
begin
  {
    VariantType
      -> VARIANT
      -> OLEVARIANT
  }

  Recognise(VARIANT_TYPES);

end;

procedure TBuildParseTree.RecogniseSubrangeType;
begin
  // SubrangeType -> ConstExpr '..' ConstExpr
  PushNode(nSubrangeType);

  RecogniseConstExpr;
  Recognise(ttDoubleDot);
  RecogniseConstExpr;

  PopNode;
end;

procedure TBuildParseTree.RecogniseEnumeratedType;
begin
  // EnumeratedType -> '(' IdentList ')'
  PushNode(nEnumeratedType);

  Recognise(ttOpenBracket);
  RecogniseIdentList;
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

 if TokenList.FirstSolidTokenWord = wString then
 begin
  Recognise(wString);
  if TokenList.FirstSolidTokenType = ttOpenSquareBracket then
  begin
    // e.g. var f = String[30];
    Recognise(ttOpenSquareBracket);
    Recognise(ttNumber);
    Recognise(ttCloseSquareBracket);

  end;
 end
 else
  Recognise([wAnsiString, wWideString]);
end;


procedure TBuildParseTree.RecogniseStrucType;
var
  lc: TSourceToken;
begin
  // StrucType -> [PACKED] (ArrayType | SetType | FileType | RecType)

  if TokenList.FirstSolidTokenWord = wPacked then
    Recognise(wPacked);

  lc := TokenList.FirstSolidToken;

  case lc.Word of
    wArray:
      RecogniseArrayType;
    wSet:
      RecogniseSetType;
    wFile:
      RecogniseFileType;
    wRecord:
      RecogniseRecordType;
    else
      Raise TEParseError.Create('Expected array, set, file or record type', lc);
  end;
end;

procedure TBuildParseTree.RecogniseArrayType;
begin
  // ArrayType -> ARRAY ['[' OrdinalType/','... ']'] OF Type
  PushNode(nArrayType);

  Recognise(wArray);

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
  Recognise(wOf);
  RecogniseType;

  PopNode;
end;

procedure TBuildParseTree.RecogniseRecordType;
begin
  // RecType -> RECORD [FieldList] END

  PushNode(nRecordType);

  Recognise(wRecord);
  if TokenList.FirstSolidTokenWord <> wEnd then
    RecogniseFieldList;
  Recognise(wEnd);

  RecogniseHintDirectives;

  PopNode;

end;

procedure TBuildParseTree.RecogniseFieldList;
begin
  // FieldList ->  FieldDecl/';'... [VariantSection] [';']

  while (not (TokenList.FirstSolidTokenWord in [wEnd, wCase]))
    and (not (TokenList.FirstSolidTokenType = ttCloseBracket))  do
  begin
    RecogniseFieldDecl;
    if TokenList.FirstSolidTokenType = ttSemiColon then
      Recognise(ttSemiColon)
    else
      Break;
  end;

  if TokenList.FirstSolidTokenWord = wCase then
    RecogniseVariantSection;

  if TokenList.FirstSolidTokenType = ttSemiColon then
    Recognise(ttSemiColon);
end;

procedure TBuildParseTree.RecogniseFieldDecl;
begin
  // FieldDecl -> IdentList ':' Type
  PushNode(nFieldDeclaration);

  RecogniseIdentList;
  Recognise(ttColon);
  RecogniseType;

  PopNode;
end;

procedure TBuildParseTree.RecogniseVariantSection;
begin
  PushNode(nRecordVariantSection);

  // VariantSection -> CASE [Ident ':'] TypeId OF RecVariant/';'...
  Recognise(wCase);

  // is there an 'of' 2 tokens hence? If not, must be 'ident:' first 
  if not (TokenList.SolidTokenWord(2) = wOf) then
  begin
    RecogniseIdent;
    Recognise(ttColon);
  end;

  RecogniseTypeId;
  Recognise(wOf);

  // I have tested and that there must be at least 1 case in a var section
  repeat
    RecogniseRecVariant;

    // semicolon is optional on the last one
    if TokenList.FirstSolidTokenType = ttSemiColon then
      Recognise(ttSemiColon)
    else
      break;

  until (TokenList.FirstSolidTokenWord = wEnd) or
    (TokenList.FirstSolidTokenType = ttCloseBracket);

  PopNode;
end;

procedure TBuildParseTree.RecogniseRecVariant;
begin
 // RecVariant -> ConstExpr/','...  ':' '(' [FieldList] ')'

 PushNode(nRecordVariant);

  RecogniseConstExpr;
  while TokenList.FirstSolidTokenType = ttComma do
  begin
    Recognise(ttComma);
    RecogniseConstExpr;
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

  Recognise(wSet);
  Recognise(wOf);

  //RecogniseOrdinalType;
  RecogniseType;

  PopNode;
end;

procedure TBuildParseTree.RecogniseFileType;
begin
  // FileType -> FILE OF TypeId

  Recognise(wfile);
  Recognise(wOf);
  RecogniseTypeId;
end;

procedure TBuildParseTree.RecognisePointerType;
begin
  // PointerType -> '^' TypeId
  Recognise(wHat);
  RecogniseTypeId;
end;

procedure TBuildParseTree.RecogniseProcedureType;
begin
  PushNode(nProcedureType);

  // ProcedureType -> (ProcedureHeading | FunctionHeading) [OF OBJECT]
  if TokenList.FirstSolidTokenWord = wProcedure then
    RecogniseProcedureHeading(True)
  else if TokenList.FirstSolidTokenWord = wFunction then
    RecogniseFunctionHeading(True)
  else
    Raise TEParseError.Create('Expected procedure or function type', TokenList.FirstSolidToken);

  if TokenList.FirstSolidTokenWord = wOf then
  begin
    Recognise(wOf);
    Recognise(wObject);
  end;

  PopNode;
end;

procedure TBuildParseTree.RecogniseVarSection;
const
  END_VAR_SECTION: TWordSet =
    [wVar, wThreadVar, wConst, wLabel, wResourceString, wType,
      wBegin, wEnd, wImplementation, wInitialization,
      wProcedure, wFunction, wConstructor, wDestructor];
begin
  PushNode(nVarSection);

  // VarSection -> VAR (VarDecl ';')...
  Recognise([wVar, wThreadvar]);

  repeat
    RecogniseVarDecl;
    Recognise(ttSemiColon);
  until (TokenList.FirstSolidTokenWord in END_VAR_SECTION);

  PopNode;
end;


procedure TBuildParseTree.RecogniseVarDecl;
var
  lc: TSourceToken;
begin
  // VarDecl -> IdentList ':' Type [(ABSOLUTE (Ident | ConstExpr)) | '=' ConstExpr]

  PushNode(nVarDecl);

  RecogniseIdentList;
  Recognise(ttColon);
  RecogniseType;

  lc := TokenList.FirstSolidToken;

  if lc.Word = wAbsolute then
  begin
    PushNode(nAbsoluteVar);
    Recognise(wAbsolute);

    if (TokenList.FirstSolidTokenType = ttWord) then
      RecogniseIdent
    else
      RecogniseConstExpr;

    PopNode;
  end
  else if lc.Word = wEquals then
  begin
    PushNode(nVariableInit);

    Recognise(wEquals);

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

  while TokenList.FirstSolidTokenWord in RelationalOperators do
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

  PopNode;
end;

procedure TBuildParseTree.RecogniseSimpleExpression;
var
  lc: TSourceToken;
begin
  // SimpleExpression -> ['+' | '-'] Term [AddOp Term]...

  lc := TokenList.FirstSolidToken;

  if lc.Word = wMinus then
    Recognise(wMinus)
  else if lc.Word = wPlus then
    Recognise(wPlus);

  RecogniseTerm;
  while TokenList.FirstSolidTokenWord in AddOperators do
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

  while TokenList.FirstSolidTokenWord in MulOperators do
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

    WHat about unary operators other than not,
    e.g. b := b * -2;

    PossiblyUnarySymbolOperators
   }
  lc := TokenList.FirstSolidToken;

  if (IdentifierNext) then
  begin
    RecogniseDesignator;
    if TokenList.FirstSolidTokenType = ttOpenBracket then
    begin
      PushNode(nActualParams);

      Recognise(ttOpenBracket);
      RecogniseExprList;
      Recognise(ttCloseBracket);

      PopNode;
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
  else if (TokenList.FirstSolidTokenType = ttBuiltInConstant) then
  begin
    // nil, true, false
    Recognise([ttBuiltInConstant], [wNil, wTrue, wFalse]);
  end
  else if (TokenList.FirstSolidTokenType = ttOpenBracket) then
  begin
    Recognise(ttOpenBracket);
    RecogniseExpr;
    Recognise(ttCloseBracket);
  end
  else if (TokenList.FirstSolidTokenWord = wNot) then
  begin
    Recognise(wNot);
    RecogniseFactor;
  end
  else if TokenList.FirstSolidTokenWord in PossiblyUnarySymbolOperators then
  begin
    Recognise(PossiblyUnarySymbolOperators);
    RecogniseFactor;
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

  if lc.Word in RelationalOperators then
    Recognise(RelationalOperators)
  else
    Raise TEParseError.Create('unexpected token in rel op', lc);
end;

procedure TBuildParseTree.RecogniseAddOp;
var
  lc: TSourceToken;
begin
  lc := TokenList.FirstSolidToken;

  if lc.Word in AddOperators then
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

  if lc.Word in MulOperators then
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

  RecogniseQualId;

  while (TokenList.FirstSolidTokenType in [ttDot, ttOpenSquareBracket]) or
    (TokenList.FirstSolidTokenWord = wHat) do
  begin
    if TokenList.FirstSolidTokenType = ttDot then
    begin
      Recognise(ttDot);
      RecogniseIdent;
    end
    else if TokenList.FirstSolidTokenWord = wHat then
    begin
      Recognise(wHat);
      // and after the deref operator ?
    end
    else if TokenList.FirstSolidTokenType = ttOpenSquareBracket then
    begin
      Recognise(ttOpenSquareBracket);
      RecogniseExprList;
      Recognise(ttCloseSquareBracket);
    end
    else
      Assert(False, 'Should not be here - bad token type');
  end;
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
  if TokenList.FirstSolidTokenType = ttSemiColon then
  begin
    PopNode;
    Exit;
  end;

  if TokenList.FirstSolidTokenWord = wEnd then
  begin
    PopNode;
    Exit;
  end;

  lc2 := TokenList.SolidToken(2);
  lbColonSecond := (lc2.TokenType = ttColon);
  if (lbColonSecond) then
  begin
    PushNode(nStatementLabel);
    RecogniseIdent;
    Recognise(ttColon);
    PopNode;
  end;

  lc := TokenList.FirstSolidToken;
  if lc.Word in STRUCT_STATEMENT_WORDS then
    RecogniseStructStmnt
  else
    RecogniseSimpleStmnt;

  PopNode;
end;

procedure TBuildParseTree.RecogniseStmntList(const peEndWords: TWordSet);
begin
  // StmtList -> Statement/';'...
  PushNode(nStatementList);

  while not (TokenList.FirstSolidTokenWord in peEndWords) do
  begin
    RecogniseStatement;

    // last semicolon is optional
    if TokenList.FirstSolidTokenType = ttSemiColon then
      Recognise(ttSemiColon)
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
}

  lc := TokenList.FirstSolidToken;

  if (IdentifierNext) or (lc.TokenType = ttOpenBracket) then
  begin
    RecogniseDesignator;

    if TokenList.FirstSolidTokenType = ttOpenBracket then
    begin
      Recognise(ttOpenBracket);
      RecogniseExprList;
      Recognise(ttCloseBracket);
    end;

    if TokenList.FirstSolidTokenType = ttAssign then
    begin
      Recognise(ttAssign);
      RecogniseExpr;
    end;
    // else nothing at all is also ok. i.e. procedure call
  end
  else if lc.Word = wInherited then
  begin
     Recognise(wInherited);
     // can be followed by a method name with or without params
     if IdentifierNext then
     begin
      RecogniseIdent;

      if TokenList.FirstSolidTokenType = ttOpenbracket then
      begin
        PushNode(nActualParams);

        Recognise(ttOpenBracket);
        RecogniseExprList;
        Recognise(ttCloseBracket);

        PopNode;
      end;
     end;

  end
  else if lc.Word = wGoto then
  begin
     Recognise(wGoto);
     RecogniseIdent;
  end
  else if lc.Word = wRaise then
  begin
    // another omission - raise expr  or just raise (in except block)
    Recognise(wRaise);
    if TokenList.FirstTokenType <> ttSemiColon then
      RecogniseExpr;
  end
  else if lc.TokenType = ttSemiColon then
  begin
    // empty statement
    // this gets doen later in common code Recognise(ttSemiColon);
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

  case lc.Word of
    wBegin:
      RecogniseCompoundStmnt;
    wAsm:
      RecogniseAsmBlock;
    wIf:
      RecogniseIfStmnt;
    wCase:
      RecogniseCaseStmnt;
    wRepeat:
      RecogniseRepeatStmnt;
    wWhile:
      RecogniseWhileStmnt;
    wFor:
      RecogniseForStmnt;
    wWith:
      RecogniseWithStmnt;
    wTry:
      RecogniseTryStatement;
    else
      Raise TEParseError.Create('expected structured statement', lc);
  end;

end;

procedure TBuildParseTree.RecogniseCompoundStmnt;
begin
  { CompoundStmt -> BEGIN StmtList END }
  PushNode(nCompoundStatement);
  Recognise(wBegin);
  RecogniseStmntList([wEnd]);
  Recognise(wEnd);
  PopNode;
end;



procedure TBuildParseTree.RecogniseIfStmnt;
begin
  // IfStmt -> IF Expression THEN Statement [ELSE Statement]

  Recognise(wIf);
  PushNode(nIfCondition);
  RecogniseExpr;
  PopNode;
  Recognise(wThen);

  PushNode(nIfBlock);
  RecogniseStatement;
  PopNode;

  if TokenList.FirstSolidTokenWord = wElse then
  begin
    Recognise(wElse);
    PushNode(nElseBlock);
    RecogniseStatement;
    PopNode;
  end;
end;


procedure TBuildParseTree.RecogniseCaseStmnt;
begin
  // CaseStmt -> CASE Expression OF CaseSelector/';'... [ELSE Statement] [';'] END
  PushNode(nCaseStatement);

  Recognise(wCase);
  RecogniseExpr;
  Recognise(wOf);

  while not (TokenList.FirstSolidTokenWord in [wElse, wEnd]) do
    RecogniseCaseSelector;

  if TokenList.FirstSolidTokenWord = wElse then
  begin
    PushNode(nElseCase);
    Recognise(wElse);
    RecogniseStmntList([wEnd]);
    PopNode;
  end;

  if TokenList.FirstSolidTokenType = ttSemiColon then
    Recognise(ttSemiColon);

  Recognise(wEnd);

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

  if TokenList.FirstSolidTokenType = ttSemiColon then
    Recognise(ttSemicolon);

  PopNode;
end;

procedure TBuildParseTree.RecogniseCaseLabel;
begin
  // CaseLabel -> ConstExpr ['..' ConstExpr]

  PushNode(nCaseLabel);

  RecogniseConstExpr;
  if (TokenList.FirstSolidTokenType = ttDoubleDot) then
  begin
    Recognise(ttDoubleDot);
    RecogniseConstExpr;
  end;

  PopNode;
end;


procedure TBuildParseTree.RecogniseRepeatStmnt;
begin
  // RepeatStmt -> REPEAT Statement UNTIL Expression
  Recognise(wRepeat);
  RecogniseStatement;

  if TokenList.FirstSolidTokenType = ttSemicolon then
    Recognise(ttSemicolon);

  Recognise(wUntil);
  RecogniseExpr;
end;

procedure TBuildParseTree.RecogniseWhileStmnt;
begin
  // WhileStmt -> WHILE Expression DO Statement

  Recognise(wWhile);
  RecogniseExpr;
  Recognise(wDo);
  RecogniseStatement;
end;

procedure TBuildParseTree.RecogniseForStmnt;
begin
  // ForStmt -> FOR QualId ':=' Expression (TO | DOWNTO) Expression DO Statement
  Recognise(wFor);
  RecogniseQualId;
  Recognise(ttAssign);
  RecogniseExpr;
  Recognise([wTo, wDownto]);
  RecogniseExpr;
  Recognise([wDo]);
  RecogniseStatement;
end;

procedure TBuildParseTree.RecogniseWithStmnt;
begin
  { WithStmt -> WITH IdentList DO Statement

   it's not an identlist, but an expression list
  }
  Recognise([wWith]);

  //RecogniseIdentList;
  RecogniseExprList;

  Recognise([wDo]);
  RecogniseStatement;
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

  Recognise(wTry);
  RecogniseStmntList([wEnd, wFinally, wExcept]);

  PopNode;

  lc := TokenList.FirstSolidToken;
  case lc.Word of
    wFinally:
    begin
      PushNode(nFinallyBlock);

      Recognise(wFinally);
      RecogniseStmntList([wEnd]);
      Recognise(wEnd);

      PopNode;
    end;
    wExcept:
    begin
      PushNode(nExceptBlock);

      Recognise(wExcept);
      RecogniseExceptionHandlerBlock;
      Recognise(wEnd);

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
    can be a statement or those 'on Excepttype' thingies
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
  PushNode(nExceptionHandler);

  if TokenList.FirstSolidTokenWord in [wOn, wElse]  then
  begin
    while TokenList.FirstSolidTokenWord in [wOn, wElse] do
      RecogniseExceptionHandler;
  end
  else
  begin
    RecogniseStatement;
    if TokenList.FirstSolidTokenType = ttSemiColon then
      Recognise(ttSemiColon);
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
  if TokenList.FirstSolidTokenWord = wElse then
  begin
    Recognise(wElse);
    RecogniseStatement;
  end
  else if TokenList.FirstSolidTokenWord = wOn then
  begin
    Recognise(wOn);
    if TokenList.SolidTokenType(2) = ttColon then
    begin
      RecogniseIdent;
      Recognise(ttColon);
    end;
    RecogniseIdent;
    Recognise(wDo);

    RecogniseStatement;
  end
  else
    RecogniseStatement;

  if TokenList.FirstSolidTokenType = ttSemiColon then
    Recognise(ttSemiColon);

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

  case lc.Word of
    wProcedure:
      RecogniseProcedureDecl;
    wFunction:
      RecogniseFunctionDecl;
    wConstructor:
      RecogniseConstructorDecl;
    wDestructor:
      RecogniseDestructorDecl;

    wClass:
    begin
      // class proc or function
      case TokenList.SolidTokenWord(2) of
        wProcedure:
          RecogniseProcedureDecl;
        wFunction:
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

  RecogniseProcedureHeading(False);
  Recognise(ttSemiColon);

  if TokenList.FirstSolidTokenWord in ProcedureDirectives then
    RecogniseProcedureDirectives;

  { if the proc declaration has the directive external or forward,
    it will not have a body }
  lcTop := TParseTreeNode(fcStack.Peek);
  if not lcTop.HasChildNode([wExternal, wForward]) then
  begin
    RecogniseBlock;
    Recognise(ttSemiColon);
  end;

  PopNode;
end;

procedure TBuildParseTree.RecogniseFunctionDecl;
var
  lcTop: TParseTreeNode;
begin
  // ProcedureDecl -> FunctionHeading ';' [Directive] Block ';'

  PushNode(nFunctionDecl);

  RecogniseFunctionHeading(False);
  Recognise(ttSemiColon);
  //opt
  if TokenList.FirstSolidTokenWord in ProcedureDirectives then
    RecogniseProcedureDirectives;

  { if the proc declaration has the directive external or forward,
    it will not have a body }
  lcTop := TParseTreeNode(fcStack.Peek);
  if not lcTop.HasChildNode([wExternal, wForward]) then
  begin
    RecogniseBlock;
    Recognise(ttSemiColon);
  end;

  PopNode;
end;


procedure TBuildParseTree.RecogniseConstructorDecl;
begin
  // ProcedureDecl -> ProcedureHeading ';' [Directive] Block ';'

  PushNode(nConstructorDecl);

  RecogniseConstructorHeading(False);
  Recognise(ttSemiColon);

  if TokenList.FirstSolidTokenWord in ProcedureDirectives then
    RecogniseProcedureDirectives;
  RecogniseBlock;
  Recognise(ttSemiColon);

  PopNode;
end;

procedure TBuildParseTree.RecogniseDestructorDecl;
begin
  // ProcedureDecl -> ProcedureHeading ';' [Directive] Block ';'

  PushNode(nDestructorDecl);

  RecogniseDestructorHeading(false);
  Recognise(ttSemiColon);

  if TokenList.FirstSolidTokenWord in ProcedureDirectives then
    RecogniseProcedureDirectives;
  RecogniseBlock;
  Recognise(ttSemiColon);

  PopNode;
end;

procedure TBuildParseTree.RecogniseFunctionHeading(const pbAnon: Boolean);
begin
  // FunctionHeading -> FUNCTION Ident [FormalParameters] ':' (SimpleType | STRING)
  PushNode(nFunctionHeading);

  // class procs
  if TokenList.FirstSolidTokenWord = wClass then
    Recognise(wClass);

  Recognise(wFunction);
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
    RecogniseType;
  end;

  RecogniseProcedureDirectives;

  if TokenList.FirstSolidTokenWord = wEquals then
  begin
    Recognise(wEquals);
    RecogniseIdent;
  end;


  PopNode;
end;

procedure TBuildParseTree.RecogniseProcedureHeading(const pbAnon: Boolean);
begin
  { ProcedureHeading -> PROCEDURE Ident [FormalParameters]

    can also map to an itnerface name
    e.g.
      type
        TFoo = class(TObject, IFoo)
          public
            procedure IFoo.P1 = MyP1;
            Procedure MyP1;
        end;
  }

  PushNode(nProcedureHeading);

  if TokenList.FirstSolidTokenWord = wClass then
    Recognise(wClass);

  Recognise(wProcedure);
  if not pbAnon then
    RecogniseMethodName(False);

  if TokenList.FirstSolidTokenType = ttOpenBracket then
    RecogniseFormalParameters;

  RecogniseProcedureDirectives;

  if TokenList.FirstSolidTokenWord = wEquals then
  begin
    Recognise(wEquals);
    RecogniseIdent;
  end;

  PopNode;
end;

procedure TBuildParseTree.RecogniseFormalParameters;
begin
  // FormalParameters -> '(' FormalParm/';'... ')'

  PushNode(nFormalParams);

  Recognise(ttOpenBracket);

  RecogniseFormalParam;
  while TokenList.FirstSolidTokenType = ttSemiColon do
  begin
    Recognise(ttSemiColon);
    RecogniseFormalParam;
  end;

  Recognise(ttCloseBracket);

  PopNode;
end;

procedure TBuildParseTree.RecogniseFormalParam;
const
  PARAM_PREFIXES: TWordSet = [wVar, wConst, wOut];
begin
  PushNode(nFormalParam);

  // FormalParm -> [VAR | CONST | OUT] Parameter

  if TokenList.FirstSolidTokenWord in PARAM_PREFIXES then
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
  RecogniseIdentList;
  if TokenList.FirstSolidTokenType = ttColon then
  begin
    Recognise(ttColon);

    if TokenList.FirstSolidTokenWord = wArray then
    begin
      Recognise(wArray);
      Recognise(wOf);
      lbArray := True;
    end;

    // type is optional in params ie procedure foo(var pp);
    if (lbArray) or (not (TokenList.FirstSolidTokenType in [ttSemicolon, ttCloseBracket])) then
      RecogniseType;

   if TokenList.FirstSolidTokenWord = wEquals then
   begin
    Recognise(wEquals);
    RecogniseConstExpr;
   end;
  end;
end;


procedure TBuildParseTree.RecogniseProcedureDirectives;
var
  lc, lc2: TSourceToken;
begin
  { these are semi-colon seperated

    want to leave 'Function foo;' as is,
    but strip off the '; safecall' off 'Function bar; safecall;'

    external is more complex
  }

  lc := TokenList.FirstSolidToken;
  lc2 := TokenList.SolidToken(2);

  if (lc.TokenType = ttSemiColon) and (lc2.Word in ProcedureDirectives) then
  begin
    PushNode(nProcedureDirectives);

    while (lc.TokenType = ttSemiColon) and (lc2.Word in ProcedureDirectives) do
    begin
      Recognise(ttSemiColon);

      case lc2.Word of
        wExternal:
        begin
          RecogniseExternalProcDirective;
        end;
        wDispId:
        begin
          Recognise(wDispId);
          Recognise(ttNumber);
        end
        else
          Recognise(ProcedureDirectives);
      end;

      lc := TokenList.FirstSolidToken;
      lc2 := TokenList.SolidToken(2);
    end;

    PopNode;
  end;
end;

procedure TBuildParseTree.RecogniseExternalProcDirective;
const
  LIB_NAMES: TTokenTypeSet = [ttLiteralString, ttWord];
begin
  { right, i'll fake this one

    ExternalProcDirective ->
      External ["'" libname "'" ["name" "'" procname "'"]]
  }
  PushNode(nExternalDirective);

  Recognise(wExternal);
  if TokenList.FirstSolidTokenType in LIB_NAMES then
  begin
    Recognise(LIB_NAMES);

    if TokenList.FirstSolidTokenWord = wName then
    begin
      Recognise(wName);
      Recognise(LIB_NAMES);
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

  Recognise(wObject);

  if TokenList.FirstSolidTokenType = ttOpenbracket then
    RecogniseObjHeritage;

  // swiped this from the delphi object defs
  RecogniseClassBody;

  Recognise(wEnd);

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
  PushNode(nProcedureHeading);

  Recognise(wConstructor);
  RecogniseMethodName(not pbDeclaration);
  if TokenList.FirstSolidTokenType = ttOpenBracket then
    RecogniseFormalParameters;

  RecogniseProcedureDirectives;

  PopNode;
end;

procedure TBuildParseTree.RecogniseDestructorHeading(const pbDeclaration: boolean);
begin
  //DestructorHeading -> DESTRUCTOR Ident [FormalParameters]
  PushNode(nProcedureHeading);

  Recognise(wDestructor);
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

  case lc.Word of
    wInitialization:
    begin
      Recognise(wInitialization);
      RecogniseStmntList([wEnd, wFinalization]);

      if TokenList.FirstSolidTokenWord = wFinalization then
      begin
        Recognise(wFinalization);
        RecogniseStmntList([wEnd]);
      end;

      Recognise(wEnd);
    end;
    wBegin:
    begin
      Recognise(wBegin);
      RecogniseStmntList([wEnd]);
      Recognise(wEnd);
    end;
    wEnd:
    begin
      Recognise(wEnd);
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

  Recognise(wClass);

  if TokenList.FirstSolidTokenType = ttSemiColon then
  begin
    PopNode;
    exit;
  end;

  if TokenList.FirstSolidTokenWord = wOf then
  begin
    Recognise(wOf);
    RecogniseIdent;
    PopNode;
    exit;
  end;


  if TokenList.FirstSolidTokenType = ttOpenBracket then
    RecogniseClassHeritage;

  // can end here
  if TokenList.FirstSolidTokenType = ttSemiColon then
  begin
    PopNode;
    exit;
  end;


  RecogniseClassBody;
  Recognise(wEnd);

  RecogniseHintDirectives;

  PopNode;
end;

procedure TBuildParseTree.RecogniseClassHeritage;
begin
  PushNode(nClassHeritage);

  // ClassHeritage -> '(' IdentList ')'
  Recognise(ttOpenBracket);
  RecogniseIdentList;
  Recognise(ttCloseBracket);

  PopNode;
end;

procedure TBuildParseTree.RecogniseClassVisibility;
begin
  // ClassVisibility -> [PUBLIC | PROTECTED | PRIVATE | PUBLISHED]

  Recognise(CLASS_VISIBILITY);
end;

procedure TBuildParseTree.RecogniseClassBody;
begin
  //ClassBody -> classdeclarations (access classdeclarations) ...
  PushNode(nClassBody);

  RecogniseClassDeclarations(False);

  while(TokenList.FirstSolidTokenWord in CLASS_VISIBILITY) do
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
  CLASS_DECL_WORDS = [wProcedure, wFunction, wConstructor, wDestructor, wProperty, wClass];
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

  while TokenList.FirstSolidTokenWord in (CLASS_DECL_WORDS + [wUnknown]) do
  begin
    // only make this node if it will have children
    if not lbStarted then
      PushNode(nClassDeclarations);
    lbStarted := True;

    lc := TokenList.FirstSolidToken;

    case lc.Word of
      wProcedure:
        RecogniseProcedureHeading(False);
      wFunction:
        RecogniseFunctionHeading(False);
      wClass:
      begin
        // must be followed by 'procedure' or 'function'
        case TokenList.SolidTokenWord(2) of
          wProcedure:
            RecogniseProcedureHeading(False);
          wFunction:
            RecogniseFunctionHeading(False);
          else
            Raise TEParseError.Create('Expected class procedure or class function', lc);
        end;

      end;
      wConstructor:
      begin
        // no constructor on interface
        if pbInterface then
          Raise TEParseError.Create('unexpected token', lc);
        RecogniseConstructorHeading(True);
      end;
      wDestructor:
      begin
        // no constructor on interface
        if pbInterface then
          Raise TEParseError.Create('unexpected token', lc);
        RecogniseDestructorHeading(True);
      end;
      wProperty:
        RecogniseProperty;
      else
      begin
        // end of this list with next visibility section or class end?
        if lc.Word in CLASS_DECL_WORDS + [wEnd] then
        begin
          break;
        end
        // vars start with an identifier
        else if lc.TokenType = ttWord then
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
    if TokenList.FirstSolidTokenType = ttSemiColon then
      Recognise(ttSemicolon)
    else
      Break; // except the last

    // these end the visibility section
    if TokenList.FirstSolidTokenWord in (CLASS_VISIBILITY + [wEnd]) then
      break;
  end;

  if lbStarted then
    PopNode;
end;


procedure TBuildParseTree.RecogniseProperty;
begin
  //PropertyList -> PROPERTY  Ident [PropertyInterface]  PropertySpecifiers
  PushNode(nProperty);

  Recognise(wProperty);
  RecogniseIdent;
  RecognisePropertyInterface;
  RecognisePropertySpecifiers;

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
    if (TokenList.FirstSolidTokenWord in [wConst, wVar]) then
      Recognise([wConst, wVar]);

    RecogniseIdentList;
    Recognise(ttColon);
    RecogniseTypeId;

    if TokenList.FirstSolidTokenType = ttSemiColon then
      Recognise(ttSemiColon)
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
  PROPERTY_SPECIFIERS: TWordSet = [wIndex, wRead, wWrite, wStored,
    wDefault, wNoDefault, wImplements, wDispId, wReadOnly];
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

  while lc.Word in PROPERTY_SPECIFIERS do
  begin
    PushNode(nPropertySpecifier);

    case lc.Word of
      wIndex:
      begin
        Recognise(wIndex);
        RecogniseConstExpr;
      end;
      wRead:
      begin
        Recognise(wRead);
        RecogniseIdent;
      end;
      wWrite:
      begin
        Recognise(wWrite);
        RecogniseIdent;
      end;
      wStored:
      begin
        Recognise(wStored);
        if TokenList.FirstSolidTokenType = ttWord then
          RecogniseIdent
         else
          RecogniseConstExpr;
      end;
      wDefault:
      begin
        Recognise(wDefault);
        RecogniseConstExpr;
      end;
      wNoDefault:
      begin
        Recognise(wNoDefault);
      end;
      wImplements:
      begin
        Recognise(wImplements);
        RecogniseTypeId;
      end;
      wDispId:
      begin
        Recognise(wDispId);
        Recognise(ttNumber);
      end;
      wReadOnly:
      begin
        Recognise(wReadOnly);
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

  if TokenList.FirstSolidTokenType = ttSemiColon then
  begin
    PopNode;
    exit;
  end;

  if TokenList.FirstSolidTokenType = ttOpenBracket then
    RecogniseInterfaceHeritage;

  if TokenList.FirstSolidTokenType = ttOpenSquareBracket then
    RecogniseInterfaceGuid;

  RecogniseClassDeclarations(True);

  Recognise(wEnd);

  PopNode;
end;

procedure TBuildParseTree.RecogniseInterfaceGuid;
begin
  Recognise(ttOpenSquareBracket);
  Recognise(ttLiteralString);
  Recognise(ttCloseSquareBracket);
end;

procedure TBuildParseTree.RecogniseInterfaceHeritage;
begin
  // InterfaceHeritage -> '(' IdentList ')'
  Recognise(ttOpenBracket);
  RecogniseIdentList;
  Recognise(ttCloseBracket);
end;

procedure TBuildParseTree.RecogniseRequiresClause;
begin
  // RequiresClause -> REQUIRES IdentList... ';'

  Recognise(wRequires);
  RecogniseIdentList;
  Recognise(ttSemiColon);
end;

procedure TBuildParseTree.RecogniseContainsClause;
begin
  // ContainsClause -> CONTAINS IdentList... ';'

  Recognise(wContains);
  RecogniseIdentList;
  Recognise(ttSemicolon);
end;

{ worker for RecogniseIdentList }
procedure TBuildParseTree.RecogniseIdentValue;
begin
  if TokenList.FirstSolidTokenWord = wEquals then
  begin
    Recognise(wEquals);
    RecogniseExpr;
  end;
end;

procedure TBuildParseTree.RecogniseIdentList;
begin
  { IdentList -> Ident/','...

    now in D6 enum types can have numeric values
     e.g. (foo, bar = 3, baz)
  }
  PushNode(nIdentList);

  RecogniseIdent;
  RecogniseIdentValue;

  while TokenList.FirstSolidTokenType = ttComma do
  begin
    Recognise(ttComma);
    RecogniseIdent;
    RecogniseIdentValue;
  end;

  PopNode;
end;

procedure TBuildParseTree.RecogniseConstExpr;
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

    if (TokenList.FirstSolidTokenWord = wAs) then
    begin
      Recognise(wAs);
      RecogniseIdent;
    end;
    Recognise(ttCloseBracket);
    PopNode;
  end
  else
    // a simple ident - e.g. "x"
    RecogniseIdent;
end;

procedure TBuildParseTree.RecogniseIdent;
var
  lc: TSourceToken;
begin
  lc := TokenList.FirstSolidToken;

  if not(IdentifierNext) then
    Raise TEParseError.Create('Expected identifer', lc);

  Recognise(IdentifierTypes);
end;

{ the name of a procedure/function/constructor can be
  a plain name or classname.methodname }
procedure TBuildParseTree.RecogniseMethodName(const pbClassNameCompulsory: Boolean);
begin
  if not(IdentifierNext) then
    Raise TEParseError.Create('Expected identifer', TokenList.FirstSolidToken);

  Recognise(IdentifierTypes);

  if (TokenList.FirstSolidTokenType = ttDot) or pbClassNameCompulsory then
  begin
    Recognise(ttDot);
    RecogniseIdent;
  end
end;


procedure TBuildParseTree.RecogniseTypeId;
begin
  RecogniseIdent;
end;

procedure TBuildParseTree.RecogniseAsmBlock;
begin
  PushNode(nAsm);

  Recognise(wAsm);
  while TokenList.FirstSolidTokenWord <> wEnd do
    RecogniseAsmStatement;

  Recognise(wEnd);

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

  if TokenList.FirstSolidTokenWord = wAtSign then
  begin

    Recognise(wAtSign);
    Recognise(wAtSign);
    RecogniseAsmIdent;
    Recognise(ttColon);
  end
  else
  begin
    RecogniseAsmOpcode;

    RecogniseWhiteSpace;

    while not (TokenList.FirstTokenType in [ttSemiColon, ttReturn, ttComment]) do
    begin
      if TokenList.FirstSolidTokenType = ttComma then
        Recognise(ttComma);
      RecogniseAsmParam;

      RecogniseWhiteSpace;

      if TokenList.FirstSolidTokenWord = wEnd then
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

  if not (lc.TokenType in [ttWord, ttOperator]) and (lc.Word in ASM_IDENT_TOKENS) then
     Raise TEParseError.Create('Expected asm identifer', TokenList.FirstSolidToken);

  while (lc.TokenType in [ttWord, ttOperator]) and (lc.Word in ASM_IDENT_TOKENS) do
  begin
    Recognise(ASM_IDENT_TOKENS);
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
  RecogniseIdent;
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

  if lc.Word = wAtSign then
  begin
    Recognise(wAtSign);
    if TokenList.FirstSolidToken.Word = wAtSign then
      Recognise(wAtSign);


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
  else if (lc.TokenType = ttWord) and (lc.Word in ASM_IDENT_TOKENS) then
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
  if not (TokenList.FirstSolidTokenWord in HintDirectives) then
    exit;

  PushNode(nHintDirectives);

  while (TokenList.FirstSolidTokenWord in HintDirectives) do
  begin
    Recognise(HintDirectives);
  end;

  PopNode;
end;


procedure TBuildParseTree.RecognisePropertyDirectives;
const
  { this can be specified at the end after a semicolon
  so it's not just in the specifiers }
  PropertyDirectives = [wDefault];
begin
  while (TokenList.FirstSolidTokenType = ttSemiColon) and
    (TokenList.SolidTokenWord(2) in PropertyDirectives) do
  begin
    PushNode(nPropertyDirective);

    Recognise(ttSemiColon);
    Recognise(PropertyDirectives);

    PopNode;
  end;

end;


procedure TBuildParseTree.RecogniseExportsSection;
begin
  PushNode(nExports);

  Recognise(wExports);
  RecogniseExportedProc;

  // more to come?
  while TokenList.FirstTokenType <> ttSemiColon do
  begin
    Recognise(ttComma);
    RecogniseExportedProc;
  end;

  Recognise(ttSemiColon);

  PopNode;
end;

procedure TBuildParseTree.RecogniseExportedProc;
const
  ExportedDirectives: TWordSet = [wName, wIndex, wResident];
var
  lc: TSourceToken;
begin
  PushNode(nExportedProc);

  RecogniseIdent;
  if TokenList.FirstSolidTokenType = ttOpenBracket then
    RecogniseFormalParameters;

  while TokenList.FirstSolidTokenWord in ExportedDirectives do
  begin
    lc := TokenList.FirstSolidToken;

    case lc.Word of
      wName:
      begin
        Recognise(wName);
        Recognise([ttLiteralString, ttWord]);
      end;
      wIndex:
      begin
        Recognise(wIndex);
        Recognise(ttNumber);
      end;
      wResident:
        Recognise(wResident);
      else
         Raise TEParseError.Create('Expected export directive', lc);
    end;
  end;

  PopNode;
end;


end.
