Unit BuildParseTree;

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

Interface

Uses
  { delphi }
  Contnrs,
  { local }
  ParseTreeNode,
  ParseTreeNodeType,
  ParseError,
  SourceToken,
  SourceTokenList,
  Tokens;

Type
  TBuildParseTree = Class(TObject)
  Private
    fbMadeTree: Boolean;
    fiTokenIndex: integer;

    fcRoot: TParseTreeNode;
    fcStack: TStack;
    fcTokenList: TSourceTokenList;

    fiTokenCount: integer;

    Procedure RecogniseGoal;
    Procedure RecogniseUnit;
    Procedure RecogniseProgram;
    Procedure RecognisePackage;
    Procedure RecogniseLibrary;

    Procedure RecogniseFileEnd;

    Procedure RecogniseProgramBlock;
    Procedure RecogniseUsesClause(Const pbInFiles: Boolean);
    Procedure RecogniseUsesItem(Const pbInFiles: Boolean);

    Procedure RecogniseInterfaceSection;
    Procedure RecogniseInterfaceDecls;
    Procedure RecogniseInterfaceDecl;
    Procedure RecogniseExportedHeading;

    Procedure RecogniseIdentifier(Const pbCanHaveUnitQualifier: Boolean);
    Procedure RecogniseImplementationSection;
    Procedure RecogniseDeclSections;
    Procedure RecogniseDeclSection;
    Procedure RecogniseInitSection;
    Procedure RecogniseBlock;
    Procedure RecogniseIdentList(Const pbCanHaveUnitQualifier: Boolean);
    Procedure RecogniseIdentValue;

    Procedure RecogniseLabelDeclSection;
    Procedure RecogniseLabel;
    Procedure RecogniseConstSection;
    Procedure RecogniseConstantDecl;
    Procedure CheckLabelPrefix;

    Procedure RecogniseTypeSection;
    Procedure RecogniseVarSection;
    Procedure RecogniseProcedureDeclSection;

    // set pbAnon = true if the proc has no name
    Procedure RecogniseProcedureHeading(Const pbAnon, pbCanInterfaceMap: Boolean);
    Procedure RecogniseFunctionHeading(Const pbAnon, pbCanInterfaceMap: Boolean);
    Procedure RecogniseCompoundStmnt;
    Procedure RecogniseStatementList(Const peEndTokens: TTokenTypeSet);
    Procedure RecogniseStatement;

    Procedure RecogniseTypeId;
    Procedure RecogniseTypedConstant;
    Procedure RecogniseArrayConstant;
    Procedure RecogniseRecordConstant;
    Procedure RecogniseRecordFieldConstant;

    Procedure RecogniseTypeDecl;

    Procedure RecogniseArrayType;
    Procedure RecogniseClassRefType;
    Procedure RecogniseEnumeratedType;
    Procedure RecogniseFieldDecl;
    Procedure RecogniseFieldList;
    Procedure RecogniseFileType;
    Procedure RecogniseOrdIdent;
    Procedure RecogniseOrdinalType;
    Procedure RecognisePointerType;
    Procedure RecogniseProcedureType;
    Procedure RecogniseRealType;
    Procedure RecogniseRecordType;
    Procedure RecogniseRecVariant;
    Procedure RecogniseRestrictedType;
    Procedure RecogniseSetType;
    Procedure RecogniseSimpleType;
    Procedure RecogniseStringType;
    Procedure RecogniseStrucType;
    Procedure RecogniseSubrangeType;
    Procedure RecogniseType;
    Procedure RecogniseVariantType;
    Procedure RecogniseClassType;
    Procedure RecogniseClassBody;
    Procedure RecogniseClassDeclarations(Const pbInterface: Boolean);

    Procedure RecogniseInterfaceType;
    Procedure RecogniseObjectType;
    Procedure RecogniseVariantSection;
    Procedure RecogniseVarDecl;
    Procedure RecogniseAddOp;
    Procedure RecogniseDesignator;
    Procedure RecogniseDesignatorTail;
    Procedure RecogniseExpr(Const pbAllowRelop: Boolean);
    Procedure RecogniseExprList;
    Procedure RecogniseFactor;
    Procedure RecogniseTerm;
    Procedure RecogniseMulOp;
    Procedure RecogniseRelOp;
    Procedure RecogniseSetConstructor;
    Procedure RecogniseSetElement;
    Procedure RecogniseQualId;
    Procedure RecogniseConstantExpression;

    Procedure RecogniseSimpleExpression;
    Procedure RecogniseSimpleStmnt;

    Procedure RecogniseCaseLabel;
    Procedure RecogniseCaseSelector;
    Procedure RecogniseCaseStmnt;
    Procedure RecogniseForStmnt;
    Procedure RecogniseIfStmnt;
    Procedure RecogniseRepeatStmnt;
    Procedure RecogniseStructStmnt;
    Procedure RecogniseWhileStmnt;
    Procedure RecogniseWithStmnt;
    Procedure RecogniseTryStatement;
    Procedure RecogniseExceptionHandlerBlock;
    Procedure RecogniseExceptionHandler;
    Procedure RecogniseRaise;

    Procedure RecogniseFunctionDecl;
    Procedure RecogniseProcedureDecl;
    Procedure RecogniseConstructorDecl;
    Procedure RecogniseDestructorDecl;

    Procedure RecogniseFormalParameters;
    Procedure RecogniseFormalParam;
    Procedure RecogniseParameter;
    Procedure RecogniseActualParams;
    Procedure RecogniseActualParam;

    Procedure RecogniseProcedureDirectives;

    Procedure RecogniseExportsSection;
    Procedure RecogniseExportedProc;

    // set pbDeclaration to false if the method body is to be recognised
    Procedure RecogniseConstructorHeading(Const pbDeclaration: boolean);
    Procedure RecogniseDestructorHeading(Const pbDeclaration: boolean);
    Procedure RecogniseObjHeritage;

    Procedure RecogniseContainsClause;
    Procedure RecogniseInterfaceHeritage;
    Procedure RecogniseProperty;
    Procedure RecognisePropertyInterface;
    Procedure RecognisePropertyParameterList;
    Procedure RecognisePropertySpecifiers;
    Procedure RecognisePropertyAccess;
    Procedure RecogniseRequiresClause;
    Procedure RecogniseInterfaceGuid;
    Procedure RecogniseClassHeritage;
    Procedure RecogniseClassVisibility;
    Procedure RecogniseMethodName(Const pbClassNameCompulsory: Boolean);

    Procedure RecogniseAsmBlock;
    Procedure RecogniseAsmParam;
    Procedure RecogniseAsmStatement;
    Procedure RecogniseAsmExpr;
    Procedure RecogniseAsmOperator;
    Procedure RecogniseAsmFactor;

    Procedure RecogniseAsmIdent;
    Procedure RecogniseAsmOpcode;
    Procedure RecogniseAsmLabel(Const pbColon: boolean);
    Procedure RecogniseWhiteSpace;

    Procedure RecogniseHintDirectives;
    Procedure RecognisePropertyDirectives;
    Procedure RecogniseExternalProcDirective;

    Procedure Recognise(Const peTokenTypes: TTokenTypeSet); Overload;
    Procedure Recognise(Const peTokenType: TTokenType); Overload;

    Function PushNode(Const peNodeType: TParseTreeNodeType): TParseTreeNode;
    Function PopNode: TParseTreeNode;
    Function TopNode: TParseTreeNode;
    Function IdentifierNext: Boolean;
    Function ArrayConstantNext: boolean;
  Protected
  Public
    Constructor Create;
    Destructor Destroy; Override;
    Procedure BuildParseTree;
    Procedure Clear;
    Property Root: TParseTreeNode Read fcRoot;
    Property TokenList: TSourceTokenList Read fcTokenList Write fcTokenList;
  End;

Implementation

Uses
  { delphi } SysUtils,
  Dialogs,
  Forms,
  JclStrings,
  { local } TokenUtils;

Const
  UPDATE_INTERVAL = 512;

  {------------------------------------------------------------------------------
    standard overrides }

Constructor TBuildParseTree.Create;
Begin
  Inherited;
  fcStack := TStack.Create;
  fcRoot := Nil;
  fiTokenCount := 0;
End;

Destructor TBuildParseTree.Destroy;
Begin
  Clear;
  fcStack.Free; {FreeAndNil is pointless here}
  Inherited;
End;

Procedure TBuildParseTree.Clear;
Begin
  While fcStack.Count > 0 Do
    fcStack.Pop;
  FreeAndNil(fcRoot);
End;

Procedure TBuildParseTree.BuildParseTree;
Begin
  Assert(fcTokenList <> Nil);
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
  Assert(fcTokenList.Count = fcTokenList.StackIndex);
  fcTokenList.Clear;


  fbMadeTree := True;
End;

{-------------------------------------------------------------------------------
  recogniser support }

Procedure TBuildParseTree.Recognise(Const peTokenTypes: TTokenTypeSet);
//Var HAVING LOCAL GLOBALS SLOW DOWN THE THING
  //lcCurrentToken: TSourceToken;

  // Function Matched: Boolean; REMOVED. INLINED

  Function DescribeTarget: String;
  Begin
    Result := '"';
    If peTokenTypes <> [] Then Result := Result + TokenTypesToString(peTokenTypes);
    Result := Result + '"';
  End;
Var
  lcCurrentToken: TSourceToken;
  Index1: Integer;
Begin
  // must accept something
  Assert(peTokenTypes <> []);
  { read tokens up to and including the specified one.
    Add them to the parse tree at the current growing point  }
  Index1 := fcTokenList.StackIndex; {AdemBaba}
  While Index1 < fcTokenList.Count Do Begin
    //lcCurrentToken := fcTokenList.ExtractFirst;
    lcCurrentToken := fcTokenList.Extract(Index1);
    Assert(lcCurrentToken <> Nil);
    TopNode.AddChild(lcCurrentToken);
    // the the match must be the first solid token
    If lcCurrentToken.TokenType In peTokenTypes Then Break
    Else If Not (lcCurrentToken.TokenType In NotSolidTokens) Then Raise TEParseError.Create('Unexpected token, expected ' + DescribeTarget, lcCurrentToken);
    Inc(Index1);
  End;

  inc(fiTokenCount);
  If (fiTokenCount Mod UPDATE_INTERVAL) = 0 Then Application.ProcessMessages;
End;

Procedure TBuildParseTree.Recognise(Const peTokenType: TTokenType);
Begin
  Recognise([peTokenType]);
End;

Function TBuildParseTree.PushNode(Const peNodeType: TParseTreeNodeType): TParseTreeNode;
Begin
  Result := TParseTreeNode.Create;
  Result.NodeType := peNodeType;
  If fcStack.Count > 0 Then Begin
    TopNode.AddChild(Result);
    Result.Parent := TopNode;
  End Else fcRoot := Result;
  fcStack.Push(Result);
End;

Function TBuildParseTree.PopNode: TParseTreeNode;
Begin
  Result := fcStack.Pop;
End;

Function TBuildParseTree.TopNode: TParseTreeNode;
Begin
  Result := fcStack.Peek;
End;

{a unit / type/var name }

Function TBuildParseTree.IdentifierNext: Boolean;
Var
  lc: TSourceToken;
Begin
  lc := fcTokenList.FirstSolidToken;
  { We have to admit directives and type names as identifiers. see TestBogusDirectives.pas for the reasons why }
  Result := IsIdentifierToken(lc);
End;

{-------------------------------------------------------------------------------
  recognisers for the parse tree  top to bottom

  These procs are based on the "Appendix A Object Pascal grammar"
  Found on the Borland Web site
  All the productions should be here, in the same order
}

Procedure TBuildParseTree.RecogniseGoal;
Var
  lc: TSourceToken;
Begin
  // Goal -> (Program | Package  | Library  | Unit)

  If fcTokenList.Count < 1 Then
    Raise TEParseError.Create('No source to parse', Nil);

  lc := fcTokenList.FirstSolidToken;
  Assert(lc <> Nil);

  Case lc.TokenType Of
    ttProgram:
      RecogniseProgram;
    ttPackage:
      RecognisePackage;
    ttLibrary:
      RecogniseLibrary;
    ttUnit:
      RecogniseUnit;
  Else
    Raise TEParseError.Create('Expected program, package, library, unit', lc);
  End
End;

Procedure TBuildParseTree.RecogniseProgram;
Begin
  // Program -> [PROGRAM Ident ['(' IdentList ')'] ';']  ProgramBlock '.'
  PushNode(nProgram);

  PushNode(nUnitHeader);
  Recognise(ttProgram);

  PushNode(nUnitName);
  RecogniseIdentifier(False);
  PopNode;

  If fcTokenList.FirstSolidTokenType = ttOpenBracket Then Begin
    Recognise(ttOpenBracket);
    RecogniseIdentList(False);
    Recognise(ttCloseBracket);
  End;

  If fcTokenList.FirstSolidTokenType = ttSemiColon Then
    Recognise(ttSemicolon);

  PopNode;

  RecogniseProgramBlock;
  RecogniseFileEnd;

  PopNode;
End;

Procedure TBuildParseTree.RecogniseUnit;
Begin
  // Unit -> UNIT Ident ';' InterfaceSection ImplementationSection InitSection '.'
  PushNode(nUnit);

  PushNode(nUnitHeader);
  Recognise(ttUnit);

  PushNode(nUnitName);
  RecogniseIdentifier(False);
  PopNode;

  { unit can be "deprecated platform library" }
  If fcTokenList.FirstSolidTokenType In HintDirectives Then Begin
    PushNode(nHintDirectives);

    While fcTokenList.FirstSolidTokenType In HintDirectives Do
      Recognise(HintDirectives);

    PopNode;
  End;

  { or platform }
  If fcTokenList.FirstSolidTokenType = ttPlatform Then
    Recognise(ttPlatform);

  Recognise(ttSemicolon);

  PopNode;

  RecogniseInterfaceSection;
  RecogniseImplementationSection;
  RecogniseInitSection;
  RecogniseFileEnd;

  PopNode;
End;

Procedure TBuildParseTree.RecognisePackage;
Begin
  // Package -> PACKAGE Ident ';' [RequiresClause] [ContainsClause] END '.'
  PushNode(nPackage);

  PushNode(nUnitHeader);
  Recognise(ttPackage);

  PushNode(nUnitName);
  RecogniseIdentifier(False);
  PopNode;
  Recognise(ttSemicolon);
  PopNode;

  If fcTokenList.FirstSolidTokenType = ttRequires Then
    RecogniseRequiresClause;

  If fcTokenList.FirstSolidTokenType = ttContains Then
    RecogniseContainsClause;

  Recognise(ttEnd);
  RecogniseFileEnd;

  PopNode;
End;

Procedure TBuildParseTree.RecogniseLibrary;
Begin
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
  RecogniseFileEnd;

  PopNode;
End;

Procedure TBuildParseTree.RecogniseFileEnd;
Var
  Index1: Integer;
  lcCurrentToken: TSourceToken;
Begin
  Recognise(ttDot);

  { delphi accepts anything after the final end }
  Index1 := fcTokenList.StackIndex; {AdemBaba}
  While Index1 < fcTokenList.Count Do Begin
    lcCurrentToken := fcTokenList.Extract(Index1);
    TopNode.AddChild(lcCurrentToken);
    Inc(Index1);
  End;
End;

Procedure TBuildParseTree.RecogniseProgramBlock;
Var
  lc: TSourceToken;
Begin
  // ProgramBlock -> [UsesClause] Block

  lc := fcTokenList.FirstSolidToken;

  If lc.TokenType = ttUses Then
    RecogniseUsesClause(True);

  RecogniseBlock;
End;

Procedure TBuildParseTree.RecogniseUsesClause(Const pbInFiles: Boolean);
Begin
  // UsesClause -> USES IdentList ';'
  PushNode(nUses);

  Recognise(ttUses);

  // IdentList -> Ident/','...
  PushNode(nIdentList);

  RecogniseUsesItem(pbInFiles);

  While fcTokenList.FirstSolidTokenType = ttComma Do Begin
    Recognise(ttComma);
    RecogniseUsesItem(pbInFiles);
  End;

  PopNode;

  Recognise(ttSemicolon);

  PopNode;
End;

Procedure TBuildParseTree.RecogniseUsesItem(Const pbInFiles: Boolean);
Begin
  PushNode(nUsesItem);

  RecogniseIdentifier(False);

  If pbInFiles And (fcTokenList.FirstSolidTokenType = ttIn) Then Begin
    Recognise(ttIn);
    Recognise(ttLiteralString);
  End;

  PopNode;
End;

Procedure TBuildParseTree.RecogniseInterfaceSection;
Begin
  // InterfaceSection -> INTERFACE [UsesClause] [InterfaceDecl]...

  PushNode(nInterfaceSection);

  Recognise(ttInterface);

  If fcTokenList.FirstSolidTokenType = ttUses Then
    RecogniseUsesClause(False);

  RecogniseInterfaceDecls;

  PopNode;
End;

Procedure TBuildParseTree.RecogniseInterfaceDecls;
Begin
  { a list of InterfaceDecl sections
    e.g.

      var a,b: integer;
      const b = 3;
      type foo = integer;
      procedure fred;

      NB also threadvar

  }
  While fcTokenList.FirstSolidTokenType In [ttConst, ttResourceString, ttType, ttVar, ttThreadVar] + ProcedureWords Do
    RecogniseInterfaceDecl;
End;

Procedure TBuildParseTree.RecogniseInterfaceDecl;
Var
  lc: TSourceToken;
  lt: TTokenType;
Begin
  {
   InterfaceDecl -> ConstSection
       -> TypeSection
       -> VarSection
       -> ExportedHeading
   }
  PushNode(nDeclSection);

  lc := fcTokenList.FirstSolidToken;
  lt := fcTokenList.FirstSolidTokenType;

  Case lt Of
    ttConst, ttResourceString:
      RecogniseConstSection;
    ttType:
      RecogniseTypeSection;
    ttVar, ttThreadvar:
      RecogniseVarSection;
    ttProcedure, ttFunction:
      RecogniseExportedHeading;
  Else
    Raise TEParseError.Create('Expected const, type, var, procedure or function', lc);
  End;

  PopNode;
End;

Procedure TBuildParseTree.RecogniseExportedHeading;
Var
  lc: TSourceToken;
  lt: TTokenType;
Begin
  { ExportedHeading
     -> ProcedureHeading ';' [Directive]
     -> FunctionHeading ';' [Directive] }

  lc := fcTokenList.FirstSolidToken;
  lt := lc.TokenType;

  Case lt Of
    ttProcedure: Begin
        RecogniseProcedureHeading(False, False);
      End;
    ttFunction: Begin
        RecogniseFunctionHeading(False, False);
      End;
  Else
    Raise TEParseError.Create('Expected function or procedure', lc);
  End;

  { the ';' is ommited by lazy programmers in some rare occasions}
  If fcTokenList.FirstSolidTokenType = ttSemicolon Then
    Recognise(ttSemicolon);
End;

Procedure TBuildParseTree.RecogniseImplementationSection;
Begin
  {
    ImplementationSection -> IMPLEMENTATION
         [UsesClause]
         [DeclSection]...
  }
  PushNode(nImplementationSection);

  Recognise(ttImplementation);

  If fcTokenList.FirstSolidTokenType = ttUses Then
    RecogniseUsesClause(False);

  RecogniseDeclSections;

  PopNode;
End;

Procedure TBuildParseTree.RecogniseBlock;
Var
  lc: TSourceToken;
  lt: TTokenType;
Begin
  { Block -> [DeclSection] CompoundStmt }

  lc := fcTokenList.FirstSolidToken;
  lt := lc.TokenType;

  PushNode(nBlock);

  // [DeclSection]

  If lt In (Declarations + ProcedureWords) Then
    RecogniseDeclSections;

  If fcTokenList.FirstSolidTokenType = ttAsm Then
    RecogniseAsmBlock
  Else
    RecogniseCompoundStmnt;

  PopNode;
End;

Procedure TBuildParseTree.RecogniseDeclSections;
Begin
  { a list of Decl sections
    e.g.

      label b;
      var a: integer;
      const b = 3;
      type foo = integer;
      procedure fred;
      class procedure TFoo.bar;

  }
  While fcTokenList.FirstSolidTokenType In
    [ttClass] + Declarations + ProcedureWords Do
    RecogniseDeclSection;
End;

Procedure TBuildParseTree.RecogniseDeclSection;
Var
  lc: TSourceToken;
  lt: TTokenType;
Begin
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

  Case lt Of
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
  Else
    Raise TEParseError.Create('Expected label, const, type, var, procedure or function', lc);
  End;

  PopNode;
End;

Procedure TBuildParseTree.RecogniseLabelDeclSection;
Begin
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

  While fcTokenList.FirstSolidTokenType = ttComma Do Begin
    Recognise(ttComma);
    RecogniseLabel;
  End;

  PopNode;

  Recognise(ttSemicolon);

  PopNode;
End;

Procedure TBuildParseTree.RecogniseLabel;
Begin
  If fcTokenList.FirstSolidTokenType = ttNumber Then
    Recognise(ttNumber)
  Else
    // no unit qualifier
    RecogniseIdentifier(false);
End;

Procedure TBuildParseTree.RecogniseConstSection;
Begin
  {
    ConstSection -> CONST (ConstantDecl ';')...
  }
  PushNode(nConstSection);
  Recognise([ttConst, ttResourceString]);

  While fcTokenList.FirstSolidWordType In IdentifierTypes Do Begin
    RecogniseConstantDecl;
    Recognise(ttSemicolon);
  End;

  PopNode;
End;

Procedure TBuildParseTree.RecogniseConstantDecl;
Var
  lc: TSourceToken;
Begin
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

  lc := fcTokenList.FirstSolidToken;

  If lc.TokenType = ttEquals Then Begin
    Recognise(ttEquals);
    RecogniseConstantExpression;
  End
  Else
    If lc.TokenType = ttColon Then Begin
    Recognise(ttColon);
    //RecogniseTypeId;
    RecogniseType;
    Recognise(ttEquals);
    RecogniseTypedConstant;
  End
  Else
    Raise TEParseError.Create('Expected equals or colon', lc);

  { can be deprecated library platform }
  RecogniseHintDirectives;

  PopNode;
End;

Procedure TBuildParseTree.RecogniseTypeSection;
Begin
  {
  TypeSection -> TYPE (TypeDecl ';')...
  }
  PushNode(nTypeSection);
  Recognise(ttType);

  While fcTokenList.FirstSolidWordType In IdentifierTypes Do Begin
    RecogniseTypeDecl;
  End;

  PopNode;
End;

Procedure TBuildParseTree.RecogniseTypeDecl;
Begin
  {
  TypeDecl -> Ident '=' Type
     -> Ident '=' RestrictedType

  Need a semicolon
  }

  PushNode(nTypeDecl);

  RecogniseIdentifier(False);
  Recognise(ttEquals);

  // type or restricted type
  If (fcTokenList.FirstSolidTokenType In [ttObject, ttClass, ttInterface, ttDispInterface]) Then
    RecogniseRestrictedType
  Else
    RecogniseType;

  Recognise(ttSemicolon);

  PopNode;
End;

{ helper proc for RecogniseTypedConstant
  need to distinguish
  "expr" from "(expr, expr)"
  note that expr can -> (expr)
  so we need to notice the comma
  is there a semicolon first or a comma

  Array of records can be "((f: 1), (f: 2))"
  and if it is an array with one element then it is "((f: x))"
}

Function TBuildParseTree.ArrayConstantNext: boolean;
Var
  liIndex: integer;
  liBracketLevel: integer;
  tt: TTokenType;
Begin
  Result := False;

  If fcTokenList.FirstSolidTokenType <> ttOpenBracket Then
    exit;

  liBracketLevel := 0;
  liIndex := fcTokenList.StackIndex; {AdemBaba}
  // scan past the open bracket
  While fcTokenList.SourceTokens[liIndex].TokenType <> ttOpenBracket Do inc(liIndex);

  inc(liIndex);

  // look forward to find the first comma or semicolon
  While True Do Begin
    If liIndex >= fcTokenList.Count Then
      break;

    tt := fcTokenList.SourceTokens[liIndex].TokenType;

    If tt = ttOpenBracket Then
      inc(liBracketLevel)
    Else If tt = ttCloseBracket Then
      dec(liBracketLevel)
    Else If (tt = ttComma) And (liBracketLevel = 0) Then Begin
      Result := True;
      break;
    End
    Else If (tt = ttSemicolon) And (liBracketLevel = 0) Then Begin
      Result := False;
      break;
    End
      { if we get an semicolon at bracket level 2, it means an array of records
        e.g.
          Const MyFooRecArray = ((x: 2; y:3), (x: 5; y: 6)); }
    Else If (tt = ttSemicolon) And (liBracketLevel = 1) Then Begin
      Result := True;
      break;
    End;

    inc(liIndex);
  End;

End;

Procedure TBuildParseTree.RecogniseTypedConstant;
Begin
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
  If (fcTokenList.FirstSolidTokenType = ttOpenBracket) And
    (fcTokenList.SolidWordType(2) In IdentifierTypes) And
    (fcTokenList.SolidTokenType(3) = ttColon) Then Begin
    RecogniseRecordConstant;
  End
  Else If (ArrayConstantNext) Then Begin
    RecogniseArrayConstant
  End
  Else
    RecogniseConstantExpression;
End;

Procedure TBuildParseTree.RecogniseArrayConstant;
Begin
  // ArrayConstant -> '(' TypedConstant/','... ')'

  PushNode(nArrayConstant);

  Recognise(ttOpenBracket);

  RecogniseTypedConstant;
  While (fcTokenList.FirstSolidTokenType = ttComma) Do Begin
    Recognise(ttComma);
    RecogniseTypedConstant;
  End;

  Recognise(ttCloseBracket);

  PopNode;
End;

Procedure TBuildParseTree.RecogniseRecordConstant;
Begin
  // RecordConstant -> '(' RecordFieldConstant/';'... ')'

  PushNode(nRecordConstant);

  Recognise(ttOpenBracket);

  RecogniseRecordFieldConstant;
  While (fcTokenList.FirstSolidTokenType = ttSemicolon) Do Begin
    Recognise(ttSemicolon);

    If fcTokenList.FirstSolidTokenType = ttCloseBracket Then
      break;

    RecogniseRecordFieldConstant;
  End;

  Recognise(ttCloseBracket);
  PopNode;
End;

Procedure TBuildParseTree.RecogniseRecordFieldConstant;
Begin
  // RecordFieldConstant -> Ident ':' TypedConstant

  PushNode(nRecordFieldConstant);

  RecogniseIdentifier(False);
  Recognise(ttColon);
  RecogniseTypedConstant;

  PopNode;
End;

Procedure TBuildParseTree.RecogniseType;
Var
  lc, lc2: TSourceToken;
Begin
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

  lc := fcTokenList.FirstSolidToken;
  lc2 := fcTokenList.SolidToken(2);

  { type can be prefixed with a unit name, e.g.
    Classes.TList; }
  If lc2.TokenType = ttDot Then Begin
    RecogniseIdentifier(False);
    Recognise(ttDot);
  End;

  If (lc.TokenType = ttType) Then Begin
    { this can be a prefix. See help under "Declaring types".
      an e.g. is in TestDeclarations.pas }
    Recognise(ttType);
  End;
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

  Case lc.TokenType Of  {I am not sure this is faster. But it sure avoids mixing tokentypes in the conditionals}
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
  Else
    If (lc.TokenType = ttClass) And (lc2.TokenType = ttOf) Then RecogniseClassRefType
    Else If (lc.WordType In IdentifierTypes) Then Begin
      { could be a subrange on an enum, e.g. "clBlue .. clBlack". NB: this can also be Low(Integer) .. High(Integer) }
      If (AnsiSameText(lc.SourceCode, 'Low')) Or (fcTokenList.SolidTokenType(2) = ttDoubleDot) Then RecogniseSubRangeType
      Else RecogniseTypeId; // some previously declared type that this simple prog does not know of
    End Else RecogniseSimpleType;
  End;

  PopNode;
End;

Procedure TBuildParseTree.RecogniseRestrictedType;
Var
  lc: TSourceToken;
Begin
  {
  RestrictedType
    -> ObjectType
    -> ClassType
    -> InterfaceType
  }

  PushNode(nRestrictedType);

  lc := fcTokenList.FirstSolidToken;
  Case lc.TokenType Of
    ttObject:
      RecogniseObjectType;
    ttClass:
      RecogniseClassType;
    ttInterface, ttDispInterface:
      RecogniseInterfaceType;
  Else
    Raise TEParseError.Create('Expected object, class or interface', lc);
  End;

  PopNode;
End;

Procedure TBuildParseTree.RecogniseClassRefType;
Begin
  // ClassRefType -> CLASS OF TypeId

  Recognise(ttClass);
  Recognise(ttOf);
  RecogniseTypeId;
End;

Procedure TBuildParseTree.RecogniseSimpleType;
Var
  lc: TSourceToken;
Begin
  // SimpleType -> (OrdinalType | RealType)

  lc := fcTokenList.FirstSolidToken;

  If lc.TokenType In RealTypes Then
    RecogniseRealType
  Else
    RecogniseOrdinalType;
End;

Procedure TBuildParseTree.RecogniseRealType;
Begin
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
End;

Procedure TBuildParseTree.RecogniseOrdinalType;
Var
  lc: TSourceToken;
Begin
  // OrdinalType -> (SubrangeType | EnumeratedType | OrdIdent)

  lc := fcTokenList.FirstSolidToken;

  If lc.TokenType = ttOpenBracket Then
    RecogniseEnumeratedType
  Else If lc.TokenType In OrdTypes Then
    RecogniseOrdIdent
  Else
    RecogniseSubRangeType;
End;

Procedure TBuildParseTree.RecogniseOrdIdent;
Begin
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
End;

Procedure TBuildParseTree.RecogniseVariantType;
Begin
  {
    VariantType
      -> VARIANT
      -> OLEVARIANT
  }

  Recognise(VariantTypes);

End;

Procedure TBuildParseTree.RecogniseSubrangeType;
Begin
  { SubrangeType -> ConstExpr '..' ConstExpr
    this fails when an array is indexed on an entire type, eg
    'BoolArray: array[Boolean] of Boolean;'
  }
  PushNode(nSubrangeType);

  RecogniseConstantExpression;
  If fcTokenList.FirstSolidTokenType = ttDoubleDot Then Begin
    Recognise(ttDoubleDot);

    { recognising any expr is a bad idea here, as "a = 3" is an expression
      and we want this to end with a '='
      this could be "const ValidCharSet: set of 'A'..'z' = ['A'..'Z','a'..'z'];"

       }
    RecogniseExpr(False);
  End;

  PopNode;
End;

Procedure TBuildParseTree.RecogniseEnumeratedType;
Begin
  // EnumeratedType -> '(' IdentList ')'
  PushNode(nEnumeratedType);

  Recognise(ttOpenBracket);
  RecogniseIdentList(False);
  Recognise(ttCloseBracket);

  PopNode;
End;

Procedure TBuildParseTree.RecogniseStringType;
Begin
  {
    StringType
      -> STRING
       -> ANSISTRING
       -> WIDESTRING
       -> STRING '[' ConstExpr ']'
   }

  If fcTokenList.FirstSolidTokenType = ttString Then Begin
    Recognise(ttString);
    If fcTokenList.FirstSolidTokenType = ttOpenSquareBracket Then Begin
      // e.g. var f = String[30];
      Recognise(ttOpenSquareBracket);
      RecogniseConstantExpression;
      Recognise(ttCloseSquareBracket);

    End;
  End
  Else
    Recognise([ttAnsiString, ttWideString]);
End;

Procedure TBuildParseTree.RecogniseStrucType;
Var
  lc: TSourceToken;
Begin
  // StrucType -> [PACKED] (ArrayType | SetType | FileType | RecType)

  If fcTokenList.FirstSolidTokenType = ttPacked Then
    Recognise(ttPacked);

  lc := fcTokenList.FirstSolidToken;

  Case lc.TokenType Of
    ttArray:
      RecogniseArrayType;
    ttSet:
      RecogniseSetType;
    ttFile:
      RecogniseFileType;
    ttRecord:
      RecogniseRecordType;
  Else
    Raise TEParseError.Create('Expected array, set, file or record type', lc);
  End;
End;

Procedure TBuildParseTree.RecogniseArrayType;
Begin
  // ArrayType -> ARRAY ['[' OrdinalType/','... ']'] OF Type
  PushNode(nArrayType);

  Recognise(ttArray);

  If fcTokenList.FirstSolidTokenType = ttOpenSquarebracket Then Begin
    Recognise(ttOpenSquareBracket);

    RecogniseOrdinalType;
    While fcTokenList.FirstSolidTokenType = ttComma Do Begin
      Recognise(ttComma);
      RecogniseOrdinalType;
    End;

    Recognise(ttCloseSquareBracket);
  End;
  Recognise(ttOf);
  RecogniseType;

  PopNode;
End;

Procedure TBuildParseTree.RecogniseRecordType;
Begin
  // RecType -> RECORD [FieldList] END

  PushNode(nRecordType);

  Recognise(ttRecord);
  If fcTokenList.FirstSolidTokenType <> ttEnd Then
    RecogniseFieldList;
  Recognise(ttEnd);

  RecogniseHintDirectives;

  PopNode;
End;

Procedure TBuildParseTree.RecogniseFieldList;
Begin
  // FieldList ->  FieldDecl/';'... [VariantSection] [';']

  While (Not (fcTokenList.FirstSolidTokenType In [ttEnd, ttCase]))
    And (Not (fcTokenList.FirstSolidTokenType = ttCloseBracket)) Do Begin
    RecogniseFieldDecl;
    If fcTokenList.FirstSolidTokenType = ttSemicolon Then
      Recognise(ttSemicolon)
    Else
      Break;
  End;

  If fcTokenList.FirstSolidTokenType = ttCase Then
    RecogniseVariantSection;

  If fcTokenList.FirstSolidTokenType = ttSemicolon Then
    Recognise(ttSemicolon);
End;

Procedure TBuildParseTree.RecogniseFieldDecl;
Begin
  // FieldDecl -> IdentList ':' Type
  PushNode(nFieldDeclaration);

  RecogniseIdentList(False);
  Recognise(ttColon);
  RecogniseType;

  RecogniseHintDirectives;

  PopNode;
End;

Procedure TBuildParseTree.RecogniseVariantSection;
Begin
  PushNode(nRecordVariantSection);

  // VariantSection -> CASE [Ident ':'] TypeId OF RecVariant/';'...
  Recognise(ttCase);

  // is there an 'of' 2 tokens hence? If not, must be 'ident:' first
  If Not (fcTokenList.SolidTokenType(2) = ttOf) Then Begin
    RecogniseIdentifier(False);
    Recognise(ttColon);
  End;

  RecogniseTypeId;
  Recognise(ttOf);

  // I have tested and that there must be at least 1 case in a var section
  Repeat
    RecogniseRecVariant;

    // semicolon is optional on the last one
    If fcTokenList.FirstSolidTokenType = ttSemicolon Then
      Recognise(ttSemicolon)
    Else
      break;

  Until (fcTokenList.FirstSolidTokenType In [ttEnd, ttCloseBracket]);

  PopNode;
End;

Procedure TBuildParseTree.RecogniseRecVariant;
Begin
  // RecVariant -> ConstExpr/','...  ':' '(' [FieldList] ')'

  PushNode(nRecordVariant);

  RecogniseConstantExpression;
  While fcTokenList.FirstSolidTokenType = ttComma Do Begin
    Recognise(ttComma);
    RecogniseConstantExpression;
  End;

  Recognise(ttColon);
  Recognise(ttOpenBracket);

  If fcTokenList.FirstSolidTokenType <> ttCloseBracket Then
    RecogniseFieldList;

  Recognise(ttCloseBracket);

  PopNode;
End;

Procedure TBuildParseTree.RecogniseSetType;
Begin
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
End;

Procedure TBuildParseTree.RecogniseFileType;
Begin
  {
   FileType -> FILE OF TypeId

   also just plain 'file'
  }

  Recognise(ttFile);
  If fcTokenList.FirstSolidTokenType = ttOf Then Begin
    Recognise(ttOf);
    RecogniseTypeId;
  End;
End;

Procedure TBuildParseTree.RecognisePointerType;
Begin
  // PointerType -> '^' TypeId
  Recognise(ttHat);
  RecogniseTypeId;
End;

Procedure TBuildParseTree.RecogniseProcedureType;
Begin
  PushNode(nProcedureType);

  // ProcedureType -> (ProcedureHeading | FunctionHeading) [OF OBJECT]
  If fcTokenList.FirstSolidTokenType = ttProcedure Then
    RecogniseProcedureHeading(True, False)
  Else If fcTokenList.FirstSolidTokenType = ttFunction Then
    RecogniseFunctionHeading(True, False)
  Else
    Raise TEParseError.Create('Expected procedure or function type', fcTokenList.FirstSolidToken);

  If fcTokenList.FirstSolidTokenType = ttOf Then Begin
    Recognise(ttOf);
    Recognise(ttObject);
  End;

  RecogniseProcedureDirectives;

  PopNode;
End;

Procedure TBuildParseTree.RecogniseVarSection;
Const
  END_VAR_SECTION: TTokenTypeSet =
  [ttVar, ttThreadVar, ttConst, ttLabel, ttResourceString, ttType,
    ttBegin, ttEnd, ttImplementation, ttInitialization,
    ttProcedure, ttFunction, ttConstructor, ttDestructor, ttClass, ttAsm];
Begin
  PushNode(nVarSection);

  // VarSection -> VAR (VarDecl ';')...
  Recognise([ttVar, ttThreadvar]);

  Repeat
    RecogniseVarDecl;
    Recognise(ttSemicolon);
  Until (fcTokenList.FirstSolidTokenType In END_VAR_SECTION);

  PopNode;
End;

Procedure TBuildParseTree.RecogniseVarDecl;
Var
  lc: TSourceToken;
Begin
  // VarDecl -> IdentList ':' Type [(ABSOLUTE (Ident | ConstExpr)) | '=' ConstExpr]

  PushNode(nVarDecl);

  RecogniseIdentList(False);
  Recognise(ttColon);
  RecogniseType;

  lc := fcTokenList.FirstSolidToken;

  If lc.TokenType = ttAbsolute Then Begin
    PushNode(nAbsoluteVar);
    Recognise(ttAbsolute);

    If (fcTokenList.FirstSolidWordType In IdentifierTypes) Then
      RecogniseIdentifier(False)
    Else
      RecogniseConstantExpression;

    PopNode;
  End
  Else Begin
    RecogniseHintDirectives;

    If fcTokenList.FirstSolidTokenType = ttEquals Then Begin
      PushNode(nVariableInit);

      Recognise(ttEquals);

      { not just an expr - can be an array, record or the like
        reuse the code from typed constant declaration as it works the same
      }
      RecogniseTypedConstant;

      PopNode;
    End;
  End;

  { yes, they can occur here too }
  RecogniseHintDirectives;

  PopNode;
End;

Procedure TBuildParseTree.RecogniseExpr(Const pbAllowRelop: Boolean);
Begin
  { Expression -> SimpleExpression [RelOp SimpleExpression]...

    nb this doesn't parse
    lb := foo.Owner;
  }

  PushNode(nExpression);

  RecogniseSimpleExpression;

  If pbAllowRelop Then Begin
    While fcTokenList.FirstSolidTokenType In RelationalOperators Do Begin
      RecogniseRelop;
      RecogniseSimpleExpression;
    End;
  End;

  // added this to cope with real usage - see TestCastSimple
  If fcTokenList.FirstSolidTokenType = ttDot Then Begin
    Recognise(ttDot);
    RecogniseExpr(true);
  End;

  //likewise need to cope with pchar(foo)^
  If fcTokenList.FirstSolidTokenType = ttHat Then Begin
    Recognise(ttHat);
  End;

  PopNode;
End;

Procedure TBuildParseTree.RecogniseSimpleExpression;
{var
  lc: TSourceToken;}
Begin
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
  While fcTokenList.FirstSolidTokenType In AddOperators Do Begin
    RecogniseAddOp;
    RecogniseTerm;
  End;
End;

Procedure TBuildParseTree.RecogniseTerm;
Begin
  // Term -> Factor [MulOp Factor]...

  PushNode(nTerm);

  RecogniseFactor;

  While fcTokenList.FirstSolidTokenType In MulOperators Do Begin
    RecogniseMulOp;
    RecogniseFactor;
  End;

  PopNode;
End;

Procedure TBuildParseTree.RecogniseFactor;
Var
  lc: TSourceToken;
Begin
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

  If lc.TokenType = ttInherited Then Begin
    Recognise(ttInherited);

    RecogniseDesignator;
    If fcTokenList.FirstSolidTokenType = ttOpenBracket Then Begin
      RecogniseActualParams;
    End;
  End
  Else If (IdentifierNext) Then Begin
    RecogniseDesignator;
    If fcTokenList.FirstSolidTokenType = ttOpenBracket Then Begin
      RecogniseActualParams;
    End;
  End
  Else If (fcTokenList.FirstSolidTokenType = ttNumber) Then Begin
    Recognise(ttNumber);
  End
  Else If (fcTokenList.FirstSolidTokenType = ttLiteralString) Then Begin
    Recognise(ttLiteralString);
  End
  Else If (fcTokenList.FirstSolidTokenType In BuiltInConstants) Then Begin
    // nil, true, false
    Recognise(BuiltInConstants);
  End
  Else If (fcTokenList.FirstSolidTokenType = ttOpenBracket) Then Begin
    Recognise(ttOpenBracket);

    { can be empty brackets }
    If fcTokenList.FirstSolidTokenType <> ttCloseBracket Then
      RecogniseExpr(True);
    Recognise(ttCloseBracket);

    //!!! recognise expressions like (Foo.Stuff['x'].Pointer)^.MyIndex
  End
  Else If (fcTokenList.FirstSolidTokenType = ttNot) Then Begin
    Recognise(ttNot);
    RecogniseFactor;
  End
  Else If fcTokenList.FirstSolidTokenType In PossiblyUnarySymbolOperators Then Begin
    PushNode(nUnaryOp);
    Recognise(PossiblyUnarySymbolOperators);
    RecogniseFactor;

    PopNode;
  End
  Else If (fcTokenList.FirstSolidTokenType = ttOpenSquareBracket) Then Begin
    RecogniseSetConstructor;
  End
  Else
    Raise TEParseError.Create('unexpected token in factor', lc);

  If fcTokenList.FirstSolidTokenType In [ttHat, ttDot, ttOpenSquareBracket] Then
    RecogniseDesignatorTail;
End;

Procedure TBuildParseTree.RecogniseRelOp;
Var
  lc: TSourceToken;
Begin
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

  If lc.TokenType In RelationalOperators Then
    Recognise(RelationalOperators)
  Else
    Raise TEParseError.Create('unexpected token in rel op', lc);
End;

Procedure TBuildParseTree.RecogniseAddOp;
Var
  lc: TSourceToken;
Begin
  lc := fcTokenList.FirstSolidToken;

  If lc.TokenType In AddOperators Then
    Recognise(AddOperators)
  Else
    Raise TEParseError.Create('unexpected token in add op', lc);
End;

Procedure TBuildParseTree.RecogniseMulOp;
Var
  lc: TSourceToken;
Begin
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

  If lc.TokenType In MulOperators Then
    Recognise(MulOperators)
  Else
    Raise TEParseError.Create('unexpected token in mul op', lc);
End;

Procedure TBuildParseTree.RecogniseDesignator;
Begin
  { Designator -> QualId ['.' Ident | '[' ExprList ']' | '^']...

    Need brackets here too for hard typecasts like
      pointer(foo)
  }
  PushNode(nDesignator);

  If fcTokenList.FirstSolidTokenType = ttAtSign Then
    Recognise(ttAtSign);

  RecogniseQualId;

  RecogniseDesignatorTail;

  PopNode;
End;

Procedure TBuildParseTree.RecogniseDesignatorTail;
Const
  DESIGNATOR_TAIL_TOKENS = [ttDot, ttOpenBracket, ttOpenSquareBracket, ttHat,
    ttPlus, ttMinus];
Begin

  While (fcTokenList.FirstSolidTokenType In DESIGNATOR_TAIL_TOKENS) Do Begin
    Case fcTokenList.FirstSolidTokenType Of
      ttDot: Begin
          Recognise(ttDot);
          RecogniseIdentifier(False);
        End;
      ttHat: Begin
          Recognise(ttHat);
          // and after the deref operator ?
        End;
      ttOpenSquareBracket: Begin
          Recognise(ttOpenSquareBracket);
          RecogniseExprList;
          Recognise(ttCloseSquareBracket);
        End;
      ttOpenBracket: Begin
          RecogniseActualParams;
        End;
      ttPlus, ttMinus: Begin
          Recognise([ttPlus, ttMinus]);
          RecogniseExpr(True);
        End;
    Else
      Assert(False, 'Should not be here - bad token type');
    End;
  End;
End;

Procedure TBuildParseTree.RecogniseSetConstructor;
Begin
  // SetConstructor -> '[' [SetElement/','...] ']'

  Recognise(ttOpenSquareBracket);

  While fcTokenList.FirstSolidTokenType <> ttCloseSquareBracket Do Begin
    RecogniseSetElement;
    If fcTokenList.FirstSolidTokenType = ttComma Then
      Recognise(ttComma)
    Else
      break; // no comma -> no more items
  End;

  Recognise(ttCloseSquareBracket);
End;

Procedure TBuildParseTree.RecogniseSetElement;
Begin
  // SetElement -> Expression ['..' Expression]

  RecogniseExpr(True);
  If fcTokenList.FirstSolidTokenType = ttDoubleDot Then Begin
    Recognise(ttDoubleDot);
    RecogniseExpr(False);
  End;
End;

Procedure TBuildParseTree.RecogniseExprList;
Begin
  // ExprList -> Expression/','...

  RecogniseExpr(True);
  While fcTokenList.FirstSolidTokenType = ttComma Do Begin
    Recognise(ttComma);
    RecogniseExpr(True);
  End;
End;

Procedure TBuildParseTree.RecogniseStatement;
Const
  BLOCK_END: TTokenTypeSet = [ttEnd, ttFinally, ttExcept];
Var
  lc: TSourceToken;
Begin
  // Statement -> [LabelId ':'] [SimpleStatement | StructStmt]

  PushNode(nStatement);

  // empty statement
  If fcTokenList.FirstSolidTokenType = ttSemicolon Then Begin
    PopNode;
    Exit;
  End;

  If fcTokenList.FirstSolidTokenType = ttEnd Then Begin
    PopNode;
    Exit;
  End;

  CheckLabelPrefix;

  lc := fcTokenList.FirstSolidToken;

  { anything more? can just be a label at the end of the proc/block }
  If Not (lc.TokenType In BLOCK_END) Then Begin

    If lc.TokenType In StructStatementWords Then
      RecogniseStructStmnt
    Else
      RecogniseSimpleStmnt;
  End;

  PopNode;
End;

Procedure TBuildParseTree.CheckLabelPrefix;
Var
  lc2: TSourceToken;
  lbColonSecond: boolean;
Begin
  lc2 := fcTokenList.SolidToken(2);
  lbColonSecond := (lc2.TokenType = ttColon);
  If (lbColonSecond) Then Begin
    PushNode(nStatementLabel);
    RecogniseLabel;
    Recognise(ttColon);
    PopNode;

    { can be followed by another label  }
    CheckLabelPrefix
  End

End;

Procedure TBuildParseTree.RecogniseStatementList(Const peEndTokens: TTokenTypeSet);
Begin
  // StmtList -> Statement/';'...
  PushNode(nStatementList);

  While Not (fcTokenList.FirstSolidTokenType In peEndTokens) Do Begin
    RecogniseStatement;

    // last semicolon is optional
    If fcTokenList.FirstSolidTokenType = ttSemicolon Then
      Recognise(ttSemicolon)
    Else
      break;
  End;

  PopNode;
End;

Procedure TBuildParseTree.RecogniseSimpleStmnt;
Var
  lc: TSourceToken;
Begin
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

  If (IdentifierNext) Or (lc.TokenType In [ttOpenBracket, ttAtSign]) Then Begin
    // should be fullblown expression?
    RecogniseDesignator;

    If fcTokenList.FirstSolidTokenType = ttOpenBracket Then Begin
      RecogniseActualParams;
    End;

    // can be a hat after the close backets to deref the return value
    If fcTokenList.FirstSolidTokenType = ttHat Then
      Recognise(ttHat);

    // dot next ?
    If fcTokenList.FirstSolidTokenType = ttDot Then Begin
      Recognise(ttDot);
      RecogniseSimpleStmnt;
    End

    Else If fcTokenList.FirstSolidTokenType = ttAssign Then Begin
      PushNode(nAssignment);

      Recognise(ttAssign);
      RecogniseExpr(True);

      PopNode;
    End;

    // else nothing at all is also ok. i.e. procedure call with no params
  End
  Else If lc.TokenType = ttInherited Then Begin
    { can be one of
      "inherited;
      inherited Foo;
      inherited Foo(bar);
      inherited FooProp := bar;
      inherited FooProp[Bar] := Fish;
      bar :=  inherited FooProp[Bar];
      }

    Recognise(ttInherited);
    If IdentifierNext Then
      RecogniseSimpleStmnt;
  End
  Else If lc.TokenType = ttGoto Then Begin
    Recognise(ttGoto);
    RecogniseLabel;
  End
  Else If lc.TokenType = ttRaise Then Begin
    RecogniseRaise;
  End
  Else If lc.TokenType = ttSemicolon Then Begin
    // empty statement
    // this gets doen later in common code Recognise(ttSemicolon);
  End
  Else
    Raise TEParseError.Create('expected simple statement', lc);

End;

Procedure TBuildParseTree.RecogniseRaise;
Begin
  // another omission - raise expr  or just raise (in except block)
  Recognise(ttRaise);
  If Not (fcTokenList.FirstSolidTokenType In [ttSemicolon, ttEnd, ttElse]) Then
    RecogniseExpr(True);

  // can be at addr
  If fcTokenList.FirstSolidTokenType = ttAt Then Begin
    Recognise(ttAt);
    RecogniseExpr(True);
  End;
End;

Procedure TBuildParseTree.RecogniseStructStmnt;
Var
  lc: TSourceToken;
Begin
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

  Case lc.TokenType Of
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
  Else
    Raise TEParseError.Create('expected structured statement', lc);
  End;

End;

Procedure TBuildParseTree.RecogniseCompoundStmnt;
Begin
  { CompoundStmt -> BEGIN StmtList END }
  PushNode(nCompoundStatement);
  Recognise(ttBegin);
  RecogniseStatementList([ttEnd]);
  Recognise(ttEnd);
  PopNode;
End;

Procedure TBuildParseTree.RecogniseIfStmnt;
Begin
  // IfStmt -> IF Expression THEN Statement [ELSE Statement]

  Recognise(ttIf);

  PushNode(nIfCondition);
  RecogniseExpr(True);
  PopNode;

  Recognise(ttThen);

  PushNode(nIfBlock);

  { if body can be completely missing - go straight to else }
  If fcTokenList.FirstSolidTokenType <> ttElse Then
    RecogniseStatement;
  PopNode;

  If fcTokenList.FirstSolidTokenType = ttElse Then Begin
    Recognise(ttElse);
    PushNode(nElseBlock);
    If Not (fcTokenList.FirstSolidTokenType In [ttElse, ttEnd]) Then
      RecogniseStatement;
    PopNode;
  End;
End;

Procedure TBuildParseTree.RecogniseCaseStmnt;
Begin
  // CaseStmt -> CASE Expression OF CaseSelector/';'... [ELSE Statement] [';'] END
  PushNode(nCaseStatement);

  Recognise(ttCase);

  PushNode(nBlockHeaderExpr);
  RecogniseExpr(True);
  PopNode;

  Recognise(ttOf);

  While Not (fcTokenList.FirstSolidTokenType In [ttElse, ttEnd]) Do
    RecogniseCaseSelector;

  If fcTokenList.FirstSolidTokenType = ttElse Then Begin
    PushNode(nElseCase);
    Recognise(ttElse);
    RecogniseStatementList([ttEnd]);
    PopNode;
  End;

  If fcTokenList.FirstSolidTokenType = ttSemicolon Then
    Recognise(ttSemicolon);

  Recognise(ttEnd);

  PopNode;
End;

Procedure TBuildParseTree.RecogniseCaseSelector;
Begin
  // CaseSelector -> CaseLabel/','... ':' Statement ';'

  PushNode(nCaseSelector);

  PushNode(nCaseLabels);
  RecogniseCaseLabel;

  While (fcTokenList.FirstSolidTokenType = ttComma) Do Begin
    Recognise(ttComma);
    RecogniseCaseLabel;
  End;

  Recognise(ttColon);
  PopNode;

  { semicolon is optional in the last case before the else }
  If Not (fcTokenList.FirstSolidTokenType In [ttElse, ttEnd]) Then Begin
    RecogniseStatement;

    If fcTokenList.FirstSolidTokenType = ttSemicolon Then
      Recognise(ttSemicolon);
  End;

  PopNode;
End;

Procedure TBuildParseTree.RecogniseCaseLabel;
Begin
  // CaseLabel -> ConstExpr ['..' ConstExpr]

  PushNode(nCaseLabel);

  RecogniseConstantExpression;
  If (fcTokenList.FirstSolidTokenType = ttDoubleDot) Then Begin
    Recognise(ttDoubleDot);
    RecogniseConstantExpression;
  End;

  PopNode;
End;

Procedure TBuildParseTree.RecogniseRepeatStmnt;
Begin
  { RepeatStmt -> REPEAT Statement UNTIL Expression

   Incorect - it is a statement list
  }
  PushNode(nRepeatStatement);

  Recognise(ttRepeat);
  RecogniseStatementList([ttUntil]);
  Recognise(ttUntil);

  PushNode(nLoopHeaderExpr);
  RecogniseExpr(True);
  PopNode;

  PopNode;
End;

Procedure TBuildParseTree.RecogniseWhileStmnt;
Begin
  // WhileStmt -> WHILE Expression DO Statement
  PushNode(nWhileStatement);

  Recognise(ttWhile);

  PushNode(nLoopHeaderExpr);
  RecogniseExpr(True);
  PopNode;

  Recognise(ttDo);
  RecogniseStatement;

  PopNode;
End;

Procedure TBuildParseTree.RecogniseForStmnt;
Begin
  // ForStmt -> FOR QualId ':=' Expression (TO | DOWNTO) Expression DO Statement
  PushNode(nForStatement);

  Recognise(ttFor);
  RecogniseQualId;
  Recognise(ttAssign);

  PushNode(nLoopHeaderExpr);
  RecogniseExpr(True);
  PopNode;

  Recognise([ttTo, ttDownto]);

  PushNode(nLoopHeaderExpr);
  RecogniseExpr(True);
  PopNode;

  Recognise([ttDo]);
  RecogniseStatement;

  PopNode;
End;

Procedure TBuildParseTree.RecogniseWithStmnt;
Begin
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
End;

Procedure TBuildParseTree.RecogniseTryStatement;
Var
  lc: TSourceToken;
Begin
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
  Case lc.TokenType Of
    ttFinally: Begin
        PushNode(nFinallyBlock);

        Recognise(ttFinally);
        RecogniseStatementList([ttEnd]);
        Recognise(ttEnd);

        PopNode;
      End;
    ttExcept: Begin
        PushNode(nExceptBlock);

        Recognise(ttExcept);
        RecogniseExceptionHandlerBlock;
        Recognise(ttEnd);

        PopNode;
      End
  Else
    Raise TEParseError.Create('expected except or finally', lc);

  End;

  PopNode;
End;

Procedure TBuildParseTree.RecogniseExceptionHandlerBlock;
Begin
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

  If fcTokenList.FirstSolidTokenType In [ttOn, ttElse] Then Begin
    While fcTokenList.FirstSolidTokenType In [ttOn, ttElse] Do
      RecogniseExceptionHandler;
  End
  Else Begin
    // can be 0 or more statements
    RecogniseStatementList([ttEnd]);
  End;

  PopNode;
End;

Procedure TBuildParseTree.RecogniseExceptionHandler;
Begin
  {
    ExceptionSpecifier
        -> 'on' [ident ':'] ExceptType 'do' Statement
        -> 'else' Statement
  }
  PushNode(nOnExceptionHandler);

  If fcTokenList.FirstSolidTokenType = ttElse Then Begin
    Recognise(ttElse);
    RecogniseStatement;
  End
  Else If fcTokenList.FirstSolidTokenType = ttOn Then Begin
    Recognise(ttOn);
    If fcTokenList.SolidTokenType(2) = ttColon Then Begin
      RecogniseIdentifier(False);
      Recognise(ttColon);
    End;

    RecogniseIdentifier(True);
    Recognise(ttDo);

    { special case - empty statement block, go straight on to the else }
    If fcTokenList.FirstSolidTokenType <> ttElse Then
      RecogniseStatement;
  End
  Else
    RecogniseStatement;

  If fcTokenList.FirstSolidTokenType = ttSemicolon Then
    Recognise(ttSemicolon);

  PopNode;
End;

Procedure TBuildParseTree.RecogniseProcedureDeclSection;
Var
  lc: TSourceToken;
Begin
  {
  ProcedureDeclSection
    -> ProcedureDecl
    -> FunctionDecl
  }

  lc := fcTokenList.FirstSolidToken;

  Case lc.TokenType Of
    ttProcedure:
      RecogniseProcedureDecl;
    ttFunction:
      RecogniseFunctionDecl;
    ttConstructor:
      RecogniseConstructorDecl;
    ttDestructor:
      RecogniseDestructorDecl;

    ttClass: Begin
        // class proc or function
        Case fcTokenList.SolidTokenType(2) Of
          ttProcedure:
            RecogniseProcedureDecl;
          ttFunction:
            RecogniseFunctionDecl;
        Else
          Raise TEParseError.Create('expected class procedure or class function', lc);
        End;
      End;
  Else
    Raise TEParseError.Create('expected procedure or function', lc);
  End;

End;

{ the proc/function is forward or extern (ie has no body)
  if the word 'forward' or 'extern' is in the directives
  these are also valid param names }

Function IsForwardExtern(pt: TParseTreeNode): Boolean;
Var
  lcDirectives: TParseTreeNode;
Begin
  Assert(pt <> Nil);

  If pt.NodeType In ProcedureNodes Then
    pt := pt.GetImmediateChild(ProcedureHeadings);

  Assert(pt <> Nil);

  lcDirectives := pt.GetImmediateChild(nProcedureDirectives);

  Result := (lcDirectives <> Nil) And lcDirectives.HasChildNode([ttExternal, ttForward])
End;

Procedure TBuildParseTree.RecogniseProcedureDecl;
Var
  lcTop: TParseTreeNode;
Begin
  { ProcedureDecl -> ProcedureHeading ';' [Directive] Block ';'

    NB: the block is omitted if there is a 'forward' or external' directive

  }
  PushNode(nProcedureDecl);

  RecogniseProcedureHeading(False, False);

  { the ';' is ommited by lazy programmers in some rare occasions}
  If fcTokenList.FirstSolidTokenType = ttSemicolon Then
    Recognise(ttSemicolon);

  { if the proc declaration has the directive external or forward,
    it will not have a body
    note that though 'forward' is a spectacularly unfortunate variable name,
    it has happened, e.g. in ActnMenus.pas }
  lcTop := TParseTreeNode(fcStack.Peek);
  If Not IsForwardExtern(lcTop) Then Begin
    RecogniseBlock;
    Recognise(ttSemicolon);
  End;

  PopNode;
End;

Procedure TBuildParseTree.RecogniseFunctionDecl;
Var
  lcTop: TParseTreeNode;
Begin
  // ProcedureDecl -> FunctionHeading ';' [Directive] Block ';'

  PushNode(nFunctionDecl);

  RecogniseFunctionHeading(False, False);
  { the ';' is ommited by lazy programmers in some rare occasions}
  If fcTokenList.FirstSolidTokenType = ttSemicolon Then
    Recognise(ttSemicolon);

  //opt
  If fcTokenList.FirstSolidTokenType In ProcedureDirectives Then
    RecogniseProcedureDirectives;

  { if the proc declaration has the directive external or forward,
    it will not have a body }
  lcTop := TParseTreeNode(fcStack.Peek);
  If Not IsForwardExtern(lcTop) Then Begin
    RecogniseBlock;
    Recognise(ttSemicolon);
  End;

  PopNode;
End;

Procedure TBuildParseTree.RecogniseConstructorDecl;
Begin
  // ProcedureDecl -> ProcedureHeading ';' [Directive] Block ';'

  PushNode(nConstructorDecl);

  RecogniseConstructorHeading(False);
  Recognise(ttSemicolon);

  If fcTokenList.FirstSolidTokenType In ProcedureDirectives Then
    RecogniseProcedureDirectives;
  RecogniseBlock;
  Recognise(ttSemicolon);

  PopNode;
End;

Procedure TBuildParseTree.RecogniseDestructorDecl;
Begin
  // ProcedureDecl -> ProcedureHeading ';' [Directive] Block ';'

  PushNode(nDestructorDecl);

  RecogniseDestructorHeading(false);
  Recognise(ttSemicolon);

  If fcTokenList.FirstSolidTokenType In ProcedureDirectives Then
    RecogniseProcedureDirectives;
  RecogniseBlock;
  Recognise(ttSemicolon);

  PopNode;
End;

Procedure TBuildParseTree.RecogniseFunctionHeading(Const pbAnon, pbCanInterfaceMap: Boolean);
Begin
  // FunctionHeading -> FUNCTION Ident [FormalParameters] ':' (SimpleType | STRING)
  PushNode(nFunctionHeading);

  // class procs
  If fcTokenList.FirstSolidTokenType = ttClass Then
    Recognise(ttClass);

  Recognise(ttFunction);
  If Not pbAnon Then
    RecogniseMethodName(False);

  If fcTokenList.FirstSolidTokenType = ttOpenBracket Then
    RecogniseFormalParameters;

  { the colon and type is in fact optional in
    - external fns
    - when making good on a forward }
  If fcTokenList.FirstSolidTokenType = ttColon Then Begin
    Recognise(ttColon);
    PushNode(nFunctionReturnType);
    RecogniseType;
    PopNode;
  End;

  RecogniseProcedureDirectives;

  If pbCanInterfaceMap And (fcTokenList.FirstSolidTokenType = ttEquals) Then Begin
    Recognise(ttEquals);
    RecogniseIdentifier(False);
  End;

  PopNode;
End;

Procedure TBuildParseTree.RecogniseProcedureHeading(Const pbAnon, pbCanInterfaceMap: Boolean);
Begin
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

  If fcTokenList.FirstSolidTokenType = ttClass Then
    Recognise(ttClass);

  Recognise(ttProcedure);
  If Not pbAnon Then
    RecogniseMethodName(False);

  If fcTokenList.FirstSolidTokenType = ttOpenBracket Then
    RecogniseFormalParameters;

  RecogniseProcedureDirectives;

  If pbCanInterfaceMap And (fcTokenList.FirstSolidTokenType = ttEquals) Then Begin
    Recognise(ttEquals);
    RecogniseIdentifier(False);
  End;

  PopNode;
End;

Procedure TBuildParseTree.RecogniseFormalParameters;
Begin
  // FormalParameters -> '(' FormalParm/';'... ')'

  PushNode(nFormalParams);

  Recognise(ttOpenBracket);

  { funciton Foo(); is accepted so must allow empty brackets }

  If fcTokenList.FirstSolidTokenType <> ttCloseBracket Then Begin
    RecogniseFormalParam;
    While fcTokenList.FirstSolidTokenType = ttSemicolon Do Begin
      Recognise(ttSemicolon);
      RecogniseFormalParam;
    End;
  End;

  Recognise(ttCloseBracket);

  PopNode;
End;

Procedure TBuildParseTree.RecogniseFormalParam;
Const
  PARAM_PREFIXES: TTokenTypeSet = [ttVar, ttConst];
Begin
  PushNode(nFormalParam);

  { FormalParm -> [VAR | CONST | OUT] Parameter

    'out' is different as it is also a param name so this is legal
    procedure Foo(out out: integer);

    'out' with a comma, colon or ')' directly after is not a prefix, it is a param name
    if another name follows it is a prefix
  }

  If fcTokenList.FirstSolidTokenType In PARAM_PREFIXES Then
    Recognise(PARAM_PREFIXES)
  Else If fcTokenList.FirstSolidTokenType = ttOut Then Begin
    If IsIdentifierToken(fcTokenList.SolidToken(2)) Then
      Recognise(ttOut);
  End;

  RecogniseParameter;

  PopNode;
End;

Procedure TBuildParseTree.RecogniseParameter;
Var
  lbArray: boolean;
Begin
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
  If fcTokenList.FirstSolidTokenType = ttColon Then Begin
    Recognise(ttColon);

    If fcTokenList.FirstSolidTokenType = ttArray Then Begin
      Recognise(ttArray);
      Recognise(ttOf);
      lbArray := True;
    End;

    // type is optional in params ie procedure foo(var pp);
    If (lbArray) Or (Not (fcTokenList.FirstSolidTokenType In [ttSemicolon, ttCloseBracket])) Then
      RecogniseType;

    If fcTokenList.FirstSolidTokenType = ttEquals Then Begin
      Recognise(ttEquals);
      RecogniseConstantExpression;
    End;
  End;
End;

Procedure TBuildParseTree.RecogniseProcedureDirectives;
Var
  lbFirstPass: boolean;
Begin
  { these are semi-colon seperated

    want to leave 'Function foo;' as is,
    but strip off the '; safecall' off 'Function bar; safecall;'

    external is more complex
  }

  If (fcTokenList.FirstSolidTokenType In ProcedureDirectives) Or
    ((fcTokenList.FirstSolidTokenType = ttSemicolon) And (fcTokenList.SolidTokenType(2) In ProcedureDirectives)) Then Begin
    PushNode(nProcedureDirectives);

    If fcTokenList.FirstSolidTokenType = ttSemiColon Then
      Recognise(ttSemiColon);
    lbFirstPass := True;

    While (fcTokenList.FirstSolidTokenType In ProcedureDirectives) Or
      ((fcTokenList.FirstSolidTokenType = ttSemicolon) And (fcTokenList.SolidTokenType(2) In ProcedureDirectives)) Do Begin
      If (Not lbFirstPass) And (fcTokenList.FirstSolidTokenType = ttSemiColon) Then
        Recognise(ttSemiColon);

      Case fcTokenList.FirstSolidTokenType Of
        ttExternal: Begin
            RecogniseExternalProcDirective;
          End;
        ttDispId: Begin
            Recognise(ttDispId);
            RecogniseConstantExpression;
          End;
        ttMessage: Begin
            Recognise(ttMessage);
            RecogniseConstantExpression;
          End;
      Else
        Recognise(ProcedureDirectives);
      End;

      lbFirstPass := False;
    End;

    PopNode;
  End;
End;

Procedure TBuildParseTree.RecogniseExternalProcDirective;
Begin
  { right, i'll fake this one

    ExternalProcDirective ->
      External ["'" libname "'" ["name" "'" procname "'"]]
  }
  PushNode(nExternalDirective);

  Recognise(ttExternal);
  If fcTokenList.FirstSolidTokenType In (IdentiferTokens + [ttLiteralString]) Then Begin
    Recognise((IdentiferTokens + [ttLiteralString]));

    If fcTokenList.FirstSolidTokenType = ttName Then Begin
      Recognise(ttName);
      RecogniseConstantExpression;
    End;
  End;

  PopNode;
End;

Procedure TBuildParseTree.RecogniseObjectType;
Begin
  { ObjectType -> OBJECT [ObjHeritage] [ObjFieldList] [MethodList] END

      arg this is badly broken, need to
  }

  PushNode(nObjectType);

  Recognise(ttObject);

  If fcTokenList.FirstSolidTokenType = ttOpenBracket Then
    RecogniseObjHeritage;

  // swiped this from the delphi object defs
  RecogniseClassBody;

  Recognise(ttEnd);

  PopNode;
End;

Procedure TBuildParseTree.RecogniseObjHeritage;
Begin
  // ObjHeritage -> '(' QualId ')'

  Recognise(ttOpenBracket);
  RecogniseQualId;
  Recognise(ttCloseBracket);
End;

Procedure TBuildParseTree.RecogniseConstructorHeading(Const pbDeclaration: boolean);
Begin
  //ConstructorHeading -> CONSTRUCTOR Ident [FormalParameters]
  PushNode(nConstructorHeading);

  Recognise(ttConstructor);
  RecogniseMethodName(Not pbDeclaration);
  If fcTokenList.FirstSolidTokenType = ttOpenBracket Then
    RecogniseFormalParameters;

  RecogniseProcedureDirectives;

  PopNode;
End;

Procedure TBuildParseTree.RecogniseDestructorHeading(Const pbDeclaration: boolean);
Begin
  //DestructorHeading -> DESTRUCTOR Ident [FormalParameters]
  PushNode(nDestructorHeading);

  Recognise(ttDestructor);
  RecogniseMethodName(Not pbDeclaration);
  If fcTokenList.FirstSolidTokenType = ttOpenBracket Then
    RecogniseFormalParameters;

  RecogniseProcedureDirectives;

  PopNode;
End;

Procedure TBuildParseTree.RecogniseInitSection;
Var
  lc: TSourceToken;
Begin
  {
    InitSection
      -> INITIALIZATION StmtList [FINALIZATION StmtList] END
      -> BEGIN StmtList END
      -> END
  }

  lc := fcTokenList.FirstSolidToken;

  PushNode(nInitSection);

  Case lc.TokenType Of
    ttInitialization: Begin
        Recognise(ttInitialization);
        RecogniseStatementList([ttEnd, ttFinalization]);

        If fcTokenList.FirstSolidTokenType = ttFinalization Then Begin
          Recognise(ttFinalization);
          RecogniseStatementList([ttEnd]);
        End;

        Recognise(ttEnd);
      End;
    ttBegin: Begin
        Recognise(ttBegin);
        RecogniseStatementList([ttEnd]);
        Recognise(ttEnd);
      End;
    ttEnd: Begin
        Recognise(ttEnd);
      End
  Else
    Raise TEParseError.Create('expected initialisation, begin or end', lc);
  End;

  PopNode;
End;

Procedure TBuildParseTree.RecogniseClassType;
Begin
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

  If fcTokenList.FirstSolidTokenType = ttSemicolon Then Begin
    PopNode;
    exit;
  End;

  If fcTokenList.FirstSolidTokenType = ttOf Then Begin
    Recognise(ttOf);
    RecogniseIdentifier(False);
    PopNode;
    exit;
  End;

  If fcTokenList.FirstSolidTokenType = ttOpenBracket Then
    RecogniseClassHeritage;

  // can end here
  If fcTokenList.FirstSolidTokenType = ttSemicolon Then Begin
    PopNode;
    exit;
  End;

  RecogniseClassBody;
  Recognise(ttEnd);

  RecogniseHintDirectives;

  PopNode;
End;

Procedure TBuildParseTree.RecogniseClassHeritage;
Begin
  PushNode(nClassHeritage);

  // ClassHeritage -> '(' IdentList ')'
  Recognise(ttOpenBracket);
  RecogniseIdentList(True);
  Recognise(ttCloseBracket);

  PopNode;
End;

Procedure TBuildParseTree.RecogniseClassVisibility;
Begin
  // ClassVisibility -> [PUBLIC | PROTECTED | PRIVATE | PUBLISHED]

  Recognise(ClassVisibility);
End;

Procedure TBuildParseTree.RecogniseClassBody;
Begin
  //ClassBody -> classdeclarations (access classdeclarations) ...
  PushNode(nClassBody);

  RecogniseClassDeclarations(False);

  While (fcTokenList.FirstSolidTokenType In ClassVisibility) Do Begin
    PushNode(nClassVisibility);
    RecogniseClassVisibility;
    RecogniseClassDeclarations(False);
    PopNode;
  End;

  PopNode;
End;

Procedure TBuildParseTree.RecogniseClassDeclarations(Const pbInterface: Boolean);
Const
  // can declare thse things in a class
  CLASS_DECL_WORDS = [ttProcedure, ttFunction,
    ttConstructor, ttDestructor, ttProperty, ttClass];
Var
  lc: TSourceToken;
  lbStarted: Boolean;
Begin
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

  While (fcTokenList.FirstSolidTokenType In CLASS_DECL_WORDS) Or
    (fcTokenList.FirstSolidWordType In IdentifierTypes) Do Begin
    // only make this node if it will have children
    If Not lbStarted Then
      PushNode(nClassDeclarations);
    lbStarted := True;

    lc := fcTokenList.FirstSolidToken;

    // these end the visibility section
    If fcTokenList.FirstSolidTokenType In (ClassVisibility + [ttEnd]) Then
      break;

    Case lc.TokenType Of
      ttProcedure:
        RecogniseProcedureHeading(False, True);
      ttFunction:
        RecogniseFunctionHeading(False, True);
      ttClass: Begin
          // must be followed by 'procedure' or 'function'
          Case fcTokenList.SolidTokenType(2) Of
            ttProcedure:
              RecogniseProcedureHeading(False, True);
            ttFunction:
              RecogniseFunctionHeading(False, True);
          Else
            Raise TEParseError.Create('Expected class procedure or class function', lc);
          End;

        End;
      ttConstructor: Begin
          // no constructor on interface
          If pbInterface Then
            Raise TEParseError.Create('unexpected token', lc);
          RecogniseConstructorHeading(True);
        End;
      ttDestructor: Begin
          // no constructor on interface
          If pbInterface Then
            Raise TEParseError.Create('unexpected token', lc);
          RecogniseDestructorHeading(True);
        End;
      ttProperty:
        RecogniseProperty;
    Else Begin
        // end of this list with next visibility section or class end?
        If lc.TokenType In CLASS_DECL_WORDS + [ttEnd] Then Begin
          break;
        End
          // vars start with an identifier
        Else If lc.TokenType In IdentiferTokens Then Begin
          // no vars on interface
          If pbInterface Then
            Raise TEParseError.Create('unexpected token', lc);

          RecogniseVarDecl;
        End
        Else
          Raise TEParseError.Create('unexpected token', lc);
      End;
    End;

    // semicolon after each def.
    If fcTokenList.FirstSolidTokenType = ttSemicolon Then
      Recognise(ttSemicolon)
    Else
      Break; // except the last

  End;

  If lbStarted Then
    PopNode;
End;

Procedure TBuildParseTree.RecogniseProperty;
Begin
  {PropertyList -> PROPERTY  Ident [PropertyInterface]  PropertySpecifiers

  There is also the syntax of reclaring properties to raise visibility
    -> Property Ident;
  }
  PushNode(nProperty);

  Recognise(ttProperty);

  RecogniseIdentifier(False);

  { this is omitted if it is a property redeclaration for visibility raising
    in that case it may still have directives and hints }
  If fcTokenList.FirstSolidTokenType In [ttColon, ttOpenSquareBracket] Then Begin
    RecognisePropertyInterface;
  End;

  RecognisePropertySpecifiers;

  RecognisePropertyDirectives;
  RecogniseHintDirectives;

  PopNode;
End;

Procedure TBuildParseTree.RecognisePropertyInterface;
Begin
  // PropertyInterface -> [PropertyParameterList] ':' Ident

  If fcTokenList.FirstSolidTokenType <> ttColon Then
    RecognisePropertyParameterList;

  Recognise(ttColon);

  // recongising any type is overkill but hey
  RecogniseType;
End;

Procedure TBuildParseTree.RecognisePropertyParameterList;
Begin
  { PropertyParameterList -> '[' (IdentList ':' TypeId)/';'... ']'

   this forgets const and var, e.g.

   property ComplexArrayProp[const piIndex: integer; var pcsString: string]: boolean read GetComplexArrayProp ;

  }
  PushNode(nPropertyParameterList);

  Recognise(ttOpenSquareBracket);
  Repeat
    If (fcTokenList.FirstSolidTokenType In [ttConst, ttVar, ttOut]) Then
      Recognise([ttConst, ttVar, ttOut]);

    RecogniseIdentList(False);
    Recognise(ttColon);
    RecogniseTypeId;

    If fcTokenList.FirstSolidTokenType = ttSemicolon Then
      Recognise(ttSemicolon)
    Else
      break;

  Until fcTokenList.FirstSolidTokenType = ttCloseSquareBracket;

  Recognise(ttCloseSquareBracket);

  PopNode;
End;

Procedure TBuildParseTree.RecognisePropertySpecifiers;
Var
  lc: TSourceToken;
Const
  PROPERTY_SPECIFIERS: TTokenTypeSet = [ttIndex, ttRead, ttWrite, ttStored,
  ttDefault, ttNoDefault, ttImplements, ttDispId, ttReadOnly, ttWriteOnly];
Begin
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

  While lc.TokenType In PROPERTY_SPECIFIERS Do Begin
    PushNode(nPropertySpecifier);

    Case lc.TokenType Of
      ttIndex: Begin
          Recognise(ttIndex);
          RecogniseConstantExpression;
        End;
      ttRead: Begin
          Recognise(ttRead);
          RecognisePropertyAccess;
        End;
      ttWrite: Begin
          Recognise(ttWrite);
          RecognisePropertyAccess;
        End;
      ttStored: Begin
          Recognise(ttStored);
          RecogniseConstantExpression;
        End;
      ttDefault: Begin
          Recognise(ttDefault);
          RecogniseConstantExpression;
        End;
      ttNoDefault: Begin
          Recognise(ttNoDefault);
        End;
      ttImplements: Begin
          Recognise(ttImplements);
          RecogniseTypeId;

          { can be a lost of them, e.g. "implements foo, bar" }
          While fcTokenList.FirstSolidTokenType = ttComma Do Begin
            Recognise(ttComma);
            RecogniseTypeId;
          End;
        End;
      ttDispId: Begin
          Recognise(ttDispId);
          RecogniseConstantExpression;
        End;
      ttReadOnly: Begin
          Recognise(ttReadOnly);
        End;
      ttWriteOnly: Begin
          Recognise(ttWriteOnly);
        End;
    Else
      Raise TEParseError.Create('expected proeprty specifier', fcTokenList.FirstSolidToken);
    End;

    PopNode;
    lc := fcTokenList.FirstSolidToken;

  End;
End;

Procedure TBuildParseTree.RecognisePropertyAccess;
Begin
  { property access is the bit after the "read" or "write" in a property declaration
    This is usually just a procedure, function or simple var
    but sometimes it is a record or array field, .. or both e.g. "FDummy[0].ERX" }

  RecogniseIdentifier(False);

  { array access }
  If fcTokenList.FirstSolidTokenType = ttOpenSquareBracket Then Begin
    Recognise(ttOpenSquareBracket);
    // this is evaluated at compile-time, so we expect a constant subscript, e.g. "FDummy[0]"
    RecogniseConstantExpression;
    Recognise(ttCloseSquareBracket);
  End;

  { record field }
  If fcTokenList.FirstSolidTokenType = ttDot Then Begin
    Recognise(ttDot);
    // after the dot can be more structure, so recurse
    RecognisePropertyAccess;
  End

End;

Procedure TBuildParseTree.RecogniseInterfaceType;
Begin
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

  If fcTokenList.FirstSolidTokenType = ttSemicolon Then Begin
    PopNode;
    exit;
  End;

  If fcTokenList.FirstSolidTokenType = ttOpenBracket Then
    RecogniseInterfaceHeritage;

  If fcTokenList.FirstSolidTokenType = ttOpenSquareBracket Then
    RecogniseInterfaceGuid;

  If fcTokenList.FirstSolidTokenType <> ttEnd Then Begin
    PushNode(nInterfaceBody);
    RecogniseClassDeclarations(True);
    PopNode;
  End;

  Recognise(ttEnd);

  PopNode;
End;

Procedure TBuildParseTree.RecogniseInterfaceGuid;
Begin
  // interface guid can be a litteral string, or occasionally a string constant
  PushNode(nInterfaceTypeGuid);

  Recognise(ttOpenSquareBracket);
  If fcTokenList.FirstSolidTokenType = ttLiteralString Then
    Recognise(ttLiteralString)
  Else
    RecogniseIdentifier(False);

  Recognise(ttCloseSquareBracket);

  PopNode;
End;

Procedure TBuildParseTree.RecogniseInterfaceHeritage;
Begin
  // InterfaceHeritage -> '(' IdentList ')'
  PushNode(nInterfaceHeritage);

  Recognise(ttOpenBracket);
  RecogniseIdentList(True);
  Recognise(ttCloseBracket);

  PopNode;
End;

Procedure TBuildParseTree.RecogniseRequiresClause;
Begin
  // RequiresClause -> REQUIRES IdentList... ';'

  PushNode(nRequires);

  Recognise(ttRequires);
  RecogniseIdentList(False);
  Recognise(ttSemicolon);

  PopNode;
End;

Procedure TBuildParseTree.RecogniseContainsClause;
Begin
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
  While fcTokenList.FirstSolidTokenType = ttComma Do Begin
    Recognise(ttComma);
    RecogniseUsesItem(True);
  End;
  PopNode;

  Recognise(ttSemicolon);

  PopNode;
End;

{ worker for RecogniseIdentList }

Procedure TBuildParseTree.RecogniseIdentValue;
Begin
  If fcTokenList.FirstSolidTokenType = ttEquals Then Begin
    Recognise(ttEquals);
    RecogniseExpr(True);
  End;
End;

Procedure TBuildParseTree.RecogniseIdentList(Const pbCanHaveUnitQualifier: Boolean);
Begin
  { IdentList -> Ident/','...

    now in D6 enum types can have numeric values
     e.g. (foo, bar = 3, baz)
  }
  PushNode(nIdentList);

  RecogniseIdentifier(pbCanHaveUnitQualifier);
  RecogniseIdentValue;

  While fcTokenList.FirstSolidTokenType = ttComma Do Begin
    Recognise(ttComma);
    RecogniseIdentifier(pbCanHaveUnitQualifier);
    RecogniseIdentValue;
  End;

  PopNode;
End;

Procedure TBuildParseTree.RecogniseConstantExpression;
Begin
  RecogniseExpr(True);
End;

Procedure TBuildParseTree.RecogniseQualId;
Begin
  { typecast, e.g. "(x as Ty)"
     or just bracketed, as in (x).y();

     See TestCastSimple.pas for the heinous examples

     QualID ->
      -> (Designator)
      -> (Designator as type)
      -> ident
      ->(pointervar + expr)
  }
  If (fcTokenList.FirstSolidTokenType = ttOpenBracket) Then Begin
    PushNode(nBracketedQual);
    Recognise(ttOpenBracket);
    RecogniseDesignator;

    If (fcTokenList.FirstSolidTokenType = ttAs) Then Begin
      Recognise(ttAs);
      RecogniseIdentifier(True);
    End;
    Recognise(ttCloseBracket);
    PopNode;
  End
  Else
    // a simple ident - e.g. "x"
    RecogniseIdentifier(True);
End;

Procedure TBuildParseTree.RecogniseIdentifier(Const pbCanHaveUnitQualifier: Boolean);
Var
  lc: TSourceToken;
Begin
  lc := fcTokenList.FirstSolidToken;

  If Not (IdentifierNext) Then
    Raise TEParseError.Create('Expected identifer', lc);

  PushNode(nIdentifier);
  Recognise(IdentiferTokens);

  { tokens can be qualified by a unit name }
  If pbCanHaveUnitQualifier And (fcTokenList.FirstSolidTokenType = ttDot) Then Begin
    Recognise(ttDot);
    Recognise(IdentiferTokens);
  End;

  PopNode;
End;

{ the name of a procedure/function/constructor can be
  a plain name or classname.methodname }

Procedure TBuildParseTree.RecogniseMethodName(Const pbClassNameCompulsory: Boolean);
Begin
  If Not (IdentifierNext) Then
    Raise TEParseError.Create('Expected identifer', fcTokenList.FirstSolidToken);

  Recognise(IdentiferTokens);

  If (fcTokenList.FirstSolidTokenType = ttDot) Or pbClassNameCompulsory Then Begin
    Recognise(ttDot);
    RecogniseIdentifier(False);
  End
End;

Procedure TBuildParseTree.RecogniseTypeId;
Var
  lc: TSourceToken;
Begin
  lc := fcTokenList.FirstSolidToken;

  { a type is an identifier. Or a file or other Reserved word }
  If lc.TokenType In BuiltInTypes Then
    Recognise(BuiltInTypes)
  Else If lc.TokenType = ttFile Then
    Recognise(ttFile)
  Else
    RecogniseIdentifier(True);
End;

Procedure TBuildParseTree.RecogniseAsmBlock;
Begin
  PushNode(nAsm);

  Recognise(ttAsm);
  While fcTokenList.FirstSolidTokenType <> ttEnd Do
    RecogniseAsmStatement;

  Recognise(ttEnd);

  PopNode;
End;

Procedure TBuildParseTree.RecogniseAsmStatement;
Begin
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

  If fcTokenList.FirstSolidTokenType = ttAtSign Then Begin
    RecogniseAsmLabel(True);
  End
  Else Begin
    RecogniseAsmOpcode;

    RecogniseWhiteSpace;

    While Not (fcTokenList.FirstTokenType In [ttSemicolon, ttReturn, ttComment, ttEnd]) Do Begin
      If fcTokenList.FirstSolidTokenType = ttComma Then
        Recognise(ttComma);
      RecogniseAsmParam;

      RecogniseWhiteSpace;

      If fcTokenList.FirstSolidTokenType = ttEnd Then
        Break;

      If fcTokenList.FirstSolidTokenType = ttSemiColon Then Begin
        Recognise(ttSemiColon);
        break;
      End;
    End;

  End;

  PopNode;
End;

{ purpose: to consume white space
  make sure that buffertokens(0)
  contains a retunr, comment or solid token }

Procedure TBuildParseTree.RecogniseWhiteSpace;
Begin
  While fcTokenList.FirstTokenType = ttWhiteSpace Do
    Recognise(ttWhiteSpace);
End;

Procedure TBuildParseTree.RecogniseAsmIdent;
Var
  lc: TSourceToken;
Begin
  PushNode(nAsmIdent);

  { can contain '@' signs }
  lc := fcTokenList.FirstSolidToken;

  If Not (lc.TokenType In IdentiferTokens + [ttAtSign]) Then
    Raise TEParseError.Create('Expected asm identifer', lc);

  While (lc.TokenType In IdentiferTokens + [ttAtSign]) Do Begin
    Recognise(IdentiferTokens + [ttAtSign]);
    { whitespace ends this so no fcTokenList.FirstSolidToken }
    lc := fcTokenList.First;
  End;

  PopNode;
End;

Procedure TBuildParseTree.RecogniseAsmOpcode;
Begin
  { these are all short (3 chars? 4 chars)

    but it's too large a cast and varies from CPU to CPU
    so I will not enumerate them all

    some overlap with Delphi reserved words
    e.g. SHL
   }
  PushNode(nASMOpcode);
  If IdentifierNext Then
    RecogniseIdentifier(False)
  Else If WordTypeOfToken(fcTokenList.FirstSolidTokenType) In TextualWordTypes Then
    // match anything
    Recognise(fcTokenList.FirstSolidTokenType)
  Else
    Raise TEParseError.Create('Expected asm opcode', fcTokenList.FirstSolidToken);

  PopNode;
End;

Function IsAsmLabel(Const pt: TSourceToken): boolean;
Begin
  Result := False;
  If pt = Nil Then
    exit;
  Result := (pt.TokenType In [ttNumber, ttIdentifier, ttAtSign]) Or
    (pt.WordType In [wtReservedWord, wtReservedWordDirective, wtBuiltInConstant, wtBuiltInType]);
End;

Procedure TBuildParseTree.RecogniseAsmLabel(Const pbColon: boolean);
Begin
  PushNode(nAsmLabel);

  Recognise(ttAtSign);
  If fcTokenList.FirstSolidTokenType = ttAtSign Then
    Recognise(ttAtSign);

  { label can be a number, eg "@@1:"
    or an identifier that starts with a number, eg "@@2a"

    can also be a delphi keyword, e.g. "@@repeat:"
  }

  While IsAsmLabel(fcTokenList.First) Do Begin
    Recognise(fcTokenList.FirstTokenType);
  End;

  If pbColon Then
    Recognise(ttColon);

  PopNode;
End;

Procedure TBuildParseTree.RecogniseAsmParam;
Const
  ASM_EXPRESSION_START = [ttOpenBracket, ttOpenSquareBracket, ttNumber, ttNot, ttLiteralString,
    ttTrue, ttFalse, ttPlus, ttMinus, ttType, ttOffset];
Var
  lc: TSourceToken;
  lbHasLabel: boolean;
Begin
  { um.

  AsmParam
    -> Ident
    -> '@' ident
    -> '[' AsmExpr ']'
    -> Ident(number)
  }

  lbHasLabel := False;
  PushNode(nAsmParam);

  lc := fcTokenList.FirstSolidToken;

  If lc.TokenType = ttAtSign Then Begin
    RecogniseAsmLabel(False);
    lbHasLabel := True;

    If fcTokenList.FirstSolidTokenType = ttDot Then
      Recognise(ttDot);
  End;

  If IdentifierNext Or (lc.TokenType In ASM_EXPRESSION_START) Then Begin
    RecogniseAsmExpr;
  End
  Else Begin
    If Not lbHasLabel Then
      Raise TEParseError.Create('Expected asm param', lc);
  End;

  PopNode;
End;

Const
  ASM_OPERATORS = [ttPlus, ttMinus, ttAnd, ttOr, ttTimes, ttFloatDiv, ttPtr, ttColon];

  { having to wing this one. it is like expressions, but different }

Procedure TBuildParseTree.RecogniseAsmExpr;
Var
  lc: TSourceToken;
Begin
  RecogniseAsmFactor;

  { can't go past returns }
  lc := fcTokenList.FirstTokenWithExclusion([ttWhiteSpace]);
  While lc.TokenType In ASM_OPERATORS Do Begin
    RecogniseAsmOperator;
    RecogniseAsmFactor;
    lc := fcTokenList.FirstTokenWithExclusion([ttWhiteSpace]);
  End;
End;

Procedure TBuildParseTree.RecogniseAsmOperator;
Begin
  Recognise(ASM_OPERATORS);
End;

Procedure TBuildParseTree.RecogniseAsmFactor;
Begin
  If fcTokenList.FirstSolidTokenType = ttNot Then
    Recognise(ttNot);

  If fcTokenList.FirstSolidTokenType = ttMinus Then
    Recognise(ttMinus);

  If fcTokenList.FirstSolidTokenType = ttAt Then
    Recognise(ttAt);

  If fcTokenList.FirstSolidTokenType = ttType Then
    Recognise(ttType);

  If fcTokenList.FirstSolidTokenType = ttOffset Then
    Recognise(ttOffset);

  Case fcTokenList.FirstSolidTokenType Of
    ttNumber:
      Recognise(ttNumber);
    ttLiteralString:
      Recognise(ttLiteralString);
    ttTrue:
      Recognise(ttTrue);
    ttFalse:
      Recognise(ttFalse);
    ttOpenBracket: Begin
        Recognise(ttOpenBracket);
        RecogniseAsmExpr;
        Recognise(ttCloseBracket);
      End;
    ttOpenSquareBracket: Begin
        Recognise(ttOpenSquareBracket);
        RecogniseAsmExpr;
        Recognise(ttCloseSquareBracket);
      End
  Else Begin
      RecogniseAsmIdent;
    End
  End;

  While fcTokenList.FirstSolidTokenType In [ttDot] Do Begin
    Recognise([ttDot]);

    If fcTokenList.FirstSolidTokenType = ttAtSign Then
      Recognise(ttAtSign);
    RecogniseAsmIdent;
  End;

  If fcTokenList.FirstSolidTokenType = ttOpenBracket Then Begin
    Recognise(ttOpenBracket);
    RecogniseAsmFactor;
    Recognise(ttCloseBracket);
  End;

End;

Procedure TBuildParseTree.RecogniseHintDirectives;
Begin
  If ((fcTokenList.FirstSolidTokenType = ttSemicolon) And (fcTokenList.SolidTokenType(2) In HintDirectives)) Or
    (fcTokenList.FirstSolidTokenType In HintDirectives) Then Begin
    If fcTokenList.FirstSolidTokenType = ttSemicolon Then
      Recognise(ttSemicolon);

    PushNode(nHintDirectives);

    While (fcTokenList.FirstSolidTokenType In HintDirectives) Do Begin
      Recognise(HintDirectives);
    End;

    PopNode;
  End;
End;

Procedure TBuildParseTree.RecognisePropertyDirectives;
Const
  { this can be specified at the end after a semicolon
  so it's not just in the specifiers

  the default directive works differently for array and not-array properties

  for non-array properties it is followed by an identifier
  }
  PropertyDirectives = [ttDefault, ttNoDefault, ttStored];
Begin
  If ((fcTokenList.FirstSolidTokenType = ttSemicolon) And (fcTokenList.SolidTokenType(2) In PropertyDirectives)) Or
    (fcTokenList.FirstSolidTokenType In PropertyDirectives) Then Begin
    If fcTokenList.FirstSolidTokenType = ttSemicolon Then
      Recognise(ttSemicolon);

    While fcTokenList.FirstSolidTokenType In PropertyDirectives Do Begin
      PushNode(nPropertyDirective);

      Case fcTokenList.FirstSolidTokenType Of
        ttDefault: Begin
            Recognise(ttDefault);
            If fcTokenList.FirstSolidTokenType <> ttSemicolon Then
              RecogniseConstantExpression;
          End;
        ttNoDefault: Begin
            Recognise(ttNoDefault);
          End;
        ttStored: Begin
            Recognise(ttStored);
            If fcTokenList.FirstSolidTokenType <> ttSemicolon Then
              RecogniseConstantExpression;
          End;
      End;

      PopNode;
    End;

  End;

End;

Procedure TBuildParseTree.RecogniseExportsSection;
Begin
  PushNode(nExports);

  Recognise(ttExports);
  RecogniseExportedProc;

  // more to come?
  While fcTokenList.FirstTokenType <> ttSemicolon Do Begin
    Recognise(ttComma);
    RecogniseExportedProc;
  End;

  Recognise(ttSemicolon);

  PopNode;
End;

Procedure TBuildParseTree.RecogniseExportedProc;
Const
  ExportedDirectives: TTokenTypeSet = [ttName, ttIndex, ttResident];
Var
  lc: TSourceToken;
Begin
  PushNode(nExportedProc);

  RecogniseIdentifier(True);

  If fcTokenList.FirstSolidTokenType = ttOpenBracket Then
    RecogniseFormalParameters;

  While fcTokenList.FirstSolidTokenType In ExportedDirectives Do Begin
    lc := fcTokenList.FirstSolidToken;

    Case lc.TokenType Of
      ttName: Begin
          Recognise(ttName);
          Recognise(IdentiferTokens + [ttLiteralString]);
        End;
      ttIndex: Begin
          Recognise(ttIndex);
          Recognise(ttNumber);
        End;
      ttResident:
        Recognise(ttResident);
    Else
      Raise TEParseError.Create('Expected export directive', lc);
    End;
  End;

  PopNode;
End;

Procedure TBuildParseTree.RecogniseActualParams;
Var
  lbMore: boolean;
Begin
  PushNode(nActualParams);

  Recognise(ttOpenBracket);

  If fcTokenList.FirstSolidTokenType <> ttCloseBracket Then Begin
    //RecogniseExprList;

    Repeat
      RecogniseActualParam;

      lbMore := fcTokenList.FirstSolidTokenType = ttComma;
      If lbMore Then
        Recognise(ttComma);

    Until Not lbMore;

  End;

  Recognise(ttCloseBracket);

  PopNode;
End;

Procedure TBuildParseTree.RecogniseActualParam;
Const
  EXPR_TYPES = [ttNumber, ttIdentifier, ttLiteralString,
    ttPlus, ttMinus, ttOpenBracket, ttOpenSquareBracket, ttNot, ttInherited];
Var
  lc: TSourceToken;
Begin
  lc := fcTokenList.FirstSolidToken;

  { all kinds of reserved words can sometimes be param names
    thanks to COM and named params
    See LittleTest43.pas }
  If (Not (lc.TokenType In EXPR_TYPES)) And StrIsAlphaNum(lc.SourceCode) And
    (Not IsIdentifierToken(lc)) Then Begin
    { quick surgery. Perhaps even a hack -
      reclasify the token, as it isn't what it thinks it is
      e.g. if this word is 'then', then
      we don't want a linbreak after it like in if statements }
    lc.TokenType := ttIdentifier;
    Recognise(ttIdentifier);

    { this must be a named value, e.g. "end = 3". See LittleTest43.pas for e.g.s }
    Recognise(ttAssign);
    RecogniseExpr(True);
  End
  Else Begin
    RecogniseExpr(True);

    { ole named param syntax, e.g.
      " MSWord.TextToTable(ConvertFrom := 2, NumColumns := 3);"
    }

    If fcTokenList.FirstSolidTokenType = ttAssign Then Begin
      Recognise(ttAssign);
      RecogniseExpr(True);
    End

      { str width specifiers e.g. " Str(val:0, S);" this is an odd wart on the syntax }
    Else If fcTokenList.FirstSolidTokenType = ttColon Then Begin
      { can be more than one of them }
      While fcTokenList.FirstSolidTokenType = ttColon Do Begin
        Recognise(ttColon);
        RecogniseExpr(True);
      End;
    End;
  End;
End;

End.

