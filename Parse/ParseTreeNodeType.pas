unit ParseTreeNodeType;

{
  This enumeration describes all of the types of parse tree nodes
  that we are interested in
}

interface

type
  { roles that the interior node can play }
  TParseTreeNodeType = (
    nUnknown,
    nLeaf,
    nProgram,
    nUnit,
    nUnitHeader,
    nPackage,
    nLibrary,
    nUses,
    nUsesItem,
    nIdentList,
    nInterfaceSection,
    nImplementationSection,
    nBlock,
    nStatementList,
    nDeclSection,
    nLabelDeclSection,
    nConstSection,
    nConstDecl,
    nTypeSection,
    nTypeDecl,
    nArrayConstant,
    nRecordConstant,
    nRecordFieldConstant,
    nType,
    nRestrictedType,
    nSubrangeType,
    nEnumeratedType,
    nArrayType,
    nRecordType,
    nFieldDeclaration,
    nRecordVariantSection,
    nRecordVariant,
    nSetType,
    nProcedureType,
    nVarSection,
    nVarDecl,
    nAbsoluteVar,
    nVariableInit,
    nExpression,
    nTerm,
    nUnaryOp,
    nActualParams,
    nStatement,
    nAssignment,
    nStatementLabel,
    nCompoundStatement,
    nIfCondition,
    nIfBlock,
    nElseBlock,
    nCaseStatement,
    nCaseSelector,
    nCaseLabels,
    nCaseLabel,
    nElseCase,
    nRepeatStatement,
    nWhileStatement,
    nLoopHeaderExpr,
    nBlockHeaderExpr,
    nForStatement,
    nWithStatement,
    nTryAndHandlerBlock,
    nTryBlock,
    nFinallyBlock,
    nExceptBlock,
    nExceptionHandlers,
    nOnExceptionHandler,
    nProcedureDecl,
    nFunctionDecl,
    nConstructorDecl,
    nDestructorDecl,
    nFunctionHeading,
    nProcedureHeading,
    nConstructorHeading,
    nDestructorHeading,
    nFormalParams,
    nFormalParam,
    nProcedureDirectives,
    nExternalDirective,
    nObjectType,
    nInitSection,
    nClassType,
    nClassHeritage,
    nClassBody,
    nClassVisibility,
    nClassDeclarations,
    nProperty,
    nPropertyParameterList,
    nPropertySpecifier,
    nInterfaceType,
    nBracketedQual,
    nAsm,
    nAsmStatement,
    nAsmIdent,
    nASMOpcode,
    nAsmParam,
    nHintDirectives,
    nPropertyDirective,
    nExports,
    nExportedProc
    );

TParseTreeNodeTypeSet = set of TParseTreeNodeType;

const
  DirectiveNodes = [nProcedureDirectives, nExternalDirective, nHintDirectives, nPropertyDirective];
  ProcedureNodes = [nProcedureDecl, nFunctionDecl, nConstructorDecl, nDestructorDecl];
  ProcedureHeadings = [nFunctionHeading, nProcedureHeading, nConstructorHeading, nDestructorHeading];

  ObjectTypes = [nObjectType, nClassType, nInterfaceType];

  { can declare these at the start of a procedure }
  InProcedureDeclSections = [nVarSection, nConstSection, nLabelDeclSection, nTypeSection];


function NodeTypeToString(const pe: TParseTreeNodeType): string;

implementation

uses SysUtils;

function NodeTypeToString(const pe: TParseTreeNodeType): string;
begin
  case pe of
    nUnknown:
      Result := 'Unknown';
    nLeaf:
      Result := 'Leaf';
    nProgram:
      Result := 'Program';
    nUnit:
      Result := 'Unit';
    nUnitHeader:
      Result := 'Unit header';
    nPackage:
      Result := 'Package';
    nLibrary:
      Result := 'Library';
    nUses:
      Result := 'Uses';
    nUsesItem:
      result := 'Uses Item';
    nIdentList:
      Result := 'ident list';
    nInterfaceSection:
      Result := 'Interface section';
    nImplementationSection:
      Result := 'Implmentation section';
    nBlock:
      Result := 'Block';
    nStatementList:
      Result := 'Statement list';
    nDeclSection:
      Result := 'Decl section';
    nLabelDeclSection:
      Result := 'Label decl section';
    nConstSection:
      Result := 'const section';
    nConstDecl:
      Result := 'Const decl';
    nTypeSection:
      Result := 'type section';
    nTypeDecl:
      Result := 'Type Decl';
    nArrayConstant:
      Result := 'Array constant';
    nRecordConstant:
      Result := 'Record Constant';
    nRecordFieldConstant:
      Result := 'Field constant';
    nType:
      Result := 'Type';
    nRestrictedType:
      Result := 'Restricted type';
    nSubrangeType:
      Result := 'Subrange type';
    nEnumeratedType:
      Result := 'Enumerated type';
    nArrayType:
      Result := 'Array type';
    nRecordType:
      Result := 'record type';
    nFieldDeclaration:
      Result := 'Field declarations';
    nRecordVariantSection:
      Result := 'Record variant section';
    nRecordVariant:
      Result := 'Record variant';
    nSetType:
      Result := 'Set type';
    nProcedureType:
      Result := 'procedure type';
    nVarSection:
      Result := 'Var section';
    nVarDecl:
      Result := 'Var decl';
    nAbsoluteVar:
      Result := 'Absolute var';
    nVariableInit:
      Result := 'Variable init';
    nExpression:
      Result := 'Expression';
    nTerm:
      Result := 'Term';
    nUnaryOp:
      Result := 'Unary op';
    nActualParams:
      Result := 'Actual params';
    nStatement:
      Result := 'Statement';
    nAssignment:
      Result := 'Assignment';
    nStatementLabel:
      Result := 'Statement label';
    nCompoundStatement:
      Result := 'Compound statement';
    nIfCondition:
      Result := 'If Condition';
    nIfBlock:
      Result := 'If Block';
    nElseBlock:
      Result := 'Else block';
    nCaseStatement:
      Result := 'Case statement';
    nCaseSelector:
      Result := 'Case selector';
    nCaseLabels:
      Result := 'Case labels';
    nCaseLabel:
      Result := 'Case label';
    nElseCase:
      Result := 'else case';
    nRepeatStatement:
      Result := 'Repeat statement';
    nWhileStatement:
      Result := 'While Statement';
    nLoopHeaderExpr:
      Result := 'Loop header expr';
    nBlockHeaderExpr:
      Result := 'Block header expr';
    nForStatement:
      Result := 'For statement';
    nWithStatement:
      Result := 'With statement';
    nTryAndHandlerBlock:
      Result := 'try and handler block';
    nTryBlock:
      Result := 'try block';
    nFinallyBlock:
      Result := 'finally block';
    nExceptBlock:
      Result := 'except block';
    nExceptionHandlers:
      Result := 'Exception handlers';
    nOnExceptionHandler:
      Result := 'On exception handler';
    nProcedureDecl:
      Result := 'Procedure decl';
    nFunctionDecl:
      Result := 'Function Decl';
    nConstructorDecl:
      Result := 'Constructor decl';
    nDestructorDecl:
      Result := 'Destructor decl';
    nFunctionHeading:
      Result := 'Function heading';
    nProcedureHeading:
      Result := 'Procedure Heading';
    nConstructorHeading:
      Result := 'Constructor Heading';
    nDestructorHeading:
      Result := 'Destructor heading';
    nFormalParams:
      Result := 'Formal params';
    nFormalParam:
      Result := 'formal param';
    nProcedureDirectives:
      Result := 'Procedure directives';
    nExternalDirective:
      Result := 'external directive';
    nObjectType:
      Result := 'object type';
    nInitSection:
      Result := 'init section';
    nClassType:
      Result := 'class type';
    nClassHeritage:
      Result := 'class heritage';
    nClassBody:
      Result := 'class body';
    nClassVisibility:
      Result := 'class visiblity';
    nClassDeclarations:
      Result := 'class declarations';
    nProperty:
      Result := 'property';
    nPropertyParameterList:
      Result := 'property param list';
    nPropertySpecifier:
      Result := 'property specifier';
    nInterfaceType:
      Result := 'interface type';
    nBracketedQual:
      Result := 'bracketed qual';
    nAsm:
      Result := 'asm';
    nAsmStatement:
      Result := 'asm statement';
    nAsmIdent:
      Result := 'asm ident';
    nASMOpcode:
      Result := 'asm opcode';
    nAsmParam:
      Result := 'asm param';
    nHintDirectives:
      Result := 'hint directives';
    nPropertyDirective:
      Result := 'property directive';
    nExports:
      Result := 'exports';
    nExportedProc:
      Result := 'exported proc';
    else
      Result := 'Bad node type ' + IntToStr(Ord(pe));

  end;
end;


end.
 