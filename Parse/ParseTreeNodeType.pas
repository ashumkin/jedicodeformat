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
    nActualParams,
    nStatement,
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
    nTryAndHandlerBlock,
    nTryBlock,
    nFinallyBlock,
    nExceptBlock,
    nExceptionHandler,
    nProcedureDecl,
    nFunctionDecl,
    nConstructorDecl,
    nDestructorDecl,
    nFunctionHeading,
    nProcedureHeading,
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
      Result := 'Decl section';
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
    nActualParams:
      Result := 'Actual params';
    nStatement:
      Result := 'Statement';
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
    nTryAndHandlerBlock:
      Result := 'try and handler block';
    nTryBlock:
      Result := 'try block';
    nFinallyBlock:
      Result := 'finally block';
    nExceptBlock:
      Result := 'except block';
    nExceptionHandler:
      Result := 'Exception handler';
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
 