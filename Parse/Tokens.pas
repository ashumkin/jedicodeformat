{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code

The Original Code is Tokens.pas, released June 2003.
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
unit Tokens;

{ AFS 31 May 2003

  Am going to unify the Tokens and words
  as there are 149 words and about 22 tokens
  Will also remove some when joining,
  no need to have general token categories for words such as
  TokenType = ttReservedWord, WordType = wIf

  will not add in the parse tree node type
  as this would push the total over 256
  and you cannot have sets over enums with more than 256 elements
  I get a lot of value from sets over these enums

  there will still be space for new keywords later without reaching that limit

  Where these are textual tokens the enum item name is the token prefixed with 'tt'
  ie 'while' -> ttWhile
}

interface

type
  TWordType = (wtNotAWord,
    wtReservedWord, wtReservedWordDirective, wtBuiltInConstant, wtBuiltInType,
    wtIdentifier, wtOperator);

  TWordTypeSet = set of TWordType;

  TTokenType =
    (
    // errors -  should only occur when input is bad, or parser is in error
    ttPunctuation, // non-aphanum characters - a catch-all category for other symbols
    ttUnknown, // default category used for unrecognised input

    // spacing
    ttReturn, // CR & LF chars
    ttWhiteSpace, // spaces & tabs
    ttComment, // one of these

    ttNumber, // a numeric constant
    ttLiteralString, // 'This is a string'
    ttSemicolon, // ;
    ttColon, // :
    ttComma,
    ttOpenBracket,
    ttCloseBracket,
    ttOpenSquareBracket,
    ttCloseSquareBracket,
    ttDot,
    ttDoubleDot, // '..' as in '[1 .. 2]'
    ttAssign, // :=

    ttIdentifier, // a user-defined name for a var, type, unit, etc

    { built-in keywords }
    ttArray,
    ttAsm,
    ttBegin,
    ttCase,
    ttClass,
    ttConst,
    ttContains,
    ttConstructor,
    ttDestructor,
    ttDispinterface,
    ttDo,
    ttDownto,
    ttElse,
    ttEnd,
    ttExcept,
    ttExports,
    ttFile,
    ttFinalization,
    ttFinally,
    ttFor,
    ttFunction,
    ttGoto,
    ttIf,
    ttImplementation,
    ttInherited,
    ttInitialization,
    ttInline,
    ttInterface,
    ttLabel,
    ttLibrary,
    ttObject,
    ttOf,
    ttOut,
    ttPackage,
    ttPacked,
    ttProcedure,
    ttProgram,
    ttProperty,
    ttRaise,
    ttRecord,
    ttRepeat,
    ttRequires,
    ttResourcestring,
    ttSet,
    ttThen,
    ttThreadvar,
    ttTo,
    ttTry,
    ttType,
    ttUnit,
    ttUntil,
    ttUses,
    ttVar,
    ttWhile,
    ttWith,
    ttAt,
    ttOn,

    { reserved words that are directives }
    ttAbsolute,
    ttExternal,
    ttPascal,
    ttSafecall,
    ttAbstract,
    ttFar,
    ttPrivate,
    ttStdcall,
    ttAssembler,
    ttForward,
    ttProtected,
    ttStored,
    ttAutomated,
    ttIndex,
    ttPublic,
    ttVirtual,
    ttCdecl,
    ttMessage,
    ttPublished,
    ttWrite,
    ttDefault,
    ttName,
    ttRead,
    ttWriteOnly,
    ttDispId,
    ttNear,
    ttReadOnly,
    ttDynamic,
    ttNodefault,
    ttRegister,
    ttExport,
    ttOverride,
    ttOverload,
    ttResident,
    ttLocal,
    ttImplements,
    ttReintroduce,

    { Delphi 6 directives }
    ttDeprecated,
    ttPlatform,

    { built-in constants }
    ttNil,
    ttTrue,
    ttFalse,

    { built in types }
    ttBoolean,
    ttByteBool,
    ttWordBool,
    ttLongBool,
    ttInteger,
    ttCardinal,
    ttShortint,
    ttSmallint,
    ttLongint,
    ttInt64,
    ttByte,
    ttWord,
    ttLongword,
    ttChar,
    ttWidechar,
    ttString,
    ttAnsiString,
    ttWidestring,
    ttPchar,
    ttSingle,
    ttDouble,
    ttExtended,
    ttReal,
    ttReal48,
    ttComp,
    ttCurrency,

    ttVariant,
    ttOleVariant,

    { operators that are words not symbols }
    ttAnd,
    ttAs,
    ttDiv,
    ttIn,
    ttIs,
    ttMod,
    ttNot,
    ttOr,
    ttShl,
    ttShr,
    ttXor,

    // symbol operators
    ttAtSign,
    ttHat,
    ttTimes,
    ttFloatDiv,
    ttPlus,
    ttMinus,
    ttEquals,
    ttGreaterThan,
    ttLessThan,
    ttGreaterThanOrEqual,
    ttLessThanOrEqual,
    ttNotEqual);

  TTokenTypeSet = set of TTokenType;

const
  TextualWordTypes: TWordTypeSet =
    [wtReservedWord, wtReservedWordDirective,
    wtBuiltInConstant, wtOperator, wtBuiltInType, wtIdentifier];

  // identifiers can use these word categories
  IdentifierTypes: TWordTypeSet = [wtReservedWordDirective,
    wtBuiltInType, wtBuiltInConstant, wtIdentifier];


  { all tokens spelled with a-z }
  TextualTokens: TTokenTypeSet = [ttIdentifier .. ttXor];
  IdentiferTokens: TTokenTypeSet = [ttIdentifier .. ttXor];

  { same as above, with numbers added }
  TextOrNumberTokens: TTokenTypeSet = [ttNumber, ttIdentifier .. ttXor];

  BracketTokens: TTokenTypeSet =
    [ttOpenBracket, ttCloseBracket, ttOpenSquareBracket, ttCloseSquareBracket];
  OpenBrackets: TTokenTypeSet  = [ttOpenBracket, ttOpenSquareBracket];
  CloseBrackets: TTokenTypeSet = [ttCloseBracket, ttCloseSquareBracket];


  NotSolidTokens: TTokenTypeSet = [ttWhiteSpace, ttComment, ttReturn];

  { procedure can have local declarations of vars, const and yes, types }
  Declarations: TTokenTypeSet = [ttConst, ttResourceString, ttVar, ttThreadVar, ttType, ttLabel, ttExports];

  ParamTypes: TTokenTypeSet = [ttVar, ttConst, ttOut];

  BlockOutdentWords: TTokenTypeSet = [ttVar, ttThreadVar, ttConst, ttResourceString, ttType, ttLabel,
    ttBegin, ttEnd, ttTry, ttFinally, ttExcept,
    ttWhile,  ttFor, ttRepeat, ttUntil, ttWith,
    ttAsm, ttCase, ttInitialization, ttFinalization];

  PropertyDirectives: TTokenTypeSet =
    { the basics }
    [ttRead, ttWrite,
    { the advanced stuff  }
    ttStored, ttDefault, ttNoDefault, ttImplements,
    { for COM interface properties }
    ttReadOnly, ttWriteOnly, ttDispId,
    // hints
    ttDeprecated, ttLibrary, ttPlatform
    ];

  ExportDirectives: TTokenTypeSet = [ttIndex, ttName];

  VariableDirectives: TTokenTypeSet = [ttAbsolute, ttDeprecated, ttLibrary, ttPlatform];

  ClassVisibility: TTokenTypeSet = [ttPrivate, ttProtected, ttPublic, ttPublished, ttAutomated];

  ProcedureDirectives: TTokenTypeSet = [ttExternal, ttPascal, ttSafecall, ttAbstract,
    ttAutomated, ttFar, ttStdcall, ttAssembler, ttInline, ttForward,
    ttVirtual, ttCdecl, ttMessage, ttName, ttRegister, ttDispId,
    ttNear, ttDynamic, ttExport, ttOverride, ttResident, ttLocal, ttOverload, ttReintroduce,
    ttDeprecated, ttLibrary, ttPlatform];

  ClassDirectives: TTokenTypeSet = [ttPrivate, ttProtected, ttPublic, ttPublished, ttAutomated];
  HintDirectives: TTokenTypeSet = [ttDeprecated, ttLibrary, ttPlatform];

  AllDirectives: TTokenTypeSet = [ttAbsolute, ttExternal, ttPascal, ttSafecall,
    ttAbstract, ttFar, ttPrivate, ttStdcall, ttAssembler, ttForward,
    ttProtected, ttStored, ttAutomated, ttIndex, ttPublic,
    ttVirtual, ttCdecl, ttMessage, ttPublished, ttWrite,
    ttDefault, ttName, ttRead, ttWriteOnly, ttDispId,
    ttNear, ttReadOnly, ttDynamic, ttNoDefault, ttRegister,
    ttExport, ttOverride, ttOverload, ttResident, ttLocal,
    ttImplements, ttReintroduce, ttDeprecated, ttPlatform];

  ProcedureWords: TTokenTypeSet = [ttProcedure, ttFunction, ttConstructor, ttDestructor];

  StructuredTypeWords: TTokenTypeSet = [ttClass, ttObject, ttInterface, ttDispinterface, ttRecord];
  ObjectTypeWords: TTokenTypeSet = [ttClass, ttObject, ttInterface, ttDispinterface];

  InterfaceWords: TTokenTypeSet = [ttInterface, ttDispinterface];

  ConstWords: TTokenTypeSet = [ttConst, ttResourceString];

  StructStatementWords: TTokenTypeSet = [ttBegin, ttAsm,
    ttIf, ttCase, ttRepeat, ttWhile, ttFor, ttWith, ttTry];

  VariantTypes: TTokenTypeSet = [ttVariant, ttOleVariant];

  Operators: TTokenTypeSet = [ttAnd .. ttNotEqual];

  { these words are
  - operators
  - can be unary
  - have no alphabet chars in them }
  PossiblyUnarySymbolOperators: TTokenTypeSet = [ttAtSign, ttHat, ttAt, ttPlus, ttMinus];

  RelationalOperators: TTokenTypeSet = [
    ttIn, ttIs, ttAs, ttGreaterThan,
    ttLessThan, ttGreaterThanOrEqual, ttLessThanOrEqual, ttEquals, ttNotEqual];

  AddOperators: TTokenTypeSet = [ttPlus, ttMinus, ttOr, ttXor];

  MulOperators: TTokenTypeSet = [ttTimes, ttFloatDiv, ttDiv, ttMod, ttAnd, ttShl, ttShr];

  StringWords: TTokenTypeSet = [ttString, ttAnsiString, ttWideString];

  RealTypes: TTokenTypeSet =
    [ttReal48, ttReal, ttSingle, ttDouble, ttExtended, ttCurrency, ttComp];

  OrdTypes: TTokenTypeSet =
    [ttShortInt, ttSmallInt, ttInteger, ttByte,
    ttLongInt, ttInt64, ttWord,
    ttBoolean, ttByteBool, ttWordBool, ttLongBool,
    ttChar, ttWideChar, ttLongWord, ttPChar];

  UsesWords: TTokenTypeSet = [ttUses, ttRequires, ttContains];

  BuiltInConstants: TTokenTypeSet = [ttNil, ttTrue, ttFalse];
  BuiltInTypes: TTokenTypeSet = [ttBoolean .. ttOleVariant];


{ interpret a string as a token }
procedure TypeOfToken(const psWord: string; var peWordType: TWordType; var peToken: TTokenType); overload;
function TypeOfToken(const psWord: string): TTokenType; overload;
function WordTypeOfToken(const peTokenType: TTokenType): TWordType; overload;

{ back to the string for error message }
function TokenTypeToString(const peToken: TTokenType): string;

{ similarly for a token set }
function TokenTypesToString(const peTokens: TTokenTypeSet): string;


{ chars used to make the comment }
{ these} (* or these *) // or these
type
  TCommentStyle = (eNotAComment, eDoubleSlash, eBracketStar,
    eCurly, eCompilerDirective);
  TCommentStyleSet = set of TCommentStyle;

const
  CURLY_COMMENTS: TCommentStyleSet = [eCurly, eCompilerDirective];


implementation

uses SysUtils;

{ the majority of these tokens have a fixed textual representation
  e.g. ':=', 'if'. THose that don't include comments, numbers, literal strings and identifiers }
type
  TRTokenTextMap = record
    sToken: string;
    eWordType: TWordType;
    eToken: TTokenType;
  end;


const
  KeywordTextMap: array [0..159] of TRTokenTextMap =
    (
    // once were tokens
    (sToken: ';'; eWordType: wtNotAWord; eToken: ttSemicolon),
    (sToken: ':'; eWordType: wtNotAWord; eToken: ttColon),
    (sToken: ','; eWordType: wtNotAWord; eToken: ttComma),
    (sToken: '('; eWordType: wtNotAWord; eToken: ttOpenBracket),
    (sToken: ')'; eWordType: wtNotAWord; eToken: ttCloseBracket),
    (sToken: '['; eWordType: wtNotAWord; eToken: ttOpenSquareBracket),
    (sToken: ']'; eWordType: wtNotAWord; eToken: ttCloseSquareBracket),
    (sToken: '..'; eWordType: wtNotAWord; eToken: ttDoubleDot),
    (sToken: '.'; eWordType: wtNotAWord; eToken: ttDot),
    (sToken: ':='; eWordType: wtNotAWord; eToken: ttAssign),

    { reserved words }
    (sToken: 'array'; eWordType: wtReservedWord; eToken: ttArray),
    (sToken: 'asm'; eWordType: wtReservedWord; eToken: ttAsm),
    (sToken: 'begin'; eWordType: wtReservedWord; eToken: ttBegin),
    (sToken: 'case'; eWordType: wtReservedWord; eToken: ttCase),
    (sToken: 'class'; eWordType: wtReservedWord; eToken: ttClass),
    (sToken: 'const'; eWordType: wtReservedWord; eToken: ttConst),
    (sToken: 'constructor'; eWordType: wtReservedWord; eToken: ttConstructor),
    (sToken: 'destructor'; eWordType: wtReservedWord; eToken: ttDestructor),
    (sToken: 'dispinterface'; eWordType: wtReservedWord; eToken: ttDispinterface),
    (sToken: 'do'; eWordType: wtReservedWord; eToken: ttDo),
    (sToken: 'downto'; eWordType: wtReservedWord; eToken: ttDownTo),
    (sToken: 'else'; eWordType: wtReservedWord; eToken: ttElse),
    (sToken: 'end'; eWordType: wtReservedWord; eToken: ttEnd),
    (sToken: 'except'; eWordType: wtReservedWord; eToken: ttExcept),
    (sToken: 'exports'; eWordType: wtReservedWord; eToken: ttExports),
    (sToken: 'file'; eWordType: wtReservedWord; eToken: ttFile),
    (sToken: 'finalization'; eWordType: wtReservedWord; eToken: ttFinalization),
    (sToken: 'finally'; eWordType: wtReservedWord; eToken: ttFinally),
    (sToken: 'for'; eWordType: wtReservedWord; eToken: ttFor),
    (sToken: 'function'; eWordType: wtReservedWord; eToken: ttFunction),
    (sToken: 'goto'; eWordType: wtReservedWord; eToken: ttGoto),
    (sToken: 'if'; eWordType: wtReservedWord; eToken: ttIf),
    (sToken: 'implementation'; eWordType: wtReservedWord; eToken: ttImplementation),
    (sToken: 'inherited'; eWordType: wtReservedWord; eToken: ttInherited),
    (sToken: 'initialization'; eWordType: wtReservedWord; eToken: ttInitialization),
    (sToken: 'inline'; eWordType: wtReservedWord; eToken: ttInline),
    (sToken: 'interface'; eWordType: wtReservedWord; eToken: ttInterface),
    (sToken: 'label'; eWordType: wtReservedWord; eToken: ttLabel),
    (sToken: 'library'; eWordType: wtReservedWord; eToken: ttLibrary),
    (sToken: 'object'; eWordType: wtReservedWord; eToken: ttObject),
    (sToken: 'of'; eWordType: wtReservedWord; eToken: ttOf),
    (sToken: 'out'; eWordType: wtReservedWord; eToken: ttOut),
    (sToken: 'packed'; eWordType: wtReservedWord; eToken: ttPacked),
    (sToken: 'procedure'; eWordType: wtReservedWord; eToken: ttProcedure),
    (sToken: 'program'; eWordType: wtReservedWord; eToken: ttProgram),
    (sToken: 'property'; eWordType: wtReservedWord; eToken: ttProperty),
    (sToken: 'raise'; eWordType: wtReservedWord; eToken: ttRaise),
    (sToken: 'record'; eWordType: wtReservedWord; eToken: ttRecord),
    (sToken: 'repeat'; eWordType: wtReservedWord; eToken: ttRepeat),
    (sToken: 'resourcestring'; eWordType: wtReservedWord; eToken: ttResourceString),
    (sToken: 'set'; eWordType: wtReservedWord; eToken: ttSet),
    (sToken: 'then'; eWordType: wtReservedWord; eToken: ttThen),
    (sToken: 'threadvar'; eWordType: wtReservedWord; eToken: ttThreadvar),
    (sToken: 'to'; eWordType: wtReservedWord; eToken: ttTo),
    (sToken: 'try'; eWordType: wtReservedWord; eToken: ttTry),
    (sToken: 'type'; eWordType: wtReservedWord; eToken: ttType),
    (sToken: 'unit'; eWordType: wtReservedWord; eToken: ttUnit),
    (sToken: 'until'; eWordType: wtReservedWord; eToken: ttUntil),
    (sToken: 'uses'; eWordType: wtReservedWord; eToken: ttUses),
    (sToken: 'var'; eWordType: wtReservedWord; eToken: ttVar),
    (sToken: 'while'; eWordType: wtReservedWord; eToken: ttWhile),
    (sToken: 'with'; eWordType: wtReservedWord; eToken: ttWith),

    { reseved words that must be parsed as directives because they can be identifier names }
    (sToken: 'at'; eWordType: wtReservedWordDirective; eToken: ttAt),
    (sToken: 'on'; eWordType: wtReservedWordDirective; eToken: ttOn),
    (sToken: 'package'; eWordType: wtReservedWordDirective; eToken: ttPackage),
    (sToken: 'contains'; eWordType: wtReservedWordDirective; eToken: ttContains),
    (sToken: 'requires'; eWordType: wtReservedWordDirective; eToken: ttRequires),

    { reseved words that are directives }
    (sToken: 'absolute'; eWordType: wtReservedWordDirective; eToken: ttAbsolute),
    (sToken: 'external'; eWordType: wtReservedWordDirective; eToken: ttExternal),
    (sToken: 'pascal'; eWordType: wtReservedWordDirective; eToken: ttPascal),
    (sToken: 'safecall'; eWordType: wtReservedWordDirective; eToken: ttSafecall),
    (sToken: 'abstract'; eWordType: wtReservedWordDirective; eToken: ttAbstract),
    (sToken: 'far'; eWordType: wtReservedWordDirective; eToken: ttFar),
    (sToken: 'private'; eWordType: wtReservedWordDirective; eToken: ttPrivate),
    (sToken: 'stdcall'; eWordType: wtReservedWordDirective; eToken: ttStdCall),
    (sToken: 'assembler'; eWordType: wtReservedWordDirective; eToken: ttAssembler),
    (sToken: 'forward'; eWordType: wtReservedWordDirective; eToken: ttForward),
    (sToken: 'protected'; eWordType: wtReservedWordDirective; eToken: ttProtected),
    (sToken: 'stored'; eWordType: wtReservedWordDirective; eToken: ttStored),
    (sToken: 'automated'; eWordType: wtReservedWordDirective; eToken: ttAutomated),
    (sToken: 'index'; eWordType: wtReservedWordDirective; eToken: ttIndex),
    (sToken: 'public'; eWordType: wtReservedWordDirective; eToken: ttPublic),
    (sToken: 'virtual'; eWordType: wtReservedWordDirective; eToken: ttVirtual),
    (sToken: 'cdecl'; eWordType: wtReservedWordDirective; eToken: ttCdecl),
    (sToken: 'message'; eWordType: wtReservedWordDirective; eToken: ttMessage),
    (sToken: 'published'; eWordType: wtReservedWordDirective; eToken: ttPublished),
    (sToken: 'write'; eWordType: wtReservedWordDirective; eToken: ttWrite),
    (sToken: 'default'; eWordType: wtReservedWordDirective; eToken: ttDefault),
    (sToken: 'name'; eWordType: wtReservedWordDirective; eToken: ttName),
    (sToken: 'read'; eWordType: wtReservedWordDirective; eToken: ttRead),
    (sToken: 'writeonly'; eWordType: wtReservedWordDirective; eToken: ttWriteOnly),
    (sToken: 'dispid'; eWordType: wtReservedWordDirective; eToken: ttDispId),
    (sToken: 'near'; eWordType: wtReservedWordDirective; eToken: ttNear),
    (sToken: 'readonly'; eWordType: wtReservedWordDirective; eToken: ttReadOnly),
    (sToken: 'dynamic'; eWordType: wtReservedWordDirective; eToken: ttDynamic),
    (sToken: 'nodefault'; eWordType: wtReservedWordDirective; eToken: ttNoDefault),
    (sToken: 'register'; eWordType: wtReservedWordDirective; eToken: ttRegister),
    (sToken: 'export'; eWordType: wtReservedWordDirective; eToken: ttExport),
    (sToken: 'override'; eWordType: wtReservedWordDirective; eToken: ttOverride),
    (sToken: 'overload'; eWordType: wtReservedWordDirective; eToken: ttOverload),
    (sToken: 'resident'; eWordType: wtReservedWordDirective; eToken: ttResident),
    (sToken: 'local'; eWordType: wtReservedWordDirective; eToken: ttLocal),

    (sToken: 'implements'; eWordType: wtReservedWordDirective; eToken: ttImplements),
    (sToken: 'reintroduce'; eWordType: wtReservedWordDirective; eToken: ttReintroduce),

    { D6 directives }
    (sToken: 'deprecated'; eWordType: wtReservedWordDirective; eToken: ttDeprecated),
    (sToken: 'platform'; eWordType: wtReservedWordDirective; eToken: ttPlatform),

    { operators that are words not symbols }
    (sToken: 'and'; eWordType: wtOperator; eToken: ttAnd),
    (sToken: 'as'; eWordType: wtOperator; eToken: ttAs),
    (sToken: 'div'; eWordType: wtOperator; eToken: ttDiv),
    (sToken: 'in'; eWordType: wtOperator; eToken: ttIn),
    (sToken: 'is'; eWordType: wtOperator; eToken: ttIs),
    (sToken: 'mod'; eWordType: wtOperator; eToken: ttMod),
    (sToken: 'not'; eWordType: wtOperator; eToken: ttNot),
    (sToken: 'or'; eWordType: wtOperator; eToken: ttOr),
    (sToken: 'shl'; eWordType: wtOperator; eToken: ttShl),
    (sToken: 'shr'; eWordType: wtOperator; eToken: ttShr),
    (sToken: 'xor'; eWordType: wtOperator; eToken: ttXor),

    { built-in constants }
    (sToken: 'nil'; eWordType: wtBuiltInConstant; eToken: ttNil),
    (sToken: 'true'; eWordType: wtBuiltInConstant; eToken: ttTrue),
    (sToken: 'false'; eWordType: wtBuiltInConstant; eToken: ttFalse),

    { built-in types }
    (sToken: 'boolean'; eWordType: wtBuiltInType; eToken: ttBoolean),
    (sToken: 'ByteBool'; eWordType: wtBuiltInType; eToken: ttByteBool),
    (sToken: 'WordBool'; eWordType: wtBuiltInType; eToken: ttWordBool),
    (sToken: 'LongBool'; eWordType: wtBuiltInType; eToken: ttLongBool),

    (sToken: 'integer'; eWordType: wtBuiltInType; eToken: ttInteger),
    (sToken: 'cardinal'; eWordType: wtBuiltInType; eToken: ttCardinal),
    (sToken: 'shortint'; eWordType: wtBuiltInType; eToken: ttShortInt),
    (sToken: 'smallint'; eWordType: wtBuiltInType; eToken: ttSmallInt),
    (sToken: 'longint'; eWordType: wtBuiltInType; eToken: ttLongInt),
    (sToken: 'int64'; eWordType: wtBuiltInType; eToken: ttInt64),
    (sToken: 'byte'; eWordType: wtBuiltInType; eToken: ttByte),
    (sToken: 'word'; eWordType: wtBuiltInType; eToken: ttWord),
    (sToken: 'longword'; eWordType: wtBuiltInType; eToken: ttLongWord),

    (sToken: 'char'; eWordType: wtBuiltInType; eToken: ttChar),
    (sToken: 'widechar'; eWordType: wtBuiltInType; eToken: ttWideChar),
    (sToken: 'string'; eWordType: wtBuiltInType; eToken: ttString),
    (sToken: 'ansistring'; eWordType: wtBuiltInType; eToken: ttAnsiString),
    (sToken: 'widestring'; eWordType: wtBuiltInType; eToken: ttWideString),
    (sToken: 'pChar'; eWordType: wtBuiltInType; eToken: ttPchar),

    (sToken: 'single'; eWordType: wtBuiltInType; eToken: ttSingle),
    (sToken: 'double'; eWordType: wtBuiltInType; eToken: ttDouble),
    (sToken: 'extended'; eWordType: wtBuiltInType; eToken: ttExtended),
    (sToken: 'real'; eWordType: wtBuiltInType; eToken: ttReal),
    (sToken: 'real48'; eWordType: wtBuiltInType; eToken: ttReal48),
    (sToken: 'comp'; eWordType: wtBuiltInType; eToken: ttComp),
    (sToken: 'currency'; eWordType: wtBuiltInType; eToken: ttCurrency),

    (sToken: 'variant'; eWordType: wtBuiltInType; eToken: ttVariant),
    (sToken: 'OleVariant'; eWordType: wtBuiltInType; eToken: ttOleVariant),

    { operators that are symbols }
    (sToken: '@'; eWordType: wtOperator; eToken: ttAtSign),
    (sToken: '^'; eWordType: wtOperator; eToken: ttHat),
    (sToken: '*'; eWordType: wtOperator; eToken: ttTimes),
    (sToken: '/'; eWordType: wtOperator; eToken: ttFloatDiv),
    (sToken: '+'; eWordType: wtOperator; eToken: ttPlus),
    (sToken: '-'; eWordType: wtOperator; eToken: ttMinus),
    (sToken: '='; eWordType: wtOperator; eToken: ttEquals),
    (sToken: '>='; eWordType: wtOperator; eToken: ttGreaterThanOrEqual),
    (sToken: '<='; eWordType: wtOperator; eToken: ttLessThanOrEqual),
    (sToken: '<>'; eWordType: wtOperator; eToken: ttNotEqual),
    // these must come after the above as they are shorter
    (sToken: '>'; eWordType: wtOperator; eToken: ttGreaterThan),
    (sToken: '<'; eWordType: wtOperator; eToken: ttLessThan)
    );

procedure TypeOfToken(const psWord: string; var peWordType: TWordType; var peToken: TTokenType);
var
  liLoop: integer;
begin
  Assert(psWord <> '');

  { if its not found in the list, it is unknown }
  peWordType := wtNotAWord;
  peToken := ttUnknown;

  for liLoop := Low(KeywordTextMap) to High(KeywordTextMap) do
  begin
    if AnsiCompareText(KeywordTextMap[liLoop].sToken, psWord) = 0 then
    begin
      peWordType := KeywordTextMap[liLoop].eWordType;
      peToken     := KeywordTextMap[liLoop].eToken;
      break;
    end;
  end;
end;

function TypeOfToken(const psWord: string): TTokenType; overload;
var
  leWordType: TWordType;
begin
  TypeOfToken(psWord, leWordType, Result);
end;

function TokenTypeToString(const peToken: TTokenType): string;
var
  lbFound: boolean;
  liLoop: integer;
begin
  lbFound := False;

  if peToken = ttPunctuation then
  begin
    Result := 'Unknown punctuation';
    lbFound := True;
  end
  else
  if peToken = ttUnknown then
  begin
    Result := 'Unknown';
    lbFound := True;
  end
  else if peToken = ttIdentifier then
  begin
    // identifier not in the list as it has no fixed text
    Result := 'Identifier';
    lbFound := True;
  end
  else if peToken = ttNumber then
  begin
    Result := 'Number';
    lbFound := True;
  end
  else if peToken = ttLiteralString then
  begin
    Result := 'Literal string';
    lbFound := True;
  end
  else
  begin
    for liLoop := Low(KeywordTextMap) to High(KeywordTextMap) do
    begin
      if peToken = KeywordTextMap[liLoop].eToken then
      begin
        Result := KeywordTextMap[liLoop].sToken;
        lbFound := True;
        break;
      end;
    end;
  end;
  
  if not lbFound then
    Result := 'Token ' + IntToStr(Ord(peToken)) + ' not found';
end;

function TokenTypesToString(const peTokens: TTokenTypeSet): string;
var
  liLoop: integer;
begin
  if peTokens = [] then
    Result := '[]'
  else
  begin
    Result := '';

    for liLoop := Low(KeywordTextMap) to High(KeywordTextMap) do
    begin
      if KeywordTextMap[liLoop].eToken in peTokens then
      begin
        if Result <> '' then
          Result := Result + ' ';
        Result := Result + KeywordTextMap[liLoop].sToken;
      end;
    end;
  end;
end;

function WordTypeOfToken(const peTokenType: TTokenType): TWordType; overload;
var
  liLoop: integer;
begin
  Result := wtNotAWord;

  if peTokenType = ttIdentifier then
  begin
    // identifier not in the list as it has no fixed text
    Result := wtIdentifier;
  end
  else
  begin
    for liLoop := Low(KeywordTextMap) to High(KeywordTextMap) do
    begin
      if KeywordTextMap[liLoop].eToken = peTokenType then
      begin
        Result := KeywordTextMap[liLoop].eWordType;
        break;
      end;
    end;
  end;
end;

end.
