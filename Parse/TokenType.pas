{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is TokenType.pas, released April 2000.
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

unit TokenType;

{ a token is an element of source code
  This type enumerates all the possible tokens in Delphi

  Reserved words etc. have thier own enumeration for the subtypes }


interface

type
  TTokenType =
    (ttReturn, // CR & LF chars
    ttWhiteSpace, // spaces & tabs
    ttComment, // one of these

    ttWord, // an unquoted alphanumeric string, ie name of some unit, var, class, proc, const etc

    // built-in delphi key word in three flavours
    ttReservedWord,
    ttReservedWordDirective,
    ttBuiltInConstant,
    ttBuiltInType,

    ttOperator, // + - * / etc

    ttNumber, // a numeric constant
    ttLiteralString, // 'This is a string'
    ttSemiColon, // ;
    ttColon, // :
    ttComma,
    ttOpenBracket,
    ttCloseBracket,
    ttOpenSquareBracket,
    ttCloseSquareBracket,
    ttDot,
    ttDoubleDot, // '..' as in '[1 .. 2]'
    ttAssign, // :=
    ttPunctuation, // non-aphanum characters - a catch-all category for other symbols
    ttUnknown, // default category
    ttEOF // no more tokens!
    );

  TTokenTypeSet = set of TTokenType;

const
  TextualTokens: TTokenTypeSet =
    [ttReservedWord, ttReservedWordDirective,
    ttBuiltInConstant, ttOperator, ttBuiltInType, ttWord];

  { same as above, with numbers added }
  TextOrNumberTokens: TTokenTypeSet =
    [ttReservedWord, ttReservedWordDirective,
    ttBuiltInConstant, ttOperator, ttBuiltInType, ttWord, ttNumber];

  ReservedWordTokens: TTokenTypeSet =
    [ttReservedWord, ttReservedWordDirective, ttBuiltInConstant, ttOperator];

  BracketTokens: TTokenTypeSet =
    [ttOpenBracket, ttCloseBracket, ttOpenSquareBracket, ttCloseSquareBracket];
  OpenBrackets: TTokenTypeSet  = [ttOpenBracket, ttOpenSquareBracket];
  CloseBrackets: TTokenTypeSet = [ttCloseBracket, ttCloseSquareBracket];


  NotSolidTokens = [ttWhiteSpace, ttComment, ttReturn];

  IdentifierTypes = [ttReservedWordDirective, ttBuiltInType, ttWord];

function TokenTypeToString(tt: TTokenType): string;
function TokenTypesToString(const tts: TTokenTypeSet): string;


{ where are we in the file?
 a delphi file is broken down into interface, implementation, and
  optional initialization and finalization sections
}

type
  TFileSection = (fsBeforeInterface, fsInterface, fsImplementation,
    fsInitialization, fsFinalization,

    { special sections for a dpr file, not found in a normal pas file }
    fsProgram, fsLibrary
    );

  TFileSectionSet = set of TFileSection;

const
  ProgramDefSections: TFileSectionSet = [fsProgram, fsLibrary, fsImplementation];
  CodeSections: TFileSectionSet       = [fsInitialization, fsFinalization];
  // these sections are raw code without so much as a 'begin'

type
  { this type primarily covers where we are in a procedure }
  TProcedureSection =
    (psNotInProcedure,
    psProcedureDefinition, // procedure fred (var a: integer)
    psProcedureDirectives, // overload virtual safecall
    psProcedureDeclarations,
    // var jim: integer; const fred = 'Fred';  even types allowed here
    psProcedureBody, // begin .. end
    psProcedureMap // ITestInterfaceMap.Fred = ITestInterfaceMap_Fred;
    );

  TProcedureSectionSet = set of TProcedureSection;

  { this type covers type, const, var and proc declarations }
  TDeclarationSection =
    (dsNotInDeclaration,
    dsType, dsConst, dsVar, dsLabel
    );

  { this type covers where we are in a class def }
  TClassDefinitionSection =
    (cdNotInClassDefinition,
    { parts of the header }
    cdHeader, // "= class" or "= interface"
    cdHeritage,
    // (TSomeObject) or (TSomeInterface) or "" for default base class of TObject/IUnknown
    { access levels }
    cdPrivate,
    cdProtected,
    cdPublic,
    cdPublished
    );

  TClassDefinitionSectionSet = set of TClassDefinitionSection;

const
  ClassStartSections = [cdHeader, cdHeritage];
  AccessSpecifierSections: TClassDefinitionSectionSet =
    [cdPrivate, cdProtected, cdPublic, cdPublished];

type

  TStructuredType =
    (stClass,
    stInterface,
    stRecord,
    stNotInStructuredType
    );

  TStructuredTypeSet = set of TStructuredType;

const
  // those that can have properties, procs and functions
  ObjectTypes: TStructuredTypeSet = [stClass, stInterface];


type
  TCapitalisationType = (ctUpper, ctLower, ctMixed, ctLeaveAlone);

type
 { return after Then and other strategic places? }
  TBlockNewLineStyle = (eAlways, eLeave, eNever);


{ chars used to make the comment }
{ these} (* or these *) // or these
type
  TCommentStyle = (eNotAComment, eBracketStar, eCurly, eDoubleSlash);

{ can stop and restart formating using these comments
 from DelForExp - Egbbert Van Nes's program }
const
  NOFORMAT_ON  = '{(*}';
  NOFORMAT_OFF = '{*)}';

  NOFORMAT_ON_2  = '//jcf:format=off';
  NOFORMAT_OFF_2 = '//jcf:format=on';



{ what to do with return characters (Cr or CrLf)
  1) leave them as is
  2) turn to Lf
  3) turn to CrLf
  4) Will be implemente when this runs in kylix: pick 2 or 3
     depending on the Host OS, preference, ie CrLf for win, cr for 'nix
}
type
  TReturnChars = (rcLeaveAsIs, rcLinefeed, rcCrLf);


implementation

uses SysUtils;

function TokenTypeToString(tt: TTokenType): string;
begin
  case tt of
    ttReturn:
      Result := 'Return';
    ttWhiteSpace:
      Result := 'White space';
    ttComment:
      Result := 'Comment';
    ttWord:
      Result := 'Word';
    ttReservedWord:
      Result := 'Reserved word';
    ttReservedWordDirective:
      Result := 'Directive';
    ttBuiltInConstant:
      Result := 'Built-in Constant';
    ttBuiltInType:
      Result := 'Built-in Type';
    ttOperator:
      Result := 'Operator';
    ttNumber:
      Result := 'Number';
    ttLiteralString:
      Result := 'Literal string';
    ttSemiColon:
      Result := 'Semicolon';
    ttColon:
      Result := 'Colon';
    ttComma:
      Result := 'Comma';
    ttOpenBracket:
      Result := 'Open bracket';
    ttCloseBracket:
      Result := 'Close bracket';
    ttOpenSquareBracket:
      Result := 'Open square bracket';
    ttCloseSquareBracket:
      Result := 'Close square bracket';
    ttDot:
      Result := 'Dot';
    ttDoubleDot:
      Result := 'Double dot';
    ttAssign:
      Result := 'Assign';
    ttPunctuation:
      Result := 'Punctuation';
    ttUnknown:
      Result := 'Unknown';
    ttEOF:
      Result := 'EOF';
    else
      Result := 'Unhandled token type ' + IntToStr(Ord(tt));
  end;
end;

{ display the contents of a token type set }
function TokenTypesToString(const tts: TTokenTypeSet): string;
var
  lt: TTokenType;
begin
  if tts = [] then
    Result := '[]'
  else
  begin
    Result := '';

    for lt := Low(TTokenType) to High(TTokenType) do
    begin
      if lt in tts then
      begin
        if Result <> '' then
          Result := Result + ' ';
        Result := Result +  TokenTypeToString(lt);
      end;
    end;
  end;
end;


end.