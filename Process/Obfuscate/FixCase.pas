unit FixCase;

{ AFS 29 December 2002
  Obfucscate - capitalisation
}

interface

uses BaseVisitor, VisitParseTree;

type
  TFixCase = class(TBaseTreeNodeVisitor)
  public
    procedure VisitSourceToken(const pcNode: TObject; var prVisitResult: TRVisitResult); override;
  end;


implementation

uses
  { delphi } SysUtils,
  { jcl } JclStrings,
  { local } SourceToken, TokenType, ParseTreeNodeType, JcfSettings;


{ identify the cases where the compiler is case sensitive
  don't want ot mess with caps when it affects these issues }
function PutUpWithCompilerBugs(const pt: TSourceToken): boolean;
begin
  Result := False;

  { special case - 'Register' (with a capital R) as a procedure name must be preserved
    or component registration may not work in some versions of Delphi
    This is a known issue in some versions of Delphi
    note intentional use of case-sensitive compare }
  if (pt.TokenType = ttWord) and
    AnsiSameStr(pt.SourceCode, 'Register') then
      //!!! and (pt.HasParent(tnProcedureHeading) then
  begin
    Result := True;
    exit;
  end;

  { had problems - IDE could not find the base class frame
    when the frame's ancestor's name was decapitised
    most likely some lazy developer @ borland forgot to match strings without case}
  if (pt.TokenType = ttWord) and (pt.HasParentNode(nClassHeritage)) then
  begin
    Result := True;
    exit;
  end;
end;


procedure FixCaps(const pt: TSourceToken);
begin
  if PutUpWithCompilerBugs(pt) then
    exit;

  case FormatSettings.Obfuscate.Caps of
    ctUpper:
      pt.SourceCode := AnsiUpperCase(pt.SourceCode);
    ctLower:
      pt.SourceCode := AnsiLowerCase(pt.SourceCode);
    ctMixed:
      pt.SourceCode := StrSmartCase(pt.SourceCode, []);
    ctLeaveAlone:;
  end;
end;


procedure TFixCase.VisitSourceToken(const pcNode: TObject; var prVisitResult: TRVisitResult);
var
  lcSourceToken: TSourceToken;
begin
  lcSourceToken := TSourceToken(pcNode);

  if (lcSourceToken.TokenType in TextualTokens) and (lcSourceToken.SourceCode <> '') then
    FixCaps(lcSourceToken);
end;

end.
