unit Capitalisation;

{ AFS 30 December 2002
  visitor to do capitalisation according to settings
}

interface

uses BaseVisitor, VisitParseTree;


type
  TCapitalisation = class(TBaseTreeNodeVisitor)
    public
      procedure VisitSourceToken(const pcNode: TObject; var prVisitResult: TRVisitResult); override;
  end;

implementation

uses
  SysUtils,
  JclStrings,
  SourceToken, TokenType, ParseTreeNodeType, JcfSettings;

{ TCapitalisation }

procedure FixCaps(const pct: TSourceToken; const caps: TCapitalisationType);
begin
  if pct = nil then
    exit;
  if pct.SourceCode = '' then
    exit;

  case caps of
    ctUpper:
      pct.SourceCode := AnsiUpperCase(pct.SourceCode);
    ctLower:
      pct.SourceCode := AnsiLowerCase(pct.SourceCode);
    ctMixed:
      pct.SourceCode := StrSmartCase(pct.SourceCode, []);
    ctLeaveAlone:
      ;
  end;
end;

procedure TCapitalisation.VisitSourceToken(const pcNode: TObject; var prVisitResult: TRVisitResult);
var
  lcSourceToken: TSourceToken;
begin
  lcSourceToken := TSourceToken(pcNode);

  case lcSourceToken.TokenType of
    ttReservedWord:
      FixCaps(lcSourceToken, Settings.Caps.ReservedWords);
    ttReservedWordDirective:
    begin
      { directives can occur in other contexts - they are valid proc & variable names
        so we need to know if this one was parsed as a directive }
      if lcSourceToken.HasParentNode(DirectiveNodes) then
        FixCaps(lcSourceToken, Settings.Caps.Directives);
    end;
    ttBuiltInConstant:
      FixCaps(lcSourceToken, Settings.Caps.Constants);
    ttOperator:
      FixCaps(lcSourceToken, Settings.Caps.Operators);
    ttBuiltInType:
      FixCaps(lcSourceToken, Settings.Caps.Types);
  end;
end;

end.
