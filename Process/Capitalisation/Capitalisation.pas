unit Capitalisation;

{ AFS 30 December 2002
  visitor to do capitalisation according to settings
}

interface

uses SwitchableVisitor, VisitParseTree;


type
  TCapitalisation = class(TSwitchableVisitor)
    protected
      procedure EnabledVisitSourceToken(const pcNode: TObject; var prVisitResult: TRVisitResult); override;
    public
      constructor Create; override;
  end;

implementation

uses
  SysUtils,
  JclStrings,
  SourceToken, TokenType, ParseTreeNodeType, JcfSettings, FormatFlags;

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

constructor TCapitalisation.Create;
begin
  inherited;
  FormatFlags := FormatFlags + [eCapsReservedWord];
end;

procedure TCapitalisation.EnabledVisitSourceToken(const pcNode: TObject; var prVisitResult: TRVisitResult);
var
  lcSourceToken: TSourceToken;
begin
  lcSourceToken := TSourceToken(pcNode);

  case lcSourceToken.TokenType of
    ttReservedWord:
      FixCaps(lcSourceToken, FormatSettings.Caps.ReservedWords);
    ttReservedWordDirective:
    begin
      { directives can occur in other contexts - they are valid proc & variable names
        so we need to know if this one was parsed as a directive }
      if lcSourceToken.HasParentNode(DirectiveNodes) then
        FixCaps(lcSourceToken, FormatSettings.Caps.Directives);
    end;
    ttBuiltInConstant:
      FixCaps(lcSourceToken, FormatSettings.Caps.Constants);
    ttOperator:
      FixCaps(lcSourceToken, FormatSettings.Caps.Operators);
    ttBuiltInType:
      FixCaps(lcSourceToken, FormatSettings.Caps.Types);
  end;
end;

end.
