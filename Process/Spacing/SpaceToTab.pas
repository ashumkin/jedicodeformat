unit SpaceToTab;

interface

{ AFS 4 Jan 2002
  convert spaces tabs }

uses SwitchableVisitor, VisitParseTree;


type
  TSpaceToTab = class(TSwitchableVisitor)
  private
    fsSpaces: string;

  protected
    procedure EnabledVisitSourceToken(const pcNode: TObject; var prVisitResult: TRVisitResult); override;
  public
    constructor Create; override;
  end;

implementation

uses
  SysUtils,
  JclStrings,
  JcfSettings, SourceToken, TokenType, FormatFlags;

constructor TSpaceToTab.Create;
begin
  inherited;
  fsSpaces := StrRepeat(AnsiSpace, FormatSettings.Spaces.SpacesForTab);
  FormatFlags := FormatFlags + [eAddSpace, eRemoveSpace];
end;

procedure TSpaceToTab.EnabledVisitSourceToken(const pcNode: TObject; var prVisitResult: TRVisitResult);
var
  lcSourceToken: TSourceToken;
  ls, lsTab: string;
begin
  lcSourceToken := TSourceToken(pcNode);

  if (lcSourceToken.TokenType <> ttWhiteSpace) then
    exit;

  { can't pass property as var parameter so ls local var is used }
  ls := lcSourceToken.SourceCode;
  lsTab := AnsiTab;
  StrReplace(ls, fsSpaces, lsTab, [rfReplaceAll]);
  lcSourceToken.SourceCode := ls;
end;

end.
