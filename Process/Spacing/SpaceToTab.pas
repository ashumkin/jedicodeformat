unit SpaceToTab;

interface

{ AFS 4 Jan 2002
  convert spaces tabs }

uses BaseVisitor, VisitParseTree;


type
  TSpaceToTab = class(TBaseTreeNodeVisitor)
  private
    fsSpaces: string;
    fbInitialised: boolean;
  public
    constructor Create; override;

    procedure VisitSourceToken(const pcNode: TObject; var prVisitResult: TRVisitResult); override;
  end;

implementation

uses
  SysUtils,
  JclStrings,
  JcfSettings, SourceToken, TokenType;

constructor TSpaceToTab.Create;
begin
  inherited;
  fbInitialised := False;
  fsSpaces := '';
end;

procedure TSpaceToTab.VisitSourceToken(const pcNode: TObject; var prVisitResult: TRVisitResult);
var
  lcSourceToken: TSourceToken;
  ls, lsTab: string;
begin
  lcSourceToken := TSourceToken(pcNode);

  if (lcSourceToken.TokenType <> ttWhiteSpace) then
    exit;

  { set up spaces }
  if not fbInitialised then
  begin
    fsSpaces := StrRepeat(AnsiSpace, Settings.Spaces.SpacesForTab);
    fbInitialised := True;
  end;

  { can't pass property as var parameter so ls local var is used }
  ls := lcSourceToken.SourceCode;
  lsTab := AnsiTab;
  StrReplace(ls, fsSpaces, lsTab, [rfReplaceAll]);
  lcSourceToken.SourceCode := ls;
end;

end.
