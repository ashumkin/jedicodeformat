unit TabToSpace;

interface

{ AFS 4 Jan 2002
  convert tabs to spaces }

uses SwitchableVisitor, VisitParseTree;


type
  TTabToSpace = class(TSwitchableVisitor)
  private
    fsSpaces: string;

  public
    constructor Create; override;

    procedure EnabledVisitSourceToken(const pcNode: TObject; var prVisitResult: TRVisitResult); override;
  end;


implementation

uses
  SysUtils,
  JclStrings,
  JcfSettings, SourceToken, TokenType, FormatFlags;

constructor TTabToSpace.Create;
begin
  inherited;
  fsSpaces := StrRepeat(AnsiSpace, Settings.Spaces.SpacesPerTab);
  FormatFlags := FormatFlags + [eAddSpace, eRemoveSpace];
end;

procedure TTabToSpace.EnabledVisitSourceToken(const pcNode: TObject; var prVisitResult: TRVisitResult);
var
  lcSourceToken: TSourceToken;
  ls: string;
begin
  lcSourceToken := TSourceToken(pcNode);

  if (lcSourceToken.TokenType <> ttWhiteSpace) then
    exit;

  { can't pass property as var parameter so ls local var is used }
  ls := lcSourceToken.SourceCode;
  StrReplace(ls, AnsiTab, fsSpaces, [rfReplaceAll]);
  lcSourceToken.SourceCode := ls;
end;

end.
