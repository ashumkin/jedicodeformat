unit TabToSpace;

interface

{ AFS 4 Jan 2002
  convert tabs to spaces }

uses BaseVisitor, VisitParseTree;


type
  TTabToSpace = class(TBaseTreeNodeVisitor)
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

constructor TTabToSpace.Create;
begin
  inherited;
  // reset the local var - setings may have changed
  fsSpaces := '';
  fbInitialised := False;

end;

procedure TTabToSpace.VisitSourceToken(const pcNode: TObject; var prVisitResult: TRVisitResult);
var
  lcSourceToken: TSourceToken;
  ls: string;
begin
  lcSourceToken := TSourceToken(pcNode);

  if (lcSourceToken.TokenType <> ttWhiteSpace) then
    exit;

  { set up spaces }
  if not fbInitialised then
  begin
    fsSpaces := StrRepeat(AnsiSpace, Settings.Spaces.SpacesPerTab);
    fbInitialised := True;
  end;

  { can't pass property as var parameter so ls local var is used }
  ls := lcSourceToken.SourceCode;
  StrReplace(ls, AnsiTab, fsSpaces, [rfReplaceAll]);
  lcSourceToken.SourceCode := ls;
end;

end.
