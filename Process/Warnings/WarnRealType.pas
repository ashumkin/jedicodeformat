unit WarnRealType;

{ AFS 30 Dec 2002

  simple warner - these types are obsolete
}

interface


uses Warning, VisitParseTree;

type

  TWarnRealType = class(TWarning)
    public
      procedure VisitSourceToken(const pcToken: TObject; var prVisitResult: TRVisitResult); override;
  end;

implementation

uses SourceToken, ParseTreeNodeType, WordMap;

procedure TWarnRealType.VisitSourceToken(const pcToken: TObject; var prVisitResult: TRVisitResult);
const
  REAL_WARNING = ' This type is obsolete and is seldom useful. ' +
    'See the help for details';
var
  lcToken: TSourceToken;
begin
  lcToken := TSourceToken(pcToken);

  if not lcToken.HasParentNode(nType) then
    exit;

  { see delphi help on 'real' for details.
   I don't know any reason to prefer these types to 'Double'

   If the code was orignally Delphi V1, then it may be better of as "Currency"
   }

  if lcToken.word = wReal then
  begin
    SendWarning(lcToken, 'Real type used.' + REAL_WARNING);
  end
  else if lcToken.word = wReal48 then
  begin
    SendWarning(lcToken, 'Real48 type used.' + REAL_WARNING);
  end;

end;

end.
