unit VisitSetXY;

{ A visitor to set the X and Y coordinates of each token
  based on keeping a running count of the text length and number of newlines }

interface

uses BaseVisitor, VisitParseTree;

type
  TVisitSetXY = class(TBaseTreeNodeVisitor)
  private
    // running totals of x and Y pos, and count of solid tokens on the line
    fiX, fiY, fiSolidTokenOnLineIndex: integer;
  public
    constructor Create; override;

    procedure VisitSourceToken(const pcToken: TObject; var prVisitResult: TRVisitResult); override;

  end;

implementation

uses
  JclStrings,
  JcfMiscFunctions, SourceToken, TokenType;

{ TVisitSetXY }


constructor TVisitSetXY.Create;
begin
  // text coords start at 1,1
  fiX := 1;
  fiY := 1;
  fiSolidTokenOnLineIndex := 0;
end;

procedure TVisitSetXY.VisitSourceToken(const pcToken: TObject; var prVisitResult: TRVisitResult);
var
  lcToken: TSourceToken;
begin
  lcToken := TSourceToken(pcToken);

  lcToken.XPosition := fiX;
  lcToken.YPosition := fiY;
  lcToken.SolidTokenOnLineIndex := fiSolidTokenOnLineIndex;

  if lcToken.TokenType = ttReturn then
    fiSolidTokenOnLineIndex := 0
  else if (lcToken.TokenType = ttComment) and (Pos(AnsiLineBreak, lcToken.SourceCode) > 0) then
    fiSolidTokenOnLineIndex := 0
  else if lcToken.IsSolid then
    inc(fiSolidTokenOnLineIndex);

  // keep count
  AdvanceTextPos(lcToken.SourceCode, fiX, fiY);
end;

end.
