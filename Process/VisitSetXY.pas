unit VisitSetXY;

{ A visitor to set the X and Y coordinates of each token
  based on keeping a running count of the text length and number of newlines }

interface

uses BaseVisitor, VisitParseTree;

type
  TVisitSetXY = class(TBaseTreeNodeVisitor)
  private
    // running totals
    fiX, fiY: integer;
  public
    constructor Create; override;

    procedure VisitSourceToken(const pcToken: TObject; var prVisitResult: TRVisitResult); override;

  end;

implementation

uses
  JcfMiscFunctions, SourceToken;

{ TVisitSetXY }


constructor TVisitSetXY.Create;
begin
  // text coords start at 1,1
  fiX := 1;
  fiY := 1;
end;

procedure TVisitSetXY.VisitSourceToken(const pcToken: TObject; var prVisitResult: TRVisitResult);
var
  lcToken: TSourceToken;
begin
  lcToken := TSourceToken(pcToken);

  lcToken.XPosition := fiX;
  lcToken.YPosition := fiY;

  // keep count
  AdvanceTextPos(lcToken.SourceCode, fiX, fiY);
end;

end.
