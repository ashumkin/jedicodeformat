unit VisitSetXY;

{ A visitor to set the X and Y coordinates of each token
  based on keeping a running count of the text length and number of newlines }

interface

uses BaseVisitor;

type
  TVisitSetXY = class(TBaseTreeNodeVisitor)
  private
    // running totals
    fiX, fiY: integer;
  public
    constructor Create;

    procedure VisitSourceToken(const pcToken: TObject); override;

  end;

implementation

uses
  MiscFunctions, SourceToken;

{ TVisitSetXY }


constructor TVisitSetXY.Create;
begin
  // text coords start at 1,1
  fiX := 1;
  fiY := 1;
end;

procedure TVisitSetXY.VisitSourceToken(const pcToken: TObject);
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
