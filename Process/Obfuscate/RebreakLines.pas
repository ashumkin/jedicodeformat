unit RebreakLines;

{ AFS 29 December 2002

  Obfucate process
  break lines at regular intervals
}

interface

uses BaseVisitor, VisitParseTree;

type
  TRebreakLines = class(TBaseTreeNodeVisitor)
    private
      xPos: integer;
    public
      constructor Create; override;
      
      procedure VisitSourceToken(const pcNode: TObject; var prVisitResult: TRVisitResult); override;
  end;

implementation

uses
  JclStrings,
  SourceToken, TokenType;

constructor TRebreakLines.Create;
begin
  inherited;
  xPos := 1;
end;

procedure TRebreakLines.VisitSourceToken(const pcNode: TObject; var prVisitResult: TRVisitResult);
var
  lcToken: TSourceToken;
  lcNew: TSourceToken;
  liLen: integer;
begin
  lcToken := TSourceToken(pcNode);


  if lcToken.TokenType = ttReturn then
    xPos := 0
  else
  begin
    liLen := Length(lcToken.SourceCode);

    if (XPos + liLen) > 80 then
    begin
      prVisitResult.Action := aInsertAfter;

      lcNew := TSourceToken.Create;
      lcNew.TokenType := ttReturn;
      lcNew.SourceCode := AnsiLineBreak;
      XPos := 0;

      prVisitResult.NewItem := lcNew;
    end
    else
      xPos := xPos + liLen;
  end;
end;

end.
