unit RebreakLines;

{ AFS 29 December 2002

  Obfucate process
  break lines at regular intervals
}

interface

uses SwitchableVisitor, VisitParseTree;

type
  TRebreakLines = class(TSwitchableVisitor)
  private
    xPos: integer;
  protected
    procedure EnabledVisitSourceToken(const pcNode: TObject; var prVisitResult: TRVisitResult); override;
  public
    constructor Create; override;
  end;

implementation

uses
  JclStrings,
  SourceToken, TokenType, FormatFlags;

constructor TRebreakLines.Create;
begin
  inherited;
  FormatFlags := FormatFlags + [eObfuscate];
  xPos := 1;
end;

procedure TRebreakLines.EnabledVisitSourceToken(const pcNode: TObject; var prVisitResult: TRVisitResult);
var
  lcToken: TSourceToken;
  lcNext, lcNew: TSourceToken;
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
      { no space directly after the new return }
      lcNext := lcToken.NextToken;
      if (lcNext <> nil) and (lcNext.TokenType = ttWhiteSpace) then
          lcNext.SourceCode := '';

      { need a return? }
      if (lcNext <> nil) and (lcNext.TokenType <> ttReturn) then
      begin
        prVisitResult.Action := aInsertAfter;

        lcNew := TSourceToken.Create;
        lcNew.TokenType := ttReturn;
        lcNew.SourceCode := AnsiLineBreak;
        XPos := 0;

        prVisitResult.NewItem := lcNew;
      end;
    end
    else
      // not at enhd of line yet 
      xPos := xPos + liLen;
  end;
end;

end.
