unit ReturnChars;

interface

uses SourceToken, SwitchableVisitor, VisitParseTree;


type
  TReturnChars = class(TSwitchableVisitor)
    protected
      procedure EnabledVisitSourceToken(const pcNode: TObject; var prVisitResult: TRVisitResult); override;

    public
      constructor Create; override;
  end;

implementation

uses
  JclStrings,
  FormatFlags, TokenType, WordMap, ParseTreeNodeType, TokenUtils, JcfSettings;

{ TReturnChars }

constructor TReturnChars.Create;
begin
  inherited;

end;

procedure TReturnChars.EnabledVisitSourceToken(const pcNode: TObject;
  var prVisitResult: TRVisitResult);
var
  lcSourceToken: TSourceToken;
begin
  lcSourceToken := TSourceToken(pcNode);

  if  (lcSourceToken.TokenType <> ttReturn) then
    exit;

  case FormatSettings.Returns.ReturnChars of
    rcLeaveAsIs:
    begin
     // leave as is
    end;
    rcLinefeed:
    begin
      // easy case - CrLf with Lf
      lcSourceToken.SourceCode := AnsiLineFeed;
    end;
    rcCrLf:
    begin
      lcSourceToken.SourceCode := AnsiCrLf;
    end;
    rcPlatform:
    begin
      // AnsiLineBreak is set to the right value at compile time
      lcSourceToken.SourceCode := AnsiLineBreak;
    end;

  end;
end;

end.
