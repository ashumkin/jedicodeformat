unit SpecificWordCaps;

{ AFS 30 December 2002
    - fix capitalisation on specified words
}
interface

uses BaseVisitor, VisitParseTree;


type
  TSpecificWordCaps = class(TBaseTreeNodeVisitor)
    private
      fiCount: integer;
      lsLastChange: string;

    public
      constructor Create; override;

      procedure VisitSourceToken(const pcNode: TObject; var prVisitResult: TRVisitResult); override;

      { return true if you want the message logged}
      function FinalSummary(var psMessage: string): Boolean; override;
  end;

implementation

uses
  SysUtils,
  JclStrings,
  SourceToken, TokenType, ParseTreeNodeType, JcfSettings;


function Excluded(const pt: TSourceToken): boolean;
begin
  Result := False;

  { directives in context are excluded }
  if pt.HasParentNode(DirectiveNodes)  then
  begin
    Result := True;
    exit;
  end;

  { built in types that are actually being used as types are excluded
    eg.
    // this use of 'integer' is definitly the type
    var li: integer;

    // this use is definitely not
    function Integer(const ps: string): integer;

    // this use is ambigous
    li := Integer(SomeVar);

   user defined types are things that we often *want* to set a specific caps on
   so they are not excluded }

  if (pt.TokenType = ttBuiltInType) and (pt.HasParentNode(nType)) then
  begin
    Result := True;
    exit;
  end;
end;


{ TSpecificWordCaps }

constructor TSpecificWordCaps.Create;
begin
  inherited;
  fiCount      := 0;
  lsLastChange := '';
end;

function TSpecificWordCaps.FinalSummary(var psMessage: string): Boolean;
begin
  Result := (fiCount > 0);

  if Result then
  begin
    psMessage := 'Specific word caps: ';

    if fiCount = 1 then
      psMessage := psMessage + 'One change was made: ' + lsLastChange
    else
      psMessage := psMessage + IntToStr(fiCount) + ' changes were made';
  end;
end;

procedure TSpecificWordCaps.VisitSourceToken(const pcNode: TObject; var prVisitResult: TRVisitResult);
var
  lcSourceToken: TSourceToken;
  lsChange: string;
begin
  lcSourceToken := TSourceToken(pcNode);

  if Excluded(lcSourceToken) then
    exit;

  if Settings.SpecificWordCaps.HasWord(lcSourceToken.SourceCode) then
  begin
    // get the fixed version
    lsChange := Settings.SpecificWordCaps.FixWord(lcSourceToken.SourceCode);

    // case-sensitive test - see if anything to do.
    if AnsiCompareStr(lcSourceToken.SourceCode, lsChange) <> 0 then
    begin
      lsLastChange  := lcSourceToken.SourceCode + ' to ' + lsChange;
      lcSourceToken.SourceCode := lsChange;
      inc(fiCount);
    end;
  end;
end;

end.
