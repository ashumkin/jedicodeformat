unit BasicStats;

{
  AFS 25 March 03
  Basic stats on the unit
  Proof of concept to do crude code analysis on the unit

  Since it doesn't alter anything and wants to read the entire unit
  it is not a switchable visitor }

interface

uses BaseVisitor, VisitParseTree;


type
  TBasicStats = class(TBaseTreeNodeVisitor)
  private
    fiTotalTokens, fiTotalChars: integer;

    // tokens can be divided into 3 categories - comments, spaces&rets, code
    fiSpaceTokens, fiCommentTokens, fiSolidTokens: integer;
    fiSpaceChars, fiCommentChars, fiSolidChars: integer;

    fiLines: integer;

    fiConsts, fiTypes, fiClasses, fiInterfaces, fiAllProcs: integer;
    liInterfaceGlobalVars, liGlobalVars: integer;
    fiInterfaceProcs: integer;

  protected
  public

    procedure PreVisitParseTreeNode(const pcNode: TObject; var prVisitResult: TRVisitResult); override;
    procedure VisitSourceToken(const pcNode: TObject; var prVisitResult: TRVisitResult); override;
    function FinalSummary(var psMessage: string): Boolean; override;

    function IsIncludedInSettings: boolean; override;
  end;

implementation

uses
  { delphi }
  SysUtils,
  JclStrings,
  { JCF  }
  SourceToken, TokenType, ParseTreeNode, ParseTreeNodeType, TokenUtils,
  JcfSettings;

function DisplayFloat(const ex: extended): string;
begin
  Result := FloatToStrF(ex, ffNumber, 9, 2);
end;

function DisplayRatio(const exNum, exDenom: extended): string;
begin
  if exDenom = 0 then
    Result := '-'
  else
    Result := DisplayFloat(exNum / exDenom);
end;

function DisplayPercent(const exNum, exDenom: extended): string;
begin
  if exDenom = 0 then
    Result := '-'
  else
    Result := DisplayFloat(exNum * 100 / exDenom) + '%';
end;


procedure TBasicStats.PreVisitParseTreeNode(const pcNode: TObject;
  var prVisitResult: TRVisitResult);
var
  lcNode: TParseTreeNode;
begin
  lcNode := TParseTreeNode(pcNode);

  case lcNode.NodeType of
    nTypeDecl:
      inc(fiTypes);
    nConstDecl:
      inc(fiConsts);
    nClassType:
      inc(fiClasses);
    nInterfaceType:
      inc(fiInterfaces);
    else ; // no nothing
  end;

  { procedure/fn/constructor/destructor }
  if (lcNode.NodeType in ProcedureNodes) and lcNode.HasChildNode(nBlock) then
    { if it has a block of code under it, it counts as a proc }
    inc(fiAllProcs);

  { can only export a global procedure or function - not a constructor or destructor
    don't count procs in a class or itnerface def }
  if (lcNode.NodeType in [nFunctionHeading, nProcedureHeading]) and
   lcNode.HasParentNode(nInterfaceSection) and
   (not lcNode.HasParentNode([nClassType, nInterfaceType, nProcedureType])) then
   inc(fiInterfaceProcs);

  // find global vars
  if (lcNode.NodeType = nVarDecl) and (not lcNode.HasParentNode(nClassType)) and
    (not lcNode.HasParentNode(nblock)) then
  begin

    if lcNode.HasParentNode(nInterfaceSection) then
      liInterfaceGlobalVars := liInterfaceGlobalVars + VarIdentCount(lcNode)
    else
      liGlobalVars := liGlobalVars + VarIdentCount(lcNode);
  end;

end;

procedure TBasicStats.VisitSourceToken(const pcNode: TObject;
  var prVisitResult: TRVisitResult);
var
  lcSourceToken: TSourceToken;
  liLen: integer;
begin
  lcSourceToken := TSourceToken(pcNode);

  // a file with no returns has one line
  if (fiLines = 0) then
    fiLines := 1;

  inc (fiTotalTokens);
  liLen := Length(lcSourceToken.SourceCode);
  fiTotalChars := fiTotalChars + liLen;

  case lcSourceToken.TokenType of
    ttComment:
    begin
      inc(fiCommentTokens);
      fiCommentChars := fiCommentChars + liLen;

      fiLines := fiLines + StrStrCount(lcSourceToken.SourceCode, AnsiLineBreak);
    end;
    ttReturn:
    begin
      inc(fiLines);
      inc(fiSpaceTokens);
      fiSpaceChars := fiSpaceChars + liLen;
    end;
    ttWhiteSpace:
    begin
      inc(fiSpaceTokens);
      fiSpaceChars := fiSpaceChars + liLen;
    end;
    else
    begin
      inc(fiSolidTokens);
      fiSolidChars := fiSolidChars + liLen;
    end;
  end;
end;

function TBasicStats.FinalSummary(var psMessage: string): Boolean;
begin
  Result := True;

  psMessage := AnsiLineBreak + 'Basic numbers and averages: ' + AnsiLineBreak +
    'Unit is ' + IntToStr(fiLines) + ' lines long' +  AnsiLineBreak +
    'Unit has ' + IntToStr(fiTotalTokens) + ' tokens in '  +
    IntToStr(fiTotalChars) + ' characters: ' +  AnsiLineBreak +
    DisplayRatio(fiTotalChars, fiTotalTokens) + ' chars per token' + AnsiLineBreak +
    DisplayRatio(fiTotalChars, fiLines) + ' chars per line ' +  AnsiLineBreak +
    DisplayRatio(fiTotalTokens, fiLines) + ' tokens per line ' +  AnsiLineBreak + AnsiLineBreak;

  psMessage := psMessage +
    IntToStr(fiCommentTokens) + ' comments in ' + IntToStr(fiCommentChars) + ' characters ' + AnsiLineBreak +
    DisplayRatio(fiCommentChars, fiCommentTokens) + ' chars per comment' + AnsiLineBreak +
    DisplayPercent(fiCommentChars, fiTotalChars) + ' of chars are comments ' + AnsiLineBreak + AnsiLineBreak;

  psMessage := psMessage +
    IntToStr(fiSpaceTokens) + ' spacing and return tokens in ' + IntToStr(fiSpaceChars) + ' characters ' +   AnsiLineBreak +
    DisplayRatio(fiSpaceChars, fiSpaceTokens) + ' chars per token' + AnsiLineBreak +
    DisplayPercent(fiSpaceChars, fiTotalChars) + ' of chars are spacing ' + AnsiLineBreak + AnsiLineBreak;

  psMessage := psMessage +
    IntToStr(fiSolidTokens) + ' solid tokens in ' + IntToStr(fiSolidChars) + ' characters ' +  AnsiLineBreak +
    DisplayRatio(fiSolidChars, fiSolidTokens) + ' chars per token' + AnsiLineBreak +
    DisplayPercent(fiSolidChars, fiTotalChars) + ' of chars are solid' + AnsiLineBreak +
    DisplayPercent(fiSolidTokens, fiTotalTokens) + ' of tokens are solid' + AnsiLineBreak + AnsiLineBreak;

  psMessage := psMessage +
    IntToStr(fiConsts) + ' constants ' + AnsiLineBreak +
    IntToStr(fiTypes) + ' types ' + AnsiLineBreak +
    IntToStr(fiClasses) + ' classes ' + AnsiLineBreak +
    IntToStr(fiInterfaces) + ' interfaces ' + AnsiLineBreak +
    IntToStr(fiAllProcs) + ' procedures ' + AnsiLineBreak + AnsiLineBreak;

  psMessage := psMessage +
    IntToStr(liInterfaceGlobalVars) + ' global vars in interface ' + AnsiLineBreak +
    IntToStr(liGlobalVars) + ' global vars in rest of unit ' + AnsiLineBreak +
    IntToStr(fiInterfaceProcs) + ' procedures in interface ' + AnsiLineBreak + AnsiLineBreak;

end;


function TBasicStats.IsIncludedInSettings: boolean;
begin
  Result := FormatSettings.Log.LogStats;
end;

end.
