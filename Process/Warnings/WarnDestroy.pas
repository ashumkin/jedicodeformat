unit WarnDestroy;

{ AFS 30 December 2002

 warn of calls to obj.destroy;
}


interface

uses Warning, VisitParseTree;

type

  TWarnDestroy = class(TWarning)
    public
      procedure VisitSourceToken(const pcToken: TObject; var prVisitResult: TRVisitResult); override;
  end;

implementation

uses
  { delphi } SysUtils,
  { local } SourceToken, ParseTreeNodeType;

procedure TWarnDestroy.VisitSourceToken(const pcToken: TObject; var prVisitResult: TRVisitResult);
var
  lcToken: TSourceToken;
begin
  lcToken := TSourceToken(pcToken);

  { look in statements }
  if not lcToken.HasParentNode(nBlock) then
    exit;

  if AnsiSameText(lcToken.SourceCode, 'destroy') then
  begin
    SendWarning(lcToken, 'Destroy should not normally be called. ' +
        'You may want to use FreeAndNil(MyObj), or MyObj.Free, or MyForm.Release');
  end;

end;

end.
