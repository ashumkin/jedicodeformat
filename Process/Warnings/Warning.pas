unit Warning;

interface

uses BaseVisitor, ConvertTypes;

type
  TWarning = class(TBaseTreeNodeVisitor)
    private
      fOnWarning: TStatusMessageProc;

    protected
      procedure SendWarning(const pcNode: TObject; const psMessage: string);

    public
      property OnWarning: TStatusMessageProc read fOnWarning write fOnWarning;
  end;


implementation

uses ParseTreeNode, SourceToken;

procedure TWarning.SendWarning(const pcNode: TObject; const psMessage: string);
var
  lsMessage: string;
  lcToken: TSourceToken;
begin
  lsMessage := psMessage;
  if (pcNode is TSourceToken) then
  begin
    lcToken := TSourceToken(pcNode);
    lsMessage := lsMessage  + ' near ' + lcToken.Describe +  ' ' + lcToken.DescribePosition;
  end
  else if (pcNode is TParseTreeNode) then
  begin
    // use first token under this node for pos
    lcToken := TParseTreeNode(pcNode).FirstSolidLeaf as TSourceToken;

    if lcToken <> nil then
       lsMessage := lsMessage  + ' near ' + lcToken.Describe + ' ' + lcToken.DescribePosition;
  end;

  if Assigned(fOnWarning) then
    fOnWarning(lsMessage);
end;



end.
