unit SwitchableVisitor;

{ AFS 22 Feb 02
  this visitor respects the special comments "//jcf:"
  that can turn sertain processes off and on again
}

interface

uses BaseVisitor, VisitParseTree, FormatFlags;

type

  TSwitchableVisitor = class(TBaseTreeNodeVisitor)
  private
    // is this processs on?
    fbEnabled: Boolean;
    // on/off flags that this processor responds to
    feFormatFlags: TFormatFlags;

  protected
    // enabled state may be changed by this token
    procedure CheckEnabled(const pcToken: TObject); virtual;

    // every token is inspected, even when the visitor is disabled
    procedure InspectSourceToken(const pcToken: TObject); virtual;
    // this is only called when the processor is enabled
    procedure EnabledVisitSourceToken(const pcToken: TObject; var prVisitResult: TRVisitResult); virtual;

  public

    procedure VisitSourceToken(const pcToken: TObject; var prVisitResult: TRVisitResult); override;

    constructor Create; override;

    property FormatFlags: TFormatFlags read feFormatFlags write feFormatFlags;
  end;

implementation

uses SourceToken, TokenType, ParseError;

constructor TSwitchableVisitor.Create;
begin
  inherited;
  fbEnabled := True;

  //by default, format unless alll processors are turned off
  feFormatFlags := [eAllFormat];
end;

procedure TSwitchableVisitor.CheckEnabled(const pcToken: TObject);
var
  lcToken: TSourceToken;
  leFlags: TFormatFlags;
  lsError: string;
  lbHasFlags: Boolean;
  lbOn: boolean;
begin
  lcToken := TSourceToken(pcToken);

  if lcToken.TokenType <> ttComment then
    exit;

  lbHasFlags := ReadCommentJcfFlags(lcToken.SourceCode, lsError, leFlags, lbOn);

  if not lbHasFlags then
    exit;
    
  if lsError <> '' then
    Raise TEParseError.Create(lsError, lcToken);

  // does this flag affect us? 
  if (FormatFlags * leFlags) <> [] then
    fbEnabled := lbOn;
end;


procedure TSwitchableVisitor.EnabledVisitSourceToken(const pcToken: TObject; var prVisitResult: TRVisitResult);
begin
  // here for override
end;

procedure TSwitchableVisitor.InspectSourceToken(const pcToken: TObject);
begin
  // here for override
end;

procedure TSwitchableVisitor.VisitSourceToken(const pcToken: TObject;
  var prVisitResult: TRVisitResult);
begin
  CheckEnabled(pcToken);

  InspectSourceToken(pcToken);

  if fbEnabled then
    EnabledVisitSourceToken(pcToken, prVisitResult);

end;

end.
