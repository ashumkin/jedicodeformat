unit BaseTestProcess;

interface

uses
  Classes,
  JclStrings,
  TestFrameWork,
  TestConverter, BaseVisitor;

type
  TBaseTestProcess = class(TTestCase)
  private
    fcConvert: TTestConverter;
    fcInput, fcOutput, fcMessages: TStringList;

  protected

    procedure TestNoWarnings(const psUnit: string);
    procedure TestWarnings(const psUnit: string; const psWarningMatch: string); overload;
    procedure TestWarnings(const psUnit: string; const psWarningMatches: array of string); overload;
    procedure TestWarnings(const psUnit: string; const piMatchCount: integer; const psWarningMatches: array of string); overload;

    procedure TestProcessResult(processType: TTreeNodeVisitorType; const psIn, psOut: string);

  protected
    procedure Setup; override;
    procedure TearDown; override;
  published

 end;

const
  UNIT_HEADER = 'unit Test; interface implementation ';
  INTERFACE_HEADER = 'unit Test; interface ';
  UNIT_FOOTER = ' end.';

implementation

uses SysUtils;

procedure TBaseTestProcess.Setup;
begin
  fcConvert := TTestConverter.Create;

  fcInput := TStringList.Create;
  fcOutput := TStringList.Create;
  fcMessages := TStringList.Create;

  fcConvert.InputStrings := fcInput;
  fcConvert.OutputStrings := fcOutput;
  fcConvert.MessageStrings := fcMessages;
end;

procedure TBaseTestProcess.TearDown;
begin
  FreeAndNil(fcConvert);

  FreeAndNil(fcInput);
  FreeAndNil(fcOutput);
  FreeAndNil(fcMessages);
end;

procedure TBaseTestProcess.TestWarnings(const psUnit, psWarningMatch: string);
begin
  TestWarnings(psUnit, 1, [psWarningMatch]);
end;

procedure TBaseTestProcess.TestWarnings(const psUnit: string;
  const psWarningMatches: array of string);
var
  liCount: integer;
begin
  liCount := High(psWarningMatches) - Low(psWarningMatches) + 1;
  TestWarnings(psUnit, liCount, psWarningMatches);
end;

procedure TBaseTestProcess.TestWarnings(const psUnit: string;
  const piMatchCount: integer; const psWarningMatches: array of string);
var
  lbFound: boolean;
  liLoop: integer;
begin
  fcInput.Text := psUnit;

  fcConvert.Convert;

  // convert should work
  CheckEquals(False, fcConvert.ConvertError, fcConvert.ConvertErrorMessage);
  // with messages
  CheckEquals(piMatchCount, fcMessages.Count, 'Wrong number of messages');

  for liLoop := Low(psWarningMatches) to High(psWarningMatches) do
  begin
    // containing certain text
    lbFound := (StrIPos(psWarningMatches[liLoop], fcMessages.Text) > 0);

    Check(lbFound, psWarningMatches[liLoop] + ' was not found in output ' + fcMessages.Text);
  end;

  fcConvert.Clear;
end;

procedure TBaseTestProcess.TestNoWarnings(const psUnit: string);
begin
  TestWarnings(psUnit, 0, []);
end;

procedure TBaseTestProcess.TestProcessResult(processType: TTreeNodeVisitorType; const psIn, psOut: string);
var
  lsOut: string;
begin
  // run just this process
  fcInput.Text := psIn;
  fcConvert.RunAll := False;
  fcConvert.SingleProcess := processType;
  try
    fcConvert.Convert;
  finally
    fcConvert.RunAll := True;
    fcConvert.SingleProcess := nil;
  end;

  lsOut := Trim(fcOutput.Text);
  CheckEquals(psOut, lsOut, 'Bad result text');
end;

end.
