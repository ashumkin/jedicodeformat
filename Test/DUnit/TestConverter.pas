unit TestConverter;

{ AFS 18 Jan 2003
  Processor that can specify just one process, not all
  to run
}

interface

uses StringsConverter, BaseVisitor;

type
  TTestConverter = class(TStringsConverter)
  private
    fbRunAll: boolean;
    fcSingleProcess: TTreeNodeVisitorType;

  protected
    procedure ApplyProcesses; override;


  public
    constructor Create;

    property RunAll: boolean read fbRunAll write fbRunAll;
    property SingleProcess: TTreeNodeVisitorType read fcSingleProcess write fcSingleProcess;

  end;

implementation

constructor TTestConverter.Create;
begin
  inherited;
  RunAll := True;
  fcSingleProcess := nil;
end;


procedure TTestConverter.ApplyProcesses;
var
  lcProcess: TBaseTreeNodeVisitor;
begin
  if RunAll then
    inherited
  else
  begin
    lcProcess := SingleProcess.Create;
    try
      GetRoot.VisitTree(lcProcess);
    finally
      lcProcess.Free;
    end;
  end;
end;

end.
