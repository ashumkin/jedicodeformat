unit TestDelphiNetRecordProcs;

interface

type
  TSomeRecord = record
    i,j,k,l: integer;

    function Sum: integer;
    procedure Clear;

    constructor Create(iValue: integer);
  end;

implementation


{ TSomeRecord }

procedure TSomeRecord.Clear;
begin
  i := 0;
  j := 0;
  k := 0;
  l := 0;
end;

constructor TSomeRecord.Create(iValue: integer);
begin
  Clear;
  i := iValue;
end;

function TSomeRecord.Sum: integer;
begin
  Result := i + j + k + l;
end;

end.
