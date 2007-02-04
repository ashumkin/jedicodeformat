unit TestDelphiNetKeywords;
{
  This code compiles, but is not semantically meaningfull.
  It is test cases for the code-formating utility
  
  these are actually directives not full-blown keywords
  and so are valid for other uses in other contexts
  as below.
  This is not recommended
}

interface

implementation

procedure DelphiNetKeywordAbuse;
var
  operator: integer;
  helper: string;
  sealed, static: char;
begin
  operator := 12;
  sealed := 'A';
  static := 'b';
  helper := sealed + static;
  inc(operator);
end;

end.
