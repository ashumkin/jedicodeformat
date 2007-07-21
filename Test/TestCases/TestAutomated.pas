unit TestAutomated;
{ AFS 15 July 2007

 This unit compiles but is not semantically meaningfull
 it is test cases for the code formatting utility

 This unit tests use of the "automated" keyword
}

interface

type
  TPublicAndAutomated = class
  public
    function PublicMethod: boolean;
  automated
    function AuomatedMethod: WordBool; dispid 1;
  end;

  TAutomatedOnly = class
  automated
    function AuomatedMethod: WordBool; dispid 1;
  end;

implementation

{ TMyClass }

function TPublicAndAutomated.AuomatedMethod: WordBool;
begin
  Result := true;
end;

function TPublicAndAutomated.PublicMethod: boolean;
begin
  Result := true;
end;

{ TAutomatedOnly }

function TAutomatedOnly.AuomatedMethod: WordBool;
begin
  Result := true;
end;

end.
