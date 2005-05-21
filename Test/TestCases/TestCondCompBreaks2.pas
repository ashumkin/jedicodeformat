unit TestCondCompBreaks;

{ AFS 11 August 2005
 This unit compiles but is not semantically meaningfull
 it is test cases for the code formatting utility

 Testing line breaking around conditional compilation
}

interface

implementation

{$IFDEF BAR_RAISED}
uses SysUtils;
{$ENDIF}

const
{$IFDEF SYM2} SOCK_IT_TO_ME = 'Black socks'; {$ENDIF}
  NO_SOCKS = 'No Sandals';

procedure SomeStuff;
begin
  ShowMessage('Some stuff...');
end;

{$IFDEF HAS_STUFF}
procedure MoreStuff;
begin
  SomeStuff;
end;
{$ENDIF}

end.
 