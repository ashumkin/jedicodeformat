unit TestCondCompBreaks;

{ AFS 11 August 2005
 This unit compiles but is not semantically meaningfull
 it is test cases for the code formatting utility

 Testing line breaking around conditional compilation
}

interface

implementation

uses SysUtils, {$IFDEF BAR_RAISED} Classes, {$ENDIF} Dialogs;

{$IFDEF SYM2} const
  SOCK_IT_TO_ME = 'Black socks'; {$ENDIF}

procedure SomeStuff;
begin
  ShowMessage('Some stuff...');
end;

procedure MoreStuff;
begin
  {$IFDEF HAS_STUFF}
  SomeStuff;
  {$ENDIF}
end;

end.
 