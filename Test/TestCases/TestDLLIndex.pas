unit TestDllIndex;

interface

uses WIndows;

function Spaces(n : Cardinal): String; 
function RestartDialog(Wnd: HWnd; Reason: PChar; Flags: Integer): Integer; stdcall; external 'shell32.dll' index 59; 

implementation

uses SysUtils;

{*------------------------------------------- 
Returns a string filled with spaces 
 
@param n Number of spaces expected 
@return Space filled string 
---------------------------------------------} 
function Spaces(n : Cardinal): String; 
begin 
Result := IntToStr(n);
end; 

end.
