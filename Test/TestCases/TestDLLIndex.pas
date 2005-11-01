unit TestDllIndex;

interface

function Spaces(n : Cardinal): String; 
function RestartDialog(Wnd: HWnd; Reason: PChar; Flags: Integer): Integer; stdcall; external 'shell32.dll' index 59; 

implementation

{*------------------------------------------- 
Returns a string filled with spaces 
 
@param n Number of spaces expected 
@return Space filled string 
---------------------------------------------} 
function Spaces(n : Cardinal): String; 
begin 
Result := n; 
end; 

end.
