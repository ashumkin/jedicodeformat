unit TestDelphiNetUses;

{
  AFS May 2005
 This unit compiles but is not semantically meaningfull
 it is test cases for the code formatting utility

 Basic test of uses clause in Delphi.NET
}

interface

uses
Windows, Messages, SysUtils, Classes, Graphics,
Controls, Forms, Dialogs,
StdCtrls, DB, DBTables,
System.Text,
System.ComponentModel;


const
EffRandLinks = 28;
EffRandOben = 15;
EffRandUnten = EffRandOben + 102;
EffRandRechts = EffRandLinks + 293;
EffMax = 4;
EffMin = 1;
EffFaktor: Extended = (EffRandUnten - EffRandOben) /
 (EffMax - EffMin);

implementation

end.
 