unit TestDeref;

interface

implementation

type
  TAmoeba = class(TObject)
  private
    fsName: string;
    function GetName: string;
    procedure SetName(const Value: string);
    function GetStuff(const psIndex: string): TAmoeba;

  public
    function GetBar(const piIndex: integer): TAmoeba;
    function MyFudgeFactor: TAmoeba;

    property Name: string read GetName write SetName;
    property Stuff[const psIndex: string]: TAmoeba read GetStuff;
  end;


procedure DoTestDeref;
var
  foo: TAmoeba;
  ls: string;
begin
  // the goal of this unit is to get the following silly line to compile
   foo.GetBar(1).Stuff['fish'].MyFudgeFactor.GetBar(2).Name := 'Jiim';

   // let's try this one
   ls := foo.Stuff['fish'].GetBar(1).MyFudgeFactor.GetBar(2).Name;
end;

{ TAmoeba }

function TAmoeba.getBar(const piIndex: integer): TAmoeba;
begin
  Result := self;
end;

function TAmoeba.GetName: string;
begin
  result := fsName;
end;

function TAmoeba.GetStuff(const psIndex: string): TAmoeba;
begin
  Result := self;
end;

function TAmoeba.MyFudgeFactor: TAmoeba;
begin
  Result := self;
end;

procedure TAmoeba.SetName(const Value: string);
begin
  fsName := Value;
end;

end.
