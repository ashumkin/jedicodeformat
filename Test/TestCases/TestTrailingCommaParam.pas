unit TestTrailingCommaParam;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ImgList, StdCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    ImageListMng: TImageList;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
var
 bitmap: TBitmap;
begin
    bitmap := TBitMap.Create;
    try
      ImageListMng.GetBitmap(1, bitmap, );
    finally
      bitmap.Free;
    end;
end;

end.
