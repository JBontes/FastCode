unit StringBuilderTest;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TForm76 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form76: TForm76;

implementation

{$R *.dfm}

uses
  FastStringBuilder, HiResStopWatch;



procedure TForm76.Button1Click(Sender: TObject);
var
  B: FastStringBuilder.TStringBuilder;
  Ticks: Currency;
begin
  B:= FastStringBuilder.TStringBuilder.Create;
  Ticks:= THiResStopWatch.Sample(procedure() var i: integer; begin
    B.Clear;
    for i:= 0 to 10000 do begin
      B.Append(100);
    end;
  end,100);
  Button1.Caption:= Format('%m',[Ticks]) + ' Ticks, ';
end;

procedure TForm76.Button2Click(Sender: TObject);
var
  B: System.SysUtils.TStringBuilder;
  Ticks: Currency;
  S: string;
begin
  B:= System.SysUtils.TStringBuilder.Create;
  Ticks:= THiResStopWatch.Sample(procedure var i: integer; begin
    B.Clear;
    for i:= 0 to 100 do begin
      B.Append(100);
    end;
  end,100);
  Button2.Caption:= Format('%m',[Ticks]) + ' Ticks, ';
end;

end.
