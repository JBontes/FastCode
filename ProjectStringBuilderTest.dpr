program ProjectStringBuilderTest;

uses
  Vcl.Forms,
  StringBuilderTest in 'StringBuilderTest.pas' {Form76},
  FastStringBuilder in 'FastStringBuilder.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm76, Form76);
  Application.Run;
end.
