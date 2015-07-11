program RTLTestManagedRecords;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  System.Generics.Defaults,
  InlineComparer in 'InlineComparer.pas';


begin
  DoTest;
end.
