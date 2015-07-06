program InterfaceTest;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  System.Generics.FastDefaults
  ;

var
  Comparer: IComparer<integer>;
  a,b: integer;
  SComparer: IComparer<ShortString>;
  sa,sb: shortstring;

begin
  a:= 1;
  b:= 2;
  Comparer:= TComparer<integer>.Default;
  writeln(Comparer.Compare(a,b));
  sa:= 'hfghdgdhfghsfg';
  sb:= 'hfghdgdhfghsfa';
  SComparer:= TComparer<Shortstring>.Default;
  writeln(SComparer.Compare(sa,sb));
  readln;
end.
