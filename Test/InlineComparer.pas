unit InlineComparer;

interface

uses
  FastDefaults,
  System.Generics.Defaults;

type
  TTest<T> = class
  private
    class var F: FastDefaults.IComparer<T>;
    class var S: System.Generics.Defaults.IComparer<T>;
    class constructor Init;
  public
    class function Fast(const Left, Right: T): integer; static;
    class procedure Test(const Left, Right: T; const message: string = ''); static;
  end;

type
  TManagedRec = record
    a: integer;
    b: string;
  end;

var
  L,R: TManagedRec;

procedure DoTest;

implementation

uses
  System.SysUtils;

{ TTest<T> }

class function TTest<T>.Fast(const Left, Right: T): integer;
begin
  Result:= F.Compare(Left, Right);
end;

class procedure TTest<T>.Test(const Left, Right: T; const message: string);
begin
  try
    WriteLn(Format('Fast:'+message,[TTest<T>.Fast(Left,Right)]));
    WriteLn(Format('Slow:'+message,[TTest<T>.S.Compare(Left,Right)]));
  except
    WriteLn('Oops');
  end;
end;

class constructor TTest<T>.Init;
begin
  S:= System.Generics.Defaults.TComparer<T>.Default;
end;

var
  Test: TTest<TManagedRec>;

procedure DoTest;
begin
  L.a:= 1;
  R.a:= 2;
  L.b:= '7878787';
  R.b:= '7777777';
  Test:= TTest<TManagedRec>.Create;
  Test.S:= System.Generics.Defaults.TComparer<TManagedRec>.Default;
  Test.Test(L,L,'Compare(L,L) = %d');
  Test.Free;
  ReadLn;
end;

end.
