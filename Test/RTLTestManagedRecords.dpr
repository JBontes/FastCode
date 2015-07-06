program RTLTestManagedRecords;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  FastDefaults;

type
  TCompare<T> = class
    class function Compare(const Left, Right: T): integer; static; inline;
  end;

  TTest<T> = class
  private
    class var F: IComparer<T>;
  public
    class function Fast(const Left, Right: T): integer; static;
    class procedure Test(const Left, Right: T; const message: string = ''); static;
  end;

  function BinaryCompare(const Left, Right; Size: NativeInt): integer; forward;

class function TCompare<T>.Compare(const Left, Right: T): integer;
begin
  if GetTypeKind(T) = tkRecord then case SizeOf(T) of
    0: Result:= 0;
    1: Result:= (PByte(@Left)^) - (PByte(@Right)^);
    2: Result:= (PWord(@Left)^) - (PWord(@Right)^);
    4: Result:= (integer(PCardinal(@Left)^ > PCardinal(@Right)^) -
        integer(PCardinal(@Left)^ < PCardinal(@Right)^));
    else Result:= BinaryCompare(Left, Right, SizeOf(T));
  end;
end;

{pointermath on}
function BinaryCompare(const Left, Right; Size: NativeInt): integer;
var
  i: integer;
  L,R: PByte;
begin
  L:= @Left;
  R:= @Right;
  for i:= 0 to Size - 1 do begin
    if L[i] <> R[i] then exit(L[i] - R[i]);
  end;
  Result:= 0;
end;
{$pointermath off}

type
  TManagedRec = record
    a: integer;
    b: string;
  end;

var
  L,R: TManagedRec;

{ TTest<T> }

class function TTest<T>.Fast(const Left, Right: T): integer;
begin
  Result:= F.CompareUnmanaged(Left, Right);
end;

class procedure TTest<T>.Test(const Left, Right: T; const message: string);
begin
  try
    WriteLn(Format(message,[TTest<T>.Fast(Left,Left)]));
  except
    WriteLn('Oops');
  end;
end;

begin
  L.a:= 1;
  R.a:= 2;
  L.b:= '7878787';
  R.b:= '7777777';
  TTest<TManagedRec>.Test(L,R,'Compare(L,L) = %d');
  WriteLn(Format('Compare(L,R) = %d',[TCompare<TManagedRec>.Compare(L,R)]));
  WriteLn(Format('Compare(R,L) = %d',[TCompare<TManagedRec>.Compare(L,R)]));
  WriteLn(Format('Compare(R,R) = %d',[TCompare<TManagedRec>.Compare(R,R)]));
  WriteLn(Format('Compare(L,L) = %d',[IComparer<TManagedRec>.Compare(L,L)]));
  ReadLn;
end.
