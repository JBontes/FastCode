unit StackSet;

interface

type
  TSet<T: record> = record
  private const
    NoOfElements = ((SizeOf(T) shl 8) div 32);
  public type
    TSetArray = array[0 .. 1] of integer;
    TByteSet = set of byte;
  private
    FStorage: {packed} array[0 .. 31] of T;
    {TODO -oJB -cGeneralization : Cater for SizeOf(T) = 3 etc}
    function GetItem(index: integer): integer; inline;
    procedure SetItem(index: integer; const Value: integer); inline;
    function Index(const A: T): integer; inline;  //class functions don't get inlined
    function Mask(const A: T): integer; inline;
    property Item[index: integer]: integer read GetItem write SetItem;
  public
    class operator NotEqual(const A, B: TSet<T>): boolean; inline;
    class operator LessThanOrEqual(const A, B: TSet<T>): boolean; inline;
    class operator Subtract(const A, B: TSet<T>): TSet<T>; inline;
    class operator in (const A: T; const B: TSet<T>): boolean; inline;
    class operator Add(const A: TSet<T>; const B: T): TSet<T>;
    class operator Add(const A, B: TSet<T>): TSet<T>;
    class operator Subtract(const A: TSet<T>; const B: T): TSet<T>;
    class operator Multiply(const A, B: TSet<T>): TSet<T>;
    class operator Equal(const A, B: TSet<T>): boolean;
    class operator GreaterThanOrEqual(const A, B: TSet<T>): boolean;
    class operator Implicit(const [ref] A: TSetArray): TSet<T>;
    class operator Implicit(const A: TByteSet): TSet<T>;
    class function Create<C>(const A: C): TSet<T>; inline; static;
  end;

implementation

uses
  SysUtils;

  { TSet<T> }

function TSet<T>.GetItem(index: integer): integer;
begin
  Result:= TSetArray((@Self.FStorage)^)[index];
end;

procedure TSet<T>.SetItem(index: integer; const Value: integer);
begin
  TSetArray((@self.FStorage)^)[index]:= Value;
end;

function TSet<T>.Index(const A: T): integer;
begin
  if SizeOf(A) = SizeOf(byte) then begin
    //256 elements, 32 bytes,  8 integers
    Result:= byte((@A)^) div 32;
  end else if SizeOf(A) = SizeOf(Word) then begin
    //64K elements, 16 integers
    Result:= word((@A)^) div 32;
  end else if SizeOf(A) = 3 then begin
    Result:= (Integer((@A)^) and $00FFFFFF) div 32;
  end else if SizeOf(A) = SizeOf(Integer) then begin
    Result:= Integer((@A)^) div 32;
  end else if SizeOf(A) = SizeOf(NativeInt) then begin
    Result:= NativeInt((@A)^) div 32;
  end
  else raise Exception.Create('Set to large');
end;

function TSet<T>.Mask(const A: T): integer;
begin
  if SizeOf(A) = SizeOf(byte) then begin
    //256 elements, 32 bytes,  8 integers
    Result:= byte((@A)^) and 31;
  end else if SizeOf(A) = SizeOf(Word) then begin
    //64K elements, 16 integers
    Result:= word((@A)^) and 31;
  end else if SizeOf(A) in [3,4] then begin
    Result:= Integer((@A)^) and 31
  end else if SizeOf(A) = SizeOf(NativeInt) then begin
    Result:= NativeInt((@A)^) and 31;
  end
  else raise Exception.Create('Set to large');
  Result:= 1 shl Result;
end;

class operator TSet<T>.Subtract(const A, B: TSet<T>): TSet<T>;
begin
  Result:= A * B;
end;

class operator TSet<T>.NotEqual(const A, B: TSet<T>): boolean;
begin
  Result:= not(A = B);
end;

class operator TSet<T>.LessThanOrEqual(const A, B: TSet<T>): boolean;
begin
  Result:= B >= A;
end;

class operator TSet<T>.Add(const A: TSet<T>; const B: T): TSet<T>;
var
  I, M: integer;
begin
  I:= A.Index(B);
  M:= A.Mask(B);
  Move(A, Result, SizeOf(A));
  Result.Item[i]:= Result.Item[i] or M;
end;

class operator TSet<T>.Add(const A, B: TSet<T>): TSet<T>;
var
  i: integer;
begin
  for i:= 0 to NoOfElements - 1 do begin
    Result.Item[i]:= A.Item[i] or B.Item[i];
  end;
end;

class function TSet<T>.Create<C>(const A: C): TSet<T>;
begin
  Assert(SizeOf(A) <= SizeOf(Result));
  if SizeOf(A) < SizeOf(Result) then FillChar(Result, SizeOf(Result), #0);
  Move(A, Result, SizeOf(A));
end;

class operator TSet<T>.Equal(const A, B: TSet<T>): boolean;
var
  i: integer;
begin
  for i:= 0 to NoOfElements - 1 do begin
    Result:= A.Item[i] = B.Item[i];
    if not(Result) then exit;
  end;
end;

class operator TSet<T>.GreaterThanOrEqual(const A, B: TSet<T>): boolean;
var
  Sum: integer;
  i: integer;
begin
  //are all elements in B part of A?
  //true if (B or A) = A
  for i:= 1 to NoOfElements - 1 do begin
    Sum:= A.Item[i] or B.Item[i];
    Result:= (Sum = A.Item[i]);
    if not(Result) then exit;
  end;
end;

class operator TSet<T>.Implicit(const [ref] A: TSetArray): TSet<T>;
begin
  Move(A, Result, SizeOf(A));
end;

class operator TSet<T>.Implicit(const A: TByteSet): TSet<T>;
begin
  if SizeOf(A) < SizeOf(Result) then FillChar(Result, SizeOf(Result),#0);
  Move(A, Result, SizeOf(A));
end;

class operator TSet<T>.in(const A: T; const B: TSet<T>): boolean;
var
  I, M: integer;
begin
  I:= B.Index(A);
  M:= B.Mask(A);
  Result:= (B.Item[i] and M) <> 0;
end;

class operator TSet<T>.Multiply(const A, B: TSet<T>): TSet<T>;
var
  i: integer;
begin
  for i:= 0 to NoOfElements - 1 do begin
    Result.Item[i]:= A.Item[i] and B.Item[i];
  end;
end;

class operator TSet<T>.Subtract(const A: TSet<T>; const B: T): TSet<T>;
var
  I, M: integer;
begin
  I:= A.Index(B);
  M:= A.Mask(B);
  Move(A, Result, SizeOf(A));
  Result.Item[i]:= Result.Item[i] and not(M);
end;

end.
