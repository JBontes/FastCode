(*******************************************************
 Record based Comparers
 A fast and small replacement for the interface
 based comparers available in `System.Generics.Defaults` since D2009.
 This works best in XE7 and above because it depends
 on the compiler intrinsic `GetTypeKind` and its compile time
 resolution of types.
 It should compile and run in 2009 and beyond, but the benefits are much less.

 Alpha version 0.1, partially tested

 (c) Copyright 2015 J. Bontes
 *)


(* This Source Code Form is subject to the terms of the
   Mozilla Public License, v. 2.0.
   If a copy of the MPL was not distributed with this file,
   You can obtain one at http://mozilla.org/MPL/2.0/.*)

unit System.Generics.FastDefaults;

{$R-,T-,X+,H+,B-}

(*$HPPEMIT '#pragma option -w-8022'*)

interface

uses
  System.SysUtils, System.TypInfo;

type
  /// <summary>
  /// Usage: Equal:= TComparer<integer>.Default.Equals(int1, int2);
  ///        LessThan:= TComparer<integer>.Default.Compare(int1, int2) < 0;
  ///        GreaterThanOrEqual:= TComparer<integer>.Default.Compare(int1, int2) >= 0;
  ///        etc..
  ///  Obviously T can be any type whatsoever.
  ///  All comparisons will be put inline, these will be very short code snippets
  ///  or calls to optimized routines.
  /// </summary>
  TComparer<T> = record
  strict private type
    TIntCast = record
      case integer of
        1:(b: byte);
        2:(w: word);
        4:(i: integer);
        8: (i64: int64);
        255:(ba: array [0..0] of integer);
    end;
    TRealCast = record
      case integer of
        4: (s: single);
        8: (d: double);
        10: (e: extended);
    end;
    TCast = record
      case TTypeKind of
        tkUnknown: (unk: array [0 .. 0] of byte);
        tkInteger: (i: TIntCast);
        tkChar: (ac: byte);
        tkEnumeration: (enum: byte);
        tkSet: (st: TIntCast);
        tkFloat: (f: TRealCast);
        tkString: (ss: string[255]);
        tkClass, tkPointer: (p: NativeUInt);
        tkMethod: (m: TMethod);
        tkWChar: (wc: Word);
        tkLString: (ansis: pointer);
        tkWString: (wides: pointer);
        tkVariant: (va: pointer);
        tkArray: (arr: array [0 .. 0] of byte);
        tkRecord: (rec: array [0 .. 0] of byte);
        tkInterface: (intf: pointer);
        tkInt64: (i64: int64);
        tkDynArray: (da: pointer);
        tkUString: (unis: pointer);
        tkClassRef: (a: TClass);
        tkProcedure: (proc: pointer);
    end;
  public type
    Default = record
      /// <summary>
      ///   calculates L - R or the equivalent
      /// <returns>
      ///   L = R -> 0
      ///   L < R -> negative value
      ///   L > R -> positive value
      /// </returns>
      /// <remarks>
      ///   Note that records and arrays are compared on a byte by byte
      ///   basis, this may yield unexpected results for little-endian data
      ///   but at least it's consistent.
      ///   This is to remain compatible with the Embarcaro implementation
      /// </remarks>
      /// </summary>
      class function Compare(const Left, Right: T): Integer; static; inline;
      class function Equals(const Left, Right: T): Boolean; static; inline;
      /// <summary>
      ///   Result:= murmurhash3(Value)
      /// </summary>
      class function GetHashCode(const Value: T): Integer; static; inline;
    end;
  end;

  TComparison<T> = function(const Left, Right: T): Integer;

  TEqualityComparison<T> = function(const Left, Right: T): Boolean;
  THasher<T> = function(const Value: T): Integer;

  function Compare_Variant(Left, Right: Pointer): Integer;
  function BinaryCompare(Left, Right: pointer; Size: integer): integer;
  function Compare_DynArray(Left, Right: Pointer; ElementSize: integer): NativeInt;

  //For testing purposes
  function PascalMurmurHash3(const [ref] HashData; Len, Seed: Integer): Cardinal;
  function MurmurHash3(const [ref] HashData; Len, Seed: Integer): Cardinal;
  function BobJenkinsHash(const HashData; Len, Seed: Integer): Integer; inline;

implementation

uses
  {$IFNDEF NEXTGEN}
  System.AnsiStrings,
  {$ENDIF}
  System.Math, System.Generics.Collections, System.Variants, System.Generics.Defaults;

{$IF CompilerVersion < 28}
function GetTypeKind(const T): TTypeKind;
var
  PI: PTypeInfo;
begin
  PI:= TypeInfo(Typeof(T));
  if PI = nil then Result:= tkUnknown
  else Result:= PTypeInfo(PI).Kind;
end;
{$endif}

function Compare_Variant(Left, Right: Pointer): Integer;
var
  l, r: Variant;
  lAsString, rAsString: string;
begin
  Result := 0; // Avoid warning.
  l := PVariant(Left)^;
  r := PVariant(Right)^;
  try
    case VarCompareValue(l, r) of
      vrEqual:        Exit(0);
      vrLessThan:     Exit(-1);
      vrGreaterThan:  Exit(1);
      vrNotEqual:
      begin
        if VarIsEmpty(L) or VarIsNull(L) then Exit(1)
        else Exit(-1);
      end;
    end;
  except // if comparison failed with exception, compare as string.
    try
      lAsString := PVariant(Left)^;
      rAsString := PVariant(Right)^;
      Result := CompareStr(Unicodestring(Left), UnicodeString(Right));
    except  // if comparison fails again, compare bytes.
      Result := BinaryCompare(Left, Right, SizeOf(Variant));
    end;
  end;
end;

//Size is not 1,2,4 or 8
function BinaryCompare(Left, Right: pointer; Size: integer): integer;
{$IFDEF purepascal} //pure pascal
var
  i: integer;
  L: PByte absolute Left;
  R: PByte absolute Right;
begin
  for i:= 1 to Size do begin
    Result:= L^ - R^;
    if Result = 0 then exit;
  end;
end;
{$else !PurePascal}
{$IFDEF CPUX64}
asm
  .NOFRAME
  .PUSHNV RDI
  .PUSHNV RSI
  //Left: RCX
  //Right: RDX
  //Size: R8   preserve:RBX, RBP, RDI, RSI, R12-R15
  mov  RDI, RCX
  mov  RSI, RDX
  mov  RCX, R8
  repe cmpsb
  sbb  EAX, EAX
  sbb  EAX, -1
end;
{$ENDIF}
{$IFDEF CPUX86}
asm
  //Left: EAX
  //Right: EDX
  //Size: ECX
  push EDI
  push ESI
  mov  EDI, EAX
  mov  ESI, EDX
  repe cmpsb
  sbb  EAX, EAX
  sbb  EAX, -1
  pop  ESI
  pop  EDI
  ret
end;
{$ENDIF}
{$ENDIF !PurePascal}

{TODO -oJB -cOptimization : Write a version that's actually faster}
function FastBinaryCompare(Left, Right: pointer; Size: integer): integer;
{$IFDEF purepascal} //pure pascal
var
  i: integer;
  L: PByte absolute Left;
  R: PByte absolute Right;
begin
  for i:= 1 to Size do begin
    Result:= L^ - R^;
    if Result = 0 then exit;
  end;
end;
{$else !PurePascal}
{$IFDEF CPUX64}
asm
  .NOFRAME
  .PUSHNV RDI
  .PUSHNV RSI
  //Left: RCX
  //Right: RDX
  //Size: R8   preserve:RBX, RBP, RDI, RSI, R12-R15
  mov  RDI, RCX
  mov  RSI, RDX
  mov  RCX, R8
  repe cmpsb
  sbb  EAX, EAX
  sbb  EAX, -1
end;
{$ENDIF}
{$IFDEF CPUX86}
asm
  //Left: EAX
  //Right: EDX
  //Size: ECX
  push EDI
  push ESI
  mov  EDI, EAX
  mov  ESI, EDX
  repe cmpsb
  sbb  EAX, EAX
  sbb  EAX, -1
  pop  ESI
  pop  EDI
  ret
end;
{$ENDIF}
{$ENDIF !PurePascal}


function DynLen(Arr: Pointer): NativeInt; inline;
begin
  if Arr = nil then Exit(0);
  Result := PNativeInt(PByte(Arr) - SizeOf(NativeInt))^;
end;

function Compare_DynArray(Left, Right: Pointer; ElementSize: integer): NativeInt;
var
  len, lenDiff: NativeInt;
begin
  len := DynLen(Left);
  lenDiff := len - DynLen(Right);
  if lenDiff < 0 then Inc(len, lenDiff);
  Result := BinaryCompare(Left, Right, ElementSize * len);
  if Result = 0 then Result := lenDiff;
end;


class function TComparer<T>.Default.Compare(const Left, Right: T): Integer;
var
  L: TCast absolute Left;
  R: TCast absolute Right;
  i64: Int64;
begin
  case GetTypeKind(T) of
    tkUnknown,
    tkInteger, tkSet: case SizeOf(T) of
      1: Result:= L.i.b - R.i.b;
      2: Result:= L.i.w - R.i.w;
      3: Result:= (L.i.i - R.i.i) and $FFFFFF;
      4: Result:= L.i.i - R.i.i;
      5: begin
        i64:= (L.i.i64 - R.i.i64) and $FFFFFFFFFF;
        Result:= integer(i64 > 0) - integer(i64 < 0)
      end;
      6: begin
        i64:= (L.i.i64 - R.i.i64) and $FFFFFFFFFFFF;
        Result:= integer(i64 > 0) - integer(i64 < 0)
      end;
      7: begin
        i64:= (L.i.i64 - R.i.i64) and $FFFFFFFFFFFFFF;
        Result:= integer(i64 > 0) - integer(i64 < 0)
      end;
      8: begin
        i64:= (L.i.i64 - R.i.i64);
        Result:= integer(i64 > 0) - integer(i64 < 0);
      end;
      else Result:= BinaryCompare(@L.arr[0], @R.arr[0], SizeOf(T));
    end;
    tkChar: Result:= L.ac - R.ac;
    tkEnumeration: Result:= L.enum - R.enum;
    tkFloat: case SizeOf(T) of
      4: begin
        Result:= integer(L.f.s > R.f.s) - integer(L.f.s < R.f.s);
      end;
      8: begin
        Result:= integer(L.f.d > R.f.d) - integer(L.f.d < R.f.d);
      end;
      10, 12: begin
        Result:= integer(L.f.e > R.f.e) - integer(L.f.e < R.f.e);
      end;
    end;
    tkString: case SizeOf(T) of
      2: Result:= (byte(L.ss[1]) - byte(R.ss[1]));
      3: Result:= (word(L.ss[1]) - word(R.ss[1]));
      5: Result:= (integer(L.ss[1]) - integer(R.ss[1]));
      else begin
        if L.ss = R.ss then Result:= 0
        else if L.ss < R.ss then Result:= -1
        else Result:= 1;
      end;
    end;
    tkClass, tkPointer, tkInterface, tkClassRef, tkProcedure: Result:= L.p - R.p;
    tkMethod: begin
      Result:= NativeUInt(L.m.Data) - NativeUInt(R.m.Data);
      if Result = 0 then Result:= NativeUInt(L.m.Code) - NativeUInt(R.m.Code);
    end;
    tkWChar: Result:= L.wc - R.wc;
    tkLString: Result:= System.AnsiStrings.CompareStr(AnsiString(L.p),AnsiString(R.p));
    tkWString: begin
      if WideString(L.p) = WideString(R.p) then Result:= 0
      else if WideString(L.p) > WideString(R.p) then Result:= 1
      else Result:= -1;
    end;
    tkVariant: Result:= Compare_Variant(L.va, R.va);
    tkArray, tkRecord: case SizeOf(T) of
      1: Result:= L.arr[0] - R.arr[0];
      2: Result:= Word(L.arr[0]) - Word(R.arr[0]);
      4: Result:= Integer(L.arr[0]) - Integer(R.arr[0]);
      8: Result:= Int64(L.arr[0]) - Int64(R.arr[0]);
      else Result:= BinaryCompare(@L.arr[0], @R.arr[0], SizeOf(T));
    end;
    tkInt64: begin
      i64:= (l.i.i64 - r.i.i64);
      Result:= integer(i64 > 0) - integer(i64 < 0);
    end;
    tkDynArray: Result:= Compare_DynArray(L.da, R.da, GetTypeData(TypeInfo(T))^.elSize);
    tkUString: Result := CompareStr(UnicodeString(L.unis), UnicodeString(R.unis));
  end;
end;

class function TComparer<T>.Default.Equals(const Left, Right: T): Boolean;
begin
  Result:= Compare(Left, Right) = 0;
end;

class function TComparer<T>.Default.GetHashCode(const Value: T): Integer;
var
  V: TCast absolute Value;
  VarStr: string;
begin
  case GetTypeKind(T) of
    tkUnknown, //Real48, Custom enums
    tkInteger, tkChar, tkEnumeration, tkSet, tkWChar, tkRecord, tkInt64, tkFloat,
    tkClass, tkMethod, tkInterface, tkClassRef, tkPointer, tkProcedure: begin
      Result:= MurmurHash3(V.i, SizeOf(T),0);
    end;
    tkString: Result:= MurmurHash3(V.ss[Low(string)], Length(V.ss) * SizeOf(V.ss[Low(string)]),0);
    tkLString: Result:= MurmurHash3(Ansistring(V.p)[Low(string)], Length(Ansistring(V.p)),0);
    tkUString: Result:= MurmurHash3(UnicodeString(V.p)[Low(string)], Length(UnicodeString(V.p)) * SizeOf(widechar),0);
    tkWString: Result:= MurmurHash3(WideString(V.p)[1], Length(WideString(V.p)) * SizeOf(WideChar), 0);
    tkVariant: try
      VarStr:= PVariant(V.va)^;
      Result:= TComparer<string>.Default.GetHashCode(VarStr);
    except
      Result:= MurmurHash3(V.va^, SizeOf(Variant), 0);
    end;
    tkArray: Result:= Murmurhash3(v.arr, SizeOf(T), 0);
    tkDynArray: Result:= MurmurHash3(v.da^, GetTypeData(TypeInfo(T))^.elSize * DynLen(v.da),0);
  end;
end;

function BobJenkinsHash(const HashData; Len, Seed: Integer): Integer;
begin
  Result:= System.Generics.Defaults.BobJenkinsHash(HashData,Len,Seed);
end;

{$pointermath on}
function PascalMurmurHash3(const [ref] HashData; Len, Seed: Integer): Cardinal;
const
  c1 = $cc9e2d51;
  c2 = $1b873593;
  r1 = 15;
  r2 = 13;
  m = 5;
  n = $e6546b64;
  f1 = $85ebca6b;
  f2 = $c2b2ae35;
var
  i, Len2: integer;
  k: cardinal;
  remaining: cardinal;
  data: PCardinal;
label case1, case2, case3, final;
type
  ByteArray = array[0..0] of byte;
begin
  Result:= seed;
  data:= @HashData;
  for i:= 0 to (Len shr 2)-1 do begin
    k:= data[i];
    k:= k * c1;
    k:= (k shl r1) or (k shr (32-r1));
    k:= k * c2;
    Result:= Result xor k;
    Result:= (Result shl r2) or (Result shr (32-r2));
    Result:= Result * m + n;
  end; {for i}
  remaining:= 0;
  Len2:= Len;
  case Len and $3 of
    1: goto case1;
    2: goto case2;
    3: goto case3;
    else goto final;
  end;
case3:
  Dec(Len2);
  Inc(remaining, PByte(Data)[Len2] shl 16);
case2:
  Dec(Len2);
  Inc(remaining, PByte(Data)[Len2] shl 8);
case1:
  Dec(Len2);
  Inc(remaining, PByte(Data)[Len2]);
  remaining:= remaining * c1;
  remaining:= (remaining shl r1) or (remaining shr (32 - r1));
  remaining:= remaining * c2;
  Result:= Result xor remaining;
final:
  Result:= Result xor Len;

  Result:= Result xor (Result shr 16);
  Result:= Result * f1;
  Result:= Result xor (Result shr 13);
  Result:= Result * f2;
  Result:= Result xor (Result shr 16);
end;

function MurmurHash3(const [ref] HashData; Len, Seed: Integer): Cardinal;
const
    c1 = $cc9e2d51;
    c2 = $1b873593;
    r1 = 15;
    r2 = 13;
    m = 5;
    n = $e6546b64;
    f1 = $85ebca6b;
    f2 = $c2b2ae35;
{$ifdef purepascal}
var
  i, Len2: integer;
  k: cardinal;
  remaining: cardinal;
  data: PCardinal;
begin
  Result:= seed;
  data:= @HashData;
  for i:= 0 to (Len shr 2)-1 do begin
    k:= data[i];
    k:= k * c1;
    k:= (k shl r1) or (k shr (32-r1));
    k:= k * c2;
    Result:= Result xor k;
    Result:= (Result shl r2) or (Result shr (32-r2));
    Result:= Result * m + n;
  end; {for i}
  Len2:= Len;
  case Len and $3 of
    1: goto case1;
    2: goto case2;
    3: goto case3;
    else goto final;
  end;
case3:
  Dec(Len2);
  Inc(remaining, PByte(Data)[Len2] shl 16);
case2:
  Dec(Len2);
  Inc(remaining, PByte(Data)[Len2] shl 8);
case1:
  Dec(Len2);
  Inc(remaining, PByte(Data)[Len2]);
  remaining:= remaining * c1;
  remaining := (remaining shl r1) or (remaining shr (32 - r1));
  remaining := remaining * c2;
  Result := Result xor remaining;
final:
  Result := Result xor Len;

  Result := Result xor (Result shr 16);
  Result := Result * f1;
  Result := Result xor (Result shr 13);
  Result := Result * f2;
  Result := Result xor (Result shr 16);
{$else}
{$region 'asm'}
{$ifdef CPUx86}
asm
  push EBX
  push EDI
  push ESI
  xchg ECX,EDX
  //EAX = data
  //ECX = count in bytes
  //EDX = seed
  mov  ESI,ECX
  shr  ECX,2
  jz @remaining_bytes
@loop:
  mov  EDI,[EAX]
  imul EDI,EDI,c1
  rol  EDI,r1
  imul EDI,EDI,c2
  xor  EDX,EDI
  rol  EDX,r2
  lea  EDX,[EDX*4+EDX+n]
  lea  EAX,[EAX+4]
  dec  ECX
  jnz @loop
@remaining_bytes:
  mov  ECX,ESI
  and  ECX,$3
  jz @finalization
  xor  EBX,EBX
  dec  ECX
  mov  BL,byte ptr [EAX+ECX]
  jz @process_remaining
  shl  EBX,8
  dec  ECX
  mov  BL,byte ptr [EAX+ECX]
  jz @process_remaining
  shl  EBX,8
  mov  BL,byte ptr [EAX]
@process_remaining:
  imul EBX,EBX,c1
  rol  EBX,r1
  imul EBX,EBX,c2
  xor  EDX,EBX
@finalization:
  xor  EDX,ESI
  mov  EAX,EDX
  shr  EDX,16
  xor  EDX,EAX
  imul EDX,EDX,f1
  mov  EAX,EDX
  shr  EDX,13
  xor  EDX,EAX
  imul EDX,EDX,f2
  mov  EAX,EDX
  shr  EDX,16
  xor  EAX,EDX
  pop  ESI
  pop  EDI
  pop  EBX
  {$endif}
  {$ifdef CPUx64}
asm
  push RBX
  push RDI
  push RSI
  mov  RAX,RCX
  mov  RCX,RDX
  mov  RDX,R8
  //RAX = data
  //RCX = count in bytes
  //RDX = seed
  mov  ESI,ECX
  shr  ECX,2
  jz @remaining_bytes
@loop:
  mov  EDI, dword ptr [RAX]
  imul EDI,EDI,c1
  rol  EDI,r1
  imul EDI,EDI,c2
  xor  EDX,EDI
  rol  EDX,r2
  lea  EDX,dword ptr [EDX*4+EDX+n] //*5 + n
  lea  RAX,qword ptr [RAX+4]
  dec  ECX
  jnz @loop
@remaining_bytes:
  mov  ECX,ESI
  and  ECX,$3
  jz @finalization
  xor  RBX,RBX
  dec  ECX
  mov  BL,byte ptr [RAX+RCX]
  jz @process_remaining
  shl  EBX,8
  dec  ECX
  mov  BL,byte ptr [RAX+RCX]
  jz @process_remaining
  shl  EBX,8
  mov  BL,byte ptr [RAX]
@process_remaining:
  imul EBX,EBX,c1
  rol  EBX,r1
  imul EBX,EBX,c2
  xor  EDX,EBX
@finalization:
  xor  EDX,ESI
  mov  EAX,EDX
  shr  EDX,16
  xor  EDX,EAX
  imul EDX,EDX,f1
  mov  EAX,EDX
  shr  EDX,13
  xor  EDX,EAX
  imul EDX,EDX,f2
  mov  EAX,EDX
  shr  EDX,16
  xor  EAX,EDX
  pop  RSI
  pop  RDI
  pop  RBX
{$endif}
{$endregion}
{$endif}

end;
  (*
    From wikipedia:
    Murmur3_32(key, len, seed)
    // Note: In this version, all integer arithmetic is performed with unsigned 32 bit integers.
    //       In the case of overflow, the result is constrained by the application of modulo 2^{32} arithmetic.
    c1 := 0xcc9e2d51
    c2 := 0x1b873593
    r1 := 15
    r2 := 13
    m := 5
    n := 0xe6546b64

    hash := seed

    for each fourByteChunk of key
        k := fourByteChunk

        k := k * c1
        k := (k << r1) OR (k >> (32-r1))
        k := k * c2

        hash := hash XOR k
        hash := (hash << r2) OR (hash >> (32-r2))
        hash := hash * m + n

    with any remainingBytesInKey
        remainingBytes := remainingBytes * c1
        remainingBytes := (remainingBytes << r1) OR (remainingBytes >> (32 - r1))
        remainingBytes := remainingBytes * c2

        hash := hash XOR remainingBytes

    hash := hash XOR len

    hash := hash XOR (hash >> 16)
    hash := hash * 0x85ebca6b
    hash := hash XOR (hash >> 13)
    hash := hash * 0xc2b2ae35
    hash := hash XOR (hash >> 16)
}(**)


end.
