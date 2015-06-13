(*******************************************************
 Record based Comparers
 A fast and small replacement for the interface
 based comparers available in `System.Generics.Defaults` since D2009.
 This works best in XE7 and above because it depends
 on the compiler intrinsic `GetTypeKind` and its compile time
 resolution of types.
 It should compile and run in 2009 and beyond, but the benefits are much less.

 Alpha version 0.2, full tested in both Win32 and Win64

 (c) Copyright 2015 J. Bontes
 *)


(* This Source Code Form is subject to the terms of the
   Mozilla Public License, v. 2.0.
   If a copy of the MPL was not distributed with this file,
   You can obtain one at http://mozilla.org/MPL/2.0/.*)

unit FastDefaults;

{$R-,T-,X+,H+,B-}

(*$HPPEMIT '#pragma option -w-8022'*)

interface

uses
  System.SysUtils;

type

  /// <summary>
  /// Usage: Equal:= TComparer<integer>.TDefault.Equals(int1, int2);
  ///        LessThan:= TComparer<integer>.TDefault.CompareFast(int1, int2) < 0;
  ///        GreaterThanOrEqual:= TComparer<integer>.TDefault.CompareFast(int1, int2) >= 0;
  ///        etc..
  ///  Obviously T can be any type whatsoever.
  ///  All comparisons will be put inline, these will be very short code snippets
  ///  or calls to optimized routines.
  /// </summary>
  TComparer<T> = record
  strict private
  type
    TBuiltinType = (btByte, btint8, btword, btint16, btcardinal, btinteger, btUint64, btInt64, btNativeInt,
      btNativeUint, btAnsiChar, btWideChar, btUCS4Char, btSingle, btReal48, btdouble, btextended, btComp, btCurrency,
      btboolean, btwordbool, btlongbool, btbytebool, btStr1, btStr2, btStr3, btOpenString, btAnsiString,
      btUnicodeString, btRawByteString, btUTF8String, btpointer, btInterface, btObject, btVariant, btOleVariant,
      btTValue, btMethod);
  type
{$IFNDEF NEXTGEN}
    TPS1 = string[1];
    TPS2 = string[2];
    TPS3 = string[3];
{$ELSE NEXTGEN}
    ShortString = type string;
    TPS1 = string;
    TPS2 = string;
    TPS3 = string;
{$ENDIF !NEXTGEN}
    TCast = record
      case TBuiltinType of
        btByte: (u8: byte);
        btint8: (i8: int8);
        btword: (u16: word);
        btint16: (i16: int16);
        btcardinal: (u32: cardinal);
        btinteger: (i32: integer);
        btUint64: (u64: UInt64);
        btInt64: (i64: Int64);
        btNativeInt: (ni: NativeInt);
        btNativeUint: (nu: NativeUInt);
        btAnsiChar: (ac: byte);
        btWideChar: (wc: word);
        btUCS4Char: (uc: cardinal);
        btSingle: (f4: single);
        btReal48: (f6: Real48);
        btdouble: (f8: double);
        btextended: (f10: extended);
        btComp: (c: Int64);
        btCurrency: (cu: Int64);
        btboolean: (b: boolean);
        btwordbool: (bw: wordbool);
        btlongbool: (bl: longbool);
        btbytebool: (bb: bytebool);
        btStr1: (ps1: TPS1);
        btStr2: (ps2: TPS1);
        btStr3: (ps3: TPS1);
        btOpenString: (ps: ShortString);
        btpointer: (p: pointer);
        btMethod: (m: TMethod);
    end;
  public type
    TDefault = record
    strict private
      class var fSigned: boolean;
      class var fElementSize: NativeUint;
      class constructor Init;
      class property Signed: boolean read fSigned;
      class property ElementSize: NativeUInt read fElementSize;
    public
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
      ///   This is to remain compatible with the Embarcadero implementation
      /// </remarks>
      /// </summary>
      class function Compare(const Left, Right: T): Integer; overload; static; inline;
      /// <summary>
      ///   calculates L = R or the equivalent
      /// <returns>
      ///   L = R -> true
      ///   L <> R -> false
      /// </returns>
      /// <remarks>
      ///   Note that records and arrays are compared on a byte by byte
      ///   basis, this may yield unexpected results for little-endian data
      ///   but at least it's consistent.
      ///   This is to remain compatible with the Embarcadero implementation
      ///   Note that currently this function is implemented as `Compare(L,R) = 0`.
      ///   For performance reason Equals should get its own optimized implementation.
      /// </remarks>
      /// </summary>
      class function Equals(const Left, Right: T): Boolean; static; inline;
      /// <summary>
      ///   Use this for validation purposes only.
      ///   It maps the call to the corresponding overloaded CompareFast method
      ///   if one is available, if not it maps to Compare.
      /// </summary>
      class function TestCompareFast(const Left, Right: T): integer; static; inline;



      /// <summary>
      ///   Result:= murmurhash3(Value)
      /// <remarks>
      ///   Note that this function yields faster and better (less collisions)
      ///   results than the lookup3 hash used by emba, but that the results are **not**
      ///   The same.
      /// </remarks>
      /// </summary>
      class function GetHashCode(const Value: T; Seed: integer = 0): Integer; static; inline;
    end;
    public
      class var Default: TDefault;
  end;

  function CompareFast(const Left, Right: byte): integer; overload; inline;
  function CompareFast(const Left, Right: int8): integer; overload; inline;
  function CompareFast(const Left, Right: word): integer; overload; inline;
  function CompareFast(const Left, Right: int16): integer; overload; inline;
  function CompareFast(const Left, Right: cardinal): integer; overload; inline;
  function CompareFast(const Left, Right: integer): integer; overload; inline;
  function CompareFast(const Left, Right: UInt64): integer; overload; inline;
  function CompareFast(const Left, Right: Int64): integer; overload; inline;
  function CompareFast(const Left, Right: NativeInt): integer; overload; inline;
  function CompareFast(const Left, Right: NativeUInt): integer; overload; inline;
  function CompareFast(const Left, Right: AnsiChar): integer; overload; inline;
  function CompareFast(const Left, Right: WideChar): integer; overload; inline;
  function CompareFast(const Left, Right: UCS4Char): integer; overload; inline;
  function CompareFast(const Left, Right: single): integer; overload; inline;
  function CompareFast(const Left, Right: Real48): integer; overload; inline;
  function CompareFast(const Left, Right: double): integer; overload; inline;
  function CompareFast(const Left, Right: extended): integer; overload; inline;
  function CompareFast(const Left, Right: Comp): integer; overload; inline;
  function CompareFast(const Left, Right: Currency): integer; overload; inline;
  function CompareFast(const Left, Right: boolean): integer; overload; inline;
  function CompareFast(const Left, Right: wordbool): integer; overload; inline;
  function CompareFast(const Left, Right: longbool): integer; overload; inline;
  function CompareFast(const Left, Right: bytebool): integer; overload; inline;
  function CompareFast(const Left, Right: OpenString): integer; overload; inline;
  function CompareFast(const Left, Right: AnsiString): integer; overload; inline;
  function CompareFast(const Left, Right: UnicodeString): integer; overload; inline;
  function CompareFast(const Left, Right: WideString): integer; overload; inline;
  function CompareFast(const Left, Right: RawByteString): integer; overload; inline;
  function CompareFast(const Left, Right: UTF8String): integer; overload; inline;
  function CompareFast(const Left, Right: pointer): integer; overload; inline;
  function CompareFast(const Left, Right: IInterface): integer; overload; inline;
  function CompareFast(const Left, Right: TObject): integer; overload; inline;

type
  TComparison<T> = function(const Left, Right: T): Integer;

  TEqualityComparison<T> = function(const Left, Right: T): Boolean;
  THasher<T> = function(const Value: T): Integer;

  function Compare_Variant(Left, Right: Pointer): Integer;
  function BinaryCompare(Left, Right: pointer; Size: integer): integer;
  function FastBinaryCompare(Left, Right: pointer; Size: integer): integer;
  function BinaryCompare4(Left, Right: integer): integer;
  {$ifdef purepascal}
  function BinaryCompare8(const Left, Right: pointer): integer;
  {$else !purepascal}{$ifdef CPUX64}
  function BinaryCompare8(const Left, Right: UInt64): integer;
  {$else !CPUX64}
  function BinaryCompare8(const Left, Right: pointer): integer;
  {$endif}{$endif}
  function Compare_DynArray(Left, Right: Pointer; ElementSize: integer): NativeInt;
  function Compare_PSn(const Left, Right: OpenString): Integer; inline;

  //For testing purposes
  function PascalMurmurHash3(const [ref] HashData; Len, Seed: Integer): Integer;
  function MurmurHash3(const [ref] HashData; Len: integer; Seed: Integer = 0): Integer;
  function BobJenkinsHash(const HashData; Len, Seed: Integer): Integer; inline;

  function CompareStr(const S1, S2: WideString): integer; overload;

  {$IFDEF CPUX64}
  function CompareStrX64(const S1, S2: string): Integer; overload;
  {$ENDIF}

implementation

uses
  {$IFNDEF NEXTGEN}
  System.AnsiStrings,
  {$ENDIF}
  {$IF CompilerVersion < 28}
  System.RTTi,
  {$ENDIF}
  System.Math, System.Generics.Collections, System.Variants, System.Generics.Defaults, System.TypInfo;


function Compare_Variant(Left, Right: Pointer): Integer;
var
  l, r: Variant;
  lAsString, rAsString: string;
  varTypeLeft, varTypeRight: integer;
begin
  Result := 0; // Avoid warning.
  l := PVariant(Left)^;
  r := PVariant(Right)^;
  try
    case System.Variants.VarCompareValue(l, r) of
      vrEqual:        Exit(0);
      vrLessThan:     Exit(-1);
      vrGreaterThan:  Exit(1);
      vrNotEqual:
      begin
        if VarIsEmpty(L) or VarIsNull(L) then
          Exit(1)
        else
          Exit(-1);
      end;
    end;
  except // if comparison failed with exception, compare as string.
    try
      //Prevent type conversions
      varTypeLeft:= VarType(l) and varTypeMask;
      varTypeRight:= VarType(r) and varTypeMask;
      if   ((varTypeLeft = varUString) or (varTypeRight = varUString))
        or ((varTypeLeft = varOleStr)  or (varTypeRight = varOleStr)) then begin
        Result:= CompareStr(UnicodeString(l), UnicodeString(r));
      end else if (varTypeLeft = varString) or (varTypeRight = varString) then begin
        Result:= CompareStr(AnsiString(l), AnsiString(r));
      end else begin
        lAsString:= PVariant(Left)^;
        rAsString:= PVariant(Right)^;
        Result:= CompareStr(lAsString, rAsString);
      end;
    except // if comparison fails again, compare bytes.
      Result:= BinaryCompare(Left, Right, SizeOf(Variant));
    end;
  end;
end;


function BinaryCompare4(Left, Right: integer): integer;
{$IFDEF purepascal}
var
  i: integer;
  L: array [0..3] of Byte absolute Left;
  R: array [0..3] of Byte absolute Right;
begin
  for i:= 1 to Size do begin
    Result:= L[i] - R[i];
    if Result <> 0 then exit;
  end;
end;
{$else !PurePascal}
{$IFDEF CPUX64}
asm
  .NOFRAME
  //Left: RCX
  //Right: RDX
  //preserve:RBX, RBP, RDI, RSI, R12-R15
  //bswap ECX
  //bswap EDX
  sub  ECX, EDX
  setz CL
  sbb  EAX, EAX
  sbb  EAX, -1
  shr  EAX, CL
end;
{$ENDIF}
{$IFDEF CPUX86}
asm
  //Left: EAX
  //Right: EDX
  //Size: ECX
  sub  EAX, EDX
  setz CL
  sbb  EAX, EAX
  sbb  EAX, -1
  shr  EAX, CL
end;
{$ENDIF}
{$ENDIF !PurePascal}

{$IFDEF purepascal}
function BinaryCompare8(const Left, Right: pointer): integer;
var
  i: integer;
  L: PByte absolute Left;
  R: PByte absolute Right;
begin
  for i:= 1 to Size do begin
    Result:= L^[i] - R^[i];
    if Result <> 0 then exit;
  end;
end;
{$else !PurePascal}
{$IFDEF CPUX64}
function BinaryCompare8(const Left, Right: UInt64): integer;
asm
  .NOFRAME
  //Left: RCX
  //Right: RDX
  //preserve:RBX, RBP, RDI, RSI, R12-R15
  bswap RCX
  bswap RDX
  sub  RCX, RDX
  setz CL
  sbb  EAX, EAX
  sbb  EAX, -1
  shr  EAX, CL
end;
{$ENDIF}
{$IFDEF CPUX86}
function BinaryCompare8(const Left, Right: pointer): integer;
asm
  //Left: EAX
  //Right: EDX
  //Size: ECX
  push EBX
  mov  EBX, [EAX]
  mov  ECX, [EDX]
  bswap EBX
  bswap ECX
  sub  EBX, ECX
  jnz @done
  mov  EBX, [EAX+4]
  mov  ECX, [EDX+4]
  bswap EBX
  bswap ECX
  sub  EBX, ECX
@done:
  setz CL
  sbb  EBX, EBX
  sbb  EBX, -1
  shr  EBX, CL
  mov  EAX,EBX
  pop  EBX
end;
{$ENDIF}
{$ENDIF !PurePascal}


//Size is not 1,2,4 or 8
function BinaryCompare(Left, Right: pointer; Size: integer): integer;
{$IFDEF purepascal}
var
  i: integer;
  L: PByte absolute Left;
  R: PByte absolute Right;
begin
  for i:= 1 to Size do begin
    Result:= L^ - R^;
    if Result <> 0 then exit;
  end;
end;
{$else !PurePascal}
{$IFDEF CPUX64}
asm
  .NOFRAME
  push RDI
  push RSI
  //Left: RCX
  //Right: RDX
  //Size: R8   preserve:RBX, RBP, RDI, RSI, R12-R15
  mov  RSI, RCX
  mov  RDI, RDX
  mov  RCX, R8
  repe cmpsb
  jz @equal
  setz CL
  sbb  EAX, EAX
  sbb  EAX, -1
  shr  EAX, CL
  pop  RSI
  pop  RDI
  ret
@equal:
  xor  RAX,RAX
  pop  RSI
  pop  RDI
end;
{$ENDIF}
{$IFDEF CPUX86}
asm
  //Left: EAX
  //Right: EDX
  //Size: ECX
  push EDI
  push ESI
  mov  ESI, EAX
  mov  EDI, EDX
  repe cmpsb
  setz CL
  sbb  EAX, EAX
  sbb  EAX, -1
  shr  EAX,  CL
  pop  ESI
  pop  EDI
  ret
@equal:
  pop  ESI
  pop  EDI
  xor  EAX,EAX
end;
{$ENDIF}
{$ENDIF !PurePascal}

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
  //Left: RCX
  //Right: RDX
  //Size: R8   preserve:RBX, RBP, RDI, RSI, R12-R15
  xor  R10, R10
  mov  R11, R8
  and  R11, $7
  xor  R8,  R11
jz @loop2
  neg  R8
  sub  RCX, R8
  sub  RDX, R8
@loop:
  mov  R10, [RCX+R8]
  bswap R10
  mov  R9, [RDX+R8]
  bswap R9
  sub R10,R9
  jnz @different
  add  R8, 8
  jnz @loop
@loop2:
  dec  R11
  js @same
  movzx  R10, byte ptr [RCX+R8]
  movzx  R9, byte ptr [RDX+R8]
  inc  R8
  sub R10,R9
  jz @loop2
@different:
  sbb  R10,R10
  sbb  R10,-1
@same:
  mov  RAX,R10
end;

{$ENDIF}
{$IFDEF CPUX86}
asm
//  //Left: EAX
//  //Right: EDX
//  //Size: ECX
  push EBX
  push EDI
  push ESI
  xor  ESI, ESI  //In case Size = 0 and fallthrough to @same occurs.
  mov  EBX, ECX
  and  EBX, $3
  xor  ECX, EBX
jz @loop2
  neg  ECX
  sub  EAX, ECX
  sub  EDX, ECX
@loop:
  mov  ESI, [EAX+ECX]
  bswap ESI
  mov  EDI, [EDX+ECX]
  bswap EDI
  sub ESI,EDI
  jnz @different
  add  ECX, 4
  jnz @loop
@loop2:
  dec  EBX
  js @same
  movzx  ESI, byte ptr [EAX+ECX]
  movzx  EDI, byte ptr [EDX+ECX]
  inc  ECX
  sub ESI,EDI
  jz @loop2
@different:
  sbb  ESI,ESI
  sbb  ESI,-1
@same:
  mov  EAX,ESI
  pop  ESI
  pop  EDI
  pop  EBX
end;
{$ENDIF CPUX86}
{$ENDIF !PurePascal}


function DynLen(Arr: Pointer): NativeInt; inline;
begin
  if Arr = nil then Exit(0);
  Result := PNativeInt(PByte(Arr) - SizeOf(NativeInt))^;
end;

function Compare_DynArray(Left, Right: Pointer; ElementSize: integer): NativeInt;
var
  LenL, LenR, lenDiff: NativeInt;
begin
  lenL := DynLen(Left);
  LenR := DynLen(Right);
  lenDiff := LenL - LenR;
  LenL:= Min(LenL, LenR);
  Result := FastBinaryCompare(Left, Right, ElementSize * LenL);
  if Result = 0 then Result := lenDiff;
end;

function Compare_PSn(const Left, Right: OpenString): Integer;
begin
  Result:= (integer(Left > Right) - integer(Left < Right));
end;


{$Undef FusedParameters}
{$ifdef CPUX86}
{$if CompilerVersion >= 28}
{$if CompilerVersion <= 29}
{$DEFINE FusedParameters}
{$endif}{$endif}{$endif}

class function TComparer<T>.TDefault.Compare(const Left, Right: T): integer;
var
  L: TCast absolute Left;
  R: TCast absolute Right;
  //X: TTypeKind;
begin
  //X:= GetTypeKind(T);
//    tkUnknown  OK      //Have we covered all types?
//    tkInteger, OK
//    tkChar,    OK
//    tkEnumeration, OK
//    tkFloat,   OK
//    tkString,  OK
//    tkSet,     OK
//    tkClass,   OK
//    tkMethod,  OK
//    tkWChar,   OK
//    tkLString, OK
//    tkWString, OK
//    tkVariant, OK
//    tkArray,   OK
//    tkRecord,  OK
//    tkInterface, OK
//    tkInt64,   OK
//    tkDynArray,OK
//    tkUString, OK
//    tkClassRef, OK
//    tkPointer, OK
//    tkProcedure OK

  case GetTypeKind(T) of
    tkWString: Result:= (CompareStr(WideString(L.p), WideString(R.p)));
    tkUString: Result:= (CompareStr(UnicodeString(L.p), UnicodeString(R.p)));
    tkLString: Result:= (CompareStr(AnsiString(L.p), AnsiString(R.p)));
    tkDynArray: Result:= (Compare_DynArray(L.p, R.p, ElementSize));
    tkVariant: Result:= (Compare_Variant(@L, @R));
    tkClass, tkClassRef, tkPointer, tkInterface, tkProcedure: Result:= (integer(L.nu > R.nu) - integer(L.nu < R.nu));
    tkMethod: Result:= integer((((NativeUInt(L.m.Data) > NativeUInt(R.m.Data)))
      or
      (((NativeUInt(L.m.Data) = NativeUInt(R.m.Data)) and (NativeUInt(L.m.Code) > NativeUInt(R.m.Code))))))
      -
      integer(((NativeInt(L.m.Data) < NativeInt(R.m.Data))
      or
      ((NativeInt(L.m.Data) = NativeInt(R.m.Data)) and (NativeInt(L.m.Code) < NativeInt(R.m.Code)))));
    else
  //Complex cases...
    case SizeOf(T) of
      0: Result:= (0);
      1: begin
        //Keep R in one case otherwise linker will eliminate parameter Right.
        {$ifdef FusedParameters}
        if (GetTypeKind(T) = tkInteger) and Signed then Result:= (L.i8) - int8(L.i32 shr 8)
        {$else}
        if (GetTypeKind(T) = tkInteger) and Signed then Result:= (L.i8) - (R.i8)
        {$endif}
        //Prevent unaligned byte read.
        else Result:= (L.u8) - (R.u8); //L is stored at [esp], R is stored at [esp+1]
      end;
      2: begin
        case GetTypeKind(T) of
          tkInteger: if Signed then Result:= (L.i16 - R.i16)
          //Keep R in one case otherwise linker will eliminate parameter Right.
          //L is stored at [esp], R is stored at [esp+2]
          //Prevent unaligned read by reading the byte in one go.
          {$ifdef FusedParameters}
          else Result:= (L.u16 - (L.u32 shr 16));
          {$else}
          else Result:= integer(L.u16) - integer(R.u16);
          {$endif}
          tkString: Result:= (integer(L.ps1 > R.ps1) - integer(L.ps1 < R.ps1));
          else Result:= (L.u16 - R.u16);
        end;
      end;
      3: begin
        case GetTypeKind(T) of
          tkString: Result:= (integer(L.ps2 > R.ps2) - integer(L.ps2 < R.ps2));
          else Result:= (BinaryCompare(@Left, @Right, SizeOf(T)));
        end;
      end;
      4: begin
        case GetTypeKind(T) of
          tkSet, tkRecord, tkEnumeration: begin
            Result:= (byte(L.u32 > R.u32) - byte(L.u32 < R.u32));
          end;
          tkFloat: Result:= (integer(L.f4 > R.f4) - integer(L.f4 < R.f4));
          tkInteger: if Signed then Result:= (L.i32 - R.i32)
          else Result:= (integer(L.u32 > R.u32) - integer(L.u32 < R.u32));
          tkString: Result:= (integer(L.ps3 > R.ps3) - integer(L.ps3 < R.ps3));
          else Result:= BinaryCompare4(L.u32, R.u32);
        end;
      end;
      5, 6, 7: begin
        case GetTypeKind(T) of
          //tkUnknown: if (SizeOf(T) = 6) then //Real48
          //    Result:= (integer(L.f6 > R.f6) - integer(L.f6 < R.f6));
          tkString: Result:= (Compare_PSn(L.ps, R.ps));
          tkFloat: Result:= (integer(L.f6 > R.f6) - integer(L.f6 < R.f6));
          else Result:= (FastBinaryCompare(@Left, @Right, SizeOf(T)));
        end
      end;
      8: begin
        case GetTypeKind(T) of
          tkInt64: begin
            if Signed then Result:= (integer(L.i64 > R.i64) - integer(L.i64 < R.i64))
            else Result:= (integer(L.u64 > R.u64) - integer(L.u64 < R.u64))
          end;
          tkFloat: Result:= (integer(L.f8 > R.f8) - integer(L.f8 < R.f8));
          else begin
{$IFDEF purepascal}
            Result:= (BinaryCompare8(@Left, @Right));
{$ELSE !purepascal}{$IFDEF CPUX64}
            Result:= (BinaryCompare8(L.u64, R.u64));
{$ELSE !CPUX64}
            Result:= (BinaryCompare8(@Left, @Right));
{$ENDIF}{$ENDIF}
          end;
        end;
      end;
      10: begin
        if GetTypeKind(T) = tkFloat then Result:= (integer(L.f10 > R.f10) - integer(L.f10 < R.f10))
        else Result:= FastBinaryCompare(@Left, @Right, SizeOf(T));
      end;
      else case GetTypeKind(T) of
        tkString: Result:= (Integer(L.ps > R.ps) - Integer(L.ps < R.ps));
        else Result:= FastBinaryCompare(@Left, @Right, SizeOf(T));
      end; {case}
    end;
  end;
end;


class function TComparer<T>.TDefault.TestCompareFast(const Left, Right: T): integer;
var
  L: TCast absolute Left;
  R: TCast absolute Right;
begin
  case GetTypeKind(T) of
    //tkUnknown: ;
    tkInteger: case SizeOf(T) of
      1: begin
        if Signed then Result:= CompareFast(L.i8, R.i8)
        else Result:= CompareFast(l.u8, R.u8);
      end;
      2: begin
        if Signed then Result:= CompareFast(L.i16, R.i16)
        else Result:= CompareFast(l.u16, R.u16);
      end;
      4: begin
        if Signed then Result:= CompareFast(L.i32, R.i32)
        else Result:= CompareFast(l.u32, R.u32);
      end;
    end;
    tkChar: Result:= CompareFast(L.ac, R.ac);
    //tkEnumeration: ;
    tkFloat: case SizeOf(T) of
      4: Result:= CompareFast(L.f4, R.f4);
      6: Result:= CompareFast(L.f6, R.f6);
      8: Result:= CompareFast(L.f8, R.f8);
      10: Result:= CompareFast(L.f10, R.f10);
    end;
    tkString: Result:= CompareFast(L.ps, R.ps);
    //tkSet: ;
    tkClass: Result:= CompareFast(TObject(L.p), TObject(R.p));
    //tkMethod: ;
    tkWChar: Result:= CompareFast(L.wc, R.wc);
    tkLString: Result:= CompareFast(AnsiString(L.p), AnsiString(R.p));
    tkWString: Result:= CompareFast(WideString(L.p), WideString(R.p));
    //tkVariant: ;//tkArray: ;//tkRecord: ;
    tkInterface: Result:= CompareFast(IInterface(L.p), IInterface(R.p));
    tkInt64: begin
      if Signed then Result:= CompareFast(l.i64, r.i64)
      else Result:= CompareFast(l.u64, r.u64);
    end;
    //tkDynArray:;
    tkUString: Result:= CompareFast(UnicodeString(L.p), UnicodeString(R.p));
    //tkClassRef: ;
    tkPointer: Result:= CompareFast(L.p, R.p);
    //tkProcedure: ;
    else Result:= Compare(Left,Right);
  end;
end;



class function TComparer<T>.TDefault.Equals(const Left, Right: T): Boolean;
begin
  Result:= Compare(Left, Right) = 0;
end;

class function TComparer<T>.TDefault.GetHashCode(const Value: T; seed: integer = 0): Integer;
var
  V: TCast absolute Value;
  VarStr: string;
begin
  case GetTypeKind(T) of
    tkUnknown, //Custom enums
    tkInteger, tkChar, tkEnumeration, tkSet, tkWChar, tkRecord, tkInt64, tkFloat, tkClass, tkMethod, tkInterface,
      tkClassRef, tkPointer, tkProcedure:
    begin
{$IFDEF CPUX64}
      Result:= MurmurHash3(V.u64, SizeOf(T), Seed);
{$ELSE}
      Result:= MurmurHash3(V.u32, SizeOf(T), Seed);
{$ENDIF}
    end;
    tkString: Result:= MurmurHash3(V.ps[low(string)], Length(V.ps) * SizeOf(V.ps[low(string)]), Seed);
    tkLString: Result:= MurmurHash3(AnsiString(V.p)[low(string)], Length(AnsiString(V.p)) * SizeOf(AnsiChar), Seed);
    tkUString:
    Result:= MurmurHash3(UnicodeString(V.p)[low(string)], Length(UnicodeString(V.p)) * SizeOf(WideChar), Seed);
    tkWString: Result:= MurmurHash3(WideString(V.p)[1], Length(WideString(V.p)) * SizeOf(WideChar), Seed);
    tkVariant: try
      VarStr:= PVariant(V.p)^;
      Result:= TComparer<string>.TDefault.GetHashCode(VarStr, Seed);
    except
      Result:= MurmurHash3(V.p^, SizeOf(Variant), Seed);
    end;
    tkArray: Result:= MurmurHash3(V.u8, SizeOf(T), Seed);
    tkDynArray: Result:= MurmurHash3(V.p^, GetTypeData(TypeInfo(T))^.elSize * DynLen(V.p), Seed);
  end;
end;

function CompareStr(const S1, S2: WideString): integer; overload;
//{$ifdef PurePascal}
var
  i: NativeInt;
  c1,c2: Char;
begin
  if pointer(S1) = pointer(S2) then exit(0);
  if pointer(S1) = nil then exit(-1);
  if pointer(S2) = nil then exit(1);
  i:= 0;
  while (true) do begin
    c1:= S1[i+1];
    c2:= S2[i+1];
    Result:= byte(c1 > c2) - byte(c1 < c2);
    if (integer(Result = 0) and integer(c1 <> #0) and integer(c2 <> #0)) = 0 then exit;
    inc(i);
  end;
end;
//{$else !PurePascal}
//{$ifdef CPUX64}
//asm
//
//end;
//{$else !CPUX64}
//asm
//
//end;
//{$endif}{$endif}


{$IFDEF CPUX64}
function CompareStrX64(const S1, S2: string): Integer;
asm //StackAligned
  {On entry:
     rCx = @S1[1]
     rdx = @S2[1]
     //preserve:RBX, RBP, RDI, RSI, R12-R15
   On exit:
     Result in eax:
       0 if S1 = S2,
       > 0 if S1 > S2,
       < 0 if S1 < S2
   Code size: ??? bytes}
        CMP RCX, RDX
        JE @SameString
        {Is either of the strings perhaps nil?}
        TEST RCX, RDX
        JZ @PossibleNilString
        {Compare the first two characters (there has to be a trailing #0). In random
         string compares this can save a lot of CPU time.}
@BothNonNil:
        {Compare the first two characters}
        MOV EAX, DWORD PTR [RCX]
        MOV R8d, DWORD PTR [RDX]
        CMP EAX, R8d
        JE @FirstTwoCharacterSame
        {First two characters differ}
        {Swap 2 and 1 char to the correct position}
        ROL EAX,16
        ROL R8d,16
        SUB EAX, R8d
        RET
@FirstTwoCharacterSame:
        {Save ebx}
        PUSH RBX
        {Set ebx = length(S1)}
        MOV EBX, [RCX - 4]
        MOV R10,RBX
        XOR R8, R8
        {Set ebx = length(S1) - length(S2)}
        MOV R9d,[RDX - 4]
        SUB RBX, R9
        {Save the length difference on the stack}
        MOV R9,RBX
        {Set ecx = 0 if length(S1) < length(S2), $ffffffff otherwise}
        ADC R8, -1
        {Set ecx = - min(length(S1), length(S2))}
        AND R8, RBX
        SUB R8, R10
        SAL R8, 1
        {Adjust the pointers to be negative based}
        SUB RCX, R8
        SUB RDX, R8
@CompareLoop:
        MOV EBX, [RCX + R8]
        XOR EBX, [RDX + R8]
        JNZ @Mismatch
        ADD R8, 4
        JS @CompareLoop
        {All characters match - return the difference in length}
@MatchUpToLength:
        MOV RAX,R9
        POP RBX
@Done:
        RET
@Mismatch:
//        BSF EBX, EBX      //where is differing bit?
//        SHR EBX, 4        //in first or second word?
//        ADD EBX, EBX
    {proposal:}           //BSF is an expensive instruction
        AND EBX, $FFFF
        SETZ BL
        ADD EBX,EBX
    { end proposal}
        ADD R8, RBX
        JNS @MatchUpToLength
        MOVZX EAX, WORD PTR [RCX + R8]
        MOVZX EDX, WORD PTR [RDX + R8]
        SUB EAX, EDX
        POP RBX
        RET
        {It is the same string}
@SameString:
        XOR EAX, EAX
        RET
        {Good possibility that at least one of the strings are nil}
@PossibleNilString:
        TEST RCX, RCX
        JZ @FirstStringNil
        TEST RDX, RDX
        JNZ @BothNonNil
        {Return first string length: second string is nil}
        //MOV EAX, [RCX - 4]
        MOV EAX,1
        RET
@FirstStringNil:
        {Return 0 - length(S2): first string is nil}
        //SUB EAX, [RDX - 4]
        DEC EAX
end;
{$ENDIF CPUX86}

function BobJenkinsHash(const HashData; Len, Seed: Integer): Integer;
begin
  Result:= System.Generics.Defaults.BobJenkinsHash(HashData,Len,Seed);
end;

{$pointermath on}
function PascalMurmurHash3(const [ref] HashData; Len, Seed: Integer): Integer;
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
  k: integer;
  remaining: Integer;
  data: PCardinal;
label case1, case2, case3, final;
type
  ByteArray = array[0..0] of byte;
begin
  Result:= seed;
  data:= @HashData;
  for i:= 0 to (Len shr 2)-1 do begin
    k:= data[i];
    k:= k * integer(c1);
    k:= (k shl r1) or (k shr (32-r1));
    k:= k * c2;
    Result:= Result xor k;
    Result:= (Result shl r2) or (Result shr (32-r2));
    Result:= Result * m + integer(n);
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
  remaining:= remaining * integer(c1);
  remaining:= (remaining shl r1) or (remaining shr (32 - r1));
  remaining:= remaining * c2;
  Result:= Result xor remaining;
final:
  Result:= Result xor Len;

  Result:= Result xor (Result shr 16);
  Result:= Result * integer(f1);
  Result:= Result xor (Result shr 13);
  Result:= Result * integer(f2);
  Result:= Result xor (Result shr 16);
end;

function MurmurHash3(const [ref] HashData; Len: integer; Seed: Integer = 0): Integer;
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
end;
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
end;
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
end;
{$endif}
{$endregion}
{$endif}



class constructor TComparer<T>.TDefault.Init;
begin
  fSigned:= false;
  case GetTypeKind(T) of
    tkInteger: begin
      fSigned:= GetTypeData(TypeInfo(T))^.OrdType in [otSByte, otSWord, otSLong];
    end;
    tkInt64: begin
      fSigned:= GetTypeData(TypeInfo(T))^.MaxInt64Value > GetTypeData(TypeInfo(T))^.MinInt64Value;
    end;
    tkUnknown: fSigned:= SizeOf(T) = 6; //Real48;
    tkFloat: fSigned:= true;
  end;
  fElementSize:= 0;
  if GetTypeKind(T) = tkDynArray then fElementSize:= PTypeInfo(TypeInfo(T)).TypeData.elSize;
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


function CompareFast(const Left, Right: pointer): integer;
begin
  Result:= integer(NativeUInt(Left) > NativeUInt(Right)) - integer(NativeUInt(Left) < NativeUInt(Right));
end;

function CompareFast(const Left, Right: UTF8String): integer;
begin
  Result:= CompareFast(Ansistring(Left), Ansistring(Right));
end;

function CompareFast(const Left, Right: RawByteString): integer;
begin
  Result:= CompareFast(Ansistring(Left), Ansistring(Right));
end;

function CompareFast(const Left, Right: WideString): integer;
begin
  Result:= FastDefaults.CompareStr(Left, Right);
end;

function CompareFast(const Left, Right: UnicodeString): integer;
begin
  {$IFDEF PurePascal}
  Result:= CompareStr(Left, Right);
  {$ELSE !PurePascal}
  {$IFDEF CPUX64}
  Result:= CompareStrX64(Left, Right);
  {$ELSE !CPUX64}
  Result:= CompareStr(Left, Right);
  {$ENDIF}{$ENDIF}
end;

function CompareFast(const Left, Right: AnsiString): integer;
begin
  Result:= CompareStr(Left, Right);
end;

function CompareFast(const Left, Right: OpenString): integer;
begin
  Result:= CompareStr(Left, Right)
end;

function CompareFast(const Left, Right: bytebool): integer;
begin
  Result:= byte(byte(Left) <> 0) - byte(byte(Right) <> 0);
end;

function CompareFast(const Left, Right: longbool): integer;
begin
  Result:= byte(integer(Left) <> 0) - byte(integer(Right) <> 0);
end;

function CompareFast(const Left, Right: wordbool): integer;
begin
  Result:= byte(integer(Left) <> 0) - byte(integer(Right) <> 0);
end;

function CompareFast(const Left, Right: boolean): integer;
begin
  Result:= byte(byte(Left) <> 0) - byte(byte(Right) <> 0);
end;

function CompareFast(const Left, Right: Currency): integer;
begin
  Result:= byte(Left > Right) - byte(Left < Right);
end;

function CompareFast(const Left, Right: Comp): integer;
begin
  Result:= byte(Left > Right) - byte(Left < Right);
end;

function CompareFast(const Left, Right: extended): integer;
begin
  Result:= byte(Left > Right) - byte(Left < Right);
end;

function CompareFast(const Left, Right: double): integer;
begin
  Result:= byte(Left > Right) - byte(Left < Right);
end;

function CompareFast(const Left, Right: Real48): integer;
begin
  Result:= byte(Left > Right) - byte(Left < Right);
end;

function CompareFast(const Left, Right: single): integer;
begin
  Result:= byte(Left > Right) - byte(Left < Right);
end;

function CompareFast(const Left, Right: UCS4Char): integer;
begin
  Result:= CompareFast(Cardinal(Left), Cardinal(Right));
end;

function CompareFast(const Left, Right: WideChar): integer;
begin
  Result:= CompareFast(Word(Left), Word(Right));
end;

function CompareFast(const Left, Right: AnsiChar): integer;
begin
  Result:= CompareFast(Byte(Left), Byte(Right));
end;

function CompareFast(const Left, Right: NativeUInt): integer;
begin
  Result:= byte(Left > Right) - byte(Left < Right);
end;

function CompareFast(const Left, Right: NativeInt): integer;
begin
  Result:= byte(Left > Right) - byte(Left < Right);
end;

function CompareFast(const Left, Right: int64): integer;
begin
  Result:= byte(Left > Right) - byte(Left < Right);
end;

function CompareFast(const Left, Right: uint64): integer;
begin
  Result:= byte(Left > Right) - byte(Left < Right);
end;

function CompareFast(const Left, Right: integer): integer;
begin
  Result:= Left - Right;
end;

function CompareFast(const Left, Right: cardinal): integer;
begin
  Result:= byte(Left > Right) - byte(Left < Right);
end;

function CompareFast(const Left, Right: int16): integer;
begin
  Result:= Integer(Left) - Integer(Right);
end;

function CompareFast(const Left, Right: word): integer;
begin
  Result:= Integer(Left) - Integer(Right);
end;

function CompareFast(const Left, Right: int8): integer;
begin
  Result:= Integer(Left) - Integer(Right);
end;

function CompareFast(const Left, Right: byte): integer;
begin
  Result:= Integer(Left) - Integer(Right);
end;

function CompareFast(const Left, Right: TObject): integer;
begin
  Result:= CompareFast(NativeUInt(Left), NativeUInt(Right));
end;

function CompareFast(const Left, Right: IInterface): integer;
begin
  Result:= CompareFast(NativeUInt(Left), NativeUInt(Right));
end;

end.
