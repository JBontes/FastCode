(*******************************************************
 FastDefauts
 A fast and small replacement for the interface
 based comparers available in `System.Generics.Defaults` since D2009.
 This works best in XE7 and above because it depends
 on the compiler intrinsic `GetTypeKind` and its compile time
 resolution of types.
 It should compile and run in 2009 and beyond, but the benefits are much less.

 Alpha version 0.2, fully tested in Win32, Win64 and pure Pascal

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

{$define PurePascal}

type

  TComparison<T> = function(const Left, Right: T): integer;

  TRec<T> = record
  private
    class var FCompare: TComparison<T>;
    FData: T;
    class constructor Init;
  public
    class operator Explicit(const a: T): TRec<T>; static; inline;
    class operator Implicit(const a: T): TRec<T>; static; inline;
    class operator Implicit(const a: TRec<T>): T; static; inline;
    class operator GreaterThan(const L,R: TRec<T>): boolean; static; inline;
    class operator Equal(const L,R: TRec<T>): boolean; static; inline;
    class operator LessThan(const L,R: TRec<T>): boolean; static; inline;
    class operator NotEqual(const L, R: TRec<T>): boolean; static; inline;
  end;

  TEqualityComparison<T> = function(const Left, Right: T): boolean;
  THasher<T> = function(const Value: T): integer;

  PCast = ^TCast;
  TCast = record
    strict private type
    TBuiltinType = (
      btByte, btint8, btword, btint16, btcardinal, btinteger, btUint64, btInt64,
      btNativeInt, btNativeUint, btAnsiChar, btWideChar, btUCS4Char,
      btSingle, btReal48, btdouble, btextended, btComp, btCurrency,
      btboolean, btwordbool, btlongbool, btbytebool,
      btStr1, btStr2, btStr3, btOpenString,
      btpointer, btInterface, btObject, btVariant, btOleVariant,
      btMethod, btClass, btByteArray);
      strict private type
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
    public
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
          btClass: (o: TObject);
          btByteArray: (arr: array[0..0] of byte);
        end;

  /// <summary>
  /// Usage: Equal:= TComparer<integer>.TDefault.Equals(int1, int2);
  ///        LessThan:= TComparer<integer>.TDefault.CompareFast(int1, int2) < 0;
  ///        GreaterThanOrEqual:= TComparer<integer>.TDefault.CompareFast(int1, int2) >= 0;
  ///        etc..
  ///  Obviously T can be any type whatsoever.
  ///  All comparisons will be put inline, these will be very short code snippets
  ///  or calls to optimized routines.
  /// </summary>
  IComparer<T> = class
    strict private
      class var fSigned: boolean;
      class var fElementSize: NativeUInt;
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
      class function Compare(const Left, Right: T): integer; overload; static; //inline;
//      class function Compare(const Left, Right: byte): integer; overload; //inline;
//      class function Compare(const Left, Right: int8): integer; overload; //inline;

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
      class function Equals(const Left, Right: T): boolean; static; inline;
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
      class function GetHashCode(const Value: T; Seed: integer = 0): integer; static; inline;
    end;


  TComparer<T> = class
  public
    class var Default: IComparer<T>;
    class function Construct(const Comparison: TComparison<T>): TComparer<T>;
    function Compare(const Left, Right: T): Integer; virtual; abstract;
  end;


  TDelegatedComparer<T> = class(TComparer<T>)
  private
    FCompare: TComparison<T>;
  public
    constructor Create(const ACompare: TComparison<T>);
    function Compare(const Left, Right: T): Integer; override;
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


function Compare_Variant(Left, Right: pointer): integer;
function BinaryCompare(Left, Right: pointer; Size: integer): integer;
function FastBinaryCompare(Left, Right: pointer; Size: integer): integer;
function BinaryCompare4(Left, Right: Cardinal): integer;
{$IFDEF purepascal}
function BinaryCompare8(const Left, Right: pointer): integer;
{$ELSE !purepascal}{$IFDEF CPUX64}
function BinaryCompare8(const Left, Right: UInt64): integer;
{$ELSE !CPUX64}
function BinaryCompare8(const Left, Right: pointer): integer;
{$ENDIF}{$ENDIF}
function BinaryCompare3(const Left, Right: pointer): integer;
function Compare_DynArray(Left, Right: pointer; ElementSize: integer): NativeInt;
function Compare_PSn(const Left, Right: OpenString): integer; inline;

  //For testing purposes
function PascalMurmurHash3(const [ref] HashData; Len, Seed: integer): integer;
function MurmurHash3(const [ref] HashData; Len: integer; Seed: integer = 0): integer;{$ifdef purepascal}inline;{$endif}
function BobJenkinsHash(const HashData; Len, Seed: integer): integer; inline;

function CompareWideStr(const S1, S2: WideString): integer; overload;
function CompareUnicodeStr(const S1, S2: string): integer; overload;
function SameUnicodeStr(const S1, S2: string): boolean; overload;
function SameWideStr(const S1, S2: WideString): boolean; overload;
function BinaryEquals(Left, Right: pointer; Size: integer): boolean;
function DynLen(Arr: pointer): NativeInt; inline;

type
  PInt8 = ^Int8;
  PInt16 = ^Int16;
  TPS1 = string[1];
  PStr1 = ^TPS1;
  TPS2 = string[2];
  PStr2 = ^TPS2;
  TPS3 = string[3];
  PStr3 = ^TPS3;
  TPSn = shortstring;
  PStrn = ^TPSn;
  PReal48 = ^Real48;

implementation

uses
{$IFNDEF NEXTGEN}
  System.AnsiStrings,
{$ENDIF}
{$IF CompilerVersion < 28}  //Below XE7
  System.RTTi,
{$ENDIF}
  System.Math, System.Generics.Collections, System.Variants, System.Generics.Defaults, System.TypInfo
{$IFDEF  MSWindows}
    , WinApi.Windows
{$ENDIF}
  ;

{ TCompareRec<T> }

class constructor TRec<T>.Init;
begin
  TRec<T>.FCompare:= TComparer<T>.Default.Compare;
end;

class operator TRec<T>.Implicit(const a: TRec<T>): T;
begin
  Result:= a.FData;
end;

class operator TRec<T>.Implicit(const a: T): TRec<T>;
begin
  Result.FData:= a;
end;

class operator TRec<T>.GreaterThan(const L, R: TRec<T>): boolean;
begin
  Result:= (FCompare(L,R) > 0);
end;

class operator TRec<T>.Equal(const L, R: TRec<T>): boolean;
begin
  Result:= (FCompare(L,R) > 0);
end;

class operator TRec<T>.NotEqual(const L, R: TRec<T>): boolean;
begin
  Result:= (FCompare(L,R) <> 0);
end;

class operator TRec<T>.LessThan(const L, R: TRec<T>): boolean;
begin
  Result:= (FCompare(L,R) < 0);
end;

class operator TRec<T>.Explicit(const a: T): TRec<T>;
begin
  Result.FData:= a;
end;

function Compare_Variant(Left, Right: pointer): integer;
var
  l, r: Variant;
  lAsString, rAsString: string;
  varTypeLeft, varTypeRight: integer;
begin
  Result:= 0; // Avoid warning.
  l:= PVariant(Left)^;
  r:= PVariant(Right)^;
  try
    case System.Variants.VarCompareValue(l, r) of
      vrEqual: Exit(0);
      vrLessThan: Exit( -1);
      vrGreaterThan: Exit(1);
      vrNotEqual: begin
        if VarIsEmpty(l) or VarIsNull(l) then Exit(1)
        else Exit( -1);
      end;
    end;
  except // if comparison failed with exception, compare as string.
    try
      //Prevent type conversions
      varTypeLeft:= VarType(l) and varTypeMask;
      varTypeRight:= VarType(r) and varTypeMask;
      if ((varTypeLeft = varUString) or (varTypeRight = varUString)) or
        ((varTypeLeft = varOleStr) or (varTypeRight = varOleStr)) then begin
        Result:= FastDefaults.CompareUnicodeStr(UnicodeString(l), UnicodeString(r));
      end else if (varTypeLeft = varString) or (varTypeRight = varString) then begin
        Result:= CompareWideStr(AnsiString(l), AnsiString(r));
      end else begin
        lAsString:= PVariant(Left)^;
        rAsString:= PVariant(Right)^;
        Result:= FastDefaults.CompareUnicodeStr(lAsString, rAsString);
      end;
    except // if comparison fails again, compare bytes.
      Result:= FastBinaryCompare(Left, Right, SizeOf(Variant));
    end;
  end;
end;


function BinaryCompare4(Left, Right: Cardinal): integer;
{$IFDEF purepascal}
var
  i: integer;
begin
  Result:= (integer(Left > Right) - integer(Left < Right));
end;
{$ELSE !PurePascal}
{$IFDEF CPUX64}
asm
  .NOFRAME
  //Left: RCX
  //Right: RDX
  //preserve:RBX, RBP, RDI, RSI, R12-R15
  //bswap ECX
  //bswap EDX
  cmp    ECX,EDX
  sbb    EAX,EAX
  cmp    EDX,ECX
  adc    EAX,0
end;
{$ENDIF}
{$IFDEF CPUX86}
asm
  //Left: EAX
  //Right: EDX
  //Size: ECX
  cmp    eax,edx
  seta   al
  movzx  eax,al
  sbb    eax,0
end;
{$ENDIF}
{$ENDIF !PurePascal}


{$IFDEF purepascal}
function BinaryCompare8(const Left, Right: pointer): integer;
var
  pl, pr: PByte;
  i: integer;
begin
  pl:= Left;
  pr:= Right;
  for i := 0 to 7 do begin
    Result:= pl^ - pr^;
    if Result <> 0 then Exit;
    Inc(pl);
    Inc(pr);
  end;
  Result := 0;
end;
{$ELSE !PurePascal}
{$IFDEF CPUX64}
function BinaryCompare8(const Left, Right: UInt64): integer;
asm
  .NOFRAME
  //Left: RCX
  //Right: RDX
  //preserve:RBX, RBP, RDI, RSI, R12-R15
  bswap RCX
  bswap RDX
  xor    eax,eax
  cmp    rcx,rdx
  seta   al
  sbb    eax,0
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
  cmp  EBX, ECX
  jnz @done
  mov  EBX, [EAX+4]
  mov  ECX, [EDX+4]
  bswap EBX
  bswap ECX
  cmp  EBX, ECX
@done:
  sbb  EAX,EAX
  cmp  ECX,EBX
  adc  EAX,0
  pop  EBX
end;
{$ENDIF}
{$ENDIF !PurePascal}

function BinaryCompare3(const Left, Right: pointer): integer;
{$IFDEF PurePascal}
var
  pl, pr: PByte;
  i: integer;
begin
  pl:= Left;
  pr:= Right;
  for i := 0 to 2 do begin
    Result:= pl^ - pr^;
    if Result <> 0 then Exit;
    Inc(pl);
    Inc(pr);
  end;
  Result := 0;
end;
{$ELSE !PurePascal}
{$IFDEF CPUX64}
asm  //Left RCX, Right RDX
  movzx R8,  word ptr [RCX]
  shl   R8,  16
  mov   R8b, byte ptr [RCX+2]
  bswap R8
  movzx R9,  word ptr [RCX]
  shl   R9,  16
  mov   R9b, byte ptr [RCX+2]
  bswap R9
  cmp   R8,  R9
  sbb   EAX, EAX
  cmp   R9,  R8
  adc   EAX, 0
end;
{$ELSE CPUX86}
asm
  push  EBX
  movzx EBX, word ptr [EAX]
  shl   EBX, 16
  mov   BL,  byte ptr [EAX+2]
  bswap EBX
  movzx ECX, word ptr [EDX]
  shl   ECX, 16
  mov   CL,  byte ptr [EDX+2]
  bswap ECX
  cmp   EBX, ECX
  sbb   EAX, EAX
  cmp   ECX, EBX
  adc   EAX, 0
  pop   EBX
end;
{$ENDIF}{$ENDIF}

//Size is not 1,2,3,4 or 8
function BinaryCompare(Left, Right: pointer; Size: integer): integer;
{$IFDEF purepascal} // pure pascal
type
  TBSwap = record
    case integer of
      1: (arr: array[0..SizeOf(NativeInt)-1] of byte);
      2: (ni: nativeInt);
      3: (i: integer);
  end;
var
  Same: boolean;
  i: integer;
  Lq, Rq: PNativeUInt;
  Li, Ri: PInteger;
  SwapL, SwapR: TBSwap;
label
  DifferentInteger, DifferentNativeInt;
begin
  if (SizeOf(NativeUInt) > SizeOf(Integer)) and (Size < SizeOf(NativeUInt)) then begin
    Li:= Left;
    Ri:= Right;
    for i:= 0 to (Size div SizeOf(integer)) - 1 do begin
      Same:= Li^ = Ri^;
      if not(Same) then goto DifferentInteger;
      Inc(Li);
      Inc(Ri);
    end;
    // Unaligned test for few remaining bytes
    NativeInt(Li):= NativeInt(Li) + (Size and (SizeOf(integer)- 1)) - SizeOf(integer);
    NativeInt(Ri):= NativeInt(Ri) + (Size and (SizeOf(integer)- 1)) - SizeOf(integer);
    if (Li^ = Ri^) then Result:= 0
    else begin
DifferentInteger:
      SwapL.i:= Li^;
      SwapR.i:= Ri^;
      for i := 3 downto 0 do begin
        Result:= SwapL.arr[i] - SwapR.arr[i];
        if Result <> 0 then exit;
      end;
    end;
  end else begin
    Lq:= Left;
    Rq:= Right;
    for i:= 0 to (Size div SizeOf(NativeUInt)) - 1 do begin
      Same:= Lq^ = Rq^;
      if not(Same) then goto DifferentNativeInt;
      Inc(Lq);
      Inc(Rq);
    end;
    // Unaligned test for few remaining bytes
    NativeUInt(Lq):= NativeUInt(Lq) + (Size and (SizeOf(NativeUInt)- 1)) - SizeOf(NativeUInt);
    NativeUInt(Rq):= NativeUInt(Rq) + (Size and (SizeOf(NativeUInt)- 1)) - SizeOf(NativeUInt);
    if (Lq^ = Rq^) then Result:= 0
    else begin
DifferentNativeInt:
      SwapL.ni:= Lq^;
      SwapR.ni:= Rq^;
      for i := SizeOf(NativeInt)-1 downto 0 do begin
        Result:= SwapL.arr[i] - SwapR.arr[i];
        if Result <> 0 then exit;
      end;
    end;
  end;
end;
{$ELSE !PurePascal}
{$IFDEF CPUX64}
asm
  .NOFRAME
  xchg  RSI,RAX
  //Left: RCX
  //Right: RDX
  //Size: R8   preserve:RBX, RBP, RDI, RSI, R12-R15
  mov   RSI, RCX
  xchg  RDI, RDX
  mov   RCX, R8
  repe  cmpsb
  xchg  RSI,RAX
  seta  AL
  xchg  RDI, RDX
  movzx EAX,AL
  sbb   EAX,0
  ret
end;
{$ENDIF}
{$IFDEF CPUX86}
asm
  //Left: EAX
  //Right: EDX
  //Size: ECX
  xchg esi,eax
  xchg edi,edx
  repe cmpsb
  xchg esi,eax
  xchg edi,edx
  seta AL
  movzx EAX,AL
  sbb  EAX,0
end;
{$ENDIF}
{$ENDIF !PurePascal}

// Cannot be used for comparisons shorter than 4 bytes.
function BinaryEquals(Left, Right: pointer; Size: integer): boolean;
{$IFDEF purepascal}
var
  i: integer;
  Lq, Rq: PNativeUInt;
  Li, Ri: PInteger;
begin
  if (SizeOf(NativeUInt) > SizeOf(Integer)) and (Size < SizeOf(NativeUInt)) then begin
    Li:= Left;
    Ri:= Right;
    for i:= 0 to (Size div SizeOf(integer)) - 1 do begin
      Result:= Li^ = Ri^;
      if not(Result) then Exit;
      Inc(Li);
      Inc(Ri);
    end;
    // Unaligned test for few remaining bytes
    NativeInt(Li):= NativeInt(Li) + (Size and (SizeOf(integer)- 1)) - SizeOf(integer);
    NativeInt(Ri):= NativeInt(Ri) + (Size and (SizeOf(integer)- 1)) - SizeOf(integer);
    Result:= Li^ = Ri^;
  end else begin
    Lq:= Left;
    Rq:= Right;
    for i:= 0 to (Size div SizeOf(NativeUInt)) - 1 do begin
      Result:= Lq^ = Rq^;
      if not(Result) then Exit;
      Inc(Lq);
      Inc(Rq);
    end;
    // Unaligned test for few remaining bytes
    NativeUInt(Lq):= NativeUInt(Lq) + (Size and (SizeOf(NativeUInt)- 1)) - SizeOf(NativeUInt);
    NativeUInt(Rq):= NativeUInt(Rq) + (Size and (SizeOf(NativeUInt)- 1)) - SizeOf(NativeUInt);
    Result:= Lq^ = Rq^;
  end;
end;
{$ELSE !PurePascal}
{$IFDEF CPUX64}
//RCX: Left
//RDC: Right
//R8: size
asm
  .NOFRAME
  neg  R8
  //jz @equal
  sub  RCX, R8
  sub  RDX, R8
@loop8:
  add  R8,8
  jns   @check4
  mov  RAX,[RCX+R8-8]
  xor  RAX,[RDX+R8-8]
  jz @loop8
@different:
  xor eax,eax
  ret
@check4:
  sub r8,4
  jg @smaller
  mov  eax,[rcx+r8-4]
  xor  eax,[rdx+r8-4]
  jnz @different
@smaller:
  and r8,-4
  jz @equal
  mov  eax,[RCX+R8]
  xor  eax,[RDX+R8]
  jnz @different
@equal:
  mov eax,1
end;
{$ELSE !CPUX64}
//EAX: Left
//EDX: Right
//ECX: size
asm
  push EBX
  neg  ECX
  //jz @equal
  sub  EAX, ECX
  sub  EDX, ECX
@loop8:
  add  ECX,8
  jns   @check4
  mov  EBX,[EAX+ECX-8]
  xor  EBX,[EDX+ECX-8]
  jnz @different
  mov  EBX,[EAX+ECX-8+4]
  xor  EBX,[EDX+ECX-8+4]
  jz @loop8
@different:
  xor eax,eax
  pop  EBX
  ret
@check4:
  sub  ECX,4
  jg @smaller
  mov  EBX,[EAX+ECX-4]
  xor  EBX,[EDX+ECX-4]
  jnz @different
@smaller:
  and  ECX,-4
  jz @equal
  mov  EBX,[EAX+ECX]
  xor  EBX,[EDX+ECX]
  jnz @different
@equal:
  mov  AL,1
  pop  EBX
end;
{$ENDIF}
{$ENDIF}

function FastBinaryCompare(Left, Right: pointer; Size: integer): integer;
{$IFDEF purepascal} //pure pascal
var
  i: integer;
begin
  for i:= 0 to Size-1 do begin
    Result:= PByte(Left)[i] - PByte(Right)[i];
    if Result <> 0 then Exit;
  end;
end;
{$ELSE !PurePascal}
{$IFDEF CPUX64}
asm
  .NOFRAME
  //Left: RCX
  //Right: RDX
  //Size: R8   preserve:RBX, RBP, RDI, RSI, R12-R15
  xor  RAX, RAX
  mov  R11, R8
  and  R11, $7
  xor  R8,  R11
  jz @loop2
  neg  R8
  sub  RCX, R8
  sub  RDX, R8
@loop:
  mov  RAX, [RCX+R8]
  bswap RAX
  mov  R9, [RDX+R8]
  bswap R9
  sub RAX,R9
  jnz @different
  add  R8, 8
  jnz @loop
@loop2:
  dec  R11
  js @same
  movzx  RAX, byte ptr [RCX+R8]
  movzx  R9, byte ptr [RDX+R8]
  inc  R8
  sub RAX,R9
  jz @loop2
@different:
  sbb  RAX,RAX
  sbb  RAX,-1
@same:
end;

{$ENDIF}
{$IFDEF CPUX86}
asm
  // //Left: EAX
  // //Right: EDX
  // //Size: ECX
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

function DynLen(Arr: pointer): NativeInt; inline;
begin
  if Arr = nil then Result:= 0
  else Result:= PNativeInt(PByte(Arr) - SizeOf(NativeInt))^;
end;

function Compare_DynArray(Left, Right: pointer; ElementSize: integer): NativeInt;
var
  LenL, LenR, lenDiff: NativeInt;
begin
  LenL:= DynLen(Left);
  LenR:= DynLen(Right);
  lenDiff:= LenL - LenR;
  LenL:= Min(LenL, LenR);
  Result:= FastBinaryCompare(Left, Right, ElementSize * LenL);
  if Result = 0 then Result:= lenDiff;
end;

function Compare_PSn(const Left, Right: OpenString): integer;
begin
  Result:= (integer(Left > Right) - integer(Left < Right));
end;

{$UNDEF FusedParameters}
{$IFDEF CPUX86}
{$IF CompilerVersion >= 28}
{$IF CompilerVersion <= 29}
{$DEFINE FusedParameters}
{$ENDIF}{$ENDIF}{$ENDIF}

class function IComparer<T>.Compare(const Left, Right: T): integer;
//var
//  l: TCast absolute Left;
//  r: TCast absolute Right;
//  //X: TTypeKind;
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
    //tkWString: Result:= (CompareStr(WideString(l.p), WideString(r.p)));
    tkWString: Result:= (CompareWideStr(WideString((@Left)^), WideString((@Right)^)));
    tkUString: Result:= (FastDefaults.CompareUnicodeStr(PUnicodeString(@Left)^, PUnicodeString(@Right)^));
    tkLString: Result:= (CompareWideStr(PAnsiString(@Left)^, PAnsiString(@Right)^));
    tkDynArray: Result:= (Compare_DynArray(ppointer(@Left)^, ppointer(@Right)^, ElementSize));
    tkVariant: Result:= (Compare_Variant(@left, @right));
    tkClass, tkClassRef, tkPointer, tkInterface, tkProcedure: Result:= (integer(PNativeUint(@Left)^ > PNativeUint(@Right)^) - integer(PNativeUInt(@Left)^ < PNativeUInt(@Right)^));
    tkMethod: Result:= integer((((NativeUInt(PMethod(@left)^.Data) > NativeUInt(PMethod(@Right)^.Data))) or
      (((NativeUInt(PMethod(@Left)^.Data) = NativeUInt(PMethod(@Right)^.Data)) and (NativeUInt(PMethod(@Left)^.Code) > NativeUInt(PMethod(@Right)^.Code)))))) -
      integer(((NativeInt(PMethod(@Left)^.Data) < NativeInt(PMethod(@Right)^.Data)) or ((NativeInt(PMethod(@Left)^.Data) = NativeInt(PMethod(@Right)^.Data)) and
      (NativeInt(PMethod(@Left)^.Code) < NativeInt(PMethod(@Right)^.Code)))));
    else
  //Complex cases...
    case SizeOf(T) of
      0: Result:= (0);
      1: begin
        //Keep R in one case otherwise linker will eliminate parameter Right.
        //{$IFDEF FusedParameters}
        // if (GetTypeKind(T) = tkInteger) and Signed then Result:= (l.i8) - int8(l.i32 shr 8)
        if TypeInfo(T) = TypeInfo(ShortInt) then Result:= (PInt8(@Left)^) - (PInt8(@Right)^)
        else if TypeInfo(T) = TypeInfo(byte) then Result:= (PByte(@Left)^) - (PByte(@Right)^)
        else if GetTypeKind(T) <> tkInteger then Result:= (PByte(@Left)^) - (PByte(@Right)^)
        {$IFDEF win32}
        else if Signed then Result:= (PInt8(@Left)^) - int8(PInteger(@Left)^ shr 8)
        {$ELSE}
        else if Signed then Result:= (PInt8(@Left)^) - (PInt8(@Right)^)
        {$ENDIF}
        else Result:= (PByte(@Left)^) - (PByte(@Right)^)
      end;
      2: begin
        case GetTypeKind(T) of
          tkInteger: begin
            if TypeInfo(T) = TypeInfo(SmallInt) then Result:= (PInt16(@Left)^ - PInt16(@Right)^)
            else if TypeInfo(T) = TypeInfo(word) then Result:= integer(PWord(@Left)^) - integer(PWord(@Right)^)
            //Keep R in one case otherwise linker will eliminate parameter Right.
            //L is stored at [esp], R is stored at [esp+2]
            //Prevent unaligned read by reading the word in one go.
            //{$IFDEF FusedParameters}
            //else Result:= (l.u16 - (l.u32 shr 16));
            else if Signed then Result:= (PInt16(@Left)^ - PInt16(@Right)^)
            else Result:= integer(PWord(@Left)^) - integer(PWord(@Right)^)
          end;
          tkString: Result:= (integer(PStr1(@Left)^ > PStr1(@Right)^) - integer(PStr1(@Left)^ < PStr1(@Right)^));
          else Result:= integer(PWord(@Left)^) - integer(PWord(@Right)^)
        end;
      end;
      3: begin
        case GetTypeKind(T) of
          tkString: Result:= (integer(PStr2(@Left)^ > PStr2(@Right)^) - integer(PStr2(@Left)^ < PStr2(@Right)^));
          else Result:= (FastBinaryCompare(@Left, @Right, SizeOf(T)));
        end;
      end;
      4: begin
        case GetTypeKind(T) of
          tkSet, tkRecord, tkEnumeration: begin
            Result:= (integer(PCardinal(@Left)^ > PCardinal(@Right)^) - integer(PCardinal(@Left)^ < PCardinal(@Right)^));
          end;
          tkFloat: Result:= (integer(PSingle(@Left)^ > PSingle(@Right)^) - integer(PSingle(@Left)^ < PSingle(@Right)^));
          tkInteger: begin
            if (TypeInfo(T) = TypeInfo(integer))
                     or (TypeInfo(T) = TypeInfo(CppLongInt))
                     or (TypeInfo(T) = TypeInfo(HResult))
            then Result:= (PInteger(@Left)^ - PInteger(@Right)^)
            else if (TypeInfo(T) = TypeInfo(Cardinal))
                 or (TypeInfo(T) = TypeInfo(CppULongInt))
                 or (TypeInfo(T) = TypeInfo(UCS4Char))
                 {$IFDEF MSWindows}
                 or (TypeInfo(T) = TypeInfo(Handle_ptr))
                 or (TypeInfo(T) = TypeInfo(HWND))
                 or (TypeInfo(T) = TypeInfo(HHook))
                 or (TypeInfo(T) = TypeInfo(HGDIOBJ))
                 or (TypeInfo(T) = TypeInfo(HACCEL))
                 or (TypeInfo(T) = TypeInfo(HBITMAP))
                 or (TypeInfo(T) = TypeInfo(HBRUSH))
                 or (TypeInfo(T) = TypeInfo(HCOLORSPACE))
                 or (TypeInfo(T) = TypeInfo(HDC))
                 or (TypeInfo(T) = TypeInfo(HGLRC))
                 or (TypeInfo(T) = TypeInfo(HDESK))
                 or (TypeInfo(T) = TypeInfo(HENHMETAFILE))
                 or (TypeInfo(T) = TypeInfo(HFONT))
                 or (TypeInfo(T) = TypeInfo(HICON))
                 or (TypeInfo(T) = TypeInfo(HMENU))
                 or (TypeInfo(T) = TypeInfo(HMETAFILE))
                 or (TypeInfo(T) = TypeInfo(HPALETTE))
                 or (TypeInfo(T) = TypeInfo(HPEN))
                 or (TypeInfo(T) = TypeInfo(HRGN))
                 or (TypeInfo(T) = TypeInfo(HSTR))
                 or (TypeInfo(T) = TypeInfo(HTASK))
                 or (TypeInfo(T) = TypeInfo(HWINSTA))
                 or (TypeInfo(T) = TypeInfo(HKL))
                 or (TypeInfo(T) = TypeInfo(HKEY))
                 or (TypeInfo(T) = TypeInfo(HGESTUREINFO))
                 {$ENDIF}
            then Result:= (integer(PCardinal(@Left)^ > PCardinal(@Right)^) - integer(PCardinal(@Left)^ < PCardinal(@Right)^))
            else if Signed then Result:= (PInteger(@Left)^ - PInteger(@Right)^)
            else Result:= (integer(PCardinal(@Left)^ > PCardinal(@Right)^) - integer(PCardinal(@Left)^ < PCardinal(@Right)^));
          end;
          tkString: Result:= (integer(PStr3(@Left)^ > PStr3(@Right)^) - integer(PStr3(@Left)^ < PStr3(@Right)^));
          tkPointer: Result:= (integer(PCardinal(@Left)^ > PCardinal(@Right)^) - integer(PCardinal(@Left)^ < PCardinal(@Right)^))
          else Result:= BinaryCompare4(PCardinal(@Left)^, PCardinal(@Right)^);
        end;
      end;
      5, 6, 7: begin
        case GetTypeKind(T) of
          tkString: Result:= (Compare_PSn(PStrn(@Left)^, PStrn(@Right)^));
          tkFloat: Result:= (integer(PReal48(@Left)^ > PReal48(@Right)^) - integer(PReal48(@Left)^ < PReal48(@Right)^));
          else Result:= (FastBinaryCompare(@Left, @Right, SizeOf(T)));
        end
      end;
      8: begin
        case GetTypeKind(T) of
          tkInt64: begin
            if (TypeInfo(T) = TypeInfo(Int64)) then begin
              Result:= (integer(PInt64(@Left)^ > PInt64(@Right)^) - integer(PInt64(@Left)^ < PInt64(@Right)^))
            end
            else if (TypeInfo(T) = TypeInfo(UInt64))
                 {$IFDEF MSWindows}
                 or (TypeInfo(T) = TypeInfo(Handle_ptr))
                 or (TypeInfo(T) = TypeInfo(HWND))
                 or (TypeInfo(T) = TypeInfo(HHook))
                 or (TypeInfo(T) = TypeInfo(HGDIOBJ))
                 or (TypeInfo(T) = TypeInfo(HACCEL))
                 or (TypeInfo(T) = TypeInfo(HBITMAP))
                 or (TypeInfo(T) = TypeInfo(HBRUSH))
                 or (TypeInfo(T) = TypeInfo(HCOLORSPACE))
                 or (TypeInfo(T) = TypeInfo(HDC))
                 or (TypeInfo(T) = TypeInfo(HGLRC))
                 or (TypeInfo(T) = TypeInfo(HDESK))
                 or (TypeInfo(T) = TypeInfo(HENHMETAFILE))
                 or (TypeInfo(T) = TypeInfo(HFONT))
                 or (TypeInfo(T) = TypeInfo(HICON))
                 or (TypeInfo(T) = TypeInfo(HMENU))
                 or (TypeInfo(T) = TypeInfo(HMETAFILE))
                 or (TypeInfo(T) = TypeInfo(HPALETTE))
                 or (TypeInfo(T) = TypeInfo(HPEN))
                 or (TypeInfo(T) = TypeInfo(HRGN))
                 or (TypeInfo(T) = TypeInfo(HSTR))
                 or (TypeInfo(T) = TypeInfo(HTASK))
                 or (TypeInfo(T) = TypeInfo(HWINSTA))
                 or (TypeInfo(T) = TypeInfo(HKL))
                 or (TypeInfo(T) = TypeInfo(HKEY))
                 or (TypeInfo(T) = TypeInfo(HGESTUREINFO))
                 {$ENDIF}
            then Result:= (integer(PUInt64(@Left)^ > PUInt64(@Right)^) - integer(PUInt64(@Left)^ < PUInt64(@Right)^))
            else if Signed then Result:= (integer(PInt64(@Left)^ > PInt64(@Right)^) - integer(PInt64(@Left)^ < PInt64(@Right)^))
            else Result:= (integer(PUInt64(@Left)^ > PUInt64(@Right)^) - integer(PUInt64(@Left)^ < PUInt64(@Right)^))
          end;
          tkFloat: Result:= (integer(PDouble(@Left)^ > PDouble(@Right)^) - integer(PDouble(@Left)^ < PDouble(@Right)^));
          else begin
{$IFDEF purepascal}
            Result:= (BinaryCompare8(@Left, @Right));
{$ELSE !purepascal}{$IFDEF CPUX64}
            Result:= (BinaryCompare8(PUInt64(@Left)^, PUInt64(@Right)^));
{$ELSE !CPUX64}
            Result:= (BinaryCompare8(@Left, @Right));
{$ENDIF}{$ENDIF}
          end;
        end;
      end;
      10: begin
        if GetTypeKind(T) = tkFloat then Result:= (integer(PExtended(@Left)^ > PExtended(@Right)^) - integer(PExtended(@Left)^ < PExtended(@Right)^))
        else Result:= FastBinaryCompare(@Left, @Right, SizeOf(T));
      end;
      else case GetTypeKind(T) of
        tkString: Result:= (integer(PStrn(@Left)^ > PStrn(@Right)^) - integer(PStrn(@Left)^ < PStrn(@Right)^));
        else Result:= FastBinaryCompare(@Left, @Right, SizeOf(T));
      end; {case}
    end;
  end;
end;

class function IComparer<T>.TestCompareFast(const Left, Right: T): integer;
begin
  case GetTypeKind(T) of
    tkInteger: case SizeOf(T) of
      1: begin
        if Signed then Result:= CompareFast(PInt8(@left)^, PInt8(@Right)^)
        else Result:= CompareFast(PByte(@Left)^, PByte(@Right)^);
      end;
      2: begin
        if Signed then Result:= CompareFast(PInt16(@Left)^, PInt16(@Right)^)
        else Result:= CompareFast(PWord(@Left)^, PWord(@Right)^);
      end;
      4: begin
        if Signed then Result:= CompareFast(PInteger(@Left)^, PInteger(@Right)^)
        else Result:= CompareFast(PCardinal(@Left)^, PCardinal(@Right)^);
      end;
    end;
    tkChar: Result:= CompareFast(PAnsiChar(@Left)^, PAnsiChar(@Right)^);
    tkFloat: case SizeOf(T) of
      4: Result:= CompareFast(PSingle(@Left)^, PSingle(@Right)^);
      6: Result:= CompareFast(PReal48(@Left)^, PReal48(@Right)^);
      8: Result:= CompareFast(PDouble(@Left)^, PDouble(@Right)^);
      10: Result:= CompareFast(PExtended(@Left)^, PExtended(@Right)^);
    end;
    tkString: Result:= CompareFast(PStrn(@Left)^, PStrn(@Right)^);
    tkClass: Result:= CompareFast(TObject(PPointer(@Left)^), TObject(PPointer(@Right)^));
    tkWChar: Result:= CompareFast(PChar(@Left)^, PChar(@Right)^);
    tkLString: Result:= CompareFast(AnsiString(PPointer(@Left)^), AnsiString(PPointer(@Right)^));
    tkWString: Result:= CompareFast(WideString(PPointer(@Left)^), WideString(PPointer(@Right)^));
    tkInterface: Result:= CompareFast(IInterface(PPointer(@Left)^), IInterface(PPointer(@Right)^));
    tkInt64: begin
      if Signed then Result:= CompareFast(PInt64(@Left)^, PInt64(@Right)^)
      else Result:= CompareFast(PUInt64(@Left)^, PUInt64(@Right)^);
    end;
    tkUString: Result:= CompareFast(UnicodeString(PPointer(@Left)^), UnicodeString(PPointer(@Right)^));
    tkPointer: Result:= CompareFast(PPointer(@Left)^, PPointer(@Right)^);
    else Result:= Compare(Left, Right);
  end;
end;

{TODO -oJohan -cRewrite : Write special case to string comparison etc.}
class function IComparer<T>.Equals(const Left, Right: T): boolean;
begin
  if SizeOf(T) = 0 then Result:= true
  else case GetTypeKind(T) of
    tkString: case SizeOf(T) of
      1: Result:= PByte(@Left)^ = PByte(@Right)^;
      2: Result:= PStr1(@Left)^ = PStr1(@Right)^;
      3: Result:= PStr2(@Left)^ = PStr2(@Right)^;
      4: Result:= PStr3(@Left)^ = PStr3(@Right)^;
      else Result:= PStrn(@Left)^ = PStrn(@Right)^;
    end;
    tkFloat: case SizeOf(T) of
      4: Result:= (PSingle(@Left)^ = PSingle(@Right)^);
      6: Result:= (PReal48(@Left)^ = PReal48(@Right)^);
      8: Result:= (PDouble(@Left)^ = PDouble(@Right)^);
      10: Result:= (PExtended(@Left)^ = PExtended(@Right)^);
    end;
    tkUString: Result:= FastDefaults.SameUnicodeStr(UnicodeString(PPointer(@Left)^), UnicodeString(PPointer(@Right)^));
    tkLString: Result:= System.AnsiStrings.SameStr(AnsiString(PPointer(@Left)^), AnsiString(PPointer(@Right)^));
    tkWString: Result:= FastDefaults.SameWideStr(WideString(PPointer(@Left)^), WideString(PPointer(@Right)^));
    tkDynArray: Result:= (Compare_DynArray(PPointer(@Left)^, PPointer(@Right)^, ElementSize)) = 0;
    tkVariant: Result:= (Compare_Variant(@left, @right)) = 0;
    tkClassRef, tkPointer, tkInterface, tkProcedure: begin
      Result:= boolean(PNativeUInt(@Left)^ = PNativeUInt(@Right)^);
    end;
    tkMethod: begin
      Result:= (PMethod(@Left)^.Data = PMethod(@Right)^.Data) and (PMethod(@Left)^.Code = PMethod(@Right)^.Code);
    end;
    tkClass: Result:= ((PPointer(@Left)^ = nil) and (PPointer(@Right)^ = nil)) or (((PPointer(@Left)^ <> nil) and (TObject(PPointer(@Left)^).equals(TObject(PPointer(@Right)^)))));
    else case SizeOf(T) of
      0: Result:= true;
      1: Result:= (PInt8(@Left)^ = PInt8(@Right)^);
      2: Result:= (PInt16(@Left)^ = PInt16(@Right)^);
      3: Result:= (PStr2(@Left)^ = PStr2(@Right)^);
      4: Result:= (PInteger(@Left)^ = PInteger(@Right)^);
      8: Result:= (PInt64(@Left)^ = PInt64(@Right)^);
      else Result:= BinaryEquals(@left, @right, SizeOf(T));
    end;
  end;
end;

class function IComparer<T>.GetHashCode(const Value: T; Seed: integer = 0): integer;
var
  VarStr: string;
begin
  case GetTypeKind(T) of
    tkUnknown, //Custom enums
    tkInteger, tkChar, tkEnumeration, tkSet, tkWChar, tkRecord, tkInt64, tkFloat, tkClass, tkMethod, tkInterface,
      tkClassRef, tkPointer, tkProcedure:
    begin
      Result:= MurmurHash3(NativeInt((@Value)^), SizeOf(T), Seed);
    end;
    tkString: Result:= MurmurHash3(ShortString((@Value)^)[low(string)], Length(ShortString((@Value)^)), Seed);
    tkLString: Result:= MurmurHash3(AnsiString((@Value)^)[low(string)], Length(AnsiString((@Value)^)), Seed);
    tkUString:
    Result:= MurmurHash3(UnicodeString((@Value)^)[low(string)], Length(UnicodeString((@Value)^)) * SizeOf(WideChar), Seed);
    tkWString: Result:= MurmurHash3(WideString((@Value)^)[1], Length(WideString((@Value)^)) * SizeOf(WideChar), Seed);
    tkVariant: try
      VarStr:= PVariant((@Value)^)^;
      Result:= TComparer<string>.Default.GetHashCode(VarStr, Seed);
    except
      Result:= MurmurHash3(pointer((@Value)^)^, SizeOf(Variant), Seed);
    end;
    tkArray: Result:= MurmurHash3(byte((@Value)^), SizeOf(T), Seed);
    tkDynArray: Result:= MurmurHash3(pointer((@Value)^)^, GetTypeData(TypeInfo(T))^.elSize * DynLen(pointer((@Value)^)), Seed);
  end;
end;

function CompareWideStr(const S1, S2: WideString): integer; overload;
//{$ifdef PurePascal}
var
  i: NativeInt;
  c1, c2: Char;
begin
  if pointer(S1) = pointer(S2) then Exit(0);
  if pointer(S1) = nil then Exit( -1);
  if pointer(S2) = nil then Exit(1);
  i:= Low(S1);
  while (true) do begin
    c1:= S1[i];
    c2:= S2[i];
    Result:= integer(c1 > c2) - integer(c1 < c2);
    if (integer(Result = 0) and integer(c1 <> #0) and integer(c2 <> #0)) = 0 then Exit;
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

function SameWideStr(const S1, S2: WideString): boolean; overload;
{$IFDEF PurePascal}
const
  Forever = false;
var
  i: integer;
begin
  Result:= pointer(S1) = pointer(S2);
  if Result then Exit;
  Result:= not((pointer(S1) = nil) or (pointer(S2) = nil));
  if not(Result) then Exit;
  for i:= Low(S1) to High(Integer) do begin
    Result:= S1[i] = S2[i];
    if not(Result) or (S1[i] = #0) then Exit;
  end;
end;
{$ELSE}
{$IFDEF CPUX64}
asm
  .NOFRAME
  cmp   rcx, rdx
  jz @done       //pointers are the same
  test  rcx, rdx
  jz @PossibleNilPointer
@StartLoop:
  xor  	r10, r10
@loop:
  movzx	rax, word ptr [rcx + r10]	// fetch bytes
	movzx	r9, word ptr [rdx + r10]

	test	eax, r9d		// if there's a null char
	jz	@dif
	sub	  eax, r9d		// compare bytes,
	jne	@done		// quit if not equal

	add   r10,2		// else go for next round
	jmp	@loop
@dif:
  sub   eax,r9d
@done:
  setz  al
  movzx eax, al
  ret
@PossibleNilPointer:
  or    rcx, rcx
  jz @NotEqual
  or    rdx, rdx
  jnz @StartLoop
@NotEqual:
  xor   eax,eax
  ret
end;
{$ELSE !CPUX64}
asm
  push  esi
  push  edi
  cmp  eax,edx
  jz @done
  test eax, edx
  jnz @NoNilPointer
  or   eax,eax
  jz @dif
  or   edx,edx
  jz @dif
@NoNilPointer:
	xor  	ecx, ecx
@loop:
  movzx	esi, word ptr [eax + ecx]	// fetch bytes
	movzx	edi, word ptr [edx + ecx]

  cmp	  edi, esi		// compare bytes,
	jne	@done	      	// quit if not equal
	test	esi, edi		// if there's a null char
	jz	@done

	add   ecx,2		// else go for next round
	jmp	@loop
@dif:
  sub   edi,esi
@done:
  setz  al
  pop   esi
  pop   edi
  ret
end;
{$ENDIF}{$ENDIF}

function SameUnicodeStr(const S1, S2: string): boolean;
{$IFDEF PurePascal} inline;
begin
  Result:= System.SysUtils.SameStr(S1,S2);
end;
{$ELSE !PurePascal}
{$IFDEF CPUX64}
// RCX = S1
// RDX = S2
//Because the string is zero-terminated it is always safe to read 2 bytes beyond the last char.
asm
  .NOFRAME
  cmp  RCX, RDX
  je @done
  sub  RCX,4
  js @done  //S1 is nil, S2 is not
  sub  RDX,4
  js @done  //S2 is nil, S2 is not
@NoNilStrings:
  //Compare the length and 2st two chars
  mov  R8,[RCX]
  mov  R9,[RDX]
  cmp  R9,R8
  jnz @done  //The lengths are different
  mov  R8d,R8d   //zero-out non length parts.
  add  R8,R8     //WideChar = 2 bytes.
  neg  R8
  //use negative based indexing
  sub  RCX, R8
  sub  RDX, R8
  add  R8,4 //First 2 chars are already done
  jz @Equal
  mov  EAX,[RCX+R8+4]   //Realign the qword reads
  xor  EAX,[RDX+R8+4]
  jnz @done
@compareLoop:
  add  R8,8
  jns @DoTail8
  mov  RAX,[RCX+R8]
  xor  RAX,[RDX+R8]
  jz @compareLoop
  xor  RAX,RAX  //Not equal
  ret
@DoTail8:
  //mov  R8,-4
  mov  RAX,[RCX+4-8]
  xor  RAX,[RDX+4-8]
  setz AL
  ret
@done:
  setz AL
  ret
@equal:
  mov  AL,1
end;
{$ELSE !CPUX64}
// EAX = S1
// EDX = S2
asm
  push EBX
  cmp  EAX, EDX
  je @equal
  sub  EAX,4
  js @different  //S1 is nil, S2 is not
  sub  EDX,4
  js @different  //S2 is nil, S2 is not
@NoNilStrings:
  //Compare the length and 2st two chars
  mov  ECX,[EAX]
  cmp  ECX,[EDX]
  jnz @different  //The lengths are different
  mov  EBX,[EAX+4]
  xor  EBX,[EDX+4]
  jnz @different
  add  ECX, ECX
  neg  ECX
  //use negative based indexing
  sub  EAX, ECX
  sub  EDX, ECX
@compareLoop:
  add  ECX,4 //First 2 chars are already done
  jg @Equal //Strings are equal.
  mov  EBX,[EAX+ECX+4]
  xor  EBX,[EDX+ECX+4]
  jz @compareLoop
@different:
  xor  EAX,EAX
  pop  EBX
  ret
@equal:
  mov  AL,1
  pop  EBX
end;
{$ENDIF CPUX64}{$ENDIF PurePascal}

function CompareUnicodeStr(const S1, S2: string): integer;
{$IFDEF PurePascal} inline;
begin
  Result:= System.SysUtils.CompareStr(S1, S2);
end;
{$ELSE !PurePascal}
{$IFDEF CPUX64}
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
.NOFRAME
  CMP RCX, RDX
  JE @SameString
  {Is either of the strings perhaps nil?}
  TEST RCX, RDX
  JNZ @BothNonNil
  OR   RCX,RCX
  JNZ @TestStr2
  MOV  EAX,-1
  RET
@TestStr2:
  OR   RDX,RDX
  JNZ @BothNonNil
  MOV  EAX,1
  RET
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
  {Save the length difference}
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
  //BSF EBX, EBX      //where is differing bit?
  //SHR EBX, 4        //in first or second word?
  //ADD EBX, EBX
  {proposal:}           //BSF is an expensive instruction
  AND EBX, $FFFF
  SETZ BL
  ADD EBX,EBX
  {end proposal}
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
{$ELSE !CPUX64} inline;
begin
  Result:= System.SysUtils.CompareStr(S1,S2);
end;
{$ENDIF CPUX64}{$ENDIF PurePascal}

function BobJenkinsHash(const HashData; Len, Seed: integer): integer;
begin
  Result:= System.Generics.Defaults.BobJenkinsHash(HashData, Len, Seed);
end;

{$POINTERMATH on}
function PascalMurmurHash3(const [ref] HashData; Len, Seed: integer): integer;
const
  c1 = $CC9E2D51;
  c2 = $1B873593;
  r1 = 15;
  r2 = 13;
  m = 5;
  n = $E6546B64;
  f1 = $85EBCA6B;
  f2 = $C2B2AE35;
var
  i, Len2: integer;
  k: integer;
  remaining: integer;
  Data: PCardinal;
label case1, case2, case3, final;
type
  ByteArray = array [0 .. 0] of byte;
begin
  Result:= Seed;
  Data:= @HashData;
  for i:= 0 to (Len shr 2) - 1 do begin
    k:= Data[i];
    k:= k * integer(c1);
    k:= (k shl r1) or (k shr (32 - r1));
    k:= k * c2;
    Result:= Result xor k;
    Result:= (Result shl r2) or (Result shr (32 - r2));
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
  dec(Len2);
  inc(remaining, PByte(Data)[Len2] shl 16);
case2:
  dec(Len2);
  inc(remaining, PByte(Data)[Len2] shl 8);
case1:
  dec(Len2);
  inc(remaining, PByte(Data)[Len2]);
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

function MurmurHash3(const [ref] HashData; Len: integer; Seed: integer = 0): integer;
{$ifdef purepascal}
inline;
begin
  Result:= PascalMurmurHash3(HashData, Len, Seed);
end;
{$else}
const
  c1 = $CC9E2D51;
  c2 = $1B873593;
  r1 = 15;
  r2 = 13;
  m = 5;
  n = $E6546B64;
  f1 = $85EBCA6B;
  f2 = $C2B2AE35;
{$REGION 'asm'}
{$IFDEF CPUx86}
asm
    push EBX
    push EDI
    push ESI
    xchg ECX,EDX
    //EAX = data
    //ECX = count in bytes
    //EDX = seed
    add EAX, ECX
    neg ECX
    add ECX,4
    jg @remaining_bytes
@loop:
    mov  EDI,[EAX+ECX-4]
    imul EDI,EDI,c1
    rol  EDI,r1
    imul EDI,EDI,c2
    xor  EDX,EDI
    rol  EDX,r2
    lea  EDX,[EDX*4+EDX+n]
    add  ECX,4
    jle @loop
@remaining_bytes:
    cmp  ECX,4
    jz @finalization
    movzx EBX,byte ptr [EAX+ECX-4]
    cmp  ECX,3//inc  RCX
    jz @process_remaining
    ror  EBX,8//ror  R9d,24
    mov  BL,byte ptr [EAX+ECX-3]
    cmp  ECX,2//inc  RCX
    jz @shift_back
    ror  EBX,8//ror  R9d,24
    mov  bl,byte ptr [EAX+ECX-2]
    rol  EBX,8
@shift_back:
    rol  EBX,8
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
{$ENDIF}
{$IFDEF CPUx64}
asm
    .NOFRAME
    xchg R10,RDI
    xchg R11,RSI
    mov  RAX,RCX
    mov  RCX,RDX
    mov  RDX,R8
    //RAX = data
    //RCX = count in bytes
    //RDX = seed
    mov  ESI,ECX
    //make index negative based.
    //and  ECX,not(3)
    neg  RCX
    sub  RAX, RCX
    add  RCX, 4
    jg @remaining_bytes
@loop:
    mov  EDI,[RAX+RCX-4]
    imul EDI,EDI,c1
    rol  EDI,r1
    imul EDI,EDI,c2
    xor  EDX,EDI
    rol  EDX,r2
    lea  EDX,[EDX*4+EDX+n] // *5 + n
    //lea  RAX,qword ptr [RAX+4]
    add  RCX,4
    jle @loop
@remaining_bytes:
    cmp  RCX,4
    //mov  ECX,ESI
    //and  ESI,$3
    jz @finalization
    movzx r9,byte ptr [RAX+RCX-4]
    cmp  RCX,3//inc  RCX
    jz @process_remaining
    ror  R9,8//ror  R9d,24
    mov  R9b,byte ptr [RAX+RCX-3]
    cmp  RCX,2//inc  RCX
    jz @shift_back
    ror  R9,8//ror  R9d,24
    mov  R9b,byte ptr [RAX+RCX-2]
    rol  R9,8
@shift_back:
    rol  R9,8
@process_remaining:
    imul R9d,R9d,c1
    rol  R9d,r1
    imul R9d,R9d,c2
    xor  EDX,R9d
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
    xchg R10,RDI
    xchg R11,RSI
end;
{$ENDIF}
{$ENDREGION}
{$ENDIF}

class constructor IComparer<T>.Init;
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
  Result:= (integer(NativeUInt(Left) > NativeUInt(Right)) - integer(NativeUInt(Left) < NativeUInt(Right)));
end;

function CompareFast(const Left, Right: UTF8String): integer;
begin
  Result:= CompareFast(AnsiString(Left), AnsiString(Right));
end;

function CompareFast(const Left, Right: RawByteString): integer;
begin
  Result:= CompareFast(AnsiString(Left), AnsiString(Right));
end;

function CompareFast(const Left, Right: WideString): integer;
begin
  Result:= FastDefaults.CompareWideStr(Left, Right);
end;

function CompareFast(const Left, Right: UnicodeString): integer;
begin
  Result:= FastDefaults.CompareUnicodeStr(Left, Right);
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
  Result:= integer(byte(Left) <> 0) - integer(byte(Right) <> 0);
end;

function CompareFast(const Left, Right: longbool): integer;
begin
  Result:= integer(integer(Left) <> 0) - integer(integer(Right) <> 0);
end;

function CompareFast(const Left, Right: wordbool): integer;
begin
  Result:= integer(integer(Left) <> 0) - integer(integer(Right) <> 0);
end;

function CompareFast(const Left, Right: boolean): integer;
begin
  Result:= integer(byte(Left) <> 0) - integer(byte(Right) <> 0);
end;

function CompareFast(const Left, Right: Currency): integer;
begin
  Result:= integer(Left > Right) - integer(Left < Right);
end;

function CompareFast(const Left, Right: Comp): integer;
begin
  Result:= integer(Left > Right) - integer(Left < Right);
end;

function CompareFast(const Left, Right: extended): integer;
begin
  Result:= integer(Left > Right) - integer(Left < Right);
end;

function CompareFast(const Left, Right: double): integer;
begin
  Result:= integer(Left > Right) - integer(Left < Right);
end;

function CompareFast(const Left, Right: Real48): integer;
begin
  Result:= integer(Left > Right) - integer(Left < Right);
end;

function CompareFast(const Left, Right: single): integer;
begin
  Result:= integer(Left > Right) - integer(Left < Right);
end;

function CompareFast(const Left, Right: UCS4Char): integer;
begin
  Result:= CompareFast(cardinal(Left), cardinal(Right));
end;

function CompareFast(const Left, Right: WideChar): integer;
begin
  Result:= CompareFast(word(Left), word(Right));
end;

function CompareFast(const Left, Right: AnsiChar): integer;
begin
  Result:= CompareFast(byte(Left), byte(Right));
end;

function CompareFast(const Left, Right: NativeUInt): integer;
begin
  Result:= integer(Left > Right) - integer(Left < Right);
end;

function CompareFast(const Left, Right: NativeInt): integer;
begin
  Result:= integer(Left > Right) - integer(Left < Right);
end;

function CompareFast(const Left, Right: Int64): integer;
begin
  Result:= integer(Left > Right) - integer(Left < Right);
end;

function CompareFast(const Left, Right: UInt64): integer;
begin
  Result:= integer(Left > Right) - integer(Left < Right);
end;

function CompareFast(const Left, Right: integer): integer;
begin
  Result:= Left - Right;
end;

function CompareFast(const Left, Right: cardinal): integer;
begin
  Result:= integer(Left > Right) - integer(Left < Right);
end;

function CompareFast(const Left, Right: int16): integer;
begin
  Result:= integer(Left) - integer(Right);
end;

function CompareFast(const Left, Right: word): integer;
begin
  Result:= integer(Left) - integer(Right);
end;

function CompareFast(const Left, Right: int8): integer;
begin
  Result:= integer(Left) - integer(Right);
end;

function CompareFast(const Left, Right: byte): integer;
begin
  Result:= integer(Left) - integer(Right);
end;

function CompareFast(const Left, Right: TObject): integer;
begin
  Result:= CompareFast(NativeUInt(Left), NativeUInt(Right));
end;

function CompareFast(const Left, Right: IInterface): integer;
begin
  Result:= CompareFast(NativeUInt(Left), NativeUInt(Right));
end;

{ TComparer<T> }

class function TComparer<T>.Construct(const Comparison: TComparison<T>): TComparer<T>;
begin
  Result:= TDelegatedComparer<T>.Create(Comparison);
end;

{ TDelegatedComparer<T> }

constructor TDelegatedComparer<T>.Create(const ACompare: TComparison<T>);
begin
  inherited Create;
  FCompare:= ACompare;
end;

function TDelegatedComparer<T>.Compare(const Left, Right: T): Integer;
begin
  Result:= FCompare(Left, Right);
end;

end.
