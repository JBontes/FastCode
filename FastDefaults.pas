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
  System.SysUtils
  , FastCompare
  ;

//{$define PurePascal}

type
  shortstring = array[0..255] of byte;

type
  TComparison<T> = function(const Left, Right: T): integer;

//  TRec<T> = record
//  private
//    class var FCompare: TComparison<T>;
//    FData: T;
//    class constructor Init;
//  public
//    class operator Explicit(const a: T): TRec<T>; static; inline;
//    class operator Implicit(const a: T): TRec<T>; static; inline;
//    class operator Implicit(const a: TRec<T>): T; static; inline;
//    class operator GreaterThan(const L,R: TRec<T>): boolean; static; inline;
//    class operator Equal(const L,R: TRec<T>): boolean; static; inline;
//    class operator LessThan(const L,R: TRec<T>): boolean; static; inline;
//    class operator NotEqual(const L, R: TRec<T>): boolean; static; inline;
//  end;

  TEqualityComparison<T> = function(const Left, Right: T): boolean;
  THasher<T> = function(const Value: T): integer;

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
  private

    class function CompareNotInline(const Left, Right: T): integer; static; inline;
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
      class function Compare(const Left, Right: T): integer; overload; static; inline;
      class function Compare(const Left, Right: OpenString): integer; overload; static; //inline;

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
      class function Equals(const Left, Right: T): boolean; reintroduce; static; inline;
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
      ///   results than the lookup3 hash used by embacadero, but that the results are **not**
      ///   The same.
      /// </remarks>
      /// </summary>
      class function GetHashCode(const Value: T; Seed: integer = 0): integer; reintroduce; static; inline;
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




type
  PInt8 = ^Int8;
  PInt16 = ^Int16;
  TPS1 = string[1];
  PStr1 = ^TPS1;
  TPS2 = string[2];
  PStr2 = ^TPS2;
  TPS3 = string[3];
  PStr3 = ^TPS3;
  TPSn = System.ShortString;
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


{$UNDEF FusedParameters}
{$IFDEF CPUX86}
{$IF CompilerVersion >= 28}
{$IF CompilerVersion <= 29}
{$DEFINE FusedParameters}
{$ENDIF}{$ENDIF}{$ENDIF}

class function IComparer<T>.Compare(const [ref] Left, Right: OpenString): integer;
begin
  case SizeOf(T) of
    1: Result:= Byte(Left[0]) - Byte(Right[0]);
    2: Result:= (integer(TPS1(Left) > TPS1(Right)) - integer(TPS1(Left) < TPS1(Right)));
    3: Result:= (integer(TPS2(Left) > TPS2(Right)) - integer(TPS2(Left) < TPS2(Right)));
    4: Result:= (integer(TPS3(Left) > TPS3(Right)) - integer(TPS3(Left) < TPS3(Right)));
    else Result:= (Compare_PSn(Left, Right));
  end;
end;


class function IComparer<T>.Compare(const Left, Right: T): integer;
//var
//  l: TCast absolute Left;
//  r: TCast absolute Right;
//  //X: TTypeKind;
var
  Diff: Int64;
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

//  if TypeInfo(T) = TypeInfo(FastDefaults.ShortString) then begin
//    Result:= Compare_PSn(PShortstring(@Left)^, PShortstring(@Right)^);
//  end
  {else} case GetTypeKind(T) of
    tkRecord: begin
      if IsManagedType(T) then begin
        Result:= CompareNotInline(Left, Right);
        //Result:= FastBinaryCompare(Left, Right, SizeOf(T));
      end
      else case SizeOf(T) of
        0: Result:= 0;
        1: Result:= (PByte(@Left)^) - (PByte(@Right)^);
        2: Result:= (PWord(@Left)^) - (PWord(@Right)^);
        4: Result:= (integer(PCardinal(@Left)^ > PCardinal(@Right)^) - integer(PCardinal(@Left)^ < PCardinal(@Right)^));
        else Result:= ({Fast}BinaryCompare(@Left, @Right, SizeOf(T)));
      end; {else case}
    end; {tkRecord}

    tkWString: Result:= (CompareWideStr(WideString((@Left)^), WideString((@Right)^)));
    tkUString: Result:= (CompareUnicodeStr(PUnicodeString(@Left)^, PUnicodeString(@Right)^));
    tkLString: Result:= (CompareAnsiStr(PAnsiString(@Left)^, PAnsiString(@Right)^));
    tkDynArray: Result:= (Compare_DynArray(ppointer(@Left)^, ppointer(@Right)^, ElementSize));
    tkVariant: Result:= (Compare_Variant(@left, @right));
    tkClass, tkClassRef, tkPointer, tkInterface, tkProcedure: Result:= (integer(PNativeUint(@Left)^ > PNativeUint(@Right)^) - integer(PNativeUInt(@Left)^ < PNativeUInt(@Right)^));
    tkMethod: Result:= integer((((NativeUInt(PMethod(@left)^.Data) > NativeUInt(PMethod(@Right)^.Data))) or
      (((NativeUInt(PMethod(@Left)^.Data) = NativeUInt(PMethod(@Right)^.Data)) and (NativeUInt(PMethod(@Left)^.Code) > NativeUInt(PMethod(@Right)^.Code)))))) -
      integer(((NativeInt(PMethod(@Left)^.Data) < NativeInt(PMethod(@Right)^.Data)) or ((NativeInt(PMethod(@Left)^.Data) = NativeInt(PMethod(@Right)^.Data)) and
      (NativeInt(PMethod(@Left)^.Code) < NativeInt(PMethod(@Right)^.Code)))));
    tkString: case SizeOf(T) of
        1: Result:= (PByte(@Left)^) - (PByte(@Right)^);
        2: Result:= (integer(PStr1(@Left)^ > PStr1(@Right)^) -
            integer(PStr1(@Left)^ < PStr1(@Right)^));
        3: Result:= (integer(PStr2(@Left)^ > PStr2(@Right)^) -
            integer(PStr2(@Left)^ < PStr2(@Right)^));
        4: Result:= (integer(PStr3(@Left)^ > PStr3(@Right)^) -
            integer(PStr3(@Left)^ < PStr3(@Right)^));
        else begin
          Result:= (Compare_PSn(PShortString(@Left)^, PShortString(@Right)^))
          //Result:= (Compare_PSn(PPointer(@Left)^, PPointer(@Right)^))
        end;
      end;
    tkFloat: case SizeOf(T) of
      4: Result:= (integer(PSingle(@Left)^ > PSingle(@Right)^) -
            integer(PSingle(@Left)^ < PSingle(@Right)^));
      6: Result:= (integer(PReal48(@Left)^ > PReal48(@Right)^) -
          integer(PReal48(@Left)^ < PReal48(@Right)^));
      8: Result:= (integer(PDouble(@Left)^ > PDouble(@Right)^) -
          integer(PDouble(@Left)^ < PDouble(@Right)^));
      10: Result:= (integer(PExtended(@Left)^ > PExtended(@Right)^) -
          integer(PExtended(@Left)^ < PExtended(@Right)^));
    end;
  else
  //Complex cases...
    case SizeOf(T) of
      0: Result:= (0);
      1: begin
        //Keep R in one case otherwise linker will eliminate parameter Right.
        //{$IFDEF FusedParameters}
        // if (GetTypeKind(T) = tkInteger) and Signed then Result:= (l.i8) - int8(l.i32 shr 8)
        if GetTypeKind(T) = tkChar then Result:= (PByte(@Left)^) - (PByte(@Right)^)
        else if TypeInfo(T) = TypeInfo(ShortInt) then Result:= (PInt8(@Left)^) - (PInt8(@Right)^)
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
          tkWChar, tkSet, tkEnumeration: Result:= integer((PWord(@Left)^) - (PWord(@Right)^));
          tkInteger: begin
            if TypeInfo(T) = TypeInfo(SmallInt) then Result:= (PInt16(@Left)^ - PInt16(@Right)^)
            else if TypeInfo(T) = TypeInfo(word) then Result:= integer((PWord(@Left)^) - (PWord(@Right)^))
            //Keep R in one case otherwise linker will eliminate parameter Right.
            //L is stored at [esp], R is stored at [esp+2]
            //Prevent unaligned read by reading the word in one go.
            //{$IFDEF FusedParameters}
            //else Result:= (l.u16 - (l.u32 shr 16));
            else if Signed then Result:= (PInt16(@Left)^ - PInt16(@Right)^)
            else Result:= integer((PWord(@Left)^) - (PWord(@Right)^));
          end;
          else Result:= integer(PWord(@Left)^) - integer(PWord(@Right)^)
        end;
      end;
      3: Result:= (FastBinaryCompare(Left, Right, SizeOf(T)));
      4: begin
        case GetTypeKind(T) of
          tkSet, tkEnumeration: begin
            Result:= (integer(PCardinal(@Left)^ > PCardinal(@Right)^) - integer(PCardinal(@Left)^ < PCardinal(@Right)^));
          end;
          tkInteger: begin
            if (TypeInfo(T) = TypeInfo(integer))
                     or (TypeInfo(T) = TypeInfo(CppLongInt))
                     or (TypeInfo(T) = TypeInfo(HResult))
            then {$IFNDEF CPUX64} begin
              Result:= integer(PInteger(@Left)^ > PInteger(@Right)^) -
                integer(PInteger(@Left)^ < PInteger(@Right)^)
            end
            {$ELSE CPUX64}
            begin
              Diff:= Int64(PInteger(@Left)^) - Int64(PInteger(@Right)^);
              Result:= integer(Diff > 0) - integer(Diff < 0);
            end
            {$ENDIF}
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
            else if Signed then Result:= integer(PInteger(@Left)^ > PInteger(@Right)^) - integer(PInteger(@Left)^ < PInteger(@Right)^)
            else Result:= (integer(PCardinal(@Left)^ > PCardinal(@Right)^) - integer(PCardinal(@Left)^ < PCardinal(@Right)^));
          end;
          tkPointer: Result:= (integer(PCardinal(@Left)^ > PCardinal(@Right)^) - integer(PCardinal(@Left)^ < PCardinal(@Right)^))
          else Result:= BinaryCompare4(PCardinal(@Left)^, PCardinal(@Right)^);
        end;
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
          else begin
{$IFDEF purepascal}
            Result:= (BinaryCompare8(@Left, @Right));
{$ELSE !purepascal}{$IFDEF CPUX64}
            Result:= (BinaryCompare8(PUInt64(@Left)^, PUInt64(@Right)^));
{$ELSE !CPUX64}
            Result:= (BinaryCompare8(@Left, @Right));
{$ENDIF}{$ENDIF}
          end; {case else}
        end; {8:}
      end; {case SizeOf(T)}
      else Result:= FastBinaryCompare(Left, Right, SizeOf(T));
    end;
  end;
end;

class function IComparer<T>.CompareNotInline(const Left, Right: T): integer;
begin
  Result:= FastBinaryCompare(Left, Right, SizeOf(T));
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
      //else Result:= PStrn(@Left)^ = PStrn(@Right)^;
      else Result:= System.PShortString(@Left)^ = System.PShortString(@Right)^;
    end;
    tkFloat: case SizeOf(T) of
      4: Result:= (PSingle(@Left)^ = PSingle(@Right)^);
      6: Result:= (PReal48(@Left)^ = PReal48(@Right)^);
      8: Result:= (PDouble(@Left)^ = PDouble(@Right)^);
      10: Result:= (PExtended(@Left)^ = PExtended(@Right)^);
    end;
    tkUString: Result:= FastCompare.SameUnicodeStr(UnicodeString(PPointer(@Left)^), UnicodeString(PPointer(@Right)^));
    tkLString: Result:= System.AnsiStrings.SameStr(AnsiString(PPointer(@Left)^), AnsiString(PPointer(@Right)^));
    tkWString: Result:= FastCompare.SameWideStr(WideString(PPointer(@Left)^), WideString(PPointer(@Right)^));
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
  if (SizeOf(T) = 0) then Result:= 0
  else case GetTypeKind(T) of
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
    tkFloat: fSigned:= true;
  end;
  fElementSize:= 0;
  if GetTypeKind(T) = tkDynArray then fElementSize:= PTypeInfo(TypeInfo(T)).TypeData.elSize;
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
