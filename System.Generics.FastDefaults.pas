(*******************************************************
 System.Generics.FastDefauts
 A fast drop-in replacement for the comparers available in
 `System.Generics.Defaults`.
 It should compile and run in 2009 and beyond.

 Alpha version 0.3, fully tested in Win32, Win64 and pure Pascal

 (c) Copyright 2015 J. Bontes


   This Source Code Form is subject to the terms of the
   Mozilla Public License, v. 2.0.
   If a copy of the MPL was not distributed with this file,
   You can obtain one at http://mozilla.org/MPL/2.0/.

*******************************************************)


unit System.Generics.FastDefaults;

{$R-,T+,X+,H+,B-}
(*$HPPEMIT '#pragma option -w-8022'*)

interface

uses
  System.SysUtils
  , System.TypInfo
  , System.Generics.Defaults
  ;

type

  IComparer<T> = interface(System.Generics.Defaults.IComparer<T>)
  end;

  TComparison<T> = reference to function(const Left, Right: T): Integer;

  // Abstract base class for IComparer<T> implementations, and a provider
  // of default IComparer<T> implementations.
  TComparer<T> = class(TInterfacedObject, IComparer<T>)
  public
    class function Default: IComparer<T>;
    class function Construct(const Comparison: TComparison<T>): IComparer<T>;
    function Compare(const Left, Right: T): Integer; virtual; abstract;
  end;

  IEqualityComparer<T> = interface(System.Generics.Defaults.IEqualityComparer<T>)
  end;

  // Abstract base class for IEqualityComparer<T> implementations, and a provider
  // of default IEqualityComparer<T> implementations.
  TEqualityComparer<T> = class(TInterfacedObject, IEqualityComparer<T>)
  strict private
    //class constructor Init;
  public
    class function Default: IEqualityComparer<T>; static;

    class function Construct(const EqualityComparison: TEqualityComparison<T>;
      const Hasher: THasher<T> = nil): IEqualityComparer<T>;

    function Equals(const Left, Right: T): Boolean; reintroduce; overload;
      virtual; abstract;
    function GetHashCode(const Value: T): Integer; reintroduce; overload;
      virtual; abstract;
  end;

  // A non-reference-counted IInterface implementation.
  TSingletonImplementation = class(TObject, IInterface)
  protected
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  end;

  TDelegatedEqualityComparer<T> = class(TEqualityComparer<T>)
  private
    FEquals: TEqualityComparison<T>;
    FHasher: THasher<T>;
  public
    constructor Create(const AEquals: TEqualityComparison<T>; const AHasher: THasher<T>);
    function Equals(const Left, Right: T): Boolean; overload; override;
    function GetHashCode(const Value: T): Integer; overload; override;
  end;

  TDelegatedComparer<T> = class(TComparer<T>)
  private
    FCompare: TComparison<T>;
  public
    constructor Create(const ACompare: TComparison<T>);
    function Compare(const Left, Right: T): Integer; override;
  end;

  TCustomComparer<T> = class abstract(TSingletonImplementation, IComparer<T>, IEqualityComparer<T>)
  protected
    function Compare(const Left, Right: T): Integer; virtual; abstract;
    function Equals(const Left, Right: T): Boolean; reintroduce; overload; virtual; abstract;
    function GetHashCode(const Value: T): Integer; reintroduce; overload; virtual; abstract;
  end;

  TStringComparer = class(TCustomComparer<string>)
  private
    class var FOrdinal: TCustomComparer<string>;
  public
    class destructor Destroy;
    class function Ordinal: TStringComparer;
  end;

  TIStringComparer = class(TCustomComparer<string>)
  private
    class var FOrdinal: TCustomComparer<string>;
  public
    class destructor Destroy;
    class function Ordinal: TStringComparer;
  end;

  TOrdinalIStringComparer = class(TStringComparer)
  public
    function Compare(const Left, Right: string): Integer; override;
    function Equals(const Left, Right: string): Boolean; reintroduce; overload; override;
    function GetHashCode(const Value: string): Integer; reintroduce; overload; override;
  end;

//function BinaryCompare(const Left, Right: Pointer; Size: Integer): Integer;

// Must be in interface section to be used by generic method. For internal use only.
type
  //Because the VMT pointer is next to the vmt itself cache performace is improved.
  PCompareRec = ^TCompareRec;
  TCompareRec = record
    VMT: pointer;
    VMTArray: array [0..3] of pointer;
  end;

  PEqualityRec = ^TEqualityRec;
  TEqualityRec = record
    VMT: pointer;
    VMTArray: array [0..4] of pointer;
  end;

  TPrivate = record
  private type
    TDefaultGenericInterface = (giComparer, giEqualityComparer);
  private
    class function DefaultHash<T>(const Value: T): Integer; static;
    class function InitCompareVMT(Kind: TTypeKind; info: PTypeInfo; Size: NativeUInt; Signed: boolean): PCompareRec; static;
    class function GetCompareInterface(Kind: TTypeKind; info: PTypeInfo; Size: NativeUInt; Signed: boolean): pointer; static;
    class function GetEqualityInterface(Kind: TTypeKind; info: PTypeInfo; Size: NativeUInt): pointer; static;
    class function InitEqualityVMT(Kind: TTypeKind; info: PTypeInfo; Size: NativeUInt): PEqualityRec; static;
  end;

implementation

uses
{$IFNDEF NEXTGEN}
  System.AnsiStrings,
{$ENDIF}
{$IFDEF MSWindows}
  WinAPI.Windows,
{$ENDIF}
  System.Math, System.Generics.Collections, System.Variants,
  FastCompare
  ;

const
  DefaultHashSeed = FastCompare.DefaultHashSeed;

type
  PSimpleInstance = ^TSimpleInstance;

  TSimpleInstance = record
    Vtable: PPointerArray;
    Size: NativeInt;
    RefCount: Integer;
  end;

  TInfoFlags = set of (ifVariableSize, ifSelector);
  PVtableInfo = ^TVtableInfo;

  TVtableInfo = record
    Flags: TInfoFlags;
    Data: Pointer;
  end;

  TTypeInfoSelector = function(info: PTypeInfo; Size: Integer): Pointer;


function NopQueryInterface(inst: Pointer; const IID: TGUID; out Obj): HResult; stdcall;
begin
  Result:= E_NOINTERFACE;
end;

function MemAddref(inst: PSimpleInstance): Integer; stdcall;
begin
  Result:= AtomicIncrement(inst^.RefCount);
end;

function MemRelease(inst: PSimpleInstance): Integer; stdcall;
begin
  Result:= AtomicDecrement(inst^.RefCount);
  if Result = 0 then FreeMem(inst);
end;

function MakeInstance(Vtable: PCompareRec; sizeField: Integer): Pointer;
var
  inst: PSimpleInstance;
begin
  GetMem(inst, SizeOf(inst^));
  inst.Vtable:= Vtable.VMT;
  inst.Vtable^[0]:= @NopQueryInterface;
  inst.Vtable^[1]:= @MemAddRef;
  inst.Vtable^[2]:= @MemRelease;
  inst.RefCount:= 0;
  inst.Size:= sizeField;
  Result:= inst;
end;

function Nop(inst: Pointer): Integer; stdcall;
begin
  Result:= -1;
end;

function FCompareI1(inst: Pointer; const Left, Right: Shortint): Integer;
begin
  Result:= Left - Right;
end;

function Equals_U1(inst: Pointer; const Left, Right: Shortint): Boolean;
begin
  Result:= Left = Right;
end;

function GetHashCode_U1(inst: Pointer; const Value: Shortint): Integer;
begin
  Result:= MurmurHash3(Value, SizeOf(Value), 0);
end;

function FCompareI2(inst: Pointer; const Left, Right: Smallint): Integer;
begin
  Result:= Left - Right;
end;

function Equals_U2(inst: Pointer; const Left, Right: Smallint): Boolean;
begin
  Result:= Left = Right;
end;

function GetHashCode_U2(inst: Pointer; const Value: Smallint): Integer;
begin
  Result:= MurmurHash3(Value, SizeOf(Value), 0);
end;

type
  T3 = array[0..2] of byte;

function FCompareI3(inst: Pointer; const Left, Right: T3): Integer;
var
  I: Integer;
begin
  Result:= 0;
  for I := 0 to 2 do begin
    Result:= Result + ((Left[i] - Right[i]) shl (8*i))
  end;
end;

function Equals_U3(inst: Pointer; const Left, Right: T3): boolean;
var
  I: Integer;
  Equal: Integer;
begin
  Equal:= 0;
  for I:= 0 to 2 do begin
    Equal:= Equal + (Left[I] - Right[I]);
  end;
  Result:= Equal = 0;
end;

function GetHashCode_U3(inst: Pointer; const Value: T3): Integer;
begin
  Result:= MurmurHash3(Value, SizeOf(T3), 0);
end;

function FCompareI4(inst: Pointer; const Left, Right: Integer): Integer;
begin
  Result:= integer(Left > Right) - integer(Left < Right);
end;

function Equals_U4(inst: Pointer; const Left, Right: Integer): Boolean;
begin
  Result:= Left = Right;
end;

function GetHashCode_U4(inst: Pointer; const Value: Integer): Integer;
begin
  Result:= MurmurHash3(Value, SizeOf(Value), 0);
end;

function FCompareU1(inst: Pointer; const Left, Right: Byte): Integer;
begin
  Result:= Integer(Left) - Integer(Right);
end;

function FCompareU2(inst: Pointer; const Left, Right: Word): Integer;
begin
  Result:= Integer(Left) - Integer(Right);
end;

function FCompareU4(inst: Pointer; const Left, Right: LongWord): Integer;
begin
  Result:= integer(Left > Right) - integer(Left < Right);
end;

function Equals_U8(inst: Pointer; const Left, Right: Int64): Boolean;
begin
  Result:= Left = Right;
end;

function GetHashCode_U8(inst: Pointer; const Value: Int64): Integer;
begin
  Result:= MurmurHash3(Value, SizeOf(Value), 0);
end;

function FCompareI8(inst: Pointer; const Left, Right: Int64): Integer;
begin
  Result:= integer(Left > Right) - integer(Left < Right);
end;

function FCompareU8(inst: Pointer; const Left, Right: UInt64): Integer;
begin
  Result:= integer(Left > Right) - integer(Left < Right);
end;

function FCompareR4(inst: Pointer; const Left, Right: Single): Integer;
begin
  Result:= byte(Left > Right) - byte(Left < Right);
end;

function Equals_R4(inst: Pointer; const Left, Right: Single): Boolean;
begin
  Result:= Left = Right;
end;

function GetHashCode_R4(inst: Pointer; const Value: Single): Integer;
var
  X: Single;
begin
  // Denormalized floats and positive/negative 0.0 complicate things.
  X:= Normalize(Value);
  Result:= MurmurHash3(X, SizeOf(X), FastCompare.DefaultHashSeed);
end;

function FCompareR6(inst: Pointer; const Left, Right: Real48): Integer;
begin
  Result:= integer(Left > Right) - integer(Left < Right);
end;

function Equals_R6(inst: Pointer; const Left, Right: Real48): Boolean;
begin
  Result:= Left = Right;
end;

function GetHashCode_R6(inst: Pointer; const Value: Real48): Integer;
begin
  //Real48 is always normalized
  Result:= MurmurHash3(Value, SizeOf(Value), FastCompare.DefaultHashSeed);
end;

function FCompareR8(inst: Pointer; const Left, Right: Double): Integer;
begin
  Result:= integer(Left > Right) - integer(Left < Right);
end;

function Equals_R8(inst: Pointer; const Left, Right: Double): Boolean;
begin
  Result:= Left = Right;
end;

function GetHashCode_R8(inst: Pointer; const Value: Double): Integer;
var
  X: Double;
begin
  // Denormalized floats and positive/negative 0.0 complicate things.
  X:= Normalize(Value);
  Result:= MurmurHash3(X, SizeOf(X), FastCompare.DefaultHashSeed);
end;


function FCompareR10(inst: Pointer; const Left, Right: Extended): Integer;
begin
  Result:= integer(Left > Right) - integer(Left < Right);
end;

function Equals_R10(inst: Pointer; const Left, Right: Extended): Boolean;
begin
  Result:= Left = Right;
end;

function GetHashCode_R10(inst: Pointer; const Value: Extended): Integer;
var
  X: Extended;
begin
  // Denormalized floats and positive/negative 0.0 complicate things.
  X:= Normalize(Value);
  Result:= MurmurHash3(X, SizeOf(X), FastCompare.DefaultHashSeed);
end;

function FCompareRI8(inst: Pointer; const Left, Right: Comp): Integer;
begin
  Result:= integer(Left > Right) - integer(Left < Right);
end;

function Equals_RI8(inst: Pointer; const Left, Right: Comp): Boolean;
begin
  Result:= Left = Right;
end;

function GetHashCode_RI8(inst: Pointer; const Value: Comp): Integer;
begin
  Result:= MurmurHash3(Value, SizeOf(Value), 0);
end;

function FCompareRC8(inst: Pointer; const Left, Right: Currency): Integer;
begin
  Result:= integer(Left > Right) - integer(Left < Right);
end;

function Equals_RC8(inst: Pointer; const Left, Right: Currency): Boolean;
begin
  Result:= Left = Right;
end;

function GetHashCode_RC8(inst: Pointer; const Value: Currency): Integer;
begin
  Result:= MurmurHash3(Value, SizeOf(Value), 0);
end;

function FCompareBinary(inst: PSimpleInstance; const Left, Right): Integer;
begin
  Result:= BinaryCompare(Left, Right, inst^.Size);
end;

function Equals_Binary(inst: PSimpleInstance; const Left, Right): Boolean;
begin
  Result:= FastCompare.BinaryEquals(@Left, @Right, inst.Size);
end;

function GetHashCode_Binary(inst: PSimpleInstance; const Value): Integer;
begin
  Result:= MurmurHash3(Value, inst^.Size, 0);
end;

function Equals_Class(inst: PSimpleInstance; Left, Right: TObject): boolean;
begin
  if Left = nil then Result:= Right = nil
  else Result:= Left.Equals(Right);
end;

function GetHashCode_Class(inst: PSimpleInstance; Value: TObject): Integer;
begin
  if Value = nil then Result:= 42
  else Result:= Value.GetHashCode;
end;

// DynArray

function DynLen(Arr: Pointer): NativeInt; inline;
begin
  if Arr = nil then Exit(0);
  Result:= PNativeInt(PByte(Arr) - SizeOf(NativeInt))^;
end;

function FCompareDynArray(inst: PSimpleInstance; Left, Right: Pointer): NativeInt;
var
  Len, lenDiff: NativeInt;
begin
  Len:= DynLen(Left);
  lenDiff:= Len - DynLen(Right);
  if lenDiff < 0 then Inc(Len, lenDiff);
  Result:= BinaryCompare(Left^, Right^, inst^.Size * Len);
  if Result = 0 then Result:= lenDiff;
end;

function Equals_DynArray(inst: PSimpleInstance; Left, Right: Pointer): boolean;
var
  lenL, lenR: NativeInt;
begin
  lenL:= DynLen(Left);
  lenR:= DynLen(Right);
  if lenL <> lenR then Exit(False);
  Result:= BinaryEquals(Left, Right, inst.Size * lenL);
end;

function GetHashCode_DynArray(inst: PSimpleInstance; Value: Pointer): Integer;
begin
  Result:= MurmurHash3(Value^, inst^.Size * DynLen(Value), 0);
end;


// PStrings
type
{$IFNDEF NEXTGEN}
  TPS1 = string[1];
  TPS2 = string[2];
  TPS3 = string[3];
{$ELSE NEXTGEN}
  OpenString = type string;
  TPS1 = string;
  TPS2 = string;
  TPS3 = string;
{$ENDIF !NEXTGEN}

function FComparePS1(inst: PSimpleInstance; const Left, Right: PByte): Integer;
var
  Len: integer;
begin
  Len:= Min(Left[0], Right[0]);
  if Len > 0 then Result:= Left[1] - Right[1]
  else Result:= Left[0] - Right[0];
end;

function FComparePS2(inst: PSimpleInstance; const Left, Right: PByte): Integer;
var
  Len: integer;
begin
  Len:= Min(Left[0], Right[0]);
  if Len > 0 then begin
    Result:= Left[1] - Right[1];
    if Result = 0 then Result:= Left[2] - Right[2];
  end
  else Result:= Left[0] - Right[0];
end;

function FComparePS3(inst: PSimpleInstance; const Left, Right: PInteger): Integer;
var
  LenDiff, Len: integer;
  i: integer;
  L,R: integer;
begin
  L:= Left^;
  R:= Right^;
  LenDiff:= byte(L) - Byte(R);
  Len:= Min(Byte(L), byte(R));
  for i:= 0 to Len-1 do begin
    L:= L shr 8;
    R:= R shr 8;
    Result:= byte(L) - byte(R);
    if Result <> 0 then exit;
  end;
  Result:= LenDiff;
end;

function FComparePSn(inst: PSimpleInstance; const Left, Right: OpenString): Integer;
begin
  Result:= FastCompare.Compare_PSn(Left, Right);
end;

function Equals_PS1(inst: PSimpleInstance; const Left, Right: PByte): Boolean;
begin
  //Result:= Left = Right;
  Result:= (Left[0] = Right[0])
       and ((Left[0] = 0) or (Left[1] = Right[1]));
end;

function Equals_PS2(inst: PSimpleInstance; const Left, Right: PByte): Boolean;
var
  Len: byte;
  i: integer;
begin
  Len:= Left[0];
  Result:= (Len = Right[0]);
  if Result then for i:= 1 to Len do begin
    Result:= Result and not(Left[i] <> Right[i]);
  end;
end;

function Equals_PS3(inst: PSimpleInstance; const Left, Right: PByte): Boolean;
var
  Len: byte;
  i: integer;
begin
  Len:= Left[0];
  Result:= (Len = Right[0]);
  if Result then for i:= 1 to Len do begin
    Result:= Result and not(Left[i] <> Right[i]);
  end;
end;

function Equals_PSn(inst: PSimpleInstance; const Left, Right: OpenString): Boolean;
begin
  Result:= (FastCompare.Compare_PSn(Left, Right) = 0);
end;

function GetHashCode_PS1(inst: PSimpleInstance; const Value: TPS1): Integer;
begin
  Result:= MurmurHash3(Value[Low(string)], Length(Value), 0);
end;

function GetHashCode_PS2(inst: PSimpleInstance; const Value: TPS2): Integer;
begin
  Result:= MurmurHash3(Value[Low(string)], Length(Value), 0);
end;

function GetHashCode_PS3(inst: PSimpleInstance; const Value: TPS3): Integer;
begin
  Result:= MurmurHash3(Value[Low(string)], Length(Value), 0);
end;

function GetHashCode_PSn(inst: PSimpleInstance;
  const Value: OpenString): Integer;
begin
  Result:= MurmurHash3(Value[Low(string)], Length(Value), 0);
end;

{$IFNDEF NEXTGEN}

function FCompareLString(inst: PSimpleInstance; const Left, Right: AnsiString): Integer;
begin
  Result:= FastCompare.CompareAnsiStr(Left, Right);
end;

function Equals_LString(inst: PSimpleInstance; const Left, Right: AnsiString): Boolean;
begin
  Result:= FastCompare.CompareAnsiStr(Left, Right) = 0;
end;

function GetHashCode_LString(inst: PSimpleInstance; const Value: AnsiString): Integer;
begin
  Result:= MurmurHash3(Value[1], Length(Value) * SizeOf(Value[1]), 0);
end;
{$ENDIF !NEXTGEN}
// UStrings

function FCompareUString(inst: PSimpleInstance; const Left, Right: UnicodeString): Integer;
begin
  Result:= FastCompare.CompareUnicodeStr(Left, Right);
end;

function Equals_UString(inst: PSimpleInstance; const Left, Right: UnicodeString): Boolean;
begin
  Result:= FastCompare.SameUnicodeStr(Left, Right)
end;

function GetHashCode_UString(inst: PSimpleInstance; const Value: UnicodeString): Integer;
begin
  Result:= MurmurHash3(Value[low(string)], Length(Value) * SizeOf(Char), 0);
end;

type
  TEmptyRec = record
end;

function FCompareEmptyRec(inst: PSimpleInstance; const Left, Right: TEmptyRec): Integer;
begin
  Result:= 0;
end;

function Equals_EmptyRec(inst: PSimpleInstance; const Left, Right: TEmptyRec): Boolean;
begin
  Result:= true;
end;

function GetHashCode_EmptyRec(inst: PSimpleInstance; const Value: TEmptyRec): Integer;
begin
  Result:= 0;
end;

type
  TMethodPointer = procedure of object;

function FCompareMethod(inst: PSimpleInstance; const Left, Right: TMethodPointer): Integer;
begin
  Result:=
    Integer((((NativeUInt(TMethod(Left).Data) > NativeUInt(TMethod(Right).Data)))
    or      (((NativeUInt(TMethod(Left).Data) = NativeUInt(TMethod(Right).Data))
    and       (NativeUInt(TMethod(Left).Code) > NativeUInt(TMethod(Right).Code))))))
    -
     Integer(((NativeInt(TMethod(Left).Data) < NativeInt(TMethod(Right).Data))
    or       ((NativeInt(TMethod(Left).Data) = NativeInt(TMethod(Right).Data))
    and       (NativeInt(TMethod(Left).Code) < NativeInt(TMethod(Right).Code)))));
end;

function Equals_Method(inst: PSimpleInstance; const Left, Right: TMethodPointer): Boolean;
begin
  Result:= (TMethod(Left).Data = TMethod(Right).Data)
       and (TMethod(Left).Code = TMethod(Right).Code);
end;

function GetHashCode_Method(inst: PSimpleInstance; const Value: TMethodPointer): Integer;
begin
  Result:= MurmurHash3(Value, SizeOf(TMethodPointer), 0);
end;

// WStrings

{$IFNDEF NEXTGEN}

function FCompareWString(inst: PSimpleInstance; const Left, Right: WideString): Integer;
begin
  Result:= FastCompare.CompareWideStr(Left, Right);
end;

function Equals_WString(inst: PSimpleInstance; const Left, Right: WideString): Boolean;
begin
  Result:= FastCompare.SameWideStr(Left, Right);
end;

function GetHashCode_WString(inst: PSimpleInstance; const Value: WideString): Integer;
begin
  Result:= MurmurHash3(Value[1], Length(Value) * SizeOf(Value[1]), 0);
end;
{$ENDIF !NEXTGEN}
// Variants

function FCompareVariant(inst: PSimpleInstance; Left, Right: Pointer): Integer;
begin
  Result:= FastCompare.Compare_Variant(Left, Right);
end;

function Equals_Variant(inst: PSimpleInstance; Left, Right: Pointer): Boolean;
var
  l, r: Variant;
begin
  l:= PVariant(Left)^;
  r:= PVariant(Right)^;
  Result:= VarCompareValue(l, r) = vrEqual;
end;

function GetHashCode_Variant(inst: PSimpleInstance; Value: Pointer): Integer;
var
  v: string;
begin
  try
    v:= PVariant(Value)^;
    Result:= GetHashCode_UString(nil, v);
  except
    Result:= MurmurHash3(Value^, SizeOf(Variant), 0);
  end;
end;

// Pointers

function FComparePointer(inst: PSimpleInstance;
  Left, Right: NativeUInt): Integer;
begin
  Result:= integer(Left > Right) - integer(Left < Right);
end;

function Equals_Pointer(inst: Pointer; const Left, Right: NativeUInt): Boolean;
begin
  Result:= Left = Right;
end;

function GetHashCode_Pointer(inst: Pointer; const Value: NativeUInt): Integer;
begin
  Result:= MurmurHash3(Value, SizeOf(Value), 0);
end;

{ TSingletonImplementation }

function TSingletonImplementation.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then Result:= S_OK
  else Result:= E_NOINTERFACE;
end;

function TSingletonImplementation._AddRef: Integer;
begin
  Result:= -1;
end;

function TSingletonImplementation._Release: Integer;
begin
  Result:= -1;
end;

{ Delegated Comparers }

constructor TDelegatedEqualityComparer<T>.Create(const AEquals: TEqualityComparison<T>; const AHasher: System.Generics.Defaults.THasher<T>);
begin
  FEquals:= AEquals;
  if not(Assigned(AHasher)) then FHasher:= TPrivate.DefaultHash<T>
  else FHasher:= AHasher;
end;

function TDelegatedEqualityComparer<T>.Equals(const Left, Right: T): Boolean;
begin
  Result:= FEquals(Left, Right);
end;

function TDelegatedEqualityComparer<T>.GetHashCode(const Value: T): Integer;
begin
  Result:= FHasher(Value);
end;

constructor TDelegatedComparer<T>.Create(const ACompare: TComparison<T>);
begin
  FCompare:= ACompare;
end;

function TDelegatedComparer<T>.Compare(const Left, Right: T): Integer;
begin
  Result:= FCompare(Left, Right);
end;

{ TOrdinalStringComparer }

type
  TOrdinalStringComparer = class(TStringComparer)
  public
    function Compare(const Left, Right: string): Integer; override;
    function Equals(const Left, Right: string): Boolean; reintroduce; overload; override;
    function GetHashCode(const Value: string): Integer; reintroduce; overload; override;
  end;

function TOrdinalStringComparer.Compare(const Left, Right: string): Integer;
var
  Len, lenDiff: Integer;
begin
  Result:= CompareUnicodeStr(Left, Right);
end;

function TOrdinalStringComparer.Equals(const Left, Right: string): Boolean;
begin
  Result:= SameUnicodeStr(left, right);
end;

function TOrdinalStringComparer.GetHashCode(const Value: string): Integer;
begin
  Result:= MurmurHash3(PChar(Value)^, SizeOf(Char) * Length(Value), 0);
end;

{ TStringComparer }

class destructor TStringComparer.Destroy;
begin
  FreeAndNil(FOrdinal);
end;

class function TStringComparer.Ordinal: TStringComparer;
begin
  if FOrdinal = nil then FOrdinal:= TOrdinalStringComparer.Create;
  Result:= TStringComparer(FOrdinal);
end;

{ TOrdinalIStringComparer }

function TOrdinalIStringComparer.Compare(const Left, Right: string): Integer;
var
  l, r: string;
  Len, lenDiff: Integer;
begin
  l:= AnsiLowerCase(Left);
  r:= AnsiLowerCase(Right);
  Result:= CompareUnicodeStr(l,r);
end;

function TOrdinalIStringComparer.Equals(const Left, Right: string): Boolean;
var
  Len: Integer;
  l, r: string;
begin
  l:= AnsiLowerCase(Left);
  r:= AnsiLowerCase(Right);
  Result:= SameUnicodeStr(l,r);
end;

function TOrdinalIStringComparer.GetHashCode(const Value: string): Integer;
var
  S: string;
begin
  S:= AnsiLowerCase(Value);
  Result:= MurmurHash3(PChar(S)^, SizeOf(Char) * Length(S), 0);
end;

{ TIStringComparer }

class destructor TIStringComparer.Destroy;
begin
  FreeAndNil(FOrdinal);
end;

class function TIStringComparer.Ordinal: TStringComparer;
begin
  if FOrdinal = nil then FOrdinal:= TOrdinalIStringComparer.Create;
  Result:= TStringComparer(FOrdinal);
end;

//function ExtendedHashImpl(Value: Extended): Integer;
//begin
//  Result:= MurmurHash3(Normalize(Value), SizeOf(Value), 0);
//end;
//
//function ExtendedHash(Value: Pointer; Size: Integer): Integer; inline;
//begin
//  Result:= ExtendedHashImpl(PExtended(Value)^);
//end;
//
//function DoubleHash(Value: Pointer; Size: Integer): Integer; inline;
//begin
//  Result:= ExtendedHashImpl(PDouble(Value)^);
//end;
//
//function SingleHash(Value: Pointer; Size: Integer): Integer; inline;
//begin
//  Result:= ExtendedHashImpl(PSingle(Value)^);
//end;

{ TComparer<T> }


type
  TCompareType = (ctEmpty, ctPS1, ctPS2, ctPS3, ctPS4, ctPSn, ctF4,ctF6,ctF8,ctF10,
    ctUS, ctWS, ctLS, ctDynArray, ctVar, ctPointer, ctClass, ctMethod,
    ctU1, ctU2, ctU3, ctU4, ctU8, ctI1, ctI2, ctI4, ctI8, ctBinary);

var
  CompareVMTs: array[TCompareType] of TCompareRec;
  EqualityVMTs: array[TCompareType] of TEqualityRec;

class function TPrivate.InitCompareVMT(Kind: TTypeKind; Info: PTypeInfo; Size: NativeUInt; Signed: boolean): PCompareRec;

  procedure SetResult(Compare: pointer; CompareType: TCompareType);
  begin
    CompareVMTs[CompareType].VMT:= @CompareVMTs[CompareType].VMTArray[0];
    CompareVMTs[CompareType].VMTArray[0]:= @NopQueryInterface;
    CompareVMTs[CompareType].VMTArray[1]:= @Nop;
    CompareVMTs[CompareType].VMTArray[2]:= @Nop;
    CompareVMTs[CompareType].VMTArray[3]:= Compare;
    CompareVMTs[CompareType].VMT:= @(CompareVMTs[CompareType].VMTArray[0]);
    Result:= @CompareVMTs[CompareType];
  end;

begin
  case Kind of
    //tkWString: Result:= (CompareStr(WideString(l.p), WideString(r.p)));
    tkWString: SetResult(@FCompareWString, ctWS);
    tkUString: SetResult(@FCompareUString, ctUS);
    tkLString: SetResult(@FCompareLString, ctLS);
    tkDynArray: SetResult(@FCompareDynArray, ctDynArray);
    tkVariant: SetResult(@FCompareVariant, ctVar);
    tkClass, tkClassRef, tkPointer, tkInterface, tkProcedure: SetResult(@FComparePointer, ctPointer);
    tkMethod: SetResult(@FCompareMethod, ctMethod);
    tkString: case Size of
      1: SetResult(@FCompareU1, ctU1);
      2: SetResult(@FComparePS1, ctPS1);
      3: SetResult(@FComparePS2, ctPS2);
      4: SetResult(@FComparePS3, ctPS3);
      else SetResult(@FComparePSn, ctPSn);
    end;
    else
  //Complex cases...
    case Size of
      0: SetResult(@FCompareEmptyRec, ctEmpty);
      1: begin
        if Info = TypeInfo(ShortInt) then SetResult(@FCompareI1, ctI1)
        else if Info = TypeInfo(byte) then SetResult(@FCompareU1, ctU1)
        else if Kind <> tkInteger then SetResult(@FCompareU1, ctU1)
        else if Signed then SetResult(@FCompareI1, ctI1)
        else SetResult(@FCompareU1, ctU1);
      end;
      2: begin
        case Kind of
          tkInteger: begin
            if Info = TypeInfo(SmallInt) then SetResult(@FCompareI2, ctI2)
            else if Info = TypeInfo(word) then SetResult(@FCompareU2, ctU2)
            else if Signed then SetResult(@FCompareI2, ctI2)
            else SetResult(@FCompareI2, ctI2);
          end;
          else SetResult(@FCompareU2, ctU2);
        end;
      end;
      3: SetResult(@FCompareI3, ctU3);
      4: begin
        case Kind of
          tkSet, tkRecord, tkEnumeration: SetResult(@FCompareU4, ctU4);
          tkFloat: SetResult(@FCompareR4, ctF4);
          tkInteger: begin
            if (Info = TypeInfo(integer))
                     or (Info = TypeInfo(CppLongInt))
                     or (Info = TypeInfo(HResult))
            then SetResult(@FCompareI4, ctI4)
            else if (Info = TypeInfo(Cardinal))
                 or (Info = TypeInfo(CppULongInt))
                 or (Info = TypeInfo(UCS4Char))
                 {$IFDEF MSWindows}
                 or (Info = TypeInfo(Handle_ptr))
                 or (Info = TypeInfo(HWND))
                 or (Info = TypeInfo(HHook))
                 or (Info = TypeInfo(HGDIOBJ))
                 or (Info = TypeInfo(HACCEL))
                 or (Info = TypeInfo(HBITMAP))
                 or (Info = TypeInfo(HBRUSH))
                 or (Info = TypeInfo(HCOLORSPACE))
                 or (Info = TypeInfo(HDC))
                 or (Info = TypeInfo(HGLRC))
                 or (Info = TypeInfo(HDESK))
                 or (Info = TypeInfo(HENHMETAFILE))
                 or (Info = TypeInfo(HFONT))
                 or (Info = TypeInfo(HICON))
                 or (Info = TypeInfo(HMENU))
                 or (Info = TypeInfo(HMETAFILE))
                 or (Info = TypeInfo(HPALETTE))
                 or (Info = TypeInfo(HPEN))
                 or (Info = TypeInfo(HRGN))
                 or (Info = TypeInfo(HSTR))
                 or (Info = TypeInfo(HTASK))
                 or (Info = TypeInfo(HWINSTA))
                 or (Info = TypeInfo(HKL))
                 or (Info = TypeInfo(HKEY))
                 or (Info = TypeInfo(HGESTUREINFO))
                 {$ENDIF}
            then SetResult(@FCompareU4, ctU4)
            else if Signed then SetResult(@FCompareI4, ctI4)
            else SetResult(@FCompareU4, ctU4);
          end;
          tkPointer: SetResult(@FCompareU4, ctU4);
          else SetResult(@FCompareU4, ctU4);
        end;
      end;
      5, 6, 7: begin
        case Kind of
          tkFloat: SetResult(@FCompareR6, ctF6);
          else SetResult(@FCompareBinary, ctBinary);
        end
      end;
      8: begin
        case Kind of
          tkInt64: begin
            if (Info = TypeInfo(Int64)) then SetResult(@FCompareI8, ctI8)
            else if (Info = TypeInfo(UInt64))
                 {$IFDEF MSWindows}
                 or (Info = TypeInfo(Handle_ptr))
                 or (Info = TypeInfo(HWND))
                 or (Info = TypeInfo(HHook))
                 or (Info = TypeInfo(HGDIOBJ))
                 or (Info = TypeInfo(HACCEL))
                 or (Info = TypeInfo(HBITMAP))
                 or (Info = TypeInfo(HBRUSH))
                 or (Info = TypeInfo(HCOLORSPACE))
                 or (Info = TypeInfo(HDC))
                 or (Info = TypeInfo(HGLRC))
                 or (Info = TypeInfo(HDESK))
                 or (Info = TypeInfo(HENHMETAFILE))
                 or (Info = TypeInfo(HFONT))
                 or (Info = TypeInfo(HICON))
                 or (Info = TypeInfo(HMENU))
                 or (Info = TypeInfo(HMETAFILE))
                 or (Info = TypeInfo(HPALETTE))
                 or (Info = TypeInfo(HPEN))
                 or (Info = TypeInfo(HRGN))
                 or (Info = TypeInfo(HSTR))
                 or (Info = TypeInfo(HTASK))
                 or (Info = TypeInfo(HWINSTA))
                 or (Info = TypeInfo(HKL))
                 or (Info = TypeInfo(HKEY))
                 or (Info = TypeInfo(HGESTUREINFO))
                 {$ENDIF}
            then SetResult(@FCompareU8, ctU8)
            else if Signed then SetResult(@FCompareI8, ctI8)
            else SetResult(@FCompareU8, ctU8);
          end;
          tkFloat: SetResult(@FCompareR8, ctF8);
          tkSet, tkRecord, tkEnumeration: begin
            if (SizeOf(NativeInt) = 8) then SetResult(@FCompareU8, ctU8)
            else SetResult(@FCompareBinary, ctBinary);
          end;
          else SetResult(@FCompareBinary, ctBinary);
        end;
      end;
      10: begin
        if Kind = tkFloat then SetResult(@FCompareR10, ctF10)
        else SetResult(@FCompareBinary, ctBinary);
      end;
      else SetResult(@FCompareBinary, ctBinary);
    end;
  end;
end;

class function TComparer<T>.Default: IComparer<T>;
var
  Size: integer;
  Signed: boolean;
begin
  if GetTypeKind(T) = tkDynArray then Size:= PTypeInfo(TypeInfo(T)).TypeData.elSize
  else Size:= SizeOf(T);

  case GetTypeKind(T) of
    tkInteger: begin
      Signed:= GetTypeData(TypeInfo(T))^.OrdType in [otSByte, otSWord, otSLong];
    end;
    tkInt64: begin
      Signed:= GetTypeData(TypeInfo(T))^.MaxInt64Value > GetTypeData(TypeInfo(T))^.MinInt64Value;
    end;
    tkFloat: Signed:= true;
    else Signed:= false;
  end;

  Result:= IComparer<T>(TPrivate.GetCompareInterface(GetTypeKind(T), TypeInfo(T), Size, Signed));
end;

class function TComparer<T>.Construct(const Comparison: TComparison<T>): IComparer<T>;
begin
  Result:= TDelegatedComparer<T>.Create(Comparison);
end;


{ TEqualityComparer<T> }
class function TEqualityComparer<T>.Default: IEqualityComparer<T>;
var
  Size: integer;
  Signed: boolean;
begin
  if GetTypeKind(T) = tkDynArray then Size:= PTypeInfo(TypeInfo(T)).TypeData.elSize
  else Size:= SizeOf(T);
  case GetTypeKind(T) of
    tkInteger: begin
      Signed:= GetTypeData(TypeInfo(T))^.OrdType in [otSByte, otSWord, otSLong];
    end;
    tkInt64: begin
      Signed:= GetTypeData(TypeInfo(T))^.MaxInt64Value > GetTypeData(TypeInfo(T))^.MinInt64Value;
    end;
    tkFloat: Signed:= true;
    else Signed:= false;
  end;
  Result:= IEqualityComparer<T>(TPrivate.GetEqualityInterface(GetTypeKind(T), TypeInfo(T), Size));
end;

class function TPrivate.DefaultHash<T>(const Value: T): Integer;
begin
  Result:= MurmurHash3(Value, SizeOf(T));
end;

class function TEqualityComparer<T>.Construct(const EqualityComparison
  : TEqualityComparison<T>; const Hasher: THasher<T> = nil): IEqualityComparer<T>;
var
  NewHasher: THasher<T>;
begin
  if (not Assigned(Hasher)) then NewHasher:= TPrivate.DefaultHash<T>
  else NewHasher:= Hasher;
  Result:= TDelegatedEqualityComparer<T>.Create(EqualityComparison, NewHasher);
end;

//class constructor TEqualityComparer<T>.Init;
//begin
//end;

class function TPrivate.InitEqualityVMT(Kind: TTypeKind; Info: PTypeInfo; Size: NativeUInt): PEqualityRec;

  procedure SetResult(Equal, Hash: pointer; EqualityType: TCompareType);
  begin
    EqualityVMTs[EqualityType].VMT:= @EqualityVMTs[EqualityType].VMTArray[0];
    EqualityVMTs[EqualityType].VMTArray[0]:= @NopQueryInterface;
    EqualityVMTs[EqualityType].VMTArray[1]:= @Nop;
    EqualityVMTs[EqualityType].VMTArray[2]:= @Nop;
    EqualityVMTs[EqualityType].VMTArray[3]:= Equal;
    EqualityVMTs[EqualityType].VMTArray[4]:= Hash;
    Result:= @EqualityVMTs[EqualityType];
  end;

begin
  if Size = 0 then SetResult(@Equals_EmptyRec, @GetHashCode_EmptyRec, ctEmpty)
  else case Kind of
    tkString: case Size of
      1: SetResult(@Equals_U1, @GetHashCode_U1, ctPS1);
      2: SetResult(@Equals_PS1, @GetHashCode_PS1, ctPS2);
      3: SetResult(@Equals_PS2, @GetHashCode_PS2, ctPS3);
      4: SetResult(@Equals_PS3, @GetHashCode_PS3, ctPS4);
      else SetResult(@Equals_PSn, @GetHashCode_PSn, ctPSn);
    end;
    tkFloat: case Size of
      4: SetResult(@Equals_R4, @GetHashCode_R4, ctF4);
      6: SetResult(@Equals_R6, @GetHashCode_R6, ctF6);
      8: SetResult(@Equals_R8, @GetHashCode_R8, ctF8);
      10: SetResult(@Equals_R10, @GetHashCode_R10, ctF10);
    end;
    tkUString: SetResult(@Equals_UString, @GetHashCode_UString, ctUS);
    tkLString: SetResult(@Equals_LString, @GetHashCode_LString, ctLS);
    tkWString: SetResult(@Equals_WString, @GetHashCode_WString, ctWS);
    tkDynArray: SetResult(@Equals_DynArray, @GetHashCode_DynArray, ctDynArray);
    tkVariant: SetResult(@Equals_Variant, @GetHashCode_Variant, ctVar);
    tkClassRef, tkPointer, tkInterface, tkProcedure: begin
      SetResult(@Equals_Pointer, @GetHashCode_Pointer, ctPointer);
    end;
    tkMethod: SetResult(@Equals_Method, @GetHashCode_Method, ctMethod);
    tkClass: SetResult(@Equals_Class, @GetHashCode_Class, ctClass);
    tkInt64: SetResult(@Equals_U8, @GetHashCode_U8, ctU8);
    else case Size of
      1: SetResult(@Equals_U1, @GetHashCode_U1, ctU1);
      2: SetResult(@Equals_U2, @GetHashCode_U2, ctU2);
      3: SetResult(@Equals_U3, @GetHashCode_U3, ctU3);
      4: SetResult(@Equals_U4, @GetHashCode_U4, ctU4);
      8: begin
        if SizeOf(NativeInt) = 8 then SetResult(@Equals_U8, @GetHashCode_U8, ctU8)
        else SetResult(@Equals_Binary, @GetHashCode_Binary, ctBinary);
      end;
      else SetResult(@Equals_Binary, @GetHashCode_Binary, ctBinary);
    end;
  end;
end;

class function TPrivate.GetCompareInterface(Kind: TTypeKind; info: PTypeInfo; Size: NativeUInt; Signed: boolean): pointer;
var
  NeedSize: Boolean;
begin
  NeedSize:= Kind in [tkArray, tkDynArray, tkEnumeration, tkRecord, tkSet, tkString, tkUnknown];
  if NeedSize then Result:= MakeInstance(TPrivate.InitCompareVMT(Kind, info, Size, Signed), Size)
  else Result:= InitCompareVMT(Kind, info, Size, Signed);
end;

class function TPrivate.GetEqualityInterface(Kind: TTypeKind; info: PTypeInfo; Size: NativeUInt): pointer;
var
  NeedSize: Boolean;
begin
  NeedSize:= Kind in [tkArray, tkDynArray, tkEnumeration, tkRecord, tkSet, tkString, tkUnknown];
  if NeedSize then Result:= MakeInstance(PCompareRec(TPrivate.InitEqualityVMT(Kind, info, Size)), Size)
  else Result:= InitEqualityVMT(Kind, info, Size);
end;


end.
