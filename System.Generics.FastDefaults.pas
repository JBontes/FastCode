{ ******************************************************* }
{                                                         }
{ CodeGear Delphi Runtime Library                         }
{                                                         }
{ Copyright(c) 1995-2014 Embarcadero Technologies, Inc.   }
{                                                         }
{ ******************************************************* }

unit System.Generics.FastDefaults;

{$R-,T-,X+,H+,B-}
(*$HPPEMIT '#pragma option -w-8022'*)

interface

uses System.SysUtils, System.TypInfo, System.Generics.Defaults;

type

  // Abstract base class for IComparer<T> implementations, and a provider
  // of default IComparer<T> implementations.
  TComparer<T> = class(TInterfacedObject, IComparer<T>)
  public
    class function Default: IComparer<T>;
    class function Construct(const Comparison: TComparison<T>): System.Generics.Defaults.IComparer<T>;
    function Compare(const Left, Right: T): Integer; virtual; abstract;
  end;

  // Abstract base class for IEqualityComparer<T> implementations, and a provider
  // of default IEqualityComparer<T> implementations.
  TEqualityComparer<T> = class(TInterfacedObject, IEqualityComparer<T>)
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

  TDelegatedEqualityComparer<T> = class(System.Generics.Defaults.TDelegatedEqualityComparer<T>)
  private
    FEquals: TEqualityComparison<T>;
    FHasher: THasher<T>;
  public
    constructor Create(const AEquals: TEqualityComparison<T>; const AHasher: System.Generics.Defaults.THasher<T>);
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

  TCustomComparer<T> = class(TSingletonImplementation, IComparer<T>, IEqualityComparer<T>)
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

function BinaryCompare(const Left, Right: Pointer; Size: Integer): Integer;

// Must be in interface section to be used by generic method. For internal use only.
type
  TPrivate = record
  private type
    TDefaultGenericInterface = (giComparer, giEqualityComparer);
  private
    class function _LookupVtableInfo(intf: TDefaultGenericInterface; info: PTypeInfo;
                                     Size: Integer): Pointer; static;
    class function DefaultHash<T>(const Value: T): Integer; static;
  end;


implementation

uses
{$IFNDEF NEXTGEN}
  System.AnsiStrings,
{$ENDIF}
  System.Math, System.Generics.Collections, System.Variants,
  FastDefaults;

type
  PSimpleInstance = ^TSimpleInstance;

  TSimpleInstance = record
    Vtable: Pointer;
    RefCount: Integer;
    Size: Integer;
  end;

  TInfoFlags = set of (ifVariableSize, ifSelector);
  PVtableInfo = ^TVtableInfo;

  TVtableInfo = record
    Flags: TInfoFlags;
    Data: Pointer;
  end;

  TTypeInfoSelector = function(info: PTypeInfo; Size: Integer): Pointer;

function MakeInstance(Vtable: Pointer; sizeField: Integer): Pointer;
var
  inst: PSimpleInstance;
begin
  GetMem(inst, SizeOf(inst^));
  inst^.Vtable:= Vtable;
  inst^.RefCount:= 0;
  inst^.Size:= sizeField;
  Result:= inst;
end;


function Nop(inst: Pointer): Integer; stdcall;
begin
  Result:= -1;
end;

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

// I/U 1-4

function Compare_I1(inst: Pointer; const Left, Right: Shortint): Integer;
begin
  { Use subtraction }
  Result:= Left - Right;
end;

function Equals_I1(inst: Pointer; const Left, Right: Shortint): Boolean;
begin
  Result:= Left = Right;
end;

function GetHashCode_I1(inst: Pointer; const Value: Shortint): Integer;
begin
  Result:= MurmurHash3(Value, SizeOf(Value), 0);
end;

function Compare_I2(inst: Pointer; const Left, Right: Smallint): Integer;
begin
  { Use subtraction }
  Result:= Left - Right;
end;

function Equals_I2(inst: Pointer; const Left, Right: Smallint): Boolean;
begin
  Result:= Left = Right;
end;

function GetHashCode_I2(inst: Pointer; const Value: Smallint): Integer;
begin
  Result:= MurmurHash3(Value, SizeOf(Value), 0);
end;

function Compare_I4(inst: Pointer; const Left, Right: Integer): Integer;
begin
  Result:= integer(Left > Right) - integer(Left < Right);
end;

function Equals_I4(inst: Pointer; const Left, Right: Integer): Boolean;
begin
  Result:= Left = Right;
end;

function GetHashCode_I4(inst: Pointer; const Value: Integer): Integer;
begin
  Result:= MurmurHash3(Value, SizeOf(Value), 0);
end;

function Compare_U1(inst: Pointer; const Left, Right: Byte): Integer;
begin
  { Use subtraction }
  Result:= Integer(Left) - Integer(Right);
end;

function Compare_U2(inst: Pointer; const Left, Right: Word): Integer;
begin
  { Use subtraction }
  Result:= Integer(Left) - Integer(Right);
end;

function Compare_U4(inst: Pointer; const Left, Right: LongWord): Integer;
begin
  Result:= integer(Left > Right) - integer(Left < Right);
end;

const
  Comparer_Vtable_I1: array [0 .. 3] of Pointer =
    (@NopQueryInterface, @Nop, @Nop, @Compare_I1);

  Comparer_Vtable_U1: array [0 .. 3] of Pointer =
    (@NopQueryInterface, @Nop, @Nop, @Compare_U1);

  Comparer_Vtable_I2: array [0 .. 3] of Pointer =
    (@NopQueryInterface, @Nop, @Nop, @Compare_I2);

  Comparer_Vtable_U2: array [0 .. 3] of Pointer =
    (@NopQueryInterface, @Nop, @Nop, @Compare_U2);

  Comparer_Vtable_I4: array [0 .. 3] of Pointer =
    (@NopQueryInterface, @Nop, @Nop, @Compare_I4);

  Comparer_Vtable_U4: array [0 .. 3] of Pointer =
    (@NopQueryInterface, @Nop, @Nop, @Compare_U4);

  Comparer_Instance_I1: Pointer = @Comparer_Vtable_I1;
  Comparer_Instance_U1: Pointer = @Comparer_Vtable_U1;
  Comparer_Instance_I2: Pointer = @Comparer_Vtable_I2;
  Comparer_Instance_U2: Pointer = @Comparer_Vtable_U2;
  Comparer_Instance_I4: Pointer = @Comparer_Vtable_I4;
  Comparer_Instance_U4: Pointer = @Comparer_Vtable_U4;

  EqualityComparer_Vtable_I1: array [0 .. 4] of Pointer =
    (@NopQueryInterface, @Nop, @Nop, @Equals_I1, @GetHashCode_I1);

  EqualityComparer_Vtable_I2: array [0 .. 4] of Pointer =
    (@NopQueryInterface, @Nop, @Nop, @Equals_I2, @GetHashCode_I2);

  EqualityComparer_Vtable_I4: array [0 .. 4] of Pointer =
    (@NopQueryInterface, @Nop, @Nop, @Equals_I4, @GetHashCode_I4);

  EqualityComparer_Instance_I1: Pointer = @EqualityComparer_Vtable_I1;
  EqualityComparer_Instance_I2: Pointer = @EqualityComparer_Vtable_I2;
  EqualityComparer_Instance_I4: Pointer = @EqualityComparer_Vtable_I4;

function Comparer_Selector_Integer(info: PTypeInfo; Size: Integer): Pointer;
begin
  case GetTypeData(info)^.OrdType of
    otSByte:
      Result:= @Comparer_Instance_I1;
    otUByte:
      Result:= @Comparer_Instance_U1;
    otSWord:
      Result:= @Comparer_Instance_I2;
    otUWord:
      Result:= @Comparer_Instance_U2;
    otSLong:
      Result:= @Comparer_Instance_I4;
    otULong:
      Result:= @Comparer_Instance_U4;
  else
    System.Error(reRangeError);
    Exit(nil);
  end;
end;

function EqualityComparer_Selector_Integer(info: PTypeInfo;
  Size: Integer): Pointer;
begin
  case GetTypeData(info)^.OrdType of
    otSByte, otUByte:
      Result:= @EqualityComparer_Instance_I1;
    otSWord, otUWord:
      Result:= @EqualityComparer_Instance_I2;
    otSLong, otULong:
      Result:= @EqualityComparer_Instance_I4;
  else
    System.Error(reRangeError);
    Exit(nil);
  end;
end;

// I8 & U8

function Equals_I8(inst: Pointer; const Left, Right: Int64): Boolean;
begin
  Result:= Left = Right;
end;

function GetHashCode_I8(inst: Pointer; const Value: Int64): Integer;
begin
  Result:= MurmurHash3(Value, SizeOf(Value), 0);
end;

function Compare_I8(inst: Pointer; const Left, Right: Int64): Integer;
begin
  Result:= integer(Left > Right) - integer(Left < Right);
end;

function Compare_U8(inst: Pointer; const Left, Right: UInt64): Integer;
begin
  Result:= integer(Left > Right) - integer(Left < Right);
end;

const
  Comparer_Vtable_I8: array [0 .. 3] of Pointer = (@NopQueryInterface,
    @Nop, @Nop, @Compare_I8);
  Comparer_Instance_I8: Pointer = @Comparer_Vtable_I8;

  Comparer_Vtable_U8: array [0 .. 3] of Pointer = (@NopQueryInterface,
    @Nop, @Nop, @Compare_U8);
  Comparer_Instance_U8: Pointer = @Comparer_Vtable_U8;

  EqualityComparer_Vtable_I8: array [0 .. 4] of Pointer = (@NopQueryInterface,
    @Nop, @Nop, @Equals_I8, @GetHashCode_I8);
  EqualityComparer_Instance_I8: Pointer = @EqualityComparer_Vtable_I8;

function Comparer_Selector_Int64(info: PTypeInfo; Size: Integer): Pointer;
begin
  if GetTypeData(info)^.MaxInt64Value > GetTypeData(info)^.MinInt64Value then
    Result:= @Comparer_Instance_I8
  else
    Result:= @Comparer_Instance_U8;
end;

// Float

function Compare_R4(inst: Pointer; const Left, Right: Single): Integer;
begin
  Result:= integer(Left > Right) - integer(Left < Right);
end;

function Equals_R4(inst: Pointer; const Left, Right: Single): Boolean;
begin
  Result:= Left = Right;
end;

function GetHashCode_R4(inst: Pointer; const Value: Single): Integer;
var
  m: Extended;
  e: Integer;
begin
  // Denormalized floats and positive/negative 0.0 complicate things.
  Frexp(Value, m, e);
  if m = 0 then m:= Abs(m);
  Result:= MurmurHash3(m, SizeOf(m), 0);
  Result:= MurmurHash3(e, SizeOf(e), Result);
end;

const
  Comparer_Vtable_R4: array [0 .. 3] of Pointer = (@NopQueryInterface,
    @Nop, @Nop, @Compare_R4);
  Comparer_Instance_R4: Pointer = @Comparer_Vtable_R4;

  EqualityComparer_Vtable_R4: array [0 .. 4] of Pointer = (@NopQueryInterface,
    @Nop, @Nop, @Equals_R4, @GetHashCode_R4);
  EqualityComparer_Instance_R4: Pointer = @EqualityComparer_Vtable_R4;

function Compare_R8(inst: Pointer; const Left, Right: Double): Integer;
begin
  Result:= integer(Left > Right) - integer(Left < Right);
end;

function Equals_R8(inst: Pointer; const Left, Right: Double): Boolean;
begin
  Result:= Left = Right;
end;

function GetHashCode_R8(inst: Pointer; const Value: Double): Integer;
var
  m: Extended;
  e: Integer;
begin
  // Denormalized floats and positive/negative 0.0 complicate things.
  Frexp(Value, m, e);
  if m = 0 then m:= Abs(m);
  Result:= MurmurHash3(m, SizeOf(m), 0);
  Result:= MurmurHash3(e, SizeOf(e), Result);
end;

const
  Comparer_Vtable_R8: array [0 .. 3] of Pointer = (@NopQueryInterface,
    @Nop, @Nop, @Compare_R8);
  Comparer_Instance_R8: Pointer = @Comparer_Vtable_R8;

  EqualityComparer_Vtable_R8: array [0 .. 4] of Pointer = (@NopQueryInterface,
    @Nop, @Nop, @Equals_R8, @GetHashCode_R8);
  EqualityComparer_Instance_R8: Pointer = @EqualityComparer_Vtable_R8;

function Compare_R10(inst: Pointer; const Left, Right: Extended): Integer;
begin
  Result:= integer(Left > Right) - integer(Left < Right);
end;

function Equals_R10(inst: Pointer; const Left, Right: Extended): Boolean;
begin
  Result:= Left = Right;
end;

function GetHashCode_R10(inst: Pointer; const Value: Extended): Integer;
var
  m: Extended;
  e: Integer;
begin
  // Denormalized floats and positive/negative 0.0 complicate things.
  Frexp(Value, m, e);
  if m = 0 then m:= Abs(m);
  Result:= MurmurHash3(m, SizeOf(m), 0);
  Result:= MurmurHash3(e, SizeOf(e), Result);
end;

const
  Comparer_Vtable_R10: array [0 .. 3] of Pointer = (@NopQueryInterface,
    @Nop, @Nop, @Compare_R10);
  Comparer_Instance_R10: Pointer = @Comparer_Vtable_R10;

  EqualityComparer_Vtable_R10: array [0 .. 4] of Pointer = (@NopQueryInterface,
    @Nop, @Nop, @Equals_R10, @GetHashCode_R10);
  EqualityComparer_Instance_R10: Pointer = @EqualityComparer_Vtable_R10;

function Compare_RI8(inst: Pointer; const Left, Right: Comp): Integer;
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

const
  Comparer_Vtable_RI8: array [0 .. 3] of Pointer = (@NopQueryInterface,
    @Nop, @Nop, @Compare_RI8);
  Comparer_Instance_RI8: Pointer = @Comparer_Vtable_RI8;

  EqualityComparer_Vtable_RI8: array [0 .. 4] of Pointer = (@NopQueryInterface,
    @Nop, @Nop, @Equals_RI8, @GetHashCode_RI8);
  EqualityComparer_Instance_RI8: Pointer = @EqualityComparer_Vtable_RI8;

function Compare_RC8(inst: Pointer; const Left, Right: Currency): Integer;
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

const
  Comparer_Vtable_RC8: array [0 .. 3] of Pointer = (@NopQueryInterface,
    @Nop, @Nop, @Compare_RC8);
  Comparer_Instance_RC8: Pointer = @Comparer_Vtable_RC8;

  EqualityComparer_Vtable_RC8: array [0 .. 4] of Pointer = (@NopQueryInterface,
    @Nop, @Nop, @Equals_RC8, @GetHashCode_RC8);
  EqualityComparer_Instance_RC8: Pointer = @EqualityComparer_Vtable_RC8;

function Comparer_Selector_Float(info: PTypeInfo; Size: Integer): Pointer;
begin
  case GetTypeData(info)^.FloatType of
    ftSingle:
      Result:= @Comparer_Instance_R4;
    ftDouble:
      Result:= @Comparer_Instance_R8;
    ftExtended:
      Result:= @Comparer_Instance_R10;
    ftComp:
      Result:= @Comparer_Instance_RI8;
    ftCurr:
      Result:= @Comparer_Instance_RC8;
  else
    System.Error(reRangeError);
    Exit(nil);
  end;
end;

function EqualityComparer_Selector_Float(info: PTypeInfo;
  Size: Integer): Pointer;
begin
  case GetTypeData(info)^.FloatType of
    ftSingle:
      Result:= @EqualityComparer_Instance_R4;
    ftDouble:
      Result:= @EqualityComparer_Instance_R8;
    ftExtended:
      Result:= @EqualityComparer_Instance_R10;
    ftComp:
      Result:= @EqualityComparer_Instance_RI8;
    ftCurr:
      Result:= @EqualityComparer_Instance_RC8;
  else
    System.Error(reRangeError);
    Exit(nil);
  end;
end;

// Binary

function BinaryCompare(const Left, Right: Pointer; Size: Integer): Integer;
var
  pl, pr: PByte;
  Len: Integer;
begin
  pl:= Left;
  pr:= Right;
  Len:= Size;
  while Len > 0 do
  begin
    Result:= pl^ - pr^;
    if Result <> 0 then
      Exit;
    Dec(Len);
    Inc(pl);
    Inc(pr);
  end;
  Result:= 0;
end;

function Compare_Binary(inst: PSimpleInstance; const Left, Right): Integer;
begin
  Result:= BinaryCompare(@Left, @Right, inst^.Size);
end;

function Equals_Binary(inst: PSimpleInstance; const Left, Right): Boolean;
begin
  Result:= CompareMem(@Left, @Right, inst^.Size);
end;

function GetHashCode_Binary(inst: PSimpleInstance; const Value): Integer;
begin
  Result:= MurmurHash3(Value, inst^.Size, 0);
end;

const
  Comparer_Vtable_Binary: array [0 .. 3] of Pointer = (@NopQueryInterface,
    @MemAddref, @MemRelease, @Compare_Binary);

  EqualityComparer_Vtable_Binary: array [0 .. 4] of Pointer =
    (@NopQueryInterface, @MemAddref, @MemRelease, @Equals_Binary,
    @GetHashCode_Binary);

function Comparer_Selector_Binary(info: PTypeInfo; Size: Integer): Pointer;
begin
  case Size of
    // NOTE: Little-endianness may cause counterintuitive results,
    // but the results will at least be consistent.
    1:
      Result:= @Comparer_Instance_U1;
    2:
      Result:= @Comparer_Instance_U2;
    4:
      Result:= @Comparer_Instance_U4;
{$IFDEF CPUX64}
    // 64-bit will pass const args in registers
    8:
      Result:= @Comparer_Instance_U8;
{$ENDIF}
  else
    Result:= MakeInstance(@Comparer_Vtable_Binary, Size);
  end;
end;

function EqualityComparer_Selector_Binary(info: PTypeInfo;
  Size: Integer): Pointer;
begin
  case Size of
    1:
      Result:= @EqualityComparer_Instance_I1;
    2:
      Result:= @EqualityComparer_Instance_I2;
    4:
      Result:= @EqualityComparer_Instance_I4;
{$IFDEF CPUX64}
    // 64-bit will pass const args in registers
    8:
      Result:= @EqualityComparer_Instance_I8;
{$ENDIF}
  else
    Result:= MakeInstance(@EqualityComparer_Vtable_Binary, Size);
  end;
end;

// Class (i.e. instances)

function Equals_Class(inst: PSimpleInstance; Left, Right: TObject): Boolean;
begin
  if Left = nil then
    Result:= Right = nil
  else
    Result:= Left.Equals(Right);
end;

function GetHashCode_Class(inst: PSimpleInstance; Value: TObject): Integer;
begin
  if Value = nil then
    Result:= 42
  else
    Result:= Value.GetHashCode;
end;

// DynArray

function DynLen(Arr: Pointer): NativeInt; inline;
begin
  if Arr = nil then
    Exit(0);
  Result:= PNativeInt(PByte(Arr) - SizeOf(NativeInt))^;
end;

function Compare_DynArray(inst: PSimpleInstance; Left, Right: Pointer)
  : NativeInt;
var
  Len, lenDiff: NativeInt;
begin
  Len:= DynLen(Left);
  lenDiff:= Len - DynLen(Right);
  if lenDiff < 0 then
    Inc(Len, lenDiff);
  Result:= BinaryCompare(Left, Right, inst^.Size * Len);
  if Result = 0 then
    Result:= lenDiff;
end;

function Equals_DynArray(inst: PSimpleInstance; Left, Right: Pointer): Boolean;
var
  lenL, lenR: NativeInt;
begin
  lenL:= DynLen(Left);
  lenR:= DynLen(Right);
  if lenL <> lenR then
    Exit(False);
  Result:= CompareMem(Left, Right, inst^.Size * lenL);
end;

function GetHashCode_DynArray(inst: PSimpleInstance; Value: Pointer): Integer;
begin
  Result:= MurmurHash3(Value^, inst^.Size * DynLen(Value), 0);
end;

const
  Comparer_Vtable_DynArray: array [0 .. 3] of Pointer = (@NopQueryInterface,
    @MemAddref, @MemRelease, @Compare_DynArray);

  EqualityComparer_Vtable_DynArray: array [0 .. 4] of Pointer =
    (@NopQueryInterface, @MemAddref, @MemRelease, @Equals_DynArray,
    @GetHashCode_DynArray);

function Comparer_Selector_DynArray(info: PTypeInfo; Size: Integer): Pointer;
begin
  Result:= MakeInstance(@Comparer_Vtable_DynArray, GetTypeData(info)^.elSize);
end;

function EqualityComparer_Selector_DynArray(info: PTypeInfo;
  Size: Integer): Pointer;
begin
  Result:= MakeInstance(@EqualityComparer_Vtable_DynArray,
    GetTypeData(info)^.elSize);
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

function Compare_PS1(inst: PSimpleInstance; const Left, Right: TPS1): Integer;
begin
  Result:= integer(Left > Right) - integer(Left < Right);
end;

function Compare_PS2(inst: PSimpleInstance; const Left, Right: TPS2): Integer;
begin
  Result:= integer(Left > Right) - integer(Left < Right);
end;

function Compare_PS3(inst: PSimpleInstance; const Left, Right: TPS3): Integer;
begin
  Result:= integer(Left > Right) - integer(Left < Right);
end;

function Compare_PSn(inst: PSimpleInstance;
  const Left, Right: OpenString): Integer;
begin
  Result:= integer(Left > Right) - integer(Left < Right);
end;

function Equals_PS1(inst: PSimpleInstance; const Left, Right: TPS1): Boolean;
begin
  Result:= Left = Right;
end;

function Equals_PS2(inst: PSimpleInstance; const Left, Right: TPS2): Boolean;
begin
  Result:= Left = Right;
end;

function Equals_PS3(inst: PSimpleInstance; const Left, Right: TPS3): Boolean;
begin
  Result:= Left = Right;
end;

function Equals_PSn(inst: PSimpleInstance;
  const Left, Right: OpenString): Boolean;
begin
  Result:= Left = Right;
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

const
  Comparer_Vtable_PS1: array [0 .. 3] of Pointer = (@NopQueryInterface,
    @Nop, @Nop, @Compare_PS1);
  Comparer_Instance_PS1: Pointer = @Comparer_Vtable_PS1;

  Comparer_Vtable_PS2: array [0 .. 3] of Pointer = (@NopQueryInterface,
    @Nop, @Nop, @Compare_PS2);
  Comparer_Instance_PS2: Pointer = @Comparer_Vtable_PS2;

  Comparer_Vtable_PS3: array [0 .. 3] of Pointer = (@NopQueryInterface,
    @Nop, @Nop, @Compare_PS3);
  Comparer_Instance_PS3: Pointer = @Comparer_Vtable_PS3;

  Comparer_Vtable_PSn: array [0 .. 3] of Pointer = (@NopQueryInterface,
    @Nop, @Nop, @Compare_PSn);
  Comparer_Instance_PSn: Pointer = @Comparer_Vtable_PSn;

  EqualityComparer_Vtable_PS1: array [0 .. 4] of Pointer = (@NopQueryInterface,
    @Nop, @Nop, @Equals_PS1, @GetHashCode_PS1);
  EqualityComparer_Instance_PS1: Pointer = @EqualityComparer_Vtable_PS1;

  EqualityComparer_Vtable_PS2: array [0 .. 4] of Pointer = (@NopQueryInterface,
    @Nop, @Nop, @Equals_PS2, @GetHashCode_PS2);
  EqualityComparer_Instance_PS2: Pointer = @EqualityComparer_Vtable_PS2;

  EqualityComparer_Vtable_PS3: array [0 .. 4] of Pointer = (@NopQueryInterface,
    @Nop, @Nop, @Equals_PS3, @GetHashCode_PS3);
  EqualityComparer_Instance_PS3: Pointer = @EqualityComparer_Vtable_PS3;

  EqualityComparer_Vtable_PSn: array [0 .. 4] of Pointer = (@NopQueryInterface,
    @Nop, @Nop, @Equals_PSn, @GetHashCode_PSn);
  EqualityComparer_Instance_PSn: Pointer = @EqualityComparer_Vtable_PSn;

function Comparer_Selector_String(info: PTypeInfo; Size: Integer): Pointer;
begin
  case Size of
    2:
      Result:= @Comparer_Instance_PS1;
    3:
      Result:= @Comparer_Instance_PS2;
    4:
      Result:= @Comparer_Instance_PS3;
  else
    Result:= @Comparer_Instance_PSn;
  end;
end;

function EqualityComparer_Selector_String(info: PTypeInfo;
  Size: Integer): Pointer;
begin
  case Size of
    2:
      Result:= @EqualityComparer_Instance_PS1;
    3:
      Result:= @EqualityComparer_Instance_PS2;
    4:
      Result:= @EqualityComparer_Instance_PS3;
  else
    Result:= @EqualityComparer_Instance_PSn;
  end;
end;

{$IFNDEF NEXTGEN}

function Compare_LString(inst: PSimpleInstance;
  const Left, Right: AnsiString): Integer;
begin
  Result:= CompareWideStr(Left, Right);
end;

function Equals_LString(inst: PSimpleInstance;
  const Left, Right: AnsiString): Boolean;
begin
  Result:= Left = Right;
end;

function GetHashCode_LString(inst: PSimpleInstance;
  const Value: AnsiString): Integer;
begin
  Result:= MurmurHash3(Value[1], Length(Value) * SizeOf(Value[1]), 0);
end;
{$ENDIF !NEXTGEN}
// UStrings

function Compare_UString(inst: PSimpleInstance; const Left, Right: UnicodeString): Integer;
begin
  Result:= CompareWideStr(Left, Right);
end;

function Equals_UString(inst: PSimpleInstance;
  const Left, Right: UnicodeString): Boolean;
begin
  Result:= Left = Right;
end;

function GetHashCode_UString(inst: PSimpleInstance;
  const Value: UnicodeString): Integer;
begin
  Result:= MurmurHash3(Value[Low(string)], Length(Value) * SizeOf(Char), 0);
end;

// Methods

type
  TMethodPointer = procedure of object;

function Compare_Method(inst: PSimpleInstance; const Left, Right: TMethodPointer): Integer;
begin
  Result:= integer(TMethod(Left) > TMethod(Right)) - integer(TMethod(Left) < TMethod(Right));
end;

function Equals_Method(inst: PSimpleInstance;
  const Left, Right: TMethodPointer): Boolean;
begin
  Result:= TMethod(Left) = TMethod(Right);
end;

function GetHashCode_Method(inst: PSimpleInstance;
  const Value: TMethodPointer): Integer;
begin
  Result:= MurmurHash3(Value, SizeOf(TMethodPointer), 0);
end;

// WStrings

{$IFNDEF NEXTGEN}

function Compare_WString(inst: PSimpleInstance;
  const Left, Right: WideString): Integer;
begin
  Result:= integer(Left > Right) - integer(Left < Right);
end;

function Equals_WString(inst: PSimpleInstance;
  const Left, Right: WideString): Boolean;
begin
  Result:= Left = Right;
end;

function GetHashCode_WString(inst: PSimpleInstance;
  const Value: WideString): Integer;
begin
  Result:= MurmurHash3(Value[1], Length(Value) * SizeOf(Value[1]), 0);
end;
{$ENDIF !NEXTGEN}
// Variants

function Compare_Variant(inst: PSimpleInstance; Left, Right: Pointer): Integer;
var
  l, r: Variant;
  lAsString, rAsString: string;
begin
  Result:= 0; // Avoid warning.
  l:= PVariant(Left)^;
  r:= PVariant(Right)^;
  try
    case VarCompareValue(l, r) of
      vrEqual:
        Exit(0);
      vrLessThan:
        Exit(-1);
      vrGreaterThan:
        Exit(1);
      vrNotEqual:
        begin
          if VarIsEmpty(l) or VarIsNull(l) then
            Exit(1)
          else
            Exit(-1);
        end;
    end;
  except // if comparison failed with exception, compare as string.
    try
      lAsString:= PVariant(Left)^;
      rAsString:= PVariant(Right)^;
      Result:= Compare_UString(nil, lAsString, rAsString);
    except // if comparison fails again, compare bytes.
      Result:= BinaryCompare(Left, Right, SizeOf(Variant));
    end;
  end;
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

function Compare_Pointer(inst: PSimpleInstance;
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

{$IFDEF NEXTGEN}
const
  Compare_LString: Pointer = nil;
  Compare_WString: Pointer = nil;
  Equals_LString: Pointer = nil;
  GetHashCode_LString: Pointer = nil;
  Equals_WString: Pointer = nil;
  GetHashCode_WString: Pointer = nil;
{$ENDIF !NEXTGEN}

const
  Comparer_Vtable_Pointer: array [0 .. 3] of Pointer =
    (@NopQueryInterface, @Nop, @Nop, @Compare_Pointer);

  Comparer_Vtable_LString: array [0 .. 3] of Pointer =
    (@NopQueryInterface, @Nop, @Nop, @Compare_LString);

  Comparer_Vtable_WString: array [0 .. 3] of Pointer =
    (@NopQueryInterface, @Nop, @Nop, @Compare_WString);

  Comparer_Vtable_Variant: array [0 .. 3] of Pointer =
    (@NopQueryInterface, @Nop, @Nop, @Compare_Variant);

  Comparer_Vtable_UString: array [0 .. 3] of Pointer =
    (@NopQueryInterface, @Nop, @Nop, @Compare_UString);

  Comparer_Vtable_Method: array [0 .. 3] of Pointer =
    (@NopQueryInterface, @Nop, @Nop, @Compare_Method);

  Comparer_Instance_Pointer: Pointer = @Comparer_Vtable_Pointer;
  Comparer_Instance_LString: Pointer = @Comparer_Vtable_LString;
  Comparer_Instance_WString: Pointer = @Comparer_Vtable_WString;
  Comparer_Instance_Variant: Pointer = @Comparer_Vtable_Variant;
  Comparer_Instance_UString: Pointer = @Comparer_Vtable_UString;
  Comparer_Instance_Method: Pointer = @Comparer_Vtable_Method;

  EqualityComparer_Vtable_Pointer: array [0 .. 4] of Pointer =
    (@NopQueryInterface, @Nop, @Nop, @Equals_Pointer, @GetHashCode_Pointer);

  EqualityComparer_Vtable_Class: array [0 .. 4] of Pointer =
    (@NopQueryInterface, @Nop, @Nop, @Equals_Class, @GetHashCode_Class);

  EqualityComparer_Vtable_LString: array [0 .. 4] of Pointer =
    (@NopQueryInterface, @Nop, @Nop, @Equals_LString, @GetHashCode_LString);

  EqualityComparer_Vtable_WString: array [0 .. 4] of Pointer =
    (@NopQueryInterface, @Nop, @Nop, @Equals_WString, @GetHashCode_WString);

  EqualityComparer_Vtable_Variant: array [0 .. 4] of Pointer =
    (@NopQueryInterface, @Nop, @Nop, @Equals_Variant,
    @GetHashCode_Variant);

  EqualityComparer_Vtable_UString: array [0 .. 4] of Pointer =
    (@NopQueryInterface, @Nop, @Nop, @Equals_UString, @GetHashCode_UString);

  EqualityComparer_Vtable_Method: array [0 .. 4] of Pointer =
    (@NopQueryInterface, @Nop, @Nop, @Equals_Method, @GetHashCode_Method);

  EqualityComparer_Instance_Pointer: Pointer = @EqualityComparer_Vtable_Pointer;
  EqualityComparer_Instance_Class: Pointer = @EqualityComparer_Vtable_Class;
  EqualityComparer_Instance_LString: Pointer = @EqualityComparer_Vtable_LString;
  EqualityComparer_Instance_WString: Pointer = @EqualityComparer_Vtable_WString;
  EqualityComparer_Instance_Variant: Pointer = @EqualityComparer_Vtable_Variant;
  EqualityComparer_Instance_UString: Pointer = @EqualityComparer_Vtable_UString;
  EqualityComparer_Instance_Method: Pointer = @EqualityComparer_Vtable_Method;

  VtableInfo: array [TPrivate.TDefaultGenericInterface, TTypeKind] of TVtableInfo = (
    // IComparer
    (
    // tkUnknown
    (Flags: [ifSelector]; Data: @Comparer_Selector_Binary),
    // tkInteger
    (Flags: [ifSelector]; Data: @Comparer_Selector_Integer),
    // tkChar
    (Flags: [ifSelector]; Data: @Comparer_Selector_Binary),
    // tkEnumeration
    (Flags: [ifSelector]; Data: @Comparer_Selector_Integer),
    // tkFloat
    (Flags: [ifSelector]; Data: @Comparer_Selector_Float),
    // tkString
    (Flags: [ifSelector]; Data: @Comparer_Selector_String),
    // tkSet
    (Flags: [ifSelector]; Data: @Comparer_Selector_Binary),
    // tkClass
    (Flags: []; Data: @Comparer_Instance_Pointer),
    // tkMethod
    (Flags: []; Data: @Comparer_Instance_Method),
    // tkWChar
    (Flags: [ifSelector]; Data: @Comparer_Selector_Binary),
    // tkLString
    (Flags: []; Data: @Comparer_Instance_LString),
    // tkWString
    (Flags: []; Data: @Comparer_Instance_WString),
    // tkVariant
    (Flags: []; Data: @Comparer_Instance_Variant),
    // tkArray
    (Flags: [ifSelector]; Data: @Comparer_Selector_Binary),
    // tkRecord
    (Flags: [ifSelector]; Data: @Comparer_Selector_Binary),
    // tkInterface
    (Flags: []; Data: @Comparer_Instance_Pointer),
    // tkInt64
    (Flags: [ifSelector]; Data: @Comparer_Selector_Int64),
    // tkDynArray
    (Flags: [ifSelector]; Data: @Comparer_Selector_DynArray),
    // tkUString
    (Flags: []; Data: @Comparer_Instance_UString),
    // tkClassRef
    (Flags: []; Data: @Comparer_Instance_Pointer),
    // tkPointer
    (Flags: []; Data: @Comparer_Instance_Pointer),
    // tkProcedure
    (Flags: []; Data: @Comparer_Instance_Pointer)),
    // IEqualityComparer
    (
    // tkUnknown
    (Flags: [ifSelector]; Data: @EqualityComparer_Selector_Binary),
    // tkInteger
    (Flags: [ifSelector]; Data: @EqualityComparer_Selector_Integer),
    // tkChar
    (Flags: [ifSelector]; Data: @EqualityComparer_Selector_Binary),
    // tkEnumeration
    (Flags: [ifSelector]; Data: @EqualityComparer_Selector_Integer),
    // tkFloat
    (Flags: [ifSelector]; Data: @EqualityComparer_Selector_Float),
    // tkString
    (Flags: [ifSelector]; Data: @EqualityComparer_Selector_String),
    // tkSet
    (Flags: [ifSelector]; Data: @EqualityComparer_Selector_Binary),
    // tkClass
    (Flags: []; Data: @EqualityComparer_Instance_Class),
    // tkMethod
    (Flags: []; Data: @EqualityComparer_Instance_Method),
    // tkWChar
    (Flags: [ifSelector]; Data: @EqualityComparer_Selector_Binary),
    // tkLString
    (Flags: []; Data: @EqualityComparer_Instance_LString),
    // tkWString
    (Flags: []; Data: @EqualityComparer_Instance_WString),
    // tkVariant
    (Flags: []; Data: @EqualityComparer_Instance_Variant),
    // tkArray
    (Flags: [ifSelector]; Data: @EqualityComparer_Selector_Binary),
    // tkRecord
    (Flags: [ifSelector]; Data: @EqualityComparer_Selector_Binary),
    // tkInterface
    (Flags: []; Data: @EqualityComparer_Instance_Pointer),
    // tkInt64
    (Flags: []; Data: @EqualityComparer_Instance_I8),
    // tkDynArray
    (Flags: [ifSelector]; Data: @EqualityComparer_Selector_DynArray),
    // tkUString
    (Flags: []; Data: @EqualityComparer_Instance_UString),
    // tkClassRef
    (Flags: []; Data: @EqualityComparer_Instance_Pointer),
    // tkPointer
    (Flags: []; Data: @EqualityComparer_Instance_Pointer),
    // tkProcedure
    (Flags: []; Data: @EqualityComparer_Instance_Pointer)));

class function TPrivate._LookupVtableInfo(intf: TDefaultGenericInterface; info: PTypeInfo;
  Size: Integer): Pointer;
var
  pinfo: PVtableInfo;
begin
  if info <> nil then begin
    pinfo:= @VtableInfo[intf, info^.Kind];
    Result:= pinfo^.Data;
    if ifSelector in pinfo^.Flags then Result:= TTypeInfoSelector(Result)(info, Size);
    if ifVariableSize in pinfo^.Flags then Result:= MakeInstance(Result, Size);
  end else begin
    case intf of
      giComparer: Result:= Comparer_Selector_Binary(info, Size);
      giEqualityComparer: Result:= EqualityComparer_Selector_Binary(info, Size);
    else System.Error(reRangeError);
      Result:= nil;
    end;
  end;
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

constructor TDelegatedEqualityComparer<T>.Create(const AEquals: System.Generics.Defaults.TEqualityComparison<T>; const AHasher: System.Generics.Defaults.THasher<T>);
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

constructor TDelegatedComparer<T>.Create(const ACompare: System.Generics.Defaults.TComparison<T>);
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
  Len:= Length(Left);
  lenDiff:= Len - Length(Right);
  if Length(Right) < Len then Len:= Length(Right);
  Result:= BinaryCompare(PChar(Left), PChar(Right), Len * SizeOf(Char));
  if Result = 0 then Exit(lenDiff);
end;

function TOrdinalStringComparer.Equals(const Left, Right: string): Boolean;
var
  Len: Integer;
begin
  Len:= Length(Left);
  Result:= (Len = Length(Right)) and CompareMem(PChar(Left), PChar(Right), Len * SizeOf(Char));
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
  Len:= Length(l);
  lenDiff:= Len - Length(r);
  if Length(r) < Len then Len:= Length(r);
  Result:= BinaryCompare(PChar(l), PChar(r), Len * SizeOf(Char));
  if Result = 0 then Exit(lenDiff);
end;

function TOrdinalIStringComparer.Equals(const Left, Right: string): Boolean;
var
  Len: Integer;
  l, r: string;
begin
  l:= AnsiLowerCase(Left);
  r:= AnsiLowerCase(Right);
  Len:= Length(l);
  Result:= (Len = Length(r)) and CompareMem(PChar(l), PChar(r), Len * SizeOf(Char));
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

function ExtendedHashImpl(Value: Extended): Integer;
type
  TRec = record
    m: Extended;
    e: Integer;
  end;
var
  data: TRec;
begin
  // Denormalized floats and positive/negative 0.0 complicate things.
  Frexp(Value, Data.m, Data.e);
  if Data.m = 0 then Data.m:= Abs(Data.m);
  Result:= MurmurHash3(Data, SizeOf(Data), 0);
end;

function ExtendedHash(Value: Pointer; Size: Integer): Integer; inline;
begin
  Result:= ExtendedHashImpl(PExtended(Value)^);
end;

function DoubleHash(Value: Pointer; Size: Integer): Integer; inline;
begin
  Result:= ExtendedHashImpl(PDouble(Value)^);
end;

function SingleHash(Value: Pointer; Size: Integer): Integer; inline;
begin
  Result:= ExtendedHashImpl(PSingle(Value)^);
end;

{ TComparer<T> }

class function TComparer<T>.Default: System.Generics.Defaults.IComparer<T>;
begin
  Result:= System.Generics.Defaults.IComparer<T>(TPrivate._LookupVtableInfo(giComparer, TypeInfo(T), SizeOf(T)));
end;

class function TComparer<T>.Construct(const Comparison: System.Generics.Defaults.TComparison<T>)
  : System.Generics.Defaults.IComparer<T>;
begin
  Result:= TDelegatedComparer<T>.Create(Comparison);
end;

{ TEqualityComparer<T> }

class function TEqualityComparer<T>.Default: IEqualityComparer<T>;
begin
  Result:= IEqualityComparer<T>(TPrivate._LookupVtableInfo(giEqualityComparer,
    TypeInfo(T), SizeOf(T)));
end;

class function TPrivate.DefaultHash<T>(const Value: T): Integer;
begin
  Result:= MurmurHash3(Value, SizeOf(T));
end;

class function TEqualityComparer<T>.Construct(const EqualityComparison
  : System.Generics.Defaults.TEqualityComparison<T>; const Hasher: System.Generics.Defaults.THasher<T> = nil): System.Generics.Defaults.IEqualityComparer<T>;
var
  NewHasher: System.Generics.Defaults.THasher<T>;
begin
  if (not Assigned(Hasher)) then NewHasher:= TPrivate.DefaultHash<T>
  else NewHasher:= Hasher;
  Result:= TDelegatedEqualityComparer<T>.Create(EqualityComparison, NewHasher);
end;


end.
