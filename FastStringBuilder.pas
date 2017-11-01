unit FastStringBuilder;

interface

uses SysUtils;

type
  TStringBuilder = class
  private const
    MALLOC_SIZE = 10 * 1024; // 10KB
  private
    procedure ExpandCapacity(const AdditionalSize: Integer); inline;
    procedure SetCapacity(Value: NativeUInt);
    function GetChars(Index: NativeUInt): Char;
    procedure SetChars(Index: NativeUInt; Value: Char);
    //function GetLength: Integer; inline;
    procedure SetLength(Value: NativeUint); inline;
    //procedure ExpandCapacity(NewLength: NativeUInt);
    procedure ReduceCapacity; inline;
    procedure CheckBounds(Index: NativeUint); inline;
    function _Replace(Index: NativeUint; const Old, New: string): Boolean;
    //prevent ref count and exception frame for local strings.
    function _Append(const Value: PChar; Length: cardinal): TStringBuilder; overload;
    function _Append(const Value: TCharArray; StartIndex,
      CharCount: NativeUInt): TStringBuilder; overload;
  protected
    FData: TCharArray;
    //FInsertPoint: PChar;
    FLength: NativeUInt;
    FCapacity: NativeUint; //Avoid the need for nil testing the underlying storage.
    FMaxCapacity: NativeUInt;
  public
    constructor Create; overload;
    constructor Create(aCapacity: NativeUInt); overload;
    constructor Create(const Value: string); overload;
    constructor Create(aCapacity, aMaxCapacity: NativeUInt); overload;
    constructor Create(const Value: string; aCapacity: NativeUInt); overload;
    constructor Create(const Value: string; StartIndex, Length, aCapacity: NativeUInt); overload;

    function Append(const Value: string): TStringBuilder; overload;  //put first, so the inlines work.
    function Append(const Value: Boolean): TStringBuilder; overload; inline;
    function Append(const Value: Byte): TStringBuilder; overload; {$ifndef cpux64} inline; {$endif}
    function Append(const Value: Char): TStringBuilder; overload; {$ifndef cpux64} inline; {$endif}
    function Append(const Value: Currency): TStringBuilder; overload; inline;
    function Append(const Value: Double): TStringBuilder; overload; inline;
    function Append(const Value: Smallint): TStringBuilder; overload; {$ifndef cpux64} inline; {$endif}
    function Append(const Value: Integer): TStringBuilder; overload; {$ifndef cpux64} inline; {$endif}
    function Append(const Value: Int64): TStringBuilder; overload; {$ifndef cpux64} inline; {$endif}
    function Append(const Value: TObject): TStringBuilder; overload; inline;
    function Append(const Value: Shortint): TStringBuilder; overload; {$ifndef cpux64} inline; {$endif}
    function Append(const Value: Single): TStringBuilder; overload; inline;
    function Append(const Value: UInt64): TStringBuilder; overload; {$ifndef cpux64} inline; {$endif}
    function Append(const Value: TCharArray): TStringBuilder; overload; inline;
    function Append(const Value: Word): TStringBuilder; overload; {$ifndef cpux64} inline; {$endif}
    function Append(const Value: Cardinal): TStringBuilder; overload; {$ifndef cpux64} inline; {$endif}
{$IFNDEF NEXTGEN}
    function Append(const Value: PAnsiChar): TStringBuilder; overload; inline;
    function Append(const Value: RawByteString): TStringBuilder; overload; inline;
{$ENDIF !NEXTGEN}
    function Append(const Value: Char; RepeatCount: NativeUInt): TStringBuilder; overload;
    function Append(const Value: TCharArray; StartIndex, CharCount: NativeUInt): TStringBuilder; overload; inline;
    function Append(const Value: string; StartIndex, Count: NativeUInt): TStringBuilder; overload;

    function AppendFormat(const Format: string; const Args: array of const): TStringBuilder; overload;
    function AppendLine: TStringBuilder; overload;
    function AppendLine(const Value: string): TStringBuilder; overload; inline;
    procedure Clear; inline;
    procedure CopyTo(SourceIndex: NativeUInt; const Destination: TCharArray; DestinationIndex, Count: NativeUInt);
    function EnsureCapacity(aCapacity: NativeUInt): NativeUInt;
    function Equals(StringBuilder: TStringBuilder): Boolean; reintroduce;

    function Insert(Index: NativeUInt; const Value: Boolean): TStringBuilder; overload; inline;
    function Insert(Index: NativeUInt; const Value: Byte): TStringBuilder; overload; inline;
    function Insert(Index: NativeUInt; const Value: Char): TStringBuilder; overload;
    function Insert(Index: NativeUInt; const Value: Currency): TStringBuilder; overload; inline;
    function Insert(Index: NativeUInt; const Value: Double): TStringBuilder; overload; inline;
    function Insert(Index: NativeUInt; const Value: Smallint): TStringBuilder; overload; inline;
    function Insert(Index: NativeUInt; const Value: Integer): TStringBuilder; overload; inline;
    function Insert(Index: NativeUInt; const Value: TCharArray): TStringBuilder; overload;
    function Insert(Index: NativeUInt; const Value: Int64): TStringBuilder; overload; inline;
    function Insert(Index: NativeUInt; const Value: TObject): TStringBuilder; overload; inline;
    function Insert(Index: NativeUInt; const Value: Shortint): TStringBuilder; overload; inline;
    function Insert(Index: NativeUInt; const Value: Single): TStringBuilder; overload; inline;
    function Insert(Index: NativeUInt; const Value: string): TStringBuilder; overload;
    function Insert(Index: NativeUInt; const Value: Word): TStringBuilder; overload; inline;
    function Insert(Index: NativeUInt; const Value: Cardinal): TStringBuilder; overload; inline;
    function Insert(Index: NativeUInt; const Value: UInt64): TStringBuilder; overload; inline;
    function Insert(Index: NativeUInt; const Value: string; count: NativeUInt): TStringBuilder; overload;
    function Insert(Index: NativeUInt; const Value: TCharArray; startIndex: NativeUInt; charCount: NativeUInt): TStringBuilder; overload;

    function Remove(StartIndex: Integer; RemLength: Integer): TStringBuilder;

    function Replace(const OldChar: Char; const NewChar: Char): TStringBuilder; overload;
    function Replace(const OldValue: string; const NewValue: string): TStringBuilder; overload;
    function Replace(const OldChar: Char; const NewChar: Char; StartIndex, Count: NativeUInt): TStringBuilder; overload;
    function Replace(const OldValue: string; const NewValue: string; StartIndex, Count: NativeUInt): TStringBuilder; overload;

    function ToString: string; overload; override;
    function ToString(StartIndex: NativeUInt; StrLength: Cardinal): string; reintroduce; overload;

    property Capacity: NativeUInt read FCapacity write SetCapacity;
    property Chars[index: NativeUInt]: Char read GetChars write SetChars; default;
    property Length: NativeUInt read FLength write SetLength;
    property MaxCapacity: NativeUInt read FMaxCapacity;
  end;



implementation

uses
  System.SysConst, System.RTLConsts, WinAPI.Windows, system.classes;

const
  /// fast lookup table for converting any decimal number from
  // 0 to 99 into their ASCII equivalence
  // - our enhanced SysUtils.pas (normal and LVCL) contains the same array
  TwoDigitLookup: packed array[0..99] of array[1..2] of Char =
    ('00','01','02','03','04','05','06','07','08','09',
     '10','11','12','13','14','15','16','17','18','19',
     '20','21','22','23','24','25','26','27','28','29',
     '30','31','32','33','34','35','36','37','38','39',
     '40','41','42','43','44','45','46','47','48','49',
     '50','51','52','53','54','55','56','57','58','59',
     '60','61','62','63','64','65','66','67','68','69',
     '70','71','72','73','74','75','76','77','78','79',
     '80','81','82','83','84','85','86','87','88','89',
     '90','91','92','93','94','95','96','97','98','99');

    TwoDigitLookupAnsi: packed array[0..99] of array[1..2] of AnsiChar =
    ('00','01','02','03','04','05','06','07','08','09',
     '10','11','12','13','14','15','16','17','18','19',
     '20','21','22','23','24','25','26','27','28','29',
     '30','31','32','33','34','35','36','37','38','39',
     '40','41','42','43','44','45','46','47','48','49',
     '50','51','52','53','54','55','56','57','58','59',
     '60','61','62','63','64','65','66','67','68','69',
     '70','71','72','73','74','75','76','77','78','79',
     '80','81','82','83','84','85','86','87','88','89',
     '90','91','92','93','94','95','96','97','98','99');

{ TStringBuilder }
type
  TCharStorage = array [1..28] of char;
  TAnsiCharStorage = array [1..28] of AnsiChar;

{$ifdef cpux64}
/// <summary>
///  Convert value into a string.
///  Negative values will have a '-' in front.
///  Returns the first char of the string.
///  At Result -4 a length encoding will be stored.
/// </summary>
function _IntToStrAnsi(const Value: Int64; var Dump: TAnsiCharStorage): PAnsiChar; overload;
//rcx = value
//rdx = dump
//rax = length
asm
        .noframe
        push      rcx
        mov       r8,rcx
        neg       rcx
        cmovs     rcx,r8
        lea       r11, [rdx+24]
        push      r11   //save the end of the data
        lea       r10, [rip+TwoDigitLookupAnsi]
        //mov       byte ptr [rdx+60], 0
        cmp       rcx, 100
        jb        @tail
        mov       r8, $47ae147ae147ae15
@loop:
        mov       rax, rcx
        mov       r9d, ecx
        mul       r8
        sub       rcx, rdx
        shr       rcx, 1
        add       rdx, rcx
        mov       rcx, rdx
        shr       rcx, 6
        imul      rax, rcx, -100
        add       r9,  rax
        movzx     edx, word ptr [r10+r9*2]
        mov       [r11], dx
        sub       r11, 2
        cmp       rcx, 100
        jae       @loop
@tail:
        movzx     eax, word ptr [r10+rcx*2]
        mov       edx,$2d   //'-'
        mov       [r11], ax
        xor       eax, eax
        cmp       ecx, 10
        pop       r10
        pop       rcx
        setb      al
        add       r10, 4
        shl       rcx, 1     //sign flag to carry flag
        mov       [r11+rax-1],dl   //put the '-', just in case
        sbb       rax,0      //include the '-' if applicable
        lea       rax, [r11+rax]
        sub       r10,rax
        mov       [rax-4],r10d
        ret
end;
{$endif}

{$ifdef cpux64}

/// <summary>
///  Convert value into a string.
///  Negative values will have a '-' in front.
///  Returns the first char of the string.
///  At Result -4 a length encoding will be stored.
/// </summary>
function _IntToStr(const Value: Int64; var Dump: TCharStorage): PChar; overload;
//rcx = value
//rdx = dump
//rax = length
asm
  .noframe
  push      rcx            //1
  mov       r8,rcx         //3+1=4
  neg       rcx            //4+3=7
  cmovs     rcx,r8         //7+4=11      //Value = abs(value)
@@IntToStrNoSign:
  lea       r11, [rdx+26*2]
  push      r11             //save the end of the data for length calculations later
  lea       r10, [rip+TwoDigitLookup]
  cmp       rcx, 100        //process two digits at a time
  jb        @tail           //only two digits, goto tail.
  mov       r8, $47ae147ae147ae15  //division using multiplication by reciprocal
  db $0F, $1F, $84, $00, $00, $00, $00, $00  //nop 8
@loop:
  mov       rax, rcx
  mov       r9d, ecx
  mul       r8
  sub       rcx, rdx
  shr       rcx, 1
  add       rdx, rcx
  mov       rcx, rdx
  shr       rcx, 6
  imul      rax, rcx, -100  //i = remainder mod 100
  add       r9d,  eax
  mov       eax, [r10+r9*4] //digits = lookup[i]
  mov       [r11], eax      //add to digits to the string
  sub       r11, 4
  cmp       rcx, 100        //repeat ...
  jae       @loop           //... until remainder < 100
@tail:
  mov       eax,[r10+rcx*4] //lookup 2 digits
  pop       r10
  mov       edx,$2d         //sign = '-'
  mov       [r11], eax      //write 2 digits
  xor       eax, eax
  cmp       ecx, 10         //do we have a leading zero?
  pop       rcx
  setb      al              //yes, add one
  add       r10, 4          //correct starting pos
  shl       rcx, 1          //rcx = 1 if number was negative
  mov       [r11+rax*2-2],dx//put the '-', just in case
  sbb       rax,0           //correct the start pos, if we have a '-' in front
  lea       rax, [r11+rax*2]//return the start of the string
  sub       r10,rax         //r10 = length
  mov       [rax-4],r10d    //write the length before the string
  ret
end;

function _IntToStrInt64(const Value: Int64; var Dump: TCharStorage): PChar; overload;
asm
  .noframe
  //push      0               //Save positive sign
@@IntToStrNoSign:
  lea       r11, [rdx+26*2]
  push      r11             //save the end of the data for length calculations later
  lea       r10, [rip+TwoDigitLookup]
  cmp       rcx, 100        //process two digits at a time
  jb        @tail           //only two digits, goto tail.
  mov       r8, $47ae147ae147ae15  //division using multiplication by reciprocal
  db $0F, $1F, $00
@loop:
  mov       rax, rcx
  mov       r9d, ecx
  mul       r8
  sub       rcx, rdx
  shr       rcx, 1
  add       rdx, rcx
  mov       rcx, rdx
  shr       rcx, 6
  imul      rax, rcx, -100  //i = remainder mod 100
  add       r9d,  eax
  mov       eax, [r10+r9*4] //digits = lookup[i]
  mov       [r11], eax      //add to digits to the string
  sub       r11, 4
  cmp       rcx, 100        //repeat ...
  jae       @loop           //... until remainder < 100
@tail:
  mov       eax,[r10+rcx*4] //lookup 2 digits
  pop       r10
  //mov       edx,$2d         //sign = '-'
  mov       [r11], eax      //write 2 digits
  xor       eax, eax
  cmp       ecx, 10         //do we have a leading zero?
  //pop       rcx
  setb      al              //yes, add one
  add       r10, 4          //correct starting pos
  //shl       rcx, 1          //rcx = 1 if number was negative
  //mov       [r11+rax*2-2],dx//put the '-', just in case
  //sbb       rax,0           //correct the start pos, if we have a '-' in front
  lea       rax, [r11+rax*2]//return the start of the string
  sub       r10,rax         //r10 = length
  mov       [rax-4],r10d    //write the length before the string
  ret
end;
{$endif}


{$pointermath on}

procedure TStringBuilder.ExpandCapacity(const AdditionalSize: Integer);
begin
  FCapacity := FLength + AdditionalSize + MALLOC_SIZE;
  System.SetLength(FData, FCapacity);
end;

procedure TStringBuilder.SetLength(Value: NativeUInt);
begin
  if Value > Capacity then ExpandCapacity(Value - Capacity);
  FLength:= Value;
end;

function TStringBuilder.Append(const Value: string): TStringBuilder;
var
  L: NativeUInt;
  P: PInteger;
begin
  Result:= Self;
  //L:= System.Length(Value);
  P:= PInteger(Value);
  if P = nil then exit;
  Dec(P);
  L:= P^;
  if (Length + L) > Capacity then ExpandCapacity(L); //ExpandCapacity(Length + L);
  Move(pointer(Value)^, FData[FLength], L * SizeOf(Char));
  FLength := FLength + L;
end;

function TStringBuilder.Append(const Value: UInt64): TStringBuilder;
{$ifdef CPUx64}
var
  P: PChar;
  L: integer;
  Storage: TCharStorage;
begin
  P:= _IntToStrInt64(Value, Storage);
  L:= PInteger(P)[-1];
  Result:= _Append(P, L);
end;
{$else}
begin
  Result:= Append(IntToStr(Value));
end;
{$endif}

function TStringBuilder.Append(const Value: TCharArray): TStringBuilder;
begin
  Result:= _Append(Value, 0, System.Length(Value));
end;

function TStringBuilder.Append(const Value: Single): TStringBuilder;
begin
  Result:= Append(FloatToStr(Value));
end;

function TStringBuilder.Append(const Value: Word): TStringBuilder;
{$ifdef cpux64}
var
  P: PChar;
  L: integer;
  Storage: TCharStorage;
begin
  P:= _IntToStr(Value, Storage);
  L:= PInteger(P)[-1];
  Result:= _Append(P, L);
end;
{$else}
begin
  Result:= Append(IntToStr(Value));
end;
{$endif}

function TStringBuilder._Append(const Value: TCharArray; StartIndex,
  CharCount: NativeUInt): TStringBuilder;
begin
  Length := Length + CharCount;
  Move(Value[StartIndex], FData[Length - CharCount], CharCount * SizeOf(Char));
  Result := self;
end;

function TStringBuilder.Append(const Value: TCharArray; StartIndex,
  CharCount: NativeUInt): TStringBuilder;
begin
  if StartIndex + CharCount > System.Length(Value) then
    raise ERangeError.CreateResFmt(@SListIndexError, [StartIndex]);
  Result:= _Append(Value, StartIndex, CharCount);
end;

function TStringBuilder.Append(const Value: string; StartIndex,
  Count: NativeUInt): TStringBuilder;
var
  i: integer;
  L: integer;
begin
  if StartIndex + Count > System.Length(Value) then
    raise ERangeError.CreateResFmt(@SListIndexError, [StartIndex]);
  L:= System.Length(Value) - StartIndex + Low(string);
  Length := Length + (L * Count);
  for i:= Count -1 downto 0 do begin
    Move(Value[StartIndex + Low(string)], FData[Length - i * L], Count * SizeOf(Char));
  end;
  Result := Self;
end;

function TStringBuilder._Append(const Value: PChar; Length: cardinal): TStringBuilder;
var
  L: NativeUint;
begin
  L:= FLength;
  Self.Length:= L + Length;
  Move(Value^, FData[L], Length * SizeOf(Char));
  Result:= self;
end;

{$IFNDEF NEXTGEN}
function TStringBuilder.Append(const Value: PAnsiChar): TStringBuilder;
begin
  Result:= Append(string(Value));
end;

function TStringBuilder.Append(const Value: RawByteString): TStringBuilder;
begin
  Result:= Append(string(Value));
end;
{$ENDIF !NEXTGEN}

function TStringBuilder.Append(const Value: Cardinal): TStringBuilder;
{$ifdef cpux64}
var
  P: PChar;
  L: integer;
  Storage: TCharStorage;
begin
  P:= _IntToStr(Value, Storage);
  L:= PInteger(P)[-1];
  Result:= _Append(P, L);
end;
{$else}
begin
  Result:= Append(IntToStr(Value));
end;
{$endif}

function TStringBuilder.Append(const Value: Char; RepeatCount: NativeUInt): TStringBuilder;
{$ifdef CPUX64}
asm
  //RCX = self
  //DX = Value
  //r8 = Repeatcount
  and edx,$FFFF
  mov r9,[RCX+TStringBuilder.FLength]
  mov rax,$0001000100010001   //stamp with 4 copies
  imul rdx,rax
  push r9
  push r8
  push rdx
  push rcx
  lea rdx,[r9+r8]    //L = FLength + RepeatCount
  call TStringBuilder.SetLength
  pop rcx
  pop rdx
  pop r8
  lea rax,[rcx+TStringBuilder.FData]
  mov rax,[rax]  //rax = FData[0]
  pop r9
  lea r9,[rax+r9*type(char)]  //rax = FData[Length]
  mov eax,3
  and eax,r8d
  shr r8,2    //divide by four
  jz @tail
@loop:
    sub r8,1
    mov [r9],rdx
    lea r9,[r9+8]
    jnz @loop
@tail:
  cmp eax,1
  js @done
  je @one
@two_three:
  mov [r9],edx
  lea r9,[r9+4]
  cmp eax,2
  jz @done
@one:
  mov [r9],dx
@done:
  mov rax,rcx
  ret
end;
{$ELSE}
var
  S: string;
begin
  S:= StringOfChar(Value, RepeatCount);
  Result:= Append(S);
end;
{$ENDIF}

function TStringBuilder.Append(const Value: Shortint): TStringBuilder;
{$ifdef cpux64}
var
  P: PChar;
  L: integer;
  Storage: TCharStorage;
begin
  P:= _IntToStr(Value, Storage);
  L:= PInteger(P)[-1];
  Result:= _Append(P, L);
end;
{$else}
begin
  Result:= Append(IntToStr(Value));
end;
{$endif}

function TStringBuilder.Append(const Value: Char): TStringBuilder;
{$ifdef cpux64}
asm
  //rcx = self
  //dx = value
  .noframe
  mov rax,[rcx+TStringBuilder.FLength]
  mov r8,[rcx+TStringBuilder.FCapacity]
  mov r9,[rcx+TStringBuilder.FData]
  cmp rax,r8
  jne @AfterExpand
  push rax
  push rdx
  push rcx
  mov edx,1
  call TStringBuilder.ExpandCapacity
  pop rcx
  pop rdx
  pop rax
  mov r9,[rcx+TStringBuilder.FData] //may have moved
@AfterExpand:
  lea r8,[rax+1]
  mov [r9+rax*2],dx
  mov [rcx+TStringBuilder.FLength],r8
end;
{$endif}
{$ifndef cpux64}
//{$ifdef CPUx86}
//asm
//  //eax = self
//  //dx = value
//  push edi
//  push esi
//  mov ecx,[eax+TStringBuilder.FLength]
//  mov edi,[eax+TStringBuilder.FCapacity]
//  mov esi,[eax+TStringBuilder.FData]
//  cmp ecx,edi
//  jne @AfterExpand
//  push edx
//  push eax
//  mov edi,ecx
//  mov edx,1
//  call TStringBuilder.ExpandCapacity;
//  pop eax
//  pop edx
//  mov ecx,edi
//  mov esi,[eax+TStringBuilder.FData] //may have moved
//@AfterExpand:
//  mov [esi+ecx*2],dx
//  inc ecx
//  mov [eax+TStringBuilder.FLength],ecx
//  pop esi
//  pop edi
//end;
//{$else}
var
  LLength: NativeUInt;
begin
  LLength := FLength;
  if (LLength >= FCapacity) then ExpandCapacity(1);
  FData[LLength] := Value;
  FLength:= LLength + 1;
  Result := Self;
end;
//{$endif}
{$endif}

function TStringBuilder.Append(const Value: Currency): TStringBuilder;
begin
  Result:= Append(CurrToStr(Value));
end;

function TStringBuilder.Append(const Value: Boolean): TStringBuilder;
begin
  case Value of
    true: Result:= Append('True');
    else Result:= Append('False');
  end;
end;

function TStringBuilder.Append(const Value: Byte): TStringBuilder;
{$ifdef cpux64}
var
  P: PChar;
  L: integer;
  Storage: TCharStorage;
begin
  P:= _IntToStr(Value, Storage);
  L:= PInteger(P)[-1];
  Result:= _Append(P, L);
end;
{$else}
begin
  Result:= Append(IntToStr(Value));
end;
{$endif}

function TStringBuilder.Append(const Value: Double): TStringBuilder;
begin
  Result:= Append(FloatToStr(Value));
end;

function TStringBuilder.Append(const Value: Int64): TStringBuilder;
{$ifdef cpux64}
var
  P: PChar;
  L: integer;
  Storage: TCharStorage;
begin
  P:= _IntToStr(Value, Storage);
  L:= PInteger(P)[-1];
  Result:= _Append(P, L);
end;
{$else}
begin
  Result:= Append(IntToStr(Value));
end;
{$endif}

function TStringBuilder.Append(const Value: TObject): TStringBuilder;
begin
{$if CompilerVersion >= 19}
  Result:= Append(Value.ToString());
{$else}
  Result:= Append(IntToStr(Integer(Value)));
{$ENDIF}
end;

function TStringBuilder.Append(const Value: Smallint): TStringBuilder;
{$ifdef cpux64}
var
  P: PChar;
  L: integer;
  Storage: TCharStorage;
begin
  P:= _IntToStr(Value, Storage);
  L:= PInteger(P)[-1];
  Result:= _Append(P, L);
end;
{$else}
begin
  Result:= Append(IntToStr(Value));
end;
{$endif}

function TStringBuilder.Append(const Value: Integer): TStringBuilder;
{$ifdef cpux64}
var
  Storage: TCharStorage;
  P: PChar;
  L: integer;
begin
  P:= _IntToStr(Value, Storage);
  L:= PInteger(P)[-1];
  Result:= _Append(P, L);
end;
{$else}
begin
  Result:= Append(IntToStr(Value));
end;
{$endif}

function TStringBuilder.AppendFormat(const Format: string; const Args: array of const): TStringBuilder;
begin
  Result:= Append(System.SysUtils.Format(Format, Args));
end;

function TStringBuilder.AppendLine: TStringBuilder;
const
  LineBreak = $0D000A;
var
  L: NativeUInt;
begin
  L:= FLength;
  Length:= L + 2;
  PInteger(@FData[L])^:= LineBreak;
  Result:= Self;
end;

function TStringBuilder.AppendLine(const Value: string): TStringBuilder;
begin
  Append(Value);
  Result:= AppendLine;
end;

procedure TStringBuilder.Clear ;
begin
  Length := 0;
  FCapacity:= 0;
  ExpandCapacity(0);
  //Capacity :=  MALLOC_SIZE; //DefaultCapacity;
end;

procedure TStringBuilder.CheckBounds(Index: NativeUInt);
begin
  if Index >= Length then
    raise ERangeError.CreateResFmt(@SListIndexError, [Index]);
end;

procedure TStringBuilder.CopyTo(SourceIndex: NativeUInt;
  const Destination: TCharArray; DestinationIndex, Count: NativeUInt);
begin
  if DestinationIndex + Count > System.Length(Destination) then
    raise ERangeError.CreateResFmt(@SInputBufferExceed,
      ['DestinationIndex', DestinationIndex, 'Count', Count]);

  if Count > 0 then
  begin
    CheckBounds(SourceIndex);
    CheckBounds(SourceIndex + Count - 1);

    Move(FData[SourceIndex], Destination[DestinationIndex], Count * SizeOf(Char));
  end;
end;

constructor TStringBuilder.Create;
begin
  inherited Create;
  FMaxCapacity := MaxInt;
  FCapacity:= 0;
  ExpandCapacity(0);
  //Capacity := MALLOC_SIZE; // DefaultCapacity;
  //FInsertPoint:= @FData[0];
  //FLength := 0;  //fields are zero initialized
end;

constructor TStringBuilder.Create(const Value: string; aCapacity: NativeUInt);
begin
  inherited Create;
  FMaxCapacity := MaxInt;
  FCapacity:= 0;
  ExpandCapacity(aCapacity);
  //Capacity := aCapacity;
  //FLength := 0;
  Append(Value);
end;

constructor TStringBuilder.Create(const Value: string; StartIndex, Length,
  aCapacity: NativeUInt);
begin
  //Create(Copy(Value, StartIndex + 1, length), aCapacity);
  Create(Value.Substring( StartIndex, length), aCapacity);
end;

constructor TStringBuilder.Create(aCapacity, aMaxCapacity: NativeUInt);
begin
  Create(aCapacity);
  FMaxCapacity := aMaxCapacity;
end;

constructor TStringBuilder.Create(aCapacity: NativeUInt);
begin
  inherited Create;
  FMaxCapacity := MaxInt;
  FCapacity:= 0;
  ExpandCapacity(ACapacity);
  //Capacity := aCapacity;
  FLength := 0;
end;

constructor TStringBuilder.Create(const Value: string);
begin
  Create;
  Append(Value);
end;

function TStringBuilder.EnsureCapacity(aCapacity: NativeUInt): NativeUInt;
begin
  if aCapacity > MaxCapacity then
    raise ERangeError.CreateResFmt(@SListIndexError, [aCapacity]);

  if FCapacity < aCapacity then
    Capacity := aCapacity;

  Result := Capacity;
end;

function TStringBuilder.Equals(StringBuilder: TStringBuilder): Boolean;
begin
  Result := (StringBuilder <> nil) and (Length = StringBuilder.Length) and
    (MaxCapacity = StringBuilder.MaxCapacity) and
    CompareMem(@FData[0], @StringBuilder.FData[0], Length * SizeOf(Char));
end;

//function Max(a,b: NativeUInt): NativeUInt; overload; inline;
//begin
//  //jumpless max.
//  Result:= a xor ((a xor b) and -integer(a < b));
//  //Result = a.
//  //if (a >= b) then x:= 0; Result:= a xor 0;
//  //if (a < b) then x:= -1; Result:= a xor (a xor b) and -1
//  //                                                 ^^^^^^ nop
//  //                                 ^^^^^^^^^^^^^^^swap a and b
//  //                        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ Result:= b;
//end;
//
//function Min(a,b: NativeUInt): NativeUInt; overload; inline;
//begin
//  Result:= a xor ((a xor b) and -integer(a > b));
//end;

//procedure TStringBuilder.ExpandCapacity(NewLength: NativeUInt);
//var
//  NewCapacity: NativeUint;  //do not worry about overflow.
//begin
//  NewCapacity := Max(Capacity * 2, (NewLength * 3 div 2));
//  Capacity:= Min(NewCapacity, MaxCapacity);
//end;


function TStringBuilder.GetChars(Index: NativeUInt): Char;
begin
  CheckBounds(Index);

  Result := FData[Index];
end;

function TStringBuilder.Insert(Index: NativeUInt; const Value: TObject): TStringBuilder;
begin
{$if CompilerVersion >= 19}
  Result:= Insert(Index, Value.ToString());
{$else}
  Result:= Insert(Index, IntToStr(Integer(Value)));
{$ENDIF}
end;

function TStringBuilder.Insert(Index: NativeUInt; const Value: Int64): TStringBuilder;
begin
  Result:= Insert(Index, IntToStr(Value));
end;

function TStringBuilder.Insert(Index: NativeUInt; const Value: Single): TStringBuilder;
begin
  Result:= Insert(Index, FloatToStr(Value));
end;

function TStringBuilder.Insert(Index: NativeUInt; const Value: string): TStringBuilder;
var
  L: integer;
begin
  if Index > Length then
    raise ERangeError.CreateResFmt(@SListIndexError, [Index]);
  L:= System.Length(Value);
  Length := Length + L;
  Move(pointer(FData[Index])^, FData[Index + L], (Length - L - Index) * SizeOf(Char));
  Move(pointer(Value)^, FData[Index], L * SizeOf(Char));
  Result := Self;
end;

function TStringBuilder.Insert(Index: NativeUInt; const Value: Word): TStringBuilder;
begin
  Result:= Insert(Index, IntToStr(Value));
end;

function TStringBuilder.Insert(Index: NativeUInt; const Value: Shortint): TStringBuilder;
begin
  Result:= Insert(Index, IntToStr(Value));
end;

function TStringBuilder.Insert(Index: NativeUInt; const Value: TCharArray): TStringBuilder;
var
  L: NativeUint;
begin
  if Index > Length then
    raise ERangeError.CreateResFmt(@SListIndexError, [Index]);
  L:= System.Length(Value);
  Length := Length + L;
  Move(FData[Index], FData[Index + L], L * SizeOf(Char));
  Move(pointer(Value)^, FData[Index], L * SizeOf(Char));
  Result := Self;
end;

function TStringBuilder.Insert(Index: NativeUInt; const Value: Currency): TStringBuilder;
begin
  Result:= Insert(Index, CurrToStr(Value));
end;

function TStringBuilder.Insert(Index: NativeUInt; const Value: Char): TStringBuilder;
begin
  if Index > Length then
    raise ERangeError.CreateResFmt(@SListIndexError, [Index]);

  Length := Length + 1;
  Move(FData[Index], FData[Index + 1], (Length - Index - 1) * SizeOf(Char));
  FData[Index] := Value;
  Result := Self;
end;

function TStringBuilder.Insert(Index: NativeUInt; const Value: Byte): TStringBuilder;
begin
  Result:= Insert(Index, IntToStr(Value));
end;

function TStringBuilder.Insert(Index: NativeUInt; const Value: Double): TStringBuilder;
begin
  Result:= Insert(Index, FloatToStr(Value));
end;

function TStringBuilder.Insert(Index: NativeUInt; const Value: Integer): TStringBuilder;
begin
  Result:= Insert(Index, IntToStr(Value));
end;

function TStringBuilder.Insert(Index: NativeUInt; const Value: Smallint): TStringBuilder;
begin
  Result:= Insert(Index, IntToStr(Value));
end;

function TStringBuilder.Insert(Index: NativeUInt; const Value: Boolean): TStringBuilder;
begin
  Result:= Insert(Index, BoolToStr(Value, True));
end;

function TStringBuilder.Insert(Index: NativeUInt; const Value: string;
  Count: NativeUInt): TStringBuilder;
var
  I: Integer;
  S: string;
  L: integer;
  P: PChar;
begin
  L:= System.Length(Value);
  System.SetLength(S, L);
  P:= PChar(S);
  for I := 0 to Count - 1 do begin
    Move(pointer(Value)^, P^, L * SizeOf(Char));
    Inc(P,L);
  end;
  Result:= Insert(Index, S);
end;

function TStringBuilder.Insert(Index: NativeUInt; const Value: TCharArray; StartIndex,
  CharCount: NativeUInt): TStringBuilder;
begin
  if Index - 1 >= Length then
    raise ERangeError.CreateResFmt(@SListIndexError, [Index]);
  if StartIndex + CharCount > System.Length(Value) then
    raise ERangeError.CreateResFmt(@SInputBufferExceed,
      ['StartIndex', StartIndex, 'CharCount', CharCount]);

  Length := Length + CharCount;

  if Length - Index > 0 then
    Move(FData[Index], FData[Index + CharCount], (Length - Index) * SizeOf(Char));
  Move(Value[StartIndex], FData[Index], CharCount * SizeOf(Char));
  Result := Self;
end;

function TStringBuilder.Insert(Index: NativeUInt; const Value: Cardinal): TStringBuilder;
begin
  Result:= Insert(Index, IntToStr(Value));
end;

function TStringBuilder.Insert(Index: NativeUInt; const Value: UInt64): TStringBuilder;
begin
  Result:= Insert(Index, UIntToStr(Value));
end;

procedure TStringBuilder.ReduceCapacity;
begin
  if Length >= (Capacity div 4) then ExpandCapacity(0);
end;

function TStringBuilder.Remove(StartIndex, RemLength: Integer): TStringBuilder;
begin
  if RemLength <> 0 then
  begin
    CheckBounds(StartIndex);
    CheckBounds(StartIndex + RemLength - 1);

    if (Length - (StartIndex + RemLength)) > 0 then
      Move(FData[StartIndex + RemLength], FData[StartIndex], (Length - (StartIndex + RemLength)) * SizeOf(Char));
    Length := Length - RemLength;

    ReduceCapacity;
  end;
  Result := Self;
end;

function TStringBuilder.Replace(const OldValue, NewValue: string; StartIndex,
  Count: NativeUInt): TStringBuilder;
var
  CurPtr: PChar;
  EndPtr: PChar;
  Index: Integer;
  EndIndex: Integer;
  OldLen, NewLen: Integer;
begin
  Result := Self;

  if Count <> 0 then
  begin
    if StartIndex + Count > Length then
      raise ERangeError.CreateResFmt(@SInputBufferExceed,
        ['StartIndex', StartIndex, 'Count', Count]);

    OldLen := System.Length(OldValue);
    NewLen := System.Length(NewValue);
    Index := StartIndex;
    CurPtr := @FData[StartIndex];
    EndIndex := StartIndex + Count - OldLen;
    EndPtr := @FData[EndIndex];

    while CurPtr <= EndPtr do
    begin
      if CurPtr^ = OldValue[Low(string)] then
      begin
        if StrLComp(CurPtr, PChar(OldValue), OldLen) = 0 then
        begin
          if _Replace(Index, OldValue, NewValue) then
          begin
            CurPtr := @FData[Index];
            EndPtr := @FData[EndIndex];
          end;
          Inc(CurPtr, NewLen - 1);
          Inc(Index, NewLen - 1);
          Inc(EndPtr, NewLen - OldLen);
          Inc(EndIndex, NewLen - OldLen);
        end;
      end;

      Inc(CurPtr);
      Inc(Index);
    end;
  end;
end;

function TStringBuilder.Replace(const OldChar, NewChar: Char; StartIndex,
  Count: NativeUInt): TStringBuilder;
var
  Ptr: PChar;
  EndPtr: PChar;
begin
  if Count <> 0 then
  begin
    CheckBounds(StartIndex);
    CheckBounds(StartIndex + Count - 1);

    EndPtr := @FData[StartIndex + Count - 1];
    Ptr := @FData[StartIndex];
    while Ptr <= EndPtr do
    begin
      if Ptr^ = OldChar then
        Ptr^ := NewChar;
      Inc(Ptr);
    end;
  end;
  Result := Self;
end;

function TStringBuilder.Replace(const OldChar, NewChar: Char): TStringBuilder;
var
  Ptr: PChar;
  EndPtr: PChar;
begin
  EndPtr := @FData[Length - 1];
  Ptr := @FData[0];
  while Ptr <= EndPtr do
  begin
    if Ptr^ = OldChar then
      Ptr^ := NewChar;
    Inc(Ptr);
  end;
  Result := Self;
end;

function TStringBuilder.Replace(const OldValue, NewValue: string): TStringBuilder;
begin
  Result := self;
  Replace(OldValue, NewValue, 0, Length);
end;

procedure TStringBuilder.SetCapacity(Value: NativeUInt);
begin
  if Value < Length then
    raise ERangeError.CreateResFmt(@SListCapacityError, [Value]);
  if Value > FMaxCapacity then
    raise ERangeError.CreateResFmt(@SListCapacityError, [Value]);

  System.SetLength(FData, Value);
  FCapacity:= Value;
end;

procedure TStringBuilder.SetChars(Index: NativeUInt; Value: Char);
begin
  CheckBounds(Index);

  FData[Index] := Value;
end;

function TStringBuilder.ToString: string;
begin
  SetString(Result, MarshaledString(FData), Length);
end;

function TStringBuilder.ToString(StartIndex: NativeUInt; StrLength: Cardinal): string;
begin
  if StrLength <> 0 then
  begin
    CheckBounds(StartIndex);
    CheckBounds(StartIndex + StrLength - 1);

    Result := string.Create(FData, StartIndex, StrLength);
  end else
    Result := '';
end;

function TStringBuilder._Replace(Index: NativeUInt; const Old, New: string): Boolean;
var
  OldLength: Integer;
  OldCapacity: Integer;
  SizeChange: Integer;
begin
  Result := False;
  SizeChange := System.Length(New) - System.Length(Old);

  if SizeChange = 0 then
  begin
    Move(New[Low(string)], FData[Index], System.Length(New) * SizeOf(Char));
  end
  else
  begin
    OldLength := Length;
    if SizeChange > 0 then
    begin
      OldCapacity := Capacity;
      Length := Length + SizeChange;
      if OldCapacity <> Capacity then
        Result := True;
    end;

    Move(FData[Index + System.Length(Old)], FData[Index + System.Length(New)],
      (OldLength - (System.Length(Old) + Index)) * SizeOf(Char));
    Move(New[Low(String)], FData[Index], System.Length(New) * SizeOf(Char));

    if SizeChange < 0 then
      Length := Length + SizeChange;
  end;
end;

//var
//  cs: TCharStorage;
//  p: PChar;
//  S: string;
//  a: int64;
//
//LTick: int64;
//i,j: integer;
//
//const
//  TestCount:int64 = 1000 * 1000 * 100;
//
//
//initialization
//
//for j:= TestCount to TestCount + 10 do begin
//  LTick:= TThread.GetTickCount;
//  for I:= 1 to TestCount do begin
//    _IntToStrInt64(-j, cs);      //takes longer because it produces a much longer string.
//  end;
//  Writeln('IntToStr64: ', TThread.GetTickCount - LTick, 'ms');
//
//  LTick:= TThread.GetTickCount;
//  for I:= 1 to TestCount do begin
//    _IntToStr(j, cs);
//  end;
//  Writeln('IntToStr fast: ', TThread.GetTickCount - LTick, 'ms');
//
//  LTick:= TThread.GetTickCount;
//  for I:= 1 to TestCount do begin
//    IntToStr(uint64(-j));
//  end;
//  Writeln('IntToStr system: ', TThread.GetTickCount - LTick, 'ms');
//end;

end.
