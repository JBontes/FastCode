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
    //procedure ReduceCapacity;
    procedure CheckBounds(Index: NativeUint); inline;
    function _Replace(Index: NativeUint; const Old, New: string): Boolean;
    //prevent ref count and exception frame for local strings.
    function _Append(const Value: PChar; Length: cardinal): TStringBuilder; overload;
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
    function Append(const Value: TCharArray): TStringBuilder; overload;
    function Append(const Value: Word): TStringBuilder; overload; {$ifndef cpux64} inline; {$endif}
    function Append(const Value: Cardinal): TStringBuilder; overload; {$ifndef cpux64} inline; {$endif}
{$IFNDEF NEXTGEN}
    function Append(const Value: PAnsiChar): TStringBuilder; overload; inline;
    function Append(const Value: RawByteString): TStringBuilder; overload; inline;
{$ENDIF !NEXTGEN}
    function Append(const Value: Char; RepeatCount: NativeUInt): TStringBuilder; overload;
    function Append(const Value: TCharArray; StartIndex, CharCount: NativeUInt): TStringBuilder; overload;
    function Append(const Value: string; StartIndex, Count: NativeUInt): TStringBuilder; overload;

    function AppendFormat(const Format: string; const Args: array of const): TStringBuilder; overload;
    function AppendLine: TStringBuilder; overload;
    function AppendLine(const Value: string): TStringBuilder; overload; inline;
    procedure Clear; inline;
    procedure CopyTo(SourceIndex: NativeUInt; const Destination: TCharArray; DestinationIndex, Count: NativeUInt);
    //function EnsureCapacity(aCapacity: NativeUInt): NativeUInt;
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

{ TStringBuilder }
type
  TCharStorage = array [1..22] of char;

{$ifdef cpux64}
function _IntToStr(const Value: Int64; var Storage: TCharStorage): PChar;
  //rcx = value
  //rdx - temp for multiply
  //rax - temp for multiply
  //r8 - temp for multiply
asm
    .noframe
    mov   r8,rcx               //r8 = abs(value)
    mov   word ptr [rdx+44-2],0 //write trailing zero.
    push  rdi                  //rdi = upper 16 decimals
    push  rsi                  //rsi = lower 4 decimals
    push  rcx                  //save sign(value) for later
    neg   rcx                  //abs(value)
    push  rdx                  //save storage for later.
    cmovns r8,rcx              //r8 = abs value
    mov   edi, 0               //high digits = 0
    mov   esi, 0               //low digits = 0
    mov   ecx, $F8             //counter cl = -8, counter ch = 0.
    jz @WriteZero              //if value = 0 then WriteData.
    mov   r9, $47ae147ae147ae15
@SmallLoop:                    //Process the 16 least significant digits
    shl   rsi, 8               //clear next byte in rsi
    mov   rax, r8              //rax = value mod 10 000
    mov   r10, r8              //r10 = value mod 10 000
    mul   r9                   //divide by 100 using multiplication by reciprocal
    sub   r10, rdx             //... handle div by reciprocal
    shr   r10, 1               //...
    add   rdx, r10             //...
    shr   rdx, 6               //...
    imul  r11, rdx, -100       //...
    add   r8, r11              //r8 = value mod 100
    movzx r8d, r8b             //r8 = byte(r8)
    or    rsi, r8              //rsi[0] = value mod 100
    mov   r8, rdx              //a = a div 100
    cmp   rdx, 1               //CF = 1 if rdx = 0
    inc   cl                   //ZF = 1 if ecx = 0
    ja  @SmallLoop            //until done
    test  r8, r8
    je   @WriteData
    mov   r9, $47ae147ae147ae15
    mov   ch,2                 //always process 4 digits if big number
@bigloop:                      //process the 8 most significant bits
    shl   rdi, 8               //clear next byte in rdi
    mov   rax, r8              //
    mov   r10, r8
    mul   r9                   //rax:rdx = value div 100
    sub   r10, rdx             //... handle multiplication by reciprocal
    shr   r10, 1               //... handle multiplication by reciprocal
    add   rdx, r10             //...
    shr   rdx, 6               //...
    imul  r11, rdx, -100       //... Mod by reciprocal
    add   r8, r11              //r8 = value mod 100
    movzx r8d, r8b             //r8 = byte(r8)
    or    rdi, r8              //rdi[0] = byte(value mod 100)
    mov   r8, rdx
    dec   ch                   //always process 4 digits.
    jnz    @BigLoop            //... until done with big numbers
@WriteData:
    pop   r11                  //restore data
    mov   r9,r11               //save r11 (to deduce the length later)
    add   cl,8                 //cl = number of low digit-pairs
    movzx eax,cl
    shl   cl,3                 //cl = cl * 8
    mov   r8,rsi               //save MSB for later to test for leading zero.
    ror   rsi,cl               //align rsi if there are fewer than 8 digit pairs
    lea   r10,[rip+TwoDigitLookup]  //get the lookup table
    lea   r11,[r11+44-6+4]       //go to end of buffer
@SmallWriteLoop:
    lea   r11,[r11-4]          //update r11
    rol   rsi,8                //sil = LSB
    movzx edx,sil              //edx = 2 digits to process
    mov   edx,[r10+rdx*4]      //lookup 2 digits
    dec   eax
    mov   [r11],edx            //write the digit to string
    jnz @SmallWriteLoop
    test  edi,edi              //are there high digits to write?
    jz @Done
    mov   rdx,rdi
    lea   r11,[r11-4]
@BigWriteLoop:
    movzx edi,dh               //Get digit-pair 8
    movzx esi,dl               //Get digit-pair 9
    mov edx,[r10+rdi*4]        //lookup 2 digits
    mov r8,rsi                 //digit-pair 9 feeds leading zero indicator.
    mov [r11],edx              //write digit to string
    mov edx,[r10+rsi*4]
    xor r10,r10
    cmp esi,1                  //CF=1 if digit-pair 9 = 0.
    cmovc r8,rdi               //r8 = leading zero indicator
    adc r10,0                  //r10 = 1 if digit-pair 9 = 0
    mov [r11-4],edx            //write data regardless
    lea r11,[r11+r10*4-4]      //only update r11 if there was something to write
@Done:
    xor   eax,eax
    cmp   r8b,10               //do we have a leading zero?
    adc   eax,0                //rax =1 if there is.
    lea   r11,[r11+rax*2]
    pop   r8                   //Get the sign
    xor   eax,eax
    mov   edx,$002D            //edx = #00+'-'
    lea   rcx,[r11-2]          //return address if negative
    mov   [rcx],dx             //write the sign (will fall out of scope if not relevant).
    test  r8,r8                //
    mov   rax,r11              //return start of number
    cmovs rax,rcx              //or start of '-' if sign
    sub   r9,rax
    add   r9,44-2
    shr   r9,1
    pop   rsi                  //clean up.
    pop   rdi
    mov   [rax-4],r9d          //write the length
    ret
@WriteZero:
    mov   rcx,$003000000001FFFF//----> ref count = negative
    ///////////||||++++++++
    ///        ||||-------->length = 1
    ///        ====> data = '0'
    mov   [rdx+44-10],rcx
    lea   rax,[rdx+44-2-2]
    add   rsp,16
    pop   rsi
    pop   rdi
end;
{$endif}

{$pointermath on}

procedure TStringBuilder.SetLength(Value: NativeUInt);
begin
  if Value > Capacity then ExpandCapacity(Value - Capacity); //ExpandCapacity(Value);
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

function TStringBuilder.Append(const Value: TCharArray): TStringBuilder;
var
  I: Integer;
begin
  Result := self;
  for I := 0 to System.Length(Value) - 1 do
    if Value[I] = #0 then Break;

  Append(Value, 0, I);
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

function TStringBuilder.Append(const Value: TCharArray; StartIndex,
  CharCount: NativeUInt): TStringBuilder;
begin
  if StartIndex + CharCount > System.Length(Value) then
    raise ERangeError.CreateResFmt(@SListIndexError, [StartIndex]);


  Length := Length + CharCount;
  Move(Value[StartIndex], FData[Length - CharCount], CharCount * SizeOf(Char));
  Result := self;
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
    false: Result:= Append('False');
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

function TStringBuilder.AppendFormat(const Format: string; const Args: array of const): TStringBuilder;
begin
  Result:= Append(System.SysUtils.Format(Format, Args));
end;

function TStringBuilder.AppendLine: TStringBuilder;
const
  LineBreak = $0D000A00;
var
  L: NativeUInt;
begin
  L:= FLength;
  Length:= Length + 2;
  PInteger(@FData[L])^:= LineBreak;
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

//function TStringBuilder.EnsureCapacity(aCapacity: NativeUInt): NativeUInt;
//begin
//  if aCapacity > MaxCapacity then
//    raise ERangeError.CreateResFmt(@SListIndexError, [aCapacity]);
//
//  if Capacity < aCapacity then
//    Capacity := aCapacity;
//
//  Result := Capacity;
//end;

function TStringBuilder.Equals(StringBuilder: TStringBuilder): Boolean;
begin
  Result := (StringBuilder <> nil) and (Length = StringBuilder.Length) and
    (MaxCapacity = StringBuilder.MaxCapacity) and
    CompareMem(@FData[0], @StringBuilder.FData[0], Length * SizeOf(Char));
end;

function Max(a,b: NativeUInt): NativeUInt; overload; inline;
begin
  //jumpless max.
  Result:= a xor ((a xor b) and -integer(a < b));
  //Result = a.
  //if (a >= b) then x:= 0; Result:= a xor 0;
  //if (a < b) then x:= -1; Result:= a xor (a xor b) and -1
  //                                                 ^^^^^^ nop
  //                                 ^^^^^^^^^^^^^^^swap a and b
  //                        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ Result:= b;
end;

function Min(a,b: NativeUInt): NativeUInt; overload; inline;
begin
  Result:= a xor ((a xor b) and -integer(a > b));
end;

//procedure TStringBuilder.ExpandCapacity(NewLength: NativeUInt);
//var
//  NewCapacity: NativeUint;  //do not worry about overflow.
//begin
//  //NewCapacity := Max(Capacity * 2, (NewLength * 3 div 2));
//  //Capacity:= Min(NewCapacity, MaxCapacity);
//
//end;


function TStringBuilder.GetChars(Index: NativeUInt): Char;
begin
  CheckBounds(Index);

  Result := FData[Index];
end;

//function TStringBuilder.GetMaxCapacity: Integer;
//begin
//  Result := FMaxCapacity;
//end;

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

//procedure TStringBuilder.ReduceCapacity;
//var
//  NewCapacity: Integer;
//begin
//  if Length > Capacity div 4 then
//    Exit;
//
//  NewCapacity := Capacity div 2;
//  if NewCapacity < Length then
//    NewCapacity := Length;
//  Capacity := NewCapacity;
//end;

function TStringBuilder.Remove(StartIndex, RemLength: Integer): TStringBuilder;
begin
  if RemLength <> 0 then
  begin
    CheckBounds(StartIndex);
    CheckBounds(StartIndex + RemLength - 1);

    if (Length - (StartIndex + RemLength)) > 0 then
      Move(FData[StartIndex + RemLength], FData[StartIndex], (Length - (StartIndex + RemLength)) * SizeOf(Char));
    Length := Length - RemLength;

    //ReduceCapacity;
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

procedure TStringBuilder.ExpandCapacity(const AdditionalSize: Integer);
begin
  FCapacity := FLength + AdditionalSize + MALLOC_SIZE;
  System.SetLength(FData, FCapacity);
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
//  TestCount = 1000 * 1000 * 100;
//
//
//initialization
//  for j:= TestCount to TestCount+10 do begin
//  LTick := TThread.GetTickCount;
//  for i := 1 to TestCount do
//  begin
//    _IntToStr(j, cs);
//  end;
//  Writeln('IntToStrFast: ', TThread.GetTickCount - LTick, 'ms');
//
//  LTick := TThread.GetTickCount;
//  for i := 1 to TestCount do
//  begin
//    IntToStr(j);
//  end;
//  Writeln('IntToStr system: ', TThread.GetTickCount - LTick, 'ms');
//  end;
end.
