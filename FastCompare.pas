unit FastCompare;

interface

uses
  System.SysUtils;



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
function CompareFast(const Left, Right: ShortString): integer; overload; inline;
function CompareFast(const Left, Right: AnsiString): integer; overload; inline;
function CompareFast(const Left, Right: UnicodeString): integer; overload; inline;
function CompareFast(const Left, Right: WideString): integer; overload; inline;
function CompareFast(const Left, Right: RawByteString): integer; overload; inline;
function CompareFast(const Left, Right: UTF8String): integer; overload; inline;
function CompareFast(const Left, Right: pointer): integer; overload; inline;
function CompareFast(const Left, Right: IInterface): integer; overload; inline;
function CompareFast(const Left, Right: TObject): integer; overload; inline;

function Compare_Variant(const Left, Right: pointer): integer;
function BinaryCompare(const Left, Right; Size: integer): integer;
function FastBinaryCompare(const [ref] Left, Right; Size: integer): integer;
function BinaryCompare4(const Left, Right: Cardinal): integer;
{$IFDEF purepascal}
function BinaryCompare8(const Left, Right: pointer): integer;
{$ELSE !purepascal}{$IFDEF CPUX64}
function BinaryCompare8(const Left, Right: UInt64): integer;
{$ELSE !CPUX64}
function BinaryCompare8(const Left, Right: pointer): integer;
{$ENDIF}{$ENDIF}
function BinaryCompare3(const Left, Right: pointer): integer;
function Compare_DynArray(const Left, Right: pointer; ElementSize: integer): NativeInt;
function Compare_PSn(const [ref] Left, Right: OpenString): integer; {$ifdef purepascal}inline;{$else}{$ifndef CPUX64}inline;{$endif}{$endif}

  //For testing purposes
function PascalMurmurHash3(const [ref] HashData; Len, Seed: integer): integer;
function MurmurHash3(const [ref] HashData; Len: integer; Seed: integer = 0): integer;{$ifdef purepascal}inline;{$endif}

///FNV1a_Hash_Meiyan is about 30% faster than murmurhash3
function FNV1A_Hash_Meiyan(const str; wrdlen: cardinal; seed: cardinal = 2166136261): integer; {$ifdef purepascal}inline;{$endif}
function PascalFNV1A_Hash_Meiyan(const str; wrdlen: cardinal; seed: cardinal = 2166136261): integer;
function BobJenkinsHash(const HashData; Len, Seed: integer): integer; inline;

function CompareWideStr(const S1, S2: WideString): integer;
function CompareUnicodeStr(const S1, S2: string): integer;
function CompareAnsiStr(const S1, S2: AnsiString): Integer;
function SameUnicodeStr(const S1, S2: string): boolean;
function SameWideStr(const S1, S2: WideString): boolean;
function BinaryEquals(const Left, Right: pointer; Size: integer): boolean;
function DynLen(const Arr: pointer): NativeInt; inline;


implementation

uses
  System.Variants
  , System.Math
  , System.AnsiStrings
  , System.Generics.Defaults
  ;

function Compare_Variant(const Left, Right: pointer): integer;
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
        Result:= CompareWideStr(WideString(l), WideString(r));
      end else if (varTypeLeft = varString) or (varTypeRight = varString) then begin
        Result:= CompareAnsiStr(AnsiString(l), AnsiString(r));
      end else begin
        lAsString:= PVariant(Left)^;
        rAsString:= PVariant(Right)^;
        Result:= CompareUnicodeStr(lAsString, rAsString);
      end;
    except // if comparison fails again, compare bytes.
      Result:= FastBinaryCompare(Left, Right, SizeOf(Variant));
    end;
  end;
end;

function BinaryCompare4(const Left, Right: Cardinal): integer;
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
function BinaryCompare(const Left, Right; Size: integer): integer;
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
function BinaryEquals(const Left, Right: pointer; Size: integer): boolean;
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

function FastBinaryCompare(const [ref] Left, Right; Size: integer): integer;
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

function DynLen(const Arr: pointer): NativeInt; inline;
begin
  if Arr = nil then Result:= 0
  else Result:= PNativeInt(PByte(Arr) - SizeOf(NativeInt))^;
end;

function Compare_DynArray(const Left, Right: pointer; ElementSize: integer): NativeInt;
var
  LenL, LenR, lenDiff: NativeInt;
begin
  LenL:= DynLen(Left);
  LenR:= DynLen(Right);
  lenDiff:= LenL - LenR;
  LenL:= Min(LenL, LenR);
  Result:= FastBinaryCompare(Left^, Right^, ElementSize * LenL);
  if Result = 0 then Result:= lenDiff;
end;

function Compare_PSn(const [ref] Left, Right: OpenString): integer;
{$ifdef purepascal}
begin
  Result:= (integer(Left > Right) - integer(Left < Right));
end;
{$else}
{$ifdef CPUX86}
begin
  Result:= (integer(Left > Right) - integer(Left < Right));
end;
{$else}
asm
        {     ->RCX = Pointer to left string    }
        {       RDX = Pointer to right string   }

        XCHG    RBX,R8             //Push RBX
        MOV     R9,RCX
        MOV     R10,RDX

        //Are both S1 and S2 dword aligned?
        OR      ECX,EDX
        TEST    ECX,$3
        JZ @AlignedStrings
@UnAlignedStrings:
        MOVZX   EAX, byte ptr [R9]  //Length S1
        MOVZX   ECX, byte ptr [R10] //Length S2
        INC     R9
        INC     R10
        SUB     EAX,ECX { eax = len1 - len2 }
        JA      @@Skip1a
        ADD     ECX,EAX { eCX = len2 + (len1 - len2) = len1     }
@@Skip1a:
        NEG     RCX             //Count down
        SUB     R9, RCX
        SUB     R10,RCX
        CMP     RCX,-3
        JS @@LongLoop
@ShortString:
        CMP     ECX,0
        JZ @@Exit
        CMP     ECX,-1
        JNZ @LengthNot1
        MOVZX   EDX, byte ptr [R9+RCX]
        MOVZX   EBX, byte ptr [R10+RCX]
        JMP @@CompareRest
@LengthNot1:
        CMP     ECX,-2
        JNZ @LengthIs3
        MOVZX   EDX, word ptr [R9+RCX]
        MOVZX   EBX, word ptr [R10+RCX]
        JMP @@CompareRest
@LengthIs3:
        MOV     EDX,[R9+RCX-1]
        MOV     EBX,[R10+RCX-1]
        SHR     EDX,8
        SHR     EBX,8
        JMP @@CompareRest
@AlignedStrings:
        MOV     EDX, [R9]
        MOV     EBX, [R10]

        MOVZX   EAX,DL
        MOVZX   ECX,BL
        SUB     EAX,ECX { eax = len1 - len2 }
        JA      @@Skip1
        ADD     ECX,EAX { eCX = len2 + (len1 - len2) = len1     }
@@Skip1:
        SHR     EBX,8
        SHR     EDX,8
        NEG     RCX
        //JZ @@Exit
        CMP     EBX,EDX
        JNE @@MisMatch
        ADD     RCX,3
        SUB     R9, RCX
        SUB     R10,RCX
        //MOV     R11,RCX
@@longLoop:
        MOV     EDX,[R9 +RCX+4]
        MOV     EBX,[R10+RCX+4]
        CMP     EDX,EBX
        JNE     @@misMatch
        ADD     RCX,4
        JNS     @@exit
        MOV     EDX,[R9 +RCX+4]
        MOV     EBX,[R10+RCX+4]
        CMP     EDX,EBX
        JNE     @@misMatch
        ADD     RCX,4
        JS      @@longloop
@@exit:
        XCHG    RBX,R8
        RET
@@compareRest:
        //  RCX = 4/0 -> done
        //  RCX = 3 -> 1 byte
        //  RCX = 2 -> 2 bytes
        //  RCX = 1 -> 3 bytes
        TEST    RCX,$3
        JZ  @@Exit
        LEA     ECX,[ECX*8]
        SHL     EDX,CL
        SHL     EBX,CL
        BSWAP   EDX
        BSWAP   EBX
        CMP     EDX,EBX
        JNE @@Different
        //if Same, then EAX is the difference in length
        XCHG    RBX,R8
        RET
@@misMatch:
        ADD     RCX,4
        JG @@CompareRest
        BSWAP   EDX
        BSWAP   EBX
        CMP     EDX,EBX
@@Different:
        SBB     EAX,EAX
        SBB     EAX,-1
        XCHG    RBX,R8
end;
{$endif}{$endif}


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

function CompareAnsiStr(const S1, S2: AnsiString): Integer;
{$IFDEF PUREPASCAL}
var
  P1, P2: PAnsiChar;
  I: Integer;
  L1, L2: Integer;
begin
  { Length and PChar of S1 }
  L1 := Length(S1);
  P1 := PAnsiChar(S1);

  { Length and PChar of S2 }
  L2 := Length(S2);
  P2 := PAnsiChar(S2);

  { Continue the loop until the end of one string is reached. }
  I := 0;
  while (I < L1) and (I < L2) do
  begin
    if (P1^ <> P2^) then
      Exit(Ord(P1^) - Ord(P2^));

    Inc(P1);
    Inc(P2);
    Inc(I);
  end;

  { If chars were not different return the difference in length }
  Result := L1 - L2;
end;
{$ELSE !PUREPASCAL}
{$IFDEF CPUX86}
inline;
begin
  Result:= System.AnsiStrings.CompareStr(S1, S2);
end;
{$ELSE}
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
  {On entry:
     eax = @S1[1]
     edx = @S2[1]
   On exit:
     Result in eax:
       0 if S1 = S2,
       > 0 if S1 > S2,
       < 0 if S1 < S2
   Code size:
     101 bytes}
  cmp rcx, rdx
  je @SameAnsiString
  {Is either of the AnsiStrings perhaps nil?}
  test rcx, rdx
  jz @PossibleNilAnsiString
  {Compare the first four characters (there has to be a trailing #0). In random
   AnsiString compares this can save a lot of CPU time.}
@BothNonNil:
  {Compare the first character}
  movzx eax, byte ptr [rcx]
  movzx r8d, byte ptr [rdx]
  sub eax,r8d
  je @FirstCharacterSame
  {First character differs}
  ret
@FirstCharacterSame:
  {Save ebx}
  push rbx
  {Set ebx = length(S1)}
  mov ebx, [rcx - 4]
  mov r9d,ebx
  xor r8, r8
  {Set ebx = length(S1) - length(S2)}
  sub ebx, [rdx - 4]
  {Save the length difference in r11}
  mov r11,rbx
  {Set ecx = 0 if length(S1) < length(S2), $ffffffff otherwise}
  adc r8, -1
  {Set ecx = - min(length(S1), length(S2))}
  and r8d, ebx
  sub r8d, r9d
  movsx r8,r8d
  {Adjust the pointers to be negative based}
  sub rcx, r8
  sub rdx, r8
@CompareLoop:
  mov ebx, [rcx + r8]
  xor ebx, [rdx + r8]
  jnz @Mismatch
  add r8, 4
  js @CompareLoop
  {All characters match - return the difference in length}
@MatchUpToLength:
  mov rax,r11
  pop rbx
  ret
@Mismatch:
  bsf ebx, ebx
  shr ebx, 3
  add r8, rbx
  jns @MatchUpToLength
  movzx eax, byte ptr [rcx + r8]
  movzx edx, byte ptr [rdx + r8]
  sub eax, edx
  pop rbx
  ret
  {It is the same AnsiString}
@SameAnsiString:
  xor eax, eax
  ret
  {Good possibility that at least one of the AnsiStrings are nil}
@PossibleNilAnsiString:
  test rcx, rcx
  jz @FirstAnsiStringNil
  test rdx, rdx
  jnz @BothNonNil
  {Return first AnsiString length: second AnsiString is nil}
  mov eax, 1
  ret
@FirstAnsiStringNil:
  {Return 0 - length(S2): first AnsiString is nil}
  mov eax,-1
end;
{$ENDIF X86ASM}
{$ENDIF !PUREPASCAL}

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

(*  From wikipedia:
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
  i: NativeInt;
  Len1, Len2: integer;
  k: integer;
  data: PCardinal;
label case1, case2, case3, final;
type
  ByteArray = array[0..0] of byte;
begin
  Result:= seed;
  data:= @HashData;
  Len1:= (Len shr 2)-1;
  for i:= 0 to Len1 do begin
    k:= data[i];
    k:= k * integer(c1);
    k:= (k shl r1) or (k shr (32-r1));
    k:= k * c2;
    Result:= Result xor k;
    Result:= (Result shl r2) or (Result shr (32-r2));
    Result:= Result * m + integer(n);
  end; {for i}
  k:= 0;
  Len2:= Len;
  case Len and $3 of
    1: goto case1;
    2: goto case2;
    3: goto case3;
    else goto final;
  end;
case3:
  Dec(Len2);
  Inc(k, PByte(Data)[Len2] shl 16);
case2:
  Dec(Len2);
  Inc(k, PByte(Data)[Len2] shl 8);
case1:
  Dec(Len2);
  Inc(k, PByte(Data)[Len2]);
  k:= k * integer(c1);
  k:= (k shl r1) or (k shr (32 - r1));
  k:= k * c2;
  Result:= Result xor k;
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
    mov  esi,ecx
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
    cmp  ECX,3
    jz @process_remaining
    ror  EBX,8
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
    mov  RAX,RCX
    lea  RCX,[RDX-4]
    mov  R11,RDX
    mov  RDX,R8
    //RAX = data
    //RCX = count in bytes
    //RDX = seed
    //make index negative based.
    lea  RAX, [RAX+RCX+4]
    neg  RCX
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
    //mov  ECX,ER11
    //and  ER11,$3
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
    xor  EDX,R11d
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
end;
{$ENDIF}
{$ENDREGION}
{$ENDIF}

{$pointermath on}
// Meiyan means Beauty, Charming Eyes or most precisely: SOULFUL EYES.
function PascalFNV1A_Hash_Meiyan(const str; wrdlen: cardinal; seed: cardinal = 2166136261): integer;
const
  prime = 709607;
var
  Hash32: integer;
  p: PInteger;
begin
  p:= @Str;
  Hash32:= Seed;
  while wrdlen >= 2 * SizeOf(Cardinal) do begin
    Hash32:= (hash32 xor (
      ((p[0] shl 5) or (p[0] shr (32 - 5)))
     xor p[1])) * prime;
    inc(p,2);
    dec(wrdlen, SizeOf(integer)*2)
  end;
  if (wrdlen and SizeOf(integer)) <> 0 then begin
    hash32:= (hash32 xor PWord(p)^) * Prime;
    Inc(PWord(p));
    hash32:= (hash32 xor PWord(p)^) * Prime;
    Inc(PWord(p));
  end;
  if (wrdlen and SizeOf(word)) <> 0 then begin
    hash32:= (hash32 xor PWord(p)^) * prime;
    Inc(PWord(p));
  end;
  if (wrdlen and 1) <> 0 then begin
    hash32:= (hash32 xor PByte(p)^) * prime;
  end;
  Result:= Hash32 xor (hash32 shr 16);
end;
{$pointermath off}

function FNV1A_Hash_Meiyan(const str; wrdlen: cardinal; seed: cardinal = 2166136261): integer;
{$ifdef PurePascal}
begin
  Result:= PascalFNV1a_Hash_Meiyan(str,wrdlen,seed);
end;
{$else}
const
  prime = 709607;
{$REGION 'asm'}
{$ifdef CPUX86}
asm
//eax = STR
//edx = len
//ecx = seed
      push ebx
      push esi

      add eax,edx
      lea esi,[edx-8]
      neg esi
      jg @remaining
@Loop8:
      mov ebx,[eax+esi-8]
      rol ebx,5
      xor ebx,[eax+esi-8+4]
      xor ecx,ebx
      imul ecx,ecx,prime
      add esi,$08
      jle @loop8
@remaining:
      lea esi,[esi+eax-8]
      test dl,$4
      jz @wordsize

      mov ebx, [esi]
      mov eax, ebx
      and ebx,$ffff
      xor ecx,ebx
      imul ecx,ecx,prime

      shr eax,16
      xor ecx,eax
      imul ecx,ecx,prime

      add esi,$04
@wordsize:
      test dl,2
      jz @bytesize

      movzx ebx, word ptr [esi]
      xor ecx,ebx
      imul ecx,ecx,prime

      add esi,$02
@bytesize:
      test dl,1
      jz @wrapup

      movzx ebx, byte ptr [esi]
      xor ecx,ebx
      imul ecx,ecx,prime
@wrapup:
      mov eax,ecx
      shr eax,16
      xor eax,ecx
      pop esi
      pop ebx
end;
{$ENDIF}
{$IFDEF CPUX64}
asm
  .NOFRAME
//ecx = STR
//edx = len
//r8 = seed
      add rcx,rdx
      lea r11,[rdx-8]
      neg r11
      jg @remaining
@Loop8:
      mov RAX,[rcx+r11-8]
      rol EAX,5
      mov r9,RAX
      shr r9,32
      xor EAX,r9d
      xor r8d,EAX
      imul r8d,r8d,prime
      add r11,$08
      jle @loop8
@remaining:
      lea r11,[r11+rcx-8]
      test dl,$4
      jz @wordsize

      mov R10d,[r11]
      mov eax, R10d
      and R10d,$ffff
      xor r8d,R10d
      imul r8d,r8d,prime

      shr eax,16
      xor r8d,eax
      imul r8d,r8d,prime

      add r11,$04
@wordsize:

      test dl,2
      jz @bytesize

      movzx R10d, word ptr [r11]
      xor r8d,R10d
      imul r8d,r8d,prime

      add r11,$02
@bytesize:
      test dl,1
      jz @wrapup

      movzx R10d, byte ptr [r11]
      xor r8d,R10d
      imul r8d,r8d,prime
@wrapup:
      mov rax,r8
      shr eax,16
      xor eax,r8d
end;
{$ENDIF}{$ENDIF}
{$ENDREGION}

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
  Result:= CompareWideStr(Left, Right);
end;

function CompareFast(const Left, Right: UnicodeString): integer;
begin
  Result:= CompareUnicodeStr(Left, Right);
end;

function CompareFast(const Left, Right: AnsiString): integer;
begin
  Result:= CompareStr(Left, Right);
end;

function CompareFast(const Left, Right: ShortString): integer;
begin
  Result:= CompareStr(System.ShortString(Left), System.ShortString(Right));
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


end.
