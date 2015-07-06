program HashSpeedTest;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  SynCommons in '..\..\Mormot\SynCommons.pas',
  FastDefaults,
  System.Generics.Defaults,
  System.Diagnostics;

var
  HashTest: array[0..10000] of Ansistring;
const
  stringlen = 300;

procedure InitHashTest;
var
  i,j: Integer;
  Len: integer;
  r: AnsiChar;
begin
  RandSeed:= 0;
  for i := Low(HashTest) to High(HashTest) do begin
    len:= Random(StringLen)+3;
    SetLength(HashTest[i],Len);
    for j := 1 to len do begin
      r:= AnsiChar(Chr(Random(200)+50));
      HashTest[i][j]:= r;
    end;
  end;
  WriteLn('Init done');
end;

procedure crc;
var
  Timer: TStopWatch;
  i,a: integer;
  PrevCrc: integer;
begin
  for a := 0 to 5 do begin
    Timer:= TStopWatch.StartNew;
    PrevCRC:= 0;
    for i:= Low(HashTest) to High(HashTest) do begin
      PrevCrc:= crc32cfast(PrevCRC, PAnsiChar(pointer(HashTest[i])), Length(HashTest[i]));
    end;
    Timer.Stop;
    WriteLn(Format('Crc32 took: %d millisec = %d ticks ',[Timer.ElapsedMilliseconds, Timer.ElapsedTicks]));
  end;
end;


{$pointermath on}
// Meiyan means Beauty, Charming Eyes or most precisely: SOULFUL EYES.
function FNV1A_Hash_Meiyan(const str; wrdlen: cardinal; seed: integer = 2166136261): integer;
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


function FNV1A_Hash_Meiyan_asm(const str; wrdlen: cardinal; seed: integer = 2166136261): integer;
const
  prime = 709607;
{$ifdef CPUX86}
asm
//eax = STR
//edx = len
//ecx = seed
      push ebx
      push esi
//HashSpeedTest.dpr.62: while wrdlen >= 2 * SizeOf(Cardinal) do begin
      add eax,edx
      lea esi,[edx-8]
      neg esi
      jg @remaining
//HashSpeedTest.dpr.63: Hash32:= (hash32 xor (
@Loop8:
      mov ebx,[eax+esi-8]
      rol ebx,5
      xor ebx,[eax+esi-8+4]
      xor ecx,ebx
      imul ecx,ecx,prime
//HashSpeedTest.dpr.66: inc(p,2);
      add esi,$08
//HashSpeedTest.dpr.67: dec(wrdlen, SizeOf(integer)*2)
      //sub edx,$08
//HashpeedTest.dpr.62: while wrdlen >= 2 * SizeOf(Cardinal) do begin
      jle @loop8
@remaining:
//HashSpeedTest.dpr.69: if (wrdlen and SizeOf(integer)) <> 0 then begin
      //dl in 7,6,5,4,3,2,1 ->
      lea esi,[esi+eax-8]
      test dl,$4
      jz @wordsize
//HashSpeedTest.dpr.70: hash32:= (hash32 xor PWord(p)^) * Prime;
      mov ebx, [esi]
      mov eax, ebx
      and ebx,$ffff
      xor ecx,ebx
      imul ecx,ecx,prime
//HashSpeedTest.dpr.71: Inc(PWord(p));
//HashSpeedTest.dpr.72: hash32:= (hash32 xor PWord(p)^) * Prime;
      //movzx ebx, word ptr [esi-8+2]
      shr eax,16
      xor ecx,eax
      imul ecx,ecx,prime
//HashSpeedTest.dpr.73: Inc(PWord(p));
      add esi,$04
@wordsize:
//HashSpeedTest.dpr.75: if (wrdlen and SizeOf(word)) <> 0 then begin
      test dl,2
      jz @bytesize
//HashSpeedTest.dpr.76: hash32:= (hash32 xor PWord(p)^) * prime;
      movzx ebx, word ptr [esi]
      xor ecx,ebx
      imul ecx,ecx,prime
//HashSpeedTest.dpr.77: Inc(PWord(p));
      add esi,$02
//HashSpeedTest.dpr.79: if (wrdlen and 1) <> 0 then begin
@bytesize:
      test dl,1
      jz @wrapup
//HashSpeedTest.dpr.80: hash32:= (hash32 xor PByte(p)^) * prime;
      movzx ebx, byte ptr [esi]
      xor ecx,ebx
      imul ecx,ecx,prime
//HashSpeedTest.dpr.82: Result:= Hash32 xor (hash32 shr 16);
@wrapup:
      mov eax,ecx
      shr eax,16
      xor eax,ecx
      pop esi
      pop ebx
end;
{$Endif}
{$ifdef CPUX64}
asm
  .NOFRAME
//ecx = STR
//edx = len
//r8 = seed
//HashSpeedTest.dpr.62: while wrdlen >= 2 * SizeOf(Cardinal) do begin
//HashSpeedTest.dpr.62: while wrdlen >= 2 * SizeOf(Cardinal) do begin
      add rcx,rdx
      lea r11,[rdx-8]
      neg r11
      jg @remaining
//HashSpeedTest.dpr.63: Hash32:= (hash32 xor (
@Loop8:
      mov RAX,[rcx+r11-8]
      rol EAX,5
      mov r9,RAX
      shr r9,32
      xor EAX,r9d
      xor r8d,EAX
      imul r8d,r8d,prime
//HashSpeedTest.dpr.66: inc(p,2);
      add r11,$08
//HashSpeedTest.dpr.67: dec(wrdlen, SizeOf(integer)*2)
      //sub edx,$08
//HashpeedTest.dpr.62: while wrdlen >= 2 * SizeOf(Cardinal) do begin
      jle @loop8
@remaining:
//HashSpeedTest.dpr.69: if (wrdlen and SizeOf(integer)) <> 0 then begin
      //dl in 7,6,5,4,3,2,1 ->
      lea r11,[r11+rcx-8]
      test dl,$4
      jz @wordsize
//HashSpeedTest.dpr.70: hash32:= (hash32 xor PWord(p)^) * Prime;
      mov R10d,[r11]
      mov eax, R10d
      and R10d,$ffff
      xor r8d,R10d
      imul r8d,r8d,prime
//HashSpeedTest.dpr.71: Inc(PWord(p));
//HashSpeedTest.dpr.72: hash32:= (hash32 xor PWord(p)^) * Prime;
      //movzx R10d, word ptr [esi-8+2]
      shr eax,16
      xor r8d,eax
      imul r8d,r8d,prime
//HashSpeedTest.dpr.73: Inc(PWord(p));
      add r11,$04
@wordsize:
//HashSpeedTest.dpr.75: if (wrdlen and SizeOf(word)) <> 0 then begin
      test dl,2
      jz @bytesize
//HashSpeedTest.dpr.76: hash32:= (hash32 xor PWord(p)^) * prime;
      movzx R10d, word ptr [r11]
      xor r8d,R10d
      imul r8d,r8d,prime
//HashSpeedTest.dpr.77: Inc(PWord(p));
      add r11,$02
//HashSpeedTest.dpr.79: if (wrdlen and 1) <> 0 then begin
@bytesize:
      test dl,1
      jz @wrapup
//HashSpeedTest.dpr.80: hash32:= (hash32 xor PByte(p)^) * prime;
      movzx R10d, byte ptr [r11]
      xor r8d,R10d
      imul r8d,r8d,prime
//HashSpeedTest.dpr.82: Result:= Hash32 xor (hash32 shr 16);
@wrapup:
      mov rax,r8
      shr eax,16
      xor eax,r8d
end;
{$endif}

procedure Meiyanasm;
var
  Timer: TStopWatch;
  i,a: integer;
  PrevCrc: integer;
begin
  for a := 0 to 5 do begin
    Timer:= TStopWatch.StartNew;
    PrevCRC:= 2166136261;
    for i:= Low(HashTest) to High(HashTest) do begin
      PrevCrc:= FNV1A_Hash_Meiyan_asm(HashTest[i], Length(HashTest[i]), PrevCrc);
    end;
    Timer.Stop;
    WriteLn(Format('MeiyanAsm took: %d millisec = %d ticks output = %d',[Timer.ElapsedMilliseconds, Timer.ElapsedTicks, PrevCrc]));
  end;
end;



procedure MeiyanPascal;
var
  Timer: TStopWatch;
  i,a: integer;
  PrevCrc: integer;
begin
  for a := 0 to 5 do begin
    Timer:= TStopWatch.StartNew;
    PrevCRC:= 2166136261;
    for i:= Low(HashTest) to High(HashTest) do begin
      PrevCrc:= FNV1A_Hash_Meiyan(HashTest[i], Length(HashTest[i]), PrevCrc);
    end;
    Timer.Stop;
    WriteLn(Format('Meiyan took: %d millisec = %d ticks output = %d',[Timer.ElapsedMilliseconds, Timer.ElapsedTicks, PrevCrc]));
  end;
end;


procedure Murmur;
var
  Timer: TStopWatch;
  i,a: integer;
  PrevCrc: integer;
begin
  for a := 0 to 5 do begin
    Timer:= TStopWatch.StartNew;
    PrevCRC:= 0;
    for i:= Low(HashTest) to High(HashTest) do begin
      PrevCrc:= MurmurHash3(HashTest[i], Length(HashTest[i]), PrevCrc);
    end;
    Timer.Stop;
    WriteLn(Format('Murmur3asm took: %d millisec = %d ticks output = %d',[Timer.ElapsedMilliseconds, Timer.ElapsedTicks, Prevcrc]));
  end;
end;


procedure MurmurPascal;
var
  Timer: TStopWatch;
  i,a: integer;
  PrevCrc: integer;
begin
  for a := 0 to 5 do begin
    Timer:= TStopWatch.StartNew;
    PrevCRC:= 0;
    for i:= Low(HashTest) to High(HashTest) do begin
      PrevCrc:= PascalMurmurHash3(HashTest[i], Length(HashTest[i]), PrevCrc);
    end;
    Timer.Stop;
    WriteLn(Format('Murmur3 Pascal took: %d millisec = %d ticks output = %d',[Timer.ElapsedMilliseconds, Timer.ElapsedTicks, Prevcrc]));
  end;
end;


procedure BobJenkins;
var
  Timer: TStopWatch;
  i,a: integer;
  PrevCrc: integer;
begin
  for a := 0 to 5 do begin
    Timer:= TStopWatch.StartNew;
    PrevCRC:= 0;
    for i:= Low(HashTest) to High(HashTest) do begin
      PrevCrc:= BobJenkinsHash((@HashTest[i])^, Length(HashTest[i]), PrevCrc);
    end;
    Timer.Stop;
    WriteLn(Format('Bobjenkins took: %d millisec = %d ticks output = %d',[Timer.ElapsedMilliseconds, Timer.ElapsedTicks, prevcrc]));
  end;
end;

begin
  InitHashTest;
  Crc;
  Murmur;
  MurmurPascal;
  MeiyanPascal;
  MeiyanAsm;
  BobJenkins;
  WriteLn('done, press a key...');
  ReadLn;
end.
