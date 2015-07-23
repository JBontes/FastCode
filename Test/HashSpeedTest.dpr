program HashSpeedTest;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  SynCommons in '..\..\Mormot\SynCommons.pas',
  FastCompare,
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
      PrevCrc:= FNV1A_Hash_Meiyan(HashTest[i], Length(HashTest[i]), PrevCrc);
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
      PrevCrc:= PascalFNV1A_Hash_Meiyan(HashTest[i], Length(HashTest[i]), PrevCrc);
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

procedure xxHash;
var
  Timer: TStopWatch;
  i,a: integer;
  PrevCrc: integer;
begin
  for a := 0 to 5 do begin
    Timer:= TStopWatch.StartNew;
    PrevCRC:= 0;
    for i:= Low(HashTest) to High(HashTest) do begin
      PrevCrc:= xxHash32Calc(HashTest[i], Length(HashTest[i]), PrevCrc);
    end;
    Timer.Stop;
    WriteLn(Format('xxHash took: %d millisec = %d ticks output = %d',[Timer.ElapsedMilliseconds, Timer.ElapsedTicks, Prevcrc]));
  end;
end;

procedure AsmxxHash;
var
  Timer: TStopWatch;
  i,a: integer;
  PrevCrc: integer;
begin
  for a := 0 to 5 do begin
    Timer:= TStopWatch.StartNew;
    PrevCRC:= 0;
    for i:= Low(HashTest) to High(HashTest) do begin
      PrevCrc:= AsmxxHash32Calc(HashTest[i], Length(HashTest[i]), PrevCrc);
    end;
    Timer.Stop;
    WriteLn(Format('AsmxxHash took: %d millisec = %d ticks output = %d',[Timer.ElapsedMilliseconds, Timer.ElapsedTicks, Prevcrc]));
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

type
  TBigrec = record
    a,b,c: Int64;
  end;

var
  i: TBigRec;
const
  TestSize = 4;

begin
  InitHashTest;
  i.a:= Int64(Random(MaxInt))*$4567897784; i.b:= -i.a; i.c:= i.a xor i.b;
  //fillChar(i, SizeOf(i), #255);
  WriteLn(FNV1A_Hash_Meiyan(i, TestSize, 2166136261));
  WriteLn(PascalFNV1A_Hash_Meiyan(i, TestSize, 2166136261));
//  Crc;
//  Murmur;
//  MurmurPascal;
//  BobJenkins;
//  xxHash;
//  AsmxxHash;
  MeiyanAsm;
  MeiyanPascal;
  WriteLn('done, press a key...');
  ReadLn;
end.
