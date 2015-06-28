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
  HashTest: array[0..1000] of Ansistring;

procedure InitHashTest;
var
  i,j: Integer;
  Len: integer;
  r: AnsiChar;
begin
  for i := Low(HashTest) to High(HashTest) do begin
    len:= Random(30)+3;
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
    WriteLn(Format('Murmurhash3 took: %d millisec = %d ticks ',[Timer.ElapsedMilliseconds, Timer.ElapsedTicks]));
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
    WriteLn(Format('Murmurhash3 PurePascal took: %d millisec = %d ticks ',[Timer.ElapsedMilliseconds, Timer.ElapsedTicks]));
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
    WriteLn(Format('Bobjenkins took: %d millisec = %d ticks ',[Timer.ElapsedMilliseconds, Timer.ElapsedTicks]));
  end;
end;

begin
  InitHashTest;
  Crc;
  Murmur;
  MurmurPascal;
  BobJenkins;
  WriteLn('done, press a key...');
  ReadLn;
end.
