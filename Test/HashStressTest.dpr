﻿program HashStressTest;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  System.StrUtils,
  FastCompare,
  System.Generics.Defaults,
  System.Diagnostics,
  System.Generics.Collections,
  System.Classes,
  Unit1 in 'Unit1.pas';

//SynCommons;

function MakeZip(Number: integer): string;
begin
  Result:= RightStr('000000'+IntToStr(Number),6);
end;

const
  BucketMax = $0FFFF;
  ZipMax = 999999;

type
  TBucket = array [0..BucketMax] of Integer;
  THashOutcome = TArray<integer>;

  THashKind = (hkBob, hkMurmur, hkCrc32, hkMeiyan, hkxxHash32);
  TZip = array [0..ZipMax] of string;

  TMyHash = function(const Data; Len, InitData: Integer): Integer;

  TBitcount = array[0..1] of integer;
  TBitcounts = record
    Counts: array[0..31] of TBitcount;
    procedure Clear;
    procedure Add(input: integer);
    procedure Print(const Header: string = '');
  end;

var
  LowBucket: TBucket;
  HighBucket: TBucket;
  Zips: TZip;
  HashOutcome: THashOutcome;

procedure InitBuckets;
var
  H: THashKind;
  i,j: Integer;
begin
  for i := 0 to BucketMax do begin
    LowBucket[i]:= 0;
    HighBucket[i]:= 0;
  end;
  for j := 0 to ZipMax do begin
    Zips[j]:= MakeZip(j);
  end;
  SetLength(HashOutcome, ZipMax+1);
end;

var
  Passwords: TStringlist;

procedure HashPasswordsFast(Hasher: TMyHash; Flavor: THashKind);
var
  i: integer;
  H,L: integer;
begin
  SetLength(HashOutcome,1);
  for i:= 0 to Passwords.Count-1 do begin
    h:= Hasher((@Passwords[i][1])^, Length(Passwords[i])*SizeOf(Char), FastCompare.DefaultHashSeed);
  end;
end;

procedure HashPasswords(Hasher: TMyHash; Flavor: THashKind);
var
  i: integer;
  H,L: integer;
begin
  SetLength(HashOutcome, Passwords.Count);
  for i:= 0 to Passwords.Count-1 do begin
    h:= Hasher((@Passwords[i][1])^, Length(Passwords[i])*SizeOf(Char), FastCompare.DefaultHashSeed);
    HashOutcome[i]:= H;
    L:= H and BucketMax;
    H:= H shr 16;
    Inc(LowBucket[L]);
    Inc(HighBucket[H]);
  end;
end;

procedure HashZips(Hasher: TMyHash; Flavor: THashKind);
var
  i: Integer;
  H: integer;
  L: integer;
  //Zip: string;
  BitBias: TBitCounts;
begin
  for i := 0 to ZipMax do begin
    //Zip:= MakeZip(i);
    H:= Hasher((@Zips[i][1])^, Length(Zips[i])*SizeOf(Char), FastCompare.DefaultHashSeed);
    HashOutcome[i]:= H;
    L:= H and BucketMax;
    H:= H shr 16;
    Inc(LowBucket[L]);
    Inc(HighBucket[H]);
  end;
  BitBias.Clear;
  for i := 0 to ZipMax do begin
    BitBias.Add(HashOutcome[i]);
  end;
  BitBias.Print;
end;

function HashCrc32(const Data; Len, InitData: integer): integer;
begin
  //Result:= SynCommons.crc32cfast(InitData, @Data, Len);
end;

procedure HashNumbers(Hasher: TMyHash; Flavor: THashKind);
var
  i: Integer;
  H: integer;
  L: integer;
  //Zip: string;
begin
  for i := 0 to ZipMax do begin
    //Zip:= MakeZip(i);
    H:= Hasher(i, SizeOf(Integer), FastCompare.DefaultHashSeed);
    HashOutcome[i]:= H;
    L:= H and BucketMax;
    H:= H shr 16;
    Inc(LowBucket[L]);
    Inc(HighBucket[H]);
  end;
end;

function CountCollisions(var Outcome: THashOutcome): integer;
var
  i: Integer;
  Prev, count: integer;
  Maxcount: integer;
  MaxCollision: integer;
begin
  TArray.Sort<Integer>(Outcome);
  Prev:= 0;
  Count:= 0;
  MaxCount:= 0;
  //MaxCollision:= 0;
  for i:= 0 to High(Outcome) do begin
    if Outcome[i] = Prev then Inc(Count);
    if Count > MaxCount then begin
      MaxCount:= Count;
      //MaxCollision:= Outcome[i];
    end
    else begin
      Count:= 0;
      Prev:= Outcome[i];
    end;
  end;
  Result:= MaxCount;
end;

function Max(const a,b: integer): integer;
begin
  if a >= b then result:= a else result:= b;
end;

type
  TCollisionStats = record
    LowCols: integer;
    HighCols: integer;
  end;

function Collisions(ABucket: TBucket): integer;
var
  i: integer;
begin
  Result:= 0;
  for i := 0 to BucketMax do begin
    Result:= Max(Result, ABucket[i]);
  end;
end;


type
  TTestProc = procedure(Hasher: TMyHash; Flavor: THashKind);

procedure TestHash(TestProc: TTestProc; const msg: string);
var
  H: THashKind;
  Watch: TStopWatch;
  name: string;
  Stats: TCollisionStats;
  BitBias: TBitcounts;
begin
  for H := hkBob to hkxxHash32 do begin
    InitBuckets;
    Watch:= TStopwatch.StartNew;
    case H of
      hkBob: begin
        TestProc(System.Generics.Defaults.BobJenkinsHash, hkBob);
        name:= 'BobJenkins Lookup3';
      end;
      hkMurmur: begin
        TestProc(FastCompare.MurmurHash3, hkMurmur);
        name:= 'MurmurHash3';
      end;
      hkCRC32: begin
        TestProc(HashCrc32, hkCrc32);
        Name:= 'CRC32c';
      end;
      hkMeiyan: begin
        TestProc(FastCompare.FNV1A_Hash_Meiyan, hkMeiyan);
        Name:= 'FNV_1a_Meiyan';
      end;
      hkxxHash32: begin
        TestProc(FastCompare.xxHash32Calc, hkxxHash32);
        Name:= 'xxHash32';
      end;
    end;
    Watch.Stop;
    WriteLn(Msg);
    WriteLn(Format(Name+' took %d ms, %d ticks',[Watch.ElapsedMilliseconds, Watch.ElapsedTicks]));
    WriteLn(Format(Name+' has %d max low collisions',[Collisions(LowBucket)]));
    WriteLn(Format(Name+' has %d max high collisions',[Collisions(HighBucket)]));
    WriteLn(Format(Name+' has %d real collisions',[CountCollisions(HashOutcome)]));
  end;  {for}
end;


{ TBitcounts }

procedure TBitcounts.Add(input: integer);
var
  i: Integer;
  Bit: integer;
begin
  Bit:= 1;
  for i := 0 to 31 do begin
    if (input and bit) <> 0 then Inc(Counts[i][1])
    else Inc(Counts[i][0]);
    Bit:= Bit shl 1;
  end;
end;

procedure TBitcounts.Clear;
begin
  FillChar(Counts, SizeOf(Counts), #0);
end;

procedure TBitcounts.Print(const Header: string = '');
var
  Total: integer;
  Percentage: integer;
  i: Integer;
  Zeros, Ones: string;
begin
  if Header <> '' then begin
    WriteLn('Bit distribution of '+Header);
  end;
  Total:= Counts[0][0] + Counts[0][1];
  for i := 0 to 31 do begin
    Percentage:= Round((Counts[i][0] / Total) * 100);
    Zeros:= DupeString('.', Percentage div 2);
    Ones:= DupeString('#', (100 - Percentage) div 2);
    WriteLn(RightStr('0'+IntToStr(i),2)+' '+Zeros+Ones+' '+IntToStr(Percentage)+'% zeros');
  end;
end;

begin
  WriteLn('Starting testing');
  TestHash(HashZips, 'Zipcodes 00000..99999');
  TestHash(HashNumbers, 'integers');
  WriteLn('Loading 120 MB password file...');
  Passwords:= TStringList.Create;
  Passwords.LoadFromFile('c:\borland\fastcode\test\passwords.txt');
  WriteLn('Done loading, starting the hash...');
  TestHash(HashPasswords, '120 MB of Passwords');
  TestHash(HashPasswordsFast, '120 MB of Passwords as fast as possible');
  Writeln('Done, press a key....');
  ReadLn;
end.
