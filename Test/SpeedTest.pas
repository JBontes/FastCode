unit SpeedTest;

interface
uses
  DUnitX.TestFramework,
  System.Generics.Defaults,
  System.Generics.FastDefaults;

type
  [TestFixture]
  TSpeedTest<T> = class(TObject)
  private
    Bag1, Bag2: array[0..10000] of T;
    FSlowCompare: System.Generics.Defaults.IComparer<T>;
    FFastCompare: System.Generics.FastDefaults.IComparer<T>;
    function RandomT: T;
    property SlowCompare: System.Generics.Defaults.IComparer<T> read FSlowCompare;
    property FastCompare: System.Generics.FastDefaults.IComparer<T> read FFastCompare;
  public
    [SetupFixture]
    procedure FixtureSetup;
    [TearDownFixture]
    procedure TearDown;
    [Test]
    [RepeatTest(5)]
    procedure Run_Forest_Run;
    [Test]
    [RepeatTest(5)]
    procedure SimpleRun;
  end;


implementation

uses System.Diagnostics,
System.SysUtils;

function TSpeedTest<T>.RandomT: T;
type
  PByteArray = ^TByteArray;
  TByteArray = array[0..0] of byte;
  PReal48 = ^Real48;
var
  i,Len: integer;
begin
  case GetTypeKind(T) of
    //Custom enum
    tkUnknown, tkInteger, tkChar, tkEnumeration, tkSet, tkWChar, tkRecord,
    tkArray, tkInt64, tkPointer, tkClass, tkMethod, tkClassRef, tkProcedure: begin
      for i := 0 to SizeOf(T) -1 do begin
        PByteArray(@Result)^[i]:= Random(254)+1;
      end;
    end;
    tkFloat: case SizeOf(T) of
      4: PSingle(@Result)^:= Random(MaxInt) * (Random(2)-1);
      6: PReal48(@Result)^:= Random(MaxInt) * (Random(2)-1);
      8: PDouble(@Result)^:= Random(MaxInt) * (Random(2)-1);
      10: PExtended(@Result)^:= Random(MaxInt) * (Random(2)-1);
    end;
    tkString: begin
      PByteArray(@Result)^[0]:= SizeOf(T)-1;
      for i := 1 to SizeOf(T)-2 do begin
        PByteArray(@Result)^[i]:= Random(200)+50;
      end;
    end;
    tkLString: begin
      Len:= Random(255)+3;
      SetLength(PAnsiString(@Result)^, Len);
      for i := 1 to Len do begin
        PAnsiString(@Result)^[i]:= AnsiChar(chr(Random(200)+50));
      end;
      PAnsiString(@Result)^[Len+1]:= #0;
    end;
    tkWString: begin
      for i := 1 to SizeOf(T)-1 do begin
        PWideString(@Result)^[i]:= chr(Random(200)+50);
      end;
      PWideString(@Result)^[SizeOf(T)]:= #0;
    end;
    tkVariant: PVariant(@Result)^:= Random(200);
    tkInterface: PPointer(@Result)^:= nil;
    tkDynArray: ; //No idea how to construct this.
    tkUString: begin
      Len:= Random(255)+3;
      SetLength(PUnicodeString(@Result)^, Len);
      for i := 1 to Len do begin
        PUnicodeString(@Result)^[i]:= chr(Random(200)+50);
      end;
      PUnicodeString(@Result)^[Len+1]:= #0;
    end;
  end;
end;

procedure TSpeedTest<T>.FixtureSetup;
var
  i: Integer;
  Len: integer;
begin
  FSlowCompare:= System.Generics.Defaults.TComparer<T>.Default;
  FFastCompare:= System.Generics.FastDefaults.TComparer<T>.Default;
  for i := low(bag1) to High(Bag1) do begin
    Bag1[i]:= RandomT;
    if (Random(5) = 0) then case GetTypeKind(T) of
      tkLString: begin
        Bag2[i]:= Bag1[i];
        Len:= Length(AnsiString((@Bag2[i])^));
        AnsiString((@Bag2[i])^)[Len]:= AnsiChar(Random(200+50));
      end
      else Bag2[i]:= RandomT;
    end
    else Bag2[i]:= Bag1[i];
  end;
end;

procedure TSpeedTest<T>.TearDown;
var
  i: Integer;
begin
  FSlowCompare:= nil;
  FFastCompare:= nil;
  for i := low(bag1) to High(Bag1) do begin
    Bag1[i]:= Default(T);
    Bag2[i]:= Default(T);
  end;
end;


procedure TSpeedTest<T>.Run_Forest_Run;
var
  Timer, Timer1, Timer2: TStopwatch;
  i: integer;
  Outcome: integer;
  Bigger1, Smaller1, Same1: integer;
  Bigger2, Smaller2, Same2: integer;
begin
  //EmptyLoop
  Smaller1:= 1;
  ReadLn;
  Timer:= TStopwatch.StartNew;
  for i:= Low(bag1) to High(bag1) do begin
    Outcome:= Smaller1;
    if Outcome < 0 then Inc(Smaller1)
    else if Outcome = 0 then Inc(Same1)
    else if Outcome > 0 then Inc(Bigger1);
  end;
  Timer.Stop;
  WriteLn(Format('Empty loop took %d millisec, %d ticks',[Timer.ElapsedMilliseconds, Timer.ElapsedTicks]));
  //FastCompare
  Smaller2:= 0; Same2:= 0; Bigger2:= 0;
  Timer2:= TStopwatch.StartNew;
  for i:= Low(bag1) to High(bag1) do begin
    Outcome:= FastCompare.Compare(Bag1[i], Bag2[i]);
    if Outcome < 0 then Inc(Smaller2)
    else if Outcome = 0 then Inc(Same2)
    else if Outcome > 0 then Inc(Bigger2);
  end;
  Timer2.Stop;
  WriteLn(Format('Fast compare loop took %d millisec, %d ticks',[Timer2.ElapsedMilliseconds, Timer2.ElapsedTicks]));
  //SlowCompare
  Smaller1:= 0; Same1:= 0; Bigger1:= 0;
  Timer1:= TStopwatch.StartNew;
  for i:= Low(bag1) to High(bag1) do begin
    Outcome:= SlowCompare.Compare(Bag1[i], Bag2[i]);
    if Outcome < 0 then Inc(Smaller1)
    else if Outcome = 0 then Inc(Same1)
    else if Outcome > 0 then Inc(Bigger1);
  end;
  Timer1.Stop;
  WriteLn(Format('Slow compare loop took %d millisec, %d ticks',[Timer1.ElapsedMilliseconds, Timer1.ElapsedTicks]));
  Assert.AreEqual(Same1, Same2,'Compare functions do not agree on same');
  Assert.AreEqual(Smaller1, Smaller2,'Compare functions do not agree on smaller');
  Assert.AreEqual(Bigger1, Bigger2,'Compare functions do not agree on bigger');
  Assert.IsTrue(Timer1.ElapsedTicks > Timer2.ElapsedTicks,
    Format('FastCompare is slower, Slow: %d ticks %d ms; Fast: %d ticks %d ms',
            [Timer1.ElapsedTicks, Timer1.ElapsedMilliseconds, Timer2.ElapsedTicks, Timer2.ElapsedMilliseconds]));
end;

procedure TSpeedTest<T>.SimpleRun;
var
  Timer: TStopWatch;
  i: integer;
  Outcome: integer;
  Difference: integer;
begin
  //EmptyLoop
  Timer:= TStopwatch.StartNew;
  for i:= Low(bag1) to High(bag1) do begin
    Outcome:= PByte(@Bag1[i])^ + PByte(@Bag2[i])^;
  end;
  Timer.Stop;
  if Outcome = 0 then Write('');
  WriteLn(Format('Empty loop took %d millisec, %d ticks',[Timer.ElapsedMilliseconds, Timer.ElapsedTicks]));
  //SlowCompare
  Timer:= TStopwatch.StartNew;
  for i:= Low(bag1) to High(bag1) do begin
    {Outcome:= Outcome +}SlowCompare.Compare(Bag1[i], Bag2[i]);
  end;
  Timer.Stop;
  if Outcome = 0 then Write('');
  Difference:= Timer.ElapsedTicks;
  WriteLn(Format('Slow compare loop took %d millisec, %d ticks',[Timer.ElapsedMilliseconds, Timer.ElapsedTicks]));
  //FastCompare
  Timer:= TStopwatch.StartNew;
  for i:= Low(bag1) to High(bag1) do begin
    Outcome:= {Outcome + }FastCompare.Compare(Bag1[i], Bag2[i]);
  end;
  Timer.Stop;
  if Outcome = 0 then Write('');
  WriteLn(Format('Fast compare loop took %d millisec, %d ticks',[Timer.ElapsedMilliseconds, Timer.ElapsedTicks]));
  Difference:= Difference - Timer.ElapsedTicks;
  Assert.IsTrue(Difference > 0,Format('FastCompare is slower; Fastcompare: %d Slowcompare %d difference: %d',
                [Timer.ElapsedTicks, Timer.ElapsedTicks+ difference, difference]));
end;


type
  Str100 = shortstring;

initialization
  TDUnitX.RegisterTestFixture(TSpeedTest<byte>);
  TDUnitX.RegisterTestFixture(TSpeedTest<int8>);
  TDUnitX.RegisterTestFixture(TSpeedTest<word>);
  TDUnitX.RegisterTestFixture(TSpeedTest<int16>);
  TDUnitX.RegisterTestFixture(TSpeedTest<cardinal>);
  TDUnitX.RegisterTestFixture(TSpeedTest<integer>);
  TDUnitX.RegisterTestFixture(TSpeedTest<Uint64>);
  TDUnitX.RegisterTestFixture(TSpeedTest<Int64>);
  TDUnitX.RegisterTestFixture(TSpeedTest<WideChar>);
  TDUnitX.RegisterTestFixture(TSpeedTest<Char>);
  //TDUnitX.RegisterTestFixture(TSpeedTest<Str100>);
  TDUnitX.RegisterTestFixture(TSpeedTest<string>);
  TDUnitX.RegisterTestFixture(TSpeedTest<AnsiString>);
end.
