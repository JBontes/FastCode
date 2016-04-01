unit TestQuickerSort;

interface

uses
  DUnitX.TestFramework,
  System.Generics.Collections,
  QuickerSort;

type
  [TestFixture]
  TTestSort = class
  public
    //[Setup]
    procedure Setup;
    //[Teardown]
    procedure TearDown;
    [Test]
    //[Testcase('RandomIntSort 8','10000000')]

    //[Testcase('RandomIntSort 1', '2')]
    [Testcase('RandomIntSort 1', '5')]
    [Testcase('RandomIntSort 1', '30')]
    [Testcase('RandomIntSort 2', '27')]
    [Testcase('RandomIntSort 3', '100')]
    [Testcase('RandomIntSort 4', '1000')]
    [Testcase('RandomIntSort 5', '10000')]
    [Testcase('RandomIntSort 6', '100000')]
    [Testcase('RandomIntSort 7', '1000000')]
    [Testcase('RandomIntSort 8', '10000000')]
    procedure TestSortIntegerRandom(Len: integer);
  end;

implementation

uses
  System.SysUtils,
  System.Diagnostics,
  OriginalSort;

{ TTestSort }

procedure TTestSort.Setup;
begin
end;

procedure TTestSort.TearDown;
begin
end;

procedure TestSort();
begin

end;

function Min(a,b: Int64): Int64;
begin
  if a < b then Result:= a
  else Result:= b;
end;

procedure TTestSort.TestSortIntegerRandom(Len: integer);
var
  i: integer;
  Timer: TStopWatch;
  DualSortArray: TArray<integer>;
  QuickSort2Array: TArray<integer>;
  RecordSortArray: TArray<Integer>;
  EmbaQuickSortArray: TArray<integer>;
  Emba2QuickSortArray: TArray<integer>;
  MemValidationArray: TArray<integer>;
  AllSorted: boolean;
  run: Integer;
  MinTicks, MinMilis: Int64;
const
  MaxInt64 = $7FFFFFFFFFFFFF;
begin
  SetLength(DualSortArray,Len);
  for i:= 0 to high(DualSortArray) do begin
    DualSortArray[i]:= Random(High(Integer));
  end;
  QuickSort2Array:= DualSortArray;
  //Force a copy of the array.
  SetLength(QuickSort2Array, Length(QuickSort2Array));
  EmbaQuickSortArray:= DualSortArray;
  SetLength(EmbaQuickSortArray, Length(EmbaQuickSortArray));
  Emba2QuickSortArray:= DualSortArray;
  SetLength(Emba2QuickSortArray, Length(Emba2QuickSortArray));
  MemValidationArray:= DualSortArray;
  SetLength(MemValidationArray, Length(MemValidationArray));
  RecordSortArray:= DualSortArray;
  SetLength(RecordSortArray, Length(RecordSortArray));

  //************Dualsort
  WriteLn('Starting test for ' + IntToStr(Len) + ' elements, this may take a while');
  MinMilis:= MaxInt64;
  MinTicks:= MaxInt64;
  for run := 1 to 5 do begin
    Timer:= TStopWatch.StartNew;
    TArray.Sort<integer>(DualSortArray);
    Timer.Stop;
    MinMilis:= Min(MinMilis, Timer.ElapsedMilliseconds);
    MinTicks:= Min(MinTicks, Timer.ElapsedTicks);
  end;
  WriteLn('3: Dualsort:     '
           + 'elements done in ' + IntToStr(MinTicks)
           + 'ticks or ' + IntToStr(MinMilis) + 'milliseconds');
  Assert.IsTrue(true,'No exceptions generated');
  //***********QuickSort2
//  Timer:= TStopwatch.StartNew;
//  TArray.SortStackOverflow<integer>(QuickSort2Array);
//  Timer.Stop;
//  WriteLn('2: Quicksort SO: '
//           + 'elements done in ' + IntToStr(Timer.ElapsedTicks)
//           + 'ticks or ' + IntToStr(Timer.ElapsedMilliseconds) + 'milliseconds');
  //**********Quicksort old.
  MinMilis:= MaxInt64;
  MinTicks:= MaxInt64;
  for run := 1 to 5 do begin
    Timer:= TStopwatch.StartNew;
    TArray.SortEmba<integer>(EmbaQuickSortArray);
    Timer.Stop;
    MinMilis:= Min(MinMilis, Timer.ElapsedMilliseconds);
    MinTicks:= Min(MinTicks, Timer.ElapsedTicks);
  end;
  WriteLn('1: EmbaSort:     '
           + 'elements done in ' + IntToStr(MinTicks)
           + 'ticks or ' + IntToStr(MinMilis) + 'milliseconds');
  //**********Record sort
  MinMilis:= MaxInt64;
  MinTicks:= MaxInt64;
  for run := 1 to 5 do begin
    Timer:= TStopwatch.StartNew;
    TArray.SortRecord<integer>(RecordSortArray);
    Timer.Stop;
    MinMilis:= Min(MinMilis, Timer.ElapsedMilliseconds);
    MinTicks:= Min(MinTicks, Timer.ElapsedTicks);
  end;
  WriteLn('2: Record sort:     '
           + 'elements done in ' + IntToStr(MinTicks)
           + 'ticks or ' + IntToStr(MinMilis) + 'milliseconds');
  //********* Quicksort old with default comparers
  MinMilis:= MaxInt64;
  MinTicks:= MaxInt64;
  for run := 1 to 5 do begin
  Timer:= TStopwatch.StartNew;
  TOriginalArray.Sort<integer>(Emba2QuickSortArray);
  Timer.Stop;
    MinMilis:= Min(MinMilis, Timer.ElapsedMilliseconds);
    MinTicks:= Min(MinTicks, Timer.ElapsedTicks);
  end;
  WriteLn('0: Generics sort:'
           + 'elements done in ' + IntToStr(MinTicks)
           + 'ticks or ' + IntToStr(MinMilis) + 'milliseconds');
  if Len < 1000000 then begin

    for i:= 1 to High(RecordSortArray)-1 do begin
      Assert.IsTrue(NativeUInt(RecordSortArray[i-1]) <= NativeUInt(RecordSortArray[i]),'recordsort: Element '+IntToStr(i)+' is not sorted');
    end;
    for i:= 1 to High(DualSortArray)-1 do begin
      Assert.IsTrue(NativeUInt(DualSortArray[i-1]) <= NativeUInt(DualSortArray[i]),'dualsort: Element '+IntToStr(i)+' is not sorted');
    end;
    for i:= 1 to High(EmbaQuickSortArray)-1 do begin
      Assert.IsTrue(EmbaQuickSortArray[i-1] <= EmbaQuickSortArray[i],'EmbaSort: Element '+IntToStr(i)+' is not sorted');
      Assert.IsTrue(Emba2QuickSortArray[i]  = RecordSortArray[i],'Element '+IntToStr(i)+' of RecordSortArray is not the same');
      Assert.IsTrue(Emba2QuickSortArray[i]  = DualSortArray[i],'Element '+IntToStr(i)+' of DualSortArray is not the same');
    end;

    if Len > 10 then begin
      AllSorted:= false;
      for i:= 1 to High(MemValidationArray)-1 do begin
        AllSorted:= NativeUInt(MemValidationArray[i-1]) < NativeUInt(MemValidationArray[i]);
        if not(AllSorted) then Break;
      end;
      Assert.IsFalse(AllSorted, 'Unsorted copy is being sorted, should not happen');
    end;
  end
  else begin
    WriteLn('Done, press a key');
    ReadLn;
  end;
end;

initialization
  //Floats
  TDUnitX.RegisterTestFixture(TTestSort);
end.
