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

    [Testcase('RandomIntSort 1', '0')]
    [Testcase('RandomIntSort 2', '1')]
    [Testcase('RandomIntSort 3', '2')]
    [Testcase('RandomIntSort 4', '27')]
    [Testcase('RandomIntSort 5', '100')]
    [Testcase('RandomIntSort 6', '1000')]
    [Testcase('RandomIntSort 7','10000')]
    [Testcase('RandomIntSort 8','100000')]
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

procedure TTestSort.TestSortIntegerRandom(Len: integer);
var
  i: integer;
  Timer: TStopWatch;
  DualSortArray: TArray<integer>;
  QuickSort2Array: TArray<integer>;
  EmbaQuickSortArray: TArray<integer>;
  Emba2QuickSortArray: TArray<integer>;
  MemValidationArray: TArray<integer>;
  AllSorted: boolean;
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

  //************Dualsort
  WriteLn('Starting test for ' + IntToStr(Len) + ' elements, this may take a while');
  Timer:= TStopWatch.StartNew;
  TArray.Sort<integer>(DualSortArray);
  Timer.Stop;
  WriteLn('3: Dualsort:     '
           + 'elements done in ' + IntToStr(Timer.ElapsedTicks)
           + 'ticks or ' + IntToStr(Timer.ElapsedMilliseconds) + 'milliseconds');
  Assert.IsTrue(true,'No exceptions generated');
//  //***********QuickSort2
//  Timer:= TStopwatch.StartNew;
//  TArray.SortStackOverflow<integer>(QuickSort2Array);
//  Timer.Stop;
//  WriteLn('2: Quicksort SO: '
//           + 'elements done in ' + IntToStr(Timer.ElapsedTicks)
//           + 'ticks or ' + IntToStr(Timer.ElapsedMilliseconds) + 'milliseconds');
  //**********Quicksort old.
  Timer:= TStopwatch.StartNew;
  TArray.SortEmba<integer>(EmbaQuickSortArray);
  Timer.Stop;
  WriteLn('1: EmbaSort:     '
           + 'elements done in ' + IntToStr(Timer.ElapsedTicks)
           + 'ticks or ' + IntToStr(Timer.ElapsedMilliseconds) + 'milliseconds');
  //********* Quicksort old with default comparers
  Timer:= TStopwatch.StartNew;
  TOriginalArray.Sort<integer>(Emba2QuickSortArray);
  Timer.Stop;
  WriteLn('0: Generics sort:'
           + 'elements done in ' + IntToStr(Timer.ElapsedTicks)
           + 'ticks or ' + IntToStr(Timer.ElapsedMilliseconds) + 'milliseconds');
  if Len < 1000000 then begin

    for i:= 1 to High(DualSortArray)-1 do begin
      Assert.IsTrue(NativeUInt(DualSortArray[i-1]) <= NativeUInt(DualSortArray[i]),'Element '+IntToStr(i)+' is not sorted');
    end;
//    for i:= 1 to High(QuickSort2Array)-1 do begin
//      Assert.IsTrue(NativeUInt(QuickSort2Array[i-1]) <= NativeUInt(QuickSort2Array[i]),'Element '+IntToStr(i)+' is not sorted');
//    end;
    for i:= 1 to High(EmbaQuickSortArray)-1 do begin
      Assert.IsTrue(NativeUInt(EmbaQuickSortArray[i-1]) <= NativeUInt(EmbaQuickSortArray[i]),'Element '+IntToStr(i)+' is not sorted');
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
