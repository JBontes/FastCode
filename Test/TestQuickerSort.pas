unit TestQuickerSort;

interface

uses
  DUnitX.TestFramework,
  System.Generics.Collections,
  QuickerSort;

type
  [TestFixture]
  TTestSort = class
  private
    IntArray: TArray<Integer>;
  public
    [Setup]
    procedure Setup;
    [Teardown]
    procedure TearDown;
    [Test]
    procedure TestIntegerArray;
  end;

implementation

uses
  System.SysUtils;

{ TTestSort }

procedure TTestSort.Setup;
var
  i: integer;
begin
  SetLength(IntArray,100);
  for i:= 0 to high(IntArray) -1 do begin
    IntArray[i]:= Random(High(Integer));
  end;
end;

procedure TTestSort.TearDown;
begin
  SetLength(IntArray,0);
end;

procedure TTestSort.TestIntegerArray;
var
  i: integer;
begin
  TArray.Sort<Integer>(IntArray);
  for i:= 1 to High(IntArray)-1 do begin
    Assert.IsTrue(IntArray[i-1] <= IntArray[i],'Element '+IntToStr(i)+' is not sorted');
  end;
  ReadLn;
end;

initialization
  //Floats
  TDUnitX.RegisterTestFixture(TTestSort);
end.
