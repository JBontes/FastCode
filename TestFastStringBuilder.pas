unit TestFastStringBuilder;

interface

uses
  DUnitX.TestFramework,
  FastStringBuilder;

type
  [TestFixture]
  TTestFastStringBuilder<T> = class
  public
    [Setup]
    procedure Setup;
    [Teardown]
    procedure TearDown;
    [Test]
    //[Testcase('RandomIntSort 8','10000000')]

    //[Testcase('RandomIntSort 1', '2')]
    [Testcase('BuildSame 1', '5')]
    [Testcase('BuildSame 1', '30')]
    [Testcase('BuildSame 2', '27')]
    [Testcase('BuildSame 3', '100')]
    [Testcase('BuildSame 4', '1000')]
    [Testcase('BuildSame 5', '10000')]
    [Testcase('BuildSame 6', '100000')]
    [Testcase('BuildSame 7', '1000000')]
    procedure TestBuildRepeated(Len: integer);
    [Testcase('Sandwich','1')]
    procedure SandwichBetweenChars;
    [Testcase('Sandwich','1')]
    procedure SandwhichBetweenStrings;
    [Testcase('Hardcoded sandwich','1')]
    procedure HardcodedSandwhich;

  private
    FData: TStringBuilder;
  end;



implementation


{ TTestFastStringBuilder<T> }

procedure TTestFastStringBuilder<T>.HardcodedSandwhich;
var
  s: string;
  c: char;
  norm: string;
begin
  s:= 'items:{}';
  FData.Append('{');
  if TypeInfo(T) = TypeInfo(char) then begin
    FData.Append('{');
    norm := '{' + '{' + '}';
  end else if TypeInfo(T) = TypeInfo(string) then begin
    FData.Append(s);
    norm := '{' + s + '}';
  end;
  FData.Append('}');
  Assert.IsTrue(FData.ToString = norm,'HardcodedSandwhich failed, expected: '+norm+' got: '+FData.ToString);

end;

procedure TTestFastStringBuilder<T>.SandwhichBetweenStrings;
var
  s: string;
  c: char;
  norm: string;
begin
  s:= 'test';
  c:= 'a';
  FData.Append(s);
  if TypeInfo(char) = TypeInfo(T) then begin
    FData.Append(c);
    norm:= s+c+s;
  end else if TypeInfo(T) = TypeInfo(string) then begin
    FData.Append(s);
    norm:= s+s+s;
  end;
  FData.Append(s);
  Assert.IsTrue(FData.ToString = norm,'SandwhichBetweenStrings failed, expected: '+norm+' got: '+FData.ToString);
end;

procedure TTestFastStringBuilder<T>.SandwichBetweenChars;
var
  s: string;
  c: char;
  norm: string;
begin
  s:= 'items:{}';
  c:= '{';
  FData.Append(c);
  if TypeInfo(T) = TypeInfo(char) then begin
    FData.Append(c);
    norm:= c+c+c;
  end else if TypeInfo(T) = TypeInfo(string) then begin
    FData.Append(s);
    norm:= c+s+c;
  end;
  FData.Append(c);
  Assert.IsTrue(FData.ToString = norm,'SandwhichBetweenStrings failed, expected: '+norm+' got: '+FData.ToString);
end;

procedure TTestFastStringBuilder<T>.Setup;
begin
  FData:= TStringBuilder.Create;
end;

procedure TTestFastStringBuilder<T>.TearDown;
begin
  FData.Free;
end;

procedure TTestFastStringBuilder<T>.TestBuildRepeated(Len: integer);
const
  teststring = 'test';
var
  i: integer;
  c: char;
  s: string;
begin
  for i:= 0  to Len-1 do begin
    if TypeInfo(T) = TypeInfo(string) then begin
      FData.Append(teststring);
    end else if TypeInfo(T) = TypeInfo(Char) then begin
      c:= 'a';
      FData.Append(c);
    end else Assert.IsFalse(true, 'Only char and string are allowed in string builder');
  end; {for i}
  if TypeInfo(T) = TypeInfo(char) then begin
    Assert.IsTrue(FData.ToString = StringOfChar(c, Len), 'TestBuildRepeated, expected: '+StringOfChar(c, Len)+ 'got: '+FData.ToString);
  end else if TypeInfo(T) = TypeInfo(string) then begin
    for i := 0 to Len-1 do begin
      s:= s + teststring;
    end;
    Assert.IsTrue(FData.ToString = s, 'TestBuildRepeated, expected: '+s+ 'got: '+FData.ToString);
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TTestFastStringBuilder<char>);
  TDUnitX.RegisterTestFixture(TTestFastStringBuilder<string>);
end.
