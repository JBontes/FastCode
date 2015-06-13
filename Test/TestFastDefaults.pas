unit TestFastDefaults;

interface

uses
  DUnitX.TestFramework,
  System.Generics.Defaults,
  FastDefaults;//, System.Rtti;

type
  TStr1 = string[1];
  TStr2 = string[2];
  TStr3 = string[3];
  TStr100 = string[100];
  TStr255 = string[255];
  TEmptyRec = record
  end;
  TByteRec = packed record
    a: byte;
  end;
  TWordRec = packed record
    a: word;
  end;
  TIntRec = packed record
    a: integer;
  end;
  TOddRec = packed record
    a: byte;
    b: integer;
    c: byte;
  end;
  TNormalRec = record
    a: byte;
    b: integer;
    c: byte;
  end;
  TEightRec = record
    a: integer;
    b: integer;
  end;
  TManagedRec = record
    a: byte;
    b: string;
  end;
  TBigRec = record
    a,b,c,d,e: Int64;
  end;
  TEnum = (red, blue, orange);
  T5ByteEnum = (a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30,a31,a32,a33,a34,a35,a36,a37,a38,a39);
  TEnum2 = (gray = 50, apple = 1, other=255);
  TSmallSet = set of TEnum;
  TBigSet = set of TEnum2;
  TMediumSet = set of TTypeKind;
  T5ByteSet = set of T5ByteEnum;
  TByteArray = array[0..0] of byte;
  TWordArray = array[0..0] of word;
  TIntArray = array[0..0] of Integer;
  TBigArray = array[0..100] of byte;
  TRecArray = array[0..10] of TManagedRec;
  TByteDynArray = array of byte;
  TWordDynArray = array of word;
  TIntDynArray  = array of Integer;
  TBigDynArray  = array of byte;
  TRecDynArray  = array of TManagedRec;
  TProcedure = procedure;
  TTestProcedure = procedure(const L,R: byte);


//Have we covered all types?
//    tkUnknown        OK  {custom enum}
//    tkInteger,       OK
//    tkChar,          OK
//    tkEnumeration,   OK
//    tkFloat,         ok
//    tkString,        ok
//    tkSet,           OK
//    tkClass,         OK
//    tkMethod,        OK
//    tkWChar,         OK
//    tkLString,       OK
//    tkWString,       OK
//    tkVariant,       OK
//    tkArray,         OK
//    tkRecord,        OK
//    tkInterface,     OK
//    tkInt64,         OK
//    tkDynArray,      OK
//    tkUString,       OK
//    tkClassRef,      OK
//    tkPointer,       OK
//    tkProcedure      OK

  [TestFixture]
  TestComplex = class(TObject)
  public
    fSomeField: integer;
    //[Setup]
    procedure Setup;
    //[TearDown]
    procedure TearDown;
    // Sample Methods
    // Simple single Test
    [Test]
    procedure TestString1(const L, R: TStr1);
    [Test]
    procedure TestString2(const L, R: TStr2);
    [Test]
    procedure TestString3(const L, R: TStr3);
    [Test]
    procedure TestString255(const L, R: TStr255);
    [Test]
    procedure TestReal48;
    [Test]
    procedure TestEmptyRec;
    [Test]
    procedure TestByteRec;
    [Test]
    procedure TestWordRec;
    [Test]
    procedure TestIntRec;
    [Test]
    procedure TestEightRec;
    [Test]
    procedure TestOddRec;
    [Test]
    procedure TestNormalRec;
    [Test]
    procedure TestManagedRec;
    [Test]
    procedure TestBigRec;
    [Test]
    [TestCase('Normal enum', 'red,blue')]
    [TestCase('Normal enum', 'blue,blue')]
    [TestCase('Normal enum', 'orange,blue')]
    procedure TestEnums(const L, R: TEnum);
    [Test]
    procedure TestSets;
    [Test]
    procedure TestClass;
    [Test]
    procedure TestClassRef;
    [Test]
    procedure TestPointer;
    [Test]
    procedure TestInterface;
    [Test]
    procedure TestTByteArray;
    [Test]
    procedure TestTWordArray;
    [Test]
    procedure TestTIntArray;
    [Test]
    procedure TestTBigArray;
    [Test]
    procedure TestTRecArray;
    [Test]
    procedure TestTDynByteArray;
    [Test]
    procedure TestTDynWordArray;
    [Test]
    procedure TestTDynIntArray;
    [Test]
    procedure TestTDynBigArray;
    [Test]
    procedure TestTDynRecArray;
    [Test]
    procedure TestMethod;
    [Test]
    procedure TestProcedure;
    [Test]
    procedure TestVariant;
  end;

  [TestFixture]
  TTestFloat<T: record> = class(TObject)
    [Test]
    [TestCase('Float', '100.0,100.0')]
    [TestCase('Float', '-10000.0,100.0')]
    [TestCase('Float', '100000.0,-10000.0')]
    [TestCase('Float', '0.0,-10000.0')]
    [TestCase('Float', '100000.0,0.0')]
    [TestCase('Float', '0.0,0.0')]
    procedure TestFloat(const L,R: T);
  end;

  [TestFixture]
  TTestInteger<T: record> = class(TObject)
    [Test]
    [TestCase('Integer', '-1,100')]
    [TestCase('Integer', '100,$10000000')]
    [TestCase('Integer', '-1000,-12000')]
    [TestCase('Integer', '100,100')]
    [TestCase('Integer', '-100,-100')]
    procedure TestInt(const L, R: T);
  end;

  [TestFixture]
  TTestChar<T: record> = class(TObject)
    [Test]
    [TestCase('Char', 'a,a')]
    [TestCase('Char', 'a,b')]
    [TestCase('Char', 'c,a')]
    [TestCase('Char', ',a')]
    [TestCase('Char', 'a,')]
    [TestCase('Char', ',')]
    procedure TestChar(const L, R: T);
  end;

  [TestFixture]
  TTestString<T> = class(TObject)
    [Test]
     //single char
    [TestCase('String1', 'a,a')]
    [TestCase('String2', 'a,b')]
    [TestCase('String3', 'c,a')]
    [TestCase('String4', ',a')]
    [TestCase('String5', 'a,')]
    [TestCase('String6', ',')]
    //double char
    [TestCase('String7', 'aa,aa')]
    [TestCase('String8', 'aa,ba')]
    [TestCase('String9', 'ca,aa')]
    [TestCase('String10', ',aa')]
    [TestCase('String11', 'aa,')]
    //three chars
    [TestCase('String12', 'aaa,aaa')]
    [TestCase('String13', 'aaa,baa')]
    [TestCase('String14', 'caa,aaa')]
    [TestCase('String15', ',aaa')]
    [TestCase('String16', 'aaa,')]
    //4 chars fit into a 32bit register
    [TestCase('String17', 'aaaa,aaaa')]
    [TestCase('String18', 'aaaa,baaa')]
    [TestCase('String19', 'caaa,aaaa')]
    [TestCase('String20', ',aaaa')]
    [TestCase('String21', 'aaaa,')]
    //done with the special cases
    //length dividable by 4
    [TestCase('String22', 'aalonglonglo,aalonglonglo')]
    [TestCase('String23', 'aalonglonglo,balonglonglo')]
    [TestCase('String24', 'calonglonglo,aalonglonglo')]
    [TestCase('String25', ',aalonglonglo')]
    [TestCase('String26', 'aalonglonglo,')]
    //odd length
    [TestCase('String27', 'aalonglonglonga,aalonglonglonga')]
    [TestCase('String28', 'aalonglonglong,balonglonglonga')]
    [TestCase('String29', 'calonglonglonga,aalonglonglong')]
    [TestCase('String30', ',aalonglonglonga')]
    [TestCase('String31', 'aalonglonglonga,')]
    //mismatched length
    [TestCase('String32', 'aanga,aalonglonglonga')]
    [TestCase('String33', 'aalong,balonglonglonga')]
    [TestCase('String34', 'calonglonglonga,aaglong')]
    procedure TestString(const L, R: T);
  end;

  type
  TTest<T> = record
  private
    class var Def: System.Generics.Defaults.IComparer<T>;
    class var F: FastDefaults.TComparison<T>;
    class var Fr: FastDefaults.TComparison<T>;
    class function BinaryCompare(const Left, Right: Pointer; Size: Integer): Integer; static;
    class function Faster(const Left, Right: T): integer; static;
  public
    class function Real48Comparison(const Left, Right: T): Integer; static;
    class function Slow(const Left, Right: T): integer; static;
    class function Fast(const Left, Right: T): integer; static;
    class procedure Test(const Left, Right: T; message: string = ''); static;
    class constructor Init;
  end;

implementation

uses
  System.SysUtils, System.Variants, System.Rtti;

{ TTest<T> }
class function TTest<T>.Real48Comparison(const Left, Right: T): Integer;
var
  L: Real48 absolute Left;
  R: Real48 absolute Right;
begin
  Result:= integer(L > R) - integer(L < R);
end;

class constructor TTest<T>.Init;
begin
  if (GetTypeKind(T) = tkFloat) and (SizeOf(T) = 6) then begin
    Def := System.Generics.Defaults.TComparer<T>.Construct(TTest<T>.Real48Comparison);
  end else Def:= System.Generics.Defaults.TComparer<T>.Default;
  F:= FastDefaults.TComparer<T>.Default.Compare;
  Fr:= FastDefaults.TComparer<T>.Default.TestCompareFast;
end;

class function TTest<T>.BinaryCompare(const Left, Right: Pointer; Size: Integer): Integer;
var
  pl, pr: PByte;
  len: Integer;
begin
  pl := Left;
  pr := Right;
  len := Size;
  while len > 0 do
  begin
    Result := pl^ - pr^;
    if Result <> 0 then
      Exit;
    Dec(len);
    Inc(pl);
    Inc(pr);
  end;
  Result := 0;
end;

class function TTest<T>.Slow(const Left, Right: T): integer;
begin
  if (GetTypeKind(T) = tkDynArray)
    and (Def.Compare(default (T), Right) = 0)
    and (Def.Compare(default (T), Left) <> 0)
  then Exit(1);
  {$ifdef CPUX64}
  //There is no workaround for https://quality.embarcadero.com/browse/RSP-11321
  if (GetTypeKind(T) in [tkRecord, tkArray, tkUnknown, tkSet]) and (SizeOf(T) = 8) then begin
    Result:= BinaryCompare(@Left, @Right, SizeOf(T));
  end else
  {$endif}
  Result:= Def.Compare(Left, Right);
  if Result < 0 then Result:= -1
  else if Result > 0 then Result:= 1
end;

class function TTest<T>.Fast(const Left, Right: T): integer;
begin
  Result:= F(Left, Right);
  if Result < 0 then Result:= -1
  else if Result > 0 then Result:= 1
end;

class function TTest<T>.Faster(const Left, Right: T): integer;
begin
  Result:= Fr(Left, Right);
  if Result < 0 then Result:= -1
  else if Result > 0 then Result:= 1
end;


class procedure TTest<T>.Test(const Left, Right: T; message: string = '');
var
  ResultF, ResultS, ResultFr: Integer;
  Ls,Rs: string;
  X: TTypeKind;
  NewMessage: string;
begin
  X:= GetTypeKind(T);
  Assert.AreEqual(X, GetTypeKind(T));
  try
    ResultF:= Fast(Left, Right);
  except
    ResultF:= MaxInt;
  end;
  try
    ResultFr:= Faster(Left, Right);
  except
    ResultFr:= MaxInt;
  end;
  try
    ResultS:= Slow(Left, Right);
  except
    ResultS:= Maxint;
  end;
  if (GetTypeKind(T) <> tkUnknown) and (GetTypeKind(T) <> tkSet) then try
    Ls:= TValue.From<T>(Left).ToString;
    Rs:= TValue.From<T>(Right).ToString;
  except
    Ls:= '?';
    Rs:= '?';
  end;
  Newmessage:= message + 'Fast = ' + IntToStr(ResultF)
                       + ' Slow = ' + IntToStr(ResultS)
                       + ' L = ' + Ls + ' R = ' + Rs;
  Assert.IsTrue(ResultF = ResultS, NewMessage);
  Newmessage:= message + 'Faster = ' + IntToStr(ResultFr)
                       + ' Slow = ' + IntToStr(ResultS)
                       + ' L = ' + Ls + ' R = ' + Rs;
  Assert.IsTrue(ResultFr = ResultS, NewMessage);
end;

procedure TestComplex.Setup;
begin
end;

procedure TestComplex.TearDown;
begin
end;

procedure TTestInteger<T>.TestInt(const L, R: T);
begin
  TTest<T>.Test(L,R);
end;

procedure TestComplex.TestString1(const L, R: TStr1);
begin
  TTest<TStr1>.Test('a', 'a');
  TTest<TStr1>.Test('b', 'c');
  TTest<TStr1>.Test('c', 'a');
  TTest<TStr1>.Test('', 'a');
  TTest<TStr1>.Test('c', '');
  TTest<TStr1>.Test('', '');
end;

procedure TestComplex.TestString2(const L, R: TStr2);
begin
  TTest<TStr2>.Test('aa', 'aa');
  TTest<TStr2>.Test('bb', 'cc');
  TTest<TStr2>.Test('cc', 'aa');
  TTest<TStr2>.Test('', 'aa');
  TTest<TStr2>.Test('cc', '');
  TTest<TStr2>.Test('', '');
  TTest<TStr2>.Test('cc', 'a');
  TTest<TStr2>.Test('c', 'cc');
end;

procedure TestComplex.TestString3(const L, R: TStr3);
begin
  TTest<TStr3>.Test('aaa', 'aaa');
  TTest<TStr3>.Test('bbb', 'ccc');
  TTest<TStr3>.Test('ccc', 'aaa');
  TTest<TStr3>.Test('', 'aaa');
  TTest<TStr3>.Test('ccc', '');
  TTest<TStr3>.Test('', '');
  TTest<TStr3>.Test('ccc', 'a');
  TTest<TStr3>.Test('c', 'ccc');
end;

procedure TestComplex.TestString255(const L, R: TStr255);
var
  a,b: TStr255;
  i: integer;
begin
  TTest<TStr255>.Test('aaa', 'aaa');
  TTest<TStr255>.Test('bbb', 'ccc');
  TTest<TStr255>.Test('ccc', 'aaa');
  TTest<TStr255>.Test('', 'aaa');
  TTest<TStr255>.Test('ccc', '');
  TTest<TStr255>.Test('', '');
  TTest<TStr255>.Test('ccc', 'a');
  TTest<TStr255>.Test('c', 'ccc');
  for i:= 1 to 255 do begin
    a[i]:= AnsiChar(Chr(Random(255)));
    b[i]:= a[i];
  end;
  a[0]:= #255;
  b[0]:= #255;
  TTest<TStr255>.Test(a,b);
  for i:= 1 to 255 do begin
    b[i]:= AnsiChar(Chr(Random(255)));
  end;
  TTest<TStr255>.Test(a,b);
  TTest<TStr255>.Test('',b);
  TTest<TStr255>.Test(a,'');
end;

procedure TestComplex.TestEmptyRec;
var
  L,R: TEmptyRec;
begin
  TTest<TEmptyRec>.Test(L,R);
end;

procedure TTestChar<T>.TestChar(const L, R: T);
begin
  TTest<T>.Test(L,R);
end;

procedure TestComplex.TestEnums(const L, R: TEnum);
begin
  TTest<TEnum>.Test(L, R);
  TTest<TEnum2>.Test(apple, other);
  TTest<TEnum2>.Test(other, gray);
  TTest<TEnum2>.Test(apple, apple);
end;

procedure TTestFloat<T>.TestFloat(const L, R: T);
begin
  TTest<T>.Test(L,R);
end;

procedure TestComplex.TestReal48;
var
  OldDef: System.Generics.Defaults.IComparer<Real48>;
begin
  OldDef:= TTest<Real48>.Def;
  TTest<Real48>.Def := System.Generics.Defaults.TComparer<Real48>.Construct(TTest<Real48>.Real48Comparison);
  TTest<Real48>.Test(100.0,100.0);
  TTest<Real48>.Test(100000.0,-10000.0);
  TTest<Real48>.Test(0.0,-10000.0);
  TTest<Real48>.Test(100000.0, 0.0);
  TTest<Real48>.Test(0.0, 0.0);
  TTest<Real48>.Def:= OldDef;
end;

procedure TestComplex.TestSets;
begin
  WriteLn('SizeOf(T5ByteSet) = '+IntToStr(SizeOf(T5ByteSet)));
  TTest<T5ByteSet>.Test([], [], '1:');
  TTest<T5ByteSet>.Test([], [a1, a6, a30], '2:');
  TTest<T5ByteSet>.Test([a1, a6, a30], [], '3:');
  TTest<T5ByteSet>.Test([a1, a6, a30], [a1, a6, a30], '4:');
  TTest<T5ByteSet>.Test([a1,a6,a30,a39],[a1,a6,a30],'5:');
  TTest<T5ByteSet>.Test([a1,a6,a30],[a1,a6,a30,a2],'6:');
  WriteLn('SizeOf(TBigSet) = '+IntToStr(SizeOf(TBigSet)));
  TTest<TBigSet>.Test([],[],'7:');
  TTest<TBigSet>.Test([],[apple, gray, other],'8:');
  TTest<TBigSet>.Test([apple, gray, other],[apple, gray, other],'9:');
  TTest<TBigSet>.Test([apple, gray, other],[],'10:');
  TTest<TBigSet>.Test([apple, gray, other],[apple, other],'11:');
  TTest<TBigSet>.Test([other],[apple, gray, other],'12:');
  WriteLn('SizeOf(TSmallSet) = '+IntToStr(SizeOf(TSmallSet)));
  TTest<TSmallSet>.Test([],[],'13:');
  TTest<TSmallSet>.Test([],[blue, orange, red],'14:');
  TTest<TSmallSet>.Test([blue, orange, red],[],'15:');
  TTest<TSmallSet>.Test([blue, orange, red],[blue, orange, red],'16:');
  TTest<TSmallSet>.Test([blue, red],[blue, orange, red],'17:');
  TTest<TSmallSet>.Test([blue, orange, red],[blue, orange],'18:');
  TTest<TSmallSet>.Test([red],[blue],'19:');
end;

procedure TestComplex.TestClass;
var
  TestClass: TObject;
begin
  TestClass:= TObject.Create;
  try
    TTest<TObject>.Test(TestClass, Self);
    TTest<TObject>.Test(Self, TestClass);
    TTest<TObject>.Test(Self, Self);
    TTest<TObject>.Test(nil,nil);
    TTest<TObject>.Test(TestClass, nil);
    TTest<TObject>.Test(nil, Self);
  finally
    TestClass.Free;
  end;
end;

procedure TestComplex.TestClassRef;
var
  L: TClass;
  R: TClass;
begin
  L:= TObject;
  R:= TestComplex;
  TTest<TClass>.Test(L, L);
  TTest<TClass>.Test(L, R);
  TTest<TClass>.Test(R, L);
  TTest<TClass>.Test(L, nil);
  TTest<TClass>.Test(nil,L);
  TTest<TClass>.Test(nil, nil);
end;

type
  TMyMethod = procedure of object;

procedure TestComplex.TestMethod;
var
  TestClass1: TestComplex;
  TestClass2: TDUnitX;
  EmptyMethod: TMethod;
  Method1, Method2: TMethod;
begin
  TestClass1:= TestComplex.Create;
  TestClass2:= TDUnitX.Create;
  try
    EmptyMethod:= Default(TMethod);

    Method1.Data:= TestClass1;
    Method2.Data:= Self;
    Method1.Code:= TestClass1.MethodAddress('Free');
    Method2.Code:= Self.MethodAddress('Setup');
    TTest<TMyMethod>.Test(TestClass1.Free, Self.Setup,'1:');
    TTest<TMyMethod>.Test(Self.Setup, TestClass1.Free,'2:');
    Method1.Code:= TestClass1.MethodAddress('TearDown');
    TTest<TMyMethod>.Test(TestClass1.TearDown, Self.Setup,'3:');
    TTest<TMyMethod>.Test(Self.Setup, TestClass1.TearDown,'4:');
    TTest<TMyMethod>.Test(TestClass1.TearDown, Default(TMyMethod),'5:');
    TTest<TMyMethod>.Test(Default(TMyMethod), TestClass1.TearDown,'6:');
    TTest<TMyMethod>.Test(Default(TMyMethod), Default(TMyMethod),'7:');
    EmptyMethod.Code:= Self.MethodAddress('Setup');
    TTest<TMyMethod>.Test(Self.Setup, TMyMethod(EmptyMethod),'8:');
    TTest<TMyMethod>.Test(TMyMethod(EmptyMethod), Self.Setup,'9:');
    TTest<TMyMethod>.Test(TMyMethod(EmptyMethod), TMyMethod(EmptyMethod),'10:');
    TTest<TMyMethod>.Test(Self.Setup, Self.Setup,'11:');
    Method2.Data:= TestClass2;
    Method2.Code:= TestClass2.MethodAddress('QualifiedClassName');
    TTest<TMyMethod>.Test(TMyMethod(Method1), TMyMethod(Method2),'12:');
    TTest<TMyMethod>.Test(TMyMethod(Method2), TMyMethod(Method1),'13:');
  finally
    TestClass1.Free;
    TestClass2.Free;
  end;    (**)
end;

procedure SomethingToTest1;
begin
  WriteLn('hi1');
end;

procedure SomethingToTest2;
begin
  WriteLn('hi2');
end;

procedure SomeThingOther1(const L,R: byte);
begin
  WriteLn('ho1');
end;

procedure SomeThingOther2(const L,R: byte);
begin
  WriteLn('ho2');
end;

procedure TestComplex.TestProcedure;
var
  Proc1, Proc2: TProcedure;
  Proc3, Proc4: TTestProcedure;
begin
  Proc1:= SomethingToTest1;
  Proc2:= SomethingToTest2;
  Proc3:= SomeThingOther1;
  Proc4:= SomeThingOther2;
  TTest<TProcedure>.Test(Proc1, Proc1,'1:');
  TTest<TProcedure>.Test(Proc1, Proc2,'2:');
  TTest<TProcedure>.Test(Proc2, Proc1,'3:');
  TTest<TProcedure>.Test(Proc1, default (TProcedure),'4:');
  TTest<TProcedure>.Test(default (TProcedure), Proc1,'5:');
  TTest<TTestProcedure>.Test(Proc3, Proc3,'6:');
  TTest<TTestProcedure>.Test(Proc3, Proc4,'7:');
  TTest<TTestProcedure>.Test(Proc4, Proc3,'8:');
  TTest<TTestProcedure>.Test(Proc3, default (TTestProcedure),'9:');
  TTest<TTestProcedure>.Test(Default(TTestProcedure), Proc4,'10:');
  TTest<TProcedure>.Test(TProcedure(Proc1), TProcedure(Proc3),'11:');
  TTest<TProcedure>.Test(TProcedure(Proc1), TProcedure(Proc4),'12:');
  TTest<TProcedure>.Test(TProcedure(Proc4), TProcedure(Proc2),'13:');
end;


procedure TTestString<T>.TestString(const L, R: T);
begin
  TTest<T>.Test(L,R);
end;


procedure TestComplex.TestPointer;
var
  L,R: pointer;
begin
  L:= Self;
  R:= pointer(Self.GetHashCode+100);
  TTest<pointer>.Test(L,L);
  TTest<pointer>.Test(L,R);
  TTest<pointer>.Test(R,L);
  TTest<pointer>.Test(L,nil);
  TTest<pointer>.Test(nil,L);
  TTest<pointer>.Test(nil,nil);
end;

procedure TestComplex.TestInterface;
var
  L, R: IInterface;
begin
  L:= TTest<IInterface>.Def;
  R:= TTest<integer>.Def;
  TTest<IInterface>.Test(L, L);
  TTest<IInterface>.Test(L, R);
  TTest<IInterface>.Test(R, L);
  TTest<IInterface>.Test(L, nil);
  TTest<IInterface>.Test(nil, L);
  TTest<IInterface>.Test(nil, nil);
end;

procedure TestComplex.TestByteRec;
var
  ByteRec1, ByteRec2: TByteRec;
begin
  ByteRec1.a:= 1;
  ByteRec2.a:= 2;
  TTest<TByteRec>.Test(ByteRec1, ByteRec1);
  TTest<TByteRec>.Test(ByteRec1, ByteRec2);
  TTest<TByteRec>.Test(ByteRec2, ByteRec1);
  TTest<TByteRec>.Test(ByteRec2, default (TByteRec));
  TTest<TByteRec>.Test(default (TByteRec), ByteRec1);
  TTest<TByteRec>.Test(default (TByteRec), default (TByteRec));
end;

procedure TestComplex.TestWordRec;
var
  WordRec1, WordRec2: TWordRec;
begin
  WordRec1.a:= 1;
  WordRec2.a:= 2;
  TTest<TWordRec>.Test(WordRec1, WordRec1);
  TTest<TWordRec>.Test(WordRec1, WordRec2);
  TTest<TWordRec>.Test(WordRec2, WordRec1);
  TTest<TWordRec>.Test(WordRec2, default (TWordRec));
  TTest<TWordRec>.Test(default (TWordRec), WordRec1);
  TTest<TWordRec>.Test(default (TWordRec), default (TWordRec));
end;

procedure TestComplex.TestEightRec;
var
  L, R: TEightRec;
  i: integer;
begin
  for i:= 0 to 10 do begin
    L.a:= Random(high(integer)) - integer($7FFFFFFF);
    L.b:= Random(high(integer)) - integer($7FFFFFFF);
    R.a:= Random(high(integer)) - integer($7FFFFFFF);
    R.b:= Random(high(integer)) - integer($7FFFFFFF);
    TTest<TEightRec>.Test(L, L,'1:');
    TTest<TEightRec>.Test(L, R,'2:');
    TTest<TEightRec>.Test(R, L,'3:');
    TTest<TEightRec>.Test(R, default (TEightRec),'3:');
    TTest<TEightRec>.Test(default (TEightRec), L,'4:');
    TTest<TEightRec>.Test(default (TEightRec), default (TEightRec),'5:');
  end;
end;

procedure TestComplex.TestIntRec;
var
  IntRec1, IntRec2: TIntRec;
begin
  IntRec1.a:= 1;
  IntRec2.a:= 2;
  TTest<TIntRec>.Test(IntRec1, IntRec1);
  TTest<TIntRec>.Test(IntRec1, IntRec2);
  TTest<TIntRec>.Test(IntRec2, IntRec1);
  TTest<TIntRec>.Test(IntRec2, default (TIntRec));
  TTest<TIntRec>.Test(default (TIntRec), IntRec1);
  TTest<TIntRec>.Test(default (TIntRec), default (TIntRec));
end;

procedure TestComplex.TestOddRec;
var
  OddRec1, OddRec2: TOddRec;
begin
  OddRec1.a:= 1;
  OddRec1.b:= 1000;
  OddRec1.c:= byte( -10);
  OddRec2.a:= 2;
  OddRec1.b:= -9999999;
  OddRec1.c:= 10;
  TTest<TOddRec>.Test(OddRec1, OddRec1,'1:');
  TTest<TOddRec>.Test(OddRec1, OddRec2,'2:');
  TTest<TOddRec>.Test(OddRec2, OddRec1,'3:');
  TTest<TOddRec>.Test(OddRec2, default (TOddRec),'4:');
  TTest<TOddRec>.Test(default (TOddRec), OddRec1,'5:');
  TTest<TOddRec>.Test(default (TOddRec), default (TOddRec),'6:');
end;

procedure TestComplex.TestNormalRec;
var
  NormalRec1, NormalRec2: TNormalRec;
begin
  NormalRec1.a:= 1;
  NormalRec1.b:= 1000;
  NormalRec1.c:= byte( -10);
  NormalRec2.a:= 2;
  NormalRec1.b:= -9999999;
  NormalRec1.c:= 10;
  TTest<TNormalRec>.Test(NormalRec1, NormalRec1);
  TTest<TNormalRec>.Test(NormalRec1, NormalRec2);
  TTest<TNormalRec>.Test(NormalRec2, NormalRec1);
  TTest<TNormalRec>.Test(NormalRec2, default (TNormalRec));
  TTest<TNormalRec>.Test(default (TNormalRec), NormalRec1);
  TTest<TNormalRec>.Test(default (TNormalRec), default (TNormalRec));
end;

procedure TestComplex.TestManagedRec;
var
  ManagedRec1, ManagedRec2: TManagedRec;
begin
  ManagedRec1.a:= 1;
  ManagedRec1.b:= '1000';
  ManagedRec2.a:= 2;
  ManagedRec1.b:= '-9999999';
  TTest<TManagedRec>.Test(ManagedRec1, ManagedRec1);
  TTest<TManagedRec>.Test(ManagedRec1, ManagedRec2);
  TTest<TManagedRec>.Test(ManagedRec2, ManagedRec1);
  TTest<TManagedRec>.Test(ManagedRec2, default (TManagedRec));
  TTest<TManagedRec>.Test(default (TManagedRec), ManagedRec1);
  TTest<TManagedRec>.Test(default (TManagedRec), default (TManagedRec));
end;

procedure TestComplex.TestBigRec;
var
  BigRec1, BigRec2: TBigRec;
begin
  BigRec1.a:= 1;
  BigRec1.b:= 2;
  BigRec1.c:= 5;
  BigRec1.d:= -10;
  BigRec1.e:= -200;
  BigRec1.a:= -1;
  BigRec1.b:= -2;
  BigRec1.c:= -5;
  BigRec1.d:= 10;
  BigRec1.e:= 200;
  TTest<TBigRec>.Test(BigRec1, BigRec1,'1:');
  TTest<TBigRec>.Test(BigRec1, BigRec2,'2:');
  TTest<TBigRec>.Test(BigRec2, BigRec1,'3:');
  TTest<TBigRec>.Test(BigRec2, default (TBigRec),'4:');
  TTest<TBigRec>.Test(default (TBigRec), BigRec1,'5:');
  TTest<TBigRec>.Test(default (TBigRec), default (TBigRec),'6:');
end;

procedure TestComplex.TestTByteArray;
var
  L,R: TByteArray;
begin
  L[0]:= 1;
  R[0]:= 255;
  TTest<TByteArray>.Test(L,L);
  TTest<TByteArray>.Test(L,R);
  TTest<TByteArray>.Test(R,L);
  TTest<TByteArray>.Test(R,R);
  TTest<TByteArray>.Test(L,Default(TByteArray));
  TTest<TByteArray>.Test(Default(TByteArray),L);
  TTest<TByteArray>.Test(Default(TByteArray),Default(TByteArray));
end;

procedure TestComplex.TestTWordArray;
var
  L,R: TWordArray;
begin
  L[0]:= 15;
  R[0]:= 255*254;
  TTest<TWordArray>.Test(L,L);
  TTest<TWordArray>.Test(L,R);
  TTest<TWordArray>.Test(R,L);
  TTest<TWordArray>.Test(R,R);
  TTest<TWordArray>.Test(L,Default(TWordArray));
  TTest<TWordArray>.Test(Default(TWordArray),L);
  TTest<TWordArray>.Test(Default(TWordArray),Default(TWordArray));
end;

procedure TestComplex.TestTIntArray;
var
  L,R: TIntArray;
begin
  L[0]:= 15;
  R[0]:= 255*254;
  TTest<TIntArray>.Test(L,L,'1:');
  TTest<TIntArray>.Test(L,R,'2:');
  TTest<TIntArray>.Test(R,L,'3:');
  TTest<TIntArray>.Test(R,R,'4:');
  L[0]:= 15;
  R[0]:= -255*254;
  TTest<TIntArray>.Test(L,L,'5:');
  TTest<TIntArray>.Test(L,R,'6:');
  TTest<TIntArray>.Test(R,L,'7:');
  TTest<TIntArray>.Test(R,R,'8:');
  L[0]:= -15;
  R[0]:= -255*254;
  TTest<TIntArray>.Test(L,L,'9:');
  TTest<TIntArray>.Test(L,R,'10:');
  TTest<TIntArray>.Test(R,L,'11:');
  TTest<TIntArray>.Test(R,R,'12:');
  TTest<TIntArray>.Test(L,Default(TIntArray),'13:');
  TTest<TIntArray>.Test(Default(TIntArray),L,'14:');
  TTest<TIntArray>.Test(Default(TIntArray),Default(TIntArray),'1:');
end;

procedure TestComplex.TestTBigArray;
var
  L,R: TBigArray;
  i: integer;
begin
  for i:= Low(TBigArray) to High(TBigArray) do begin
    L[i]:= Random(255);
    R[i]:= Random(255);
  end;
  TTest<TBigArray>.Test(L,L);
  TTest<TBigArray>.Test(L,R);
  TTest<TBigArray>.Test(R,L);
  TTest<TBigArray>.Test(R,R);
  TTest<TBigArray>.Test(L,Default(TBigArray));
  TTest<TBigArray>.Test(Default(TBigArray),L);
  TTest<TBigArray>.Test(Default(TBigArray),Default(TBigArray));
end;

procedure TestComplex.TestTRecArray;
var
  L,R: TRecArray;
  i: integer;
begin
  for i:= Low(TRecArray) to High(TRecArray) do begin
    L[i].a:= Random(255);
    L[i].b:= IntToStr(L[i].a)+IntToStr(L[i].a)+IntToStr(L[i].a);
    R[i].a:= Random(255);
    R[i].b:= IntToStr(R[i].a)+IntToStr(R[i].a)+IntToStr(R[i].a);
  end;
  TTest<TRecArray>.Test(L,L);
  TTest<TRecArray>.Test(L,R);
  TTest<TRecArray>.Test(R,L);
  TTest<TRecArray>.Test(R,R);
  TTest<TRecArray>.Test(L,Default(TRecArray));
  TTest<TRecArray>.Test(Default(TRecArray),L);
  TTest<TRecArray>.Test(Default(TRecArray),Default(TRecArray));
end;

procedure TestComplex.TestTDynByteArray;
var
  L,R: TByteDynArray;
begin
  SetLength(L,1);
  SetLength(R,1);
  L[0]:= 1;
  R[0]:= 255;
  TTest<TByteDynArray>.Test(L,L,'1:');
  TTest<TByteDynArray>.Test(L,R,'2:');
  TTest<TByteDynArray>.Test(R,L,'3:');
  TTest<TByteDynArray>.Test(R,R,'4:');
  TTest<TByteDynArray>.Test(L,Default(TByteDynArray),'5:');
  TTest<TByteDynArray>.Test(Default(TByteDynArray),L,'6:');
  TTest<TByteDynArray>.Test(Default(TByteDynArray),Default(TByteDynArray),'7:');
end;

procedure TestComplex.TestTDynWordArray;
var
  L,R: TWordDynArray;
begin
  SetLength(L,1);
  SetLength(R,1);
  L[0]:= 15;
  R[0]:= 255*254;
  TTest<TWordDynArray>.Test(L,L);
  TTest<TWordDynArray>.Test(L,R);
  TTest<TWordDynArray>.Test(R,L);
  TTest<TWordDynArray>.Test(R,R);
  TTest<TWordDynArray>.Test(L,Default(TWordDynArray));
  TTest<TWordDynArray>.Test(Default(TWordDynArray),L);
  TTest<TWordDynArray>.Test(Default(TWordDynArray),Default(TWordDynArray));
end;

procedure TestComplex.TestTDynIntArray;
var
  L,R: TIntDynArray;
begin
  SetLength(L,1);
  SetLength(R,1);
  L[0]:= 15;
  R[0]:= 255*254;
  TTest<TIntDynArray>.Test(L,L,'1:');
  TTest<TIntDynArray>.Test(L,R,'2:');
  TTest<TIntDynArray>.Test(R,L,'3:');
  TTest<TIntDynArray>.Test(R,R,'4:');
  L[0]:= 15;
  R[0]:= -255*254;
  TTest<TIntDynArray>.Test(L,L,'5:');
  TTest<TIntDynArray>.Test(L,R,'6:');
  TTest<TIntDynArray>.Test(R,L,'7:');
  TTest<TIntDynArray>.Test(R,R,'8:');
  L[0]:= -15;
  R[0]:= -255*254;
  TTest<TIntDynArray>.Test(L,L,'9:');
  TTest<TIntDynArray>.Test(L,R,'10:');
  TTest<TIntDynArray>.Test(R,L,'11:');
  TTest<TIntDynArray>.Test(R,R,'12:');
  TTest<TIntDynArray>.Test(L,Default(TIntDynArray),'13:');
  TTest<TIntDynArray>.Test(Default(TIntDynArray),L,'14:');
  TTest<TIntDynArray>.Test(Default(TIntDynArray),Default(TIntDynArray),'1:');
end;

procedure TestComplex.TestTDynBigArray;
var
  L,R: TBigDynArray;
  i: integer;
begin
  SetLength(L,SizeOf(TBigArray) div SizeOf(byte));
  SetLength(R,SizeOf(TBigArray) div SizeOf(byte));
  for i:= 0 to Length(L) - 1 do begin
    L[i]:= Random(255);
    R[i]:= Random(255);
  end;
  TTest<TBigDynArray>.Test(L,L);
  TTest<TBigDynArray>.Test(L,R);
  TTest<TBigDynArray>.Test(R,L);
  TTest<TBigDynArray>.Test(R,R);
  TTest<TBigDynArray>.Test(L,Default(TBigDynArray));
  TTest<TBigDynArray>.Test(Default(TBigDynArray),L);
  TTest<TBigDynArray>.Test(Default(TBigDynArray),Default(TBigDynArray));
end;

procedure TestComplex.TestTDynRecArray;
var
  L,R: TRecDynArray;
  i: integer;
begin
  SetLength(L,SizeOf(TRecArray) div SizeOf(TManagedRec));
  SetLength(R,SizeOf(TRecArray) div SizeOf(TManagedRec));
  for i:= 0 to Length(L)-1 do begin
    L[i].a:= Random(255);
    L[i].b:= IntToStr(L[i].a)+IntToStr(L[i].a)+IntToStr(L[i].a);
    R[i].a:= Random(255);
    R[i].b:= IntToStr(R[i].a)+IntToStr(R[i].a)+IntToStr(R[i].a);
  end;
  TTest<TRecDynArray>.Test(L,L);
  TTest<TRecDynArray>.Test(L,R);
  TTest<TRecDynArray>.Test(R,L);
  TTest<TRecDynArray>.Test(R,R);
  TTest<TRecDynArray>.Test(L,Default(TRecDynArray));
  TTest<TRecDynArray>.Test(Default(TRecDynArray),L);
  TTest<TRecDynArray>.Test(Default(TRecDynArray),Default(TRecDynArray));
end;

type
  VarType = (vrEmpty, vrNull, vrSmallint, vrInteger, vrSingle, vrDouble,
              vrCurrency, vrDate, vrOleStr, vrDispatch, vrError, vrBoolean,
              vrUnknown, vrShortInt, vrByte, vrWord, vrLongWord,
              vrInt64, vrUInt64, vrRecord,
              vrString, vrAny, vrUString);


{TODO -oJohan -cComplete cases : Implement VarFromRecord}
function SetVariant(Flavor: VarType): Variant;
const
  COleStr: WideString = '888';
var
  VarData1: TVarData absolute Result;
  //Rec: TManagedRec;
begin
  case Flavor of
    vrEmpty: Result:= Unassigned;
    vrNull: Result:= null;
    vrSmallint: begin
      VarData1.VType:= varSmallint;
      VarData1.VSmallInt:= Random(16000) - 8000;
    end;
    vrInteger: begin
      VarData1.VType:= varInteger;
      VarData1.VInteger:= Random(16000) - 8000;
    end;
    vrSingle: begin
      VarData1.VType:= varSingle;
      VarData1.VSingle:= Random(16000) - 8000;
    end;
    vrDouble: begin
      VarData1.VType:= varDouble;
      VarData1.VDouble:= Random(16000) - 8000;
    end;
    vrCurrency: begin
      VarData1.VType:= varCurrency;
      VarData1.VCurrency:= Random(16000) - 8000;
    end;
    vrDate: begin
      VarData1.VType:= varDate;
      VarData1.VDate:= Random(16000) - 8000;
    end;
    vrOleStr: begin
      Result:= COleStr;
    end;
    vrDispatch: begin
          //VarData1.VType:= varDispatch;
          //NativeUInt(VarData1.VDispatch):= $5454354;
    end;
    vrError: begin
      VarData1.VType:= VarError;
      VarData1.VError:= Random(16000) - 8000;
    end;
    vrBoolean: begin
      VarData1.VType:= VarBoolean;
      VarData1.VBoolean:= Odd(Random(3));
    end;
    vrUnknown: begin
      VarData1.VType:= VarUnknown;
      NativeUInt(VarData1.VUnknown):= Random(MaxInt);
    end;
    vrShortInt: begin
      VarData1.VType:= VarShortInt;
      VarData1.VShortInt:= Random(100) - 50;
    end;
    vrByte: begin
      VarData1.VType:= VarByte;
      VarData1.VShortInt:= Random(100);
    end;
    vrWord: begin
      VarData1.VType:= VarWord;
      VarData1.VShortInt:= Random(16000);
    end;
    vrLongWord: begin
      VarData1.VType:= VarLongWord;
      VarData1.VLongWord:= Random(16000);
    end;
    vrInt64: begin
      VarData1.VType:= VarInt64;
      VarData1.VInt64:= Random(16000) - 8000;
    end;
    vrUInt64: begin
      VarData1.VType:= VarUInt64;
      VarData1.VUInt64:= Random(16000);
    end;
    vrRecord: begin
      //Rec.a:= Random(128);
      //Rec.b:= Chr(Random(128)) + Chr(Random(128)) + Chr(Random(128)) + Chr(Random(128)) + Chr(Random(128));
      //Result:= VarFrom Rec;
      Result:= null;
    end;
    vrString: begin
      Result:= AnsiString('sfdgsfdgs');
    end;
    vrAny: begin
      Result:= '1';
    end;
    vrUString: begin
      Result:= UnicodeString('sndjsabdjs');
    end;
  end;
end;


procedure TestComplex.TestVariant;
var
  V1,V2: Variant;
  i1,i2: VarType;
  s1,s2: string;
  VarData1: TVarData absolute V1;
  VarData2: TVarData absolute V2;
  //Rec: TManagedRec;
begin
  for i1:= High(VarType) downto Low(VarType) do begin
    for i2:= Low(VarType) to High(VarType) do begin
      V1:= SetVariant(i1);
      V2:= SetVariant(i2);
      s1:= 'i1:'+IntToStr(Integer(i1));
      s2:= 'i2:'+IntToStr(Integer(i2));
      if (i2 = vrEmpty) then begin
        WriteLn(IntToStr(integer(i1)));
      end;
      TTest<Variant>.Test(V1,V2,s1+s2);
      TTest<Variant>.Test(V1,V1,s1+s1);
      TTest<Variant>.Test(V1,Default(variant),s1+'default');
      TTest<Variant>.Test(Default(variant),V1,'default'+s1);
    end;
  end;
end;

initialization
  //Floats
  TDUnitX.RegisterTestFixture(TTestFloat<Single>);
  TDUnitX.RegisterTestFixture(TTestFloat<Double>);
  TDUnitX.RegisterTestFixture(TTestFloat<Extended>);
  TDUnitX.RegisterTestFixture(TTestFloat<Currency>);
  TDUnitX.RegisterTestFixture(TTestFloat<Comp>);
  //Integers
  TDUnitX.RegisterTestFixture(TTestInteger<byte>);
  TDUnitX.RegisterTestFixture(TTestInteger<int8>);
  TDUnitX.RegisterTestFixture(TTestInteger<word>);
  TDUnitX.RegisterTestFixture(TTestInteger<int16>);
  TDUnitX.RegisterTestFixture(TTestInteger<cardinal>);
  TDUnitX.RegisterTestFixture(TTestInteger<integer>);
  TDUnitX.RegisterTestFixture(TTestInteger<Int64>);
  TDUnitX.RegisterTestFixture(TTestInteger<UInt64>);
  //String
  TDUnitX.RegisterTestFixture(TTestString<UnicodeString>);
  TDUnitX.RegisterTestFixture(TTestString<AnsiString>);
  TDUnitX.RegisterTestFixture(TTestString<WideString>);
  TDUnitX.RegisterTestFixture(TTestString<RawByteString>);
  TDUnitX.RegisterTestFixture(TTestString<UTF8String>);
  //Chars
  TDUnitX.RegisterTestFixture(TTestChar<AnsiChar>);
  TDUnitX.RegisterTestFixture(TTestChar<WideChar>);
  //Complex cases
  TDUnitX.RegisterTestFixture(TestComplex);
end.
