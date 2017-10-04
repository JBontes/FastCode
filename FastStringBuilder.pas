unit FastStringBuilder;

interface

uses SysUtils;

type
  TStringBuilder = class
  private const
    DefaultCapacity = $10;
  private
    procedure SetCapacity(Value: NativeUInt);
    function GetChars(Index: NativeUInt): Char;
    procedure SetChars(Index: NativeUInt; Value: Char);
    //function GetLength: Integer; inline;
    procedure SetLength(Value: NativeUint); inline;
    procedure ExpandCapacity(NewLength: NativeUInt);
    procedure ReduceCapacity;
    procedure CheckBounds(Index: NativeUint);
    function _Replace(Index: NativeUint; const Old, New: string): Boolean;
  protected
    FData: TCharArray;
    FLength: NativeUInt;
    FCapacity: NativeUint; //Avoid the need for nil testing the underlying storage.
    FMaxCapacity: NativeUInt;
  public
    constructor Create; overload;
    constructor Create(aCapacity: NativeUInt); overload;
    constructor Create(const Value: string); overload;
    constructor Create(aCapacity, aMaxCapacity: NativeUInt); overload;
    constructor Create(const Value: string; aCapacity: NativeUInt); overload;
    constructor Create(const Value: string; StartIndex, Length, aCapacity: NativeUInt); overload;

    function Append(const Value: string): TStringBuilder; overload;  //put first, so the inlines work.
    function Append(const Value: Boolean): TStringBuilder; overload; inline;
    function Append(const Value: Byte): TStringBuilder; overload; inline;
    function Append(const Value: Char): TStringBuilder; overload;
    function Append(const Value: Currency): TStringBuilder; overload; inline;
    function Append(const Value: Double): TStringBuilder; overload; inline;
    function Append(const Value: Smallint): TStringBuilder; overload; inline;
    function Append(const Value: Integer): TStringBuilder; overload; inline;
    function Append(const Value: Int64): TStringBuilder; overload; inline;
    function Append(const Value: TObject): TStringBuilder; overload; inline;
    function Append(const Value: Shortint): TStringBuilder; overload; inline;
    function Append(const Value: Single): TStringBuilder; overload; inline;
    function Append(const Value: UInt64): TStringBuilder; overload; inline;
    function Append(const Value: TCharArray): TStringBuilder; overload;
    function Append(const Value: Word): TStringBuilder; overload; inline;
    function Append(const Value: Cardinal): TStringBuilder; overload; inline;
{$IFNDEF NEXTGEN}
    function Append(const Value: PAnsiChar): TStringBuilder; overload; inline;
    function Append(const Value: RawByteString): TStringBuilder; overload; inline;
{$ENDIF !NEXTGEN}
    function Append(const Value: Char; RepeatCount: NativeUInt): TStringBuilder; overload; inline;
    function Append(const Value: TCharArray; StartIndex, CharCount: NativeUInt): TStringBuilder; overload;
    function Append(const Value: string; StartIndex, Count: NativeUInt): TStringBuilder; overload;

    function AppendFormat(const Format: string; const Args: array of const): TStringBuilder; overload;
    function AppendLine: TStringBuilder; overload; inline;
    function AppendLine(const Value: string): TStringBuilder; overload; inline;
    procedure Clear;
    procedure CopyTo(SourceIndex: NativeUInt; const Destination: TCharArray; DestinationIndex, Count: NativeUInt);
    function EnsureCapacity(aCapacity: NativeUInt): NativeUInt;
    function Equals(StringBuilder: TStringBuilder): Boolean; reintroduce;

    function Insert(Index: NativeUInt; const Value: Boolean): TStringBuilder; overload; inline;
    function Insert(Index: NativeUInt; const Value: Byte): TStringBuilder; overload; inline;
    function Insert(Index: NativeUInt; const Value: Char): TStringBuilder; overload;
    function Insert(Index: NativeUInt; const Value: Currency): TStringBuilder; overload; inline;
    function Insert(Index: NativeUInt; const Value: Double): TStringBuilder; overload; inline;
    function Insert(Index: NativeUInt; const Value: Smallint): TStringBuilder; overload; inline;
    function Insert(Index: NativeUInt; const Value: Integer): TStringBuilder; overload; inline;
    function Insert(Index: NativeUInt; const Value: TCharArray): TStringBuilder; overload;
    function Insert(Index: NativeUInt; const Value: Int64): TStringBuilder; overload; inline;
    function Insert(Index: NativeUInt; const Value: TObject): TStringBuilder; overload; inline;
    function Insert(Index: NativeUInt; const Value: Shortint): TStringBuilder; overload; inline;
    function Insert(Index: NativeUInt; const Value: Single): TStringBuilder; overload; inline;
    function Insert(Index: NativeUInt; const Value: string): TStringBuilder; overload;
    function Insert(Index: NativeUInt; const Value: Word): TStringBuilder; overload; inline;
    function Insert(Index: NativeUInt; const Value: Cardinal): TStringBuilder; overload; inline;
    function Insert(Index: NativeUInt; const Value: UInt64): TStringBuilder; overload; inline;
    function Insert(Index: NativeUInt; const Value: string; count: NativeUInt): TStringBuilder; overload;
    function Insert(Index: NativeUInt; const Value: TCharArray; startIndex: NativeUInt; charCount: NativeUInt): TStringBuilder; overload;

    function Remove(StartIndex: Integer; RemLength: Integer): TStringBuilder;

    function Replace(const OldChar: Char; const NewChar: Char): TStringBuilder; overload;
    function Replace(const OldValue: string; const NewValue: string): TStringBuilder; overload;
    function Replace(const OldChar: Char; const NewChar: Char; StartIndex, Count: NativeUInt): TStringBuilder; overload;
    function Replace(const OldValue: string; const NewValue: string; StartIndex, Count: NativeUInt): TStringBuilder; overload;

    function ToString: string; overload; override;
    function ToString(StartIndex: NativeUInt; StrLength: Cardinal): string; reintroduce; overload;

    property Capacity: NativeUInt read FCapacity write SetCapacity;
    property Chars[index: NativeUInt]: Char read GetChars write SetChars; default;
    property Length: NativeUInt read FLength write SetLength;
    property MaxCapacity: NativeUInt read FMaxCapacity;
  end;



implementation

uses
  System.SysConst, System.RTLConsts;

{ TStringBuilder }

//function TStringBuilder.GetLength: Integer;
//begin
//  Result := FLength;
//end;

procedure TStringBuilder.SetLength(Value: NativeUInt);
begin
  if Value > Capacity then ExpandCapacity(Value);
  FLength:= Value;
end;

function TStringBuilder.Append(const Value: string): TStringBuilder;
var
  L: NativeUInt;
begin
  Result:= Self;
  L:= System.Length(Value);
  if (Length + L) > Capacity then ExpandCapacity(Length + L);
  Move(pointer(Value)^, FData[Length], L * SizeOf(Char));
  FLength := FLength + L;
  //Move(Value[low(string)], FData[Length - L], L * SizeOf(Char));
end;


function TStringBuilder.Append(const Value: UInt64): TStringBuilder;
begin
  Result:= Append(UIntToStr(Value));
end;

function TStringBuilder.Append(const Value: TCharArray): TStringBuilder;
var
  I: Integer;
begin
  Result := self;

  for I := 0 to System.Length(Value) - 1 do
    if Value[I] = #0 then
      Break;

  Append(Value, 0, I);
end;

function TStringBuilder.Append(const Value: Single): TStringBuilder;
begin
  Result:= Append(FloatToStr(Value));
end;

function TStringBuilder.Append(const Value: Word): TStringBuilder;
begin
  Result:= Append(IntToStr(Value));
end;

function TStringBuilder.Append(const Value: TCharArray; StartIndex,
  CharCount: NativeUInt): TStringBuilder;
begin
  if StartIndex + CharCount > System.Length(Value) then
    raise ERangeError.CreateResFmt(@SListIndexError, [StartIndex]);


  Length := Length + CharCount;
  Move(Value[StartIndex], FData[Length - CharCount], CharCount * SizeOf(Char));
  Result := self;
end;

function TStringBuilder.Append(const Value: string; StartIndex,
  Count: NativeUInt): TStringBuilder;
begin
  if StartIndex + Count > System.Length(Value) then
    raise ERangeError.CreateResFmt(@SListIndexError, [StartIndex]);

  Length := Length + Count;
  Move(Value[StartIndex + Low(string)], FData[Length - Count], Count * SizeOf(Char));
  Result := Self;
end;

{$IFNDEF NEXTGEN}
function TStringBuilder.Append(const Value: PAnsiChar): TStringBuilder;
begin
  Result:= Append(string(Value));
end;

function TStringBuilder.Append(const Value: RawByteString): TStringBuilder;
begin
  Result:= Append(string(Value));
end;
{$ENDIF !NEXTGEN}

function TStringBuilder.Append(const Value: Cardinal): TStringBuilder;
begin
  Result:= Append(UIntToStr(Value));
end;

function TStringBuilder.Append(const Value: Char;
  RepeatCount: NativeUInt): TStringBuilder;
begin
  Result:= Append(System.StringOfChar(Value, RepeatCount));
end;

function TStringBuilder.Append(const Value: Shortint): TStringBuilder;
begin
  Result:= Append(IntToStr(Value));
end;

function TStringBuilder.Append(const Value: Char): TStringBuilder;
begin
  Length := Length + 1;
  FData[Length] := Value;
  Result := Self;
end;

function TStringBuilder.Append(const Value: Currency): TStringBuilder;
begin
  Result:= Append(CurrToStr(Value));
end;

function TStringBuilder.Append(const Value: Boolean): TStringBuilder;
begin
  case Value of
    true: Result:= Append('True');
    false: Result:= Append('False');
  end;
end;

function TStringBuilder.Append(const Value: Byte): TStringBuilder;
begin
  Result:= Append(IntToStr(Value));
end;

function TStringBuilder.Append(const Value: Double): TStringBuilder;
begin
  Result:= Append(FloatToStr(Value));
end;

function TStringBuilder.Append(const Value: Int64): TStringBuilder;
begin
  Append(IntToStr(Value));
end;

function TStringBuilder.Append(const Value: TObject): TStringBuilder;
begin
{$if CompilerVersion >= 19}
  Result:= Append(Value.ToString());
{$else}
  Result:= Append(IntToStr(Integer(Value)));
{$ENDIF}
end;

function TStringBuilder.Append(const Value: Smallint): TStringBuilder;
begin
  Result:= Append(IntToStr(Value));
end;

function TStringBuilder.Append(const Value: Integer): TStringBuilder;
begin
  Result:= Append(IntToStr(Value));
end;

function TStringBuilder.AppendFormat(const Format: string; const Args: array of const): TStringBuilder;
begin
  Result:= Append(System.SysUtils.Format(Format, Args));
end;

function TStringBuilder.AppendLine: TStringBuilder;
begin
  Result:= Append(sLineBreak);
end;

function TStringBuilder.AppendLine(const Value: string): TStringBuilder;
begin
  Append(Value);
  Result:= AppendLine;
end;

procedure TStringBuilder.Clear ;
begin
  Length := 0;
  Capacity := DefaultCapacity;
end;

procedure TStringBuilder.CheckBounds(Index: NativeUInt);
begin
  if Index >= Length then
    raise ERangeError.CreateResFmt(@SListIndexError, [Index]);
end;

procedure TStringBuilder.CopyTo(SourceIndex: NativeUInt;
  const Destination: TCharArray; DestinationIndex, Count: NativeUInt);
begin
  if DestinationIndex + Count > System.Length(Destination) then
    raise ERangeError.CreateResFmt(@SInputBufferExceed,
      ['DestinationIndex', DestinationIndex, 'Count', Count]);

  if Count > 0 then
  begin
    CheckBounds(SourceIndex);
    CheckBounds(SourceIndex + Count - 1);

    Move(FData[SourceIndex], Destination[DestinationIndex], Count * SizeOf(Char));
  end;
end;

constructor TStringBuilder.Create;
begin
  inherited Create;
  FMaxCapacity := MaxInt;
  Capacity := DefaultCapacity;
  //FLength := 0;  //fields are zero initialized
end;

constructor TStringBuilder.Create(const Value: string; aCapacity: NativeUInt);
begin
  inherited Create;
  FMaxCapacity := MaxInt;
  Capacity := aCapacity;
  //FLength := 0;
  Append(Value);
end;

constructor TStringBuilder.Create(const Value: string; StartIndex, Length,
  aCapacity: NativeUInt);
begin
  //Create(Copy(Value, StartIndex + 1, length), aCapacity);
  Create(Value.Substring( StartIndex, length), aCapacity);
end;

constructor TStringBuilder.Create(aCapacity, aMaxCapacity: NativeUInt);
begin
  Create(aCapacity);
  FMaxCapacity := aMaxCapacity;
end;

constructor TStringBuilder.Create(aCapacity: NativeUInt);
begin
  inherited Create;
  FMaxCapacity := MaxInt;
  Capacity := aCapacity;
  FLength := 0;
end;

constructor TStringBuilder.Create(const Value: string);
begin
  Create;
  Append(Value);
end;

function TStringBuilder.EnsureCapacity(aCapacity: NativeUInt): NativeUInt;
begin
  if aCapacity > MaxCapacity then
    raise ERangeError.CreateResFmt(@SListIndexError, [aCapacity]);

  if Capacity < aCapacity then
    Capacity := aCapacity;

  Result := Capacity;
end;

function TStringBuilder.Equals(StringBuilder: TStringBuilder): Boolean;
begin
  Result := (StringBuilder <> nil) and (Length = StringBuilder.Length) and
    (MaxCapacity = StringBuilder.MaxCapacity) and
    CompareMem(@FData[0], @StringBuilder.FData[0], Length * SizeOf(Char));
end;

function Max(a,b: NativeUInt): NativeUInt; overload; inline;
begin
  //jumpless max.
  Result:= a xor ((a xor b) and -integer(a < b));
  //Result = a.
  //if (a >= b) then x:= 0; Result:= a xor 0;
  //if (a < b) then x:= -1; Result:= a xor (a xor b) and -1
  //                                                 ^^^^^^ nop
  //                                 ^^^^^^^^^^^^^^^swap a and b
  //                        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ Result:= b;
end;

function Min(a,b: NativeUInt): NativeUInt; overload; inline;
begin
  Result:= a xor ((a xor b) and -integer(a > b));
end;

procedure TStringBuilder.ExpandCapacity(NewLength: NativeUInt);
var
  NewCapacity: NativeUint;  //do not worry about overflow.
begin
  NewCapacity := Max(Capacity * 2, (NewLength * 3 div 2));
  Capacity:= Min(NewCapacity, MaxCapacity);
end;


function TStringBuilder.GetChars(Index: NativeUInt): Char;
begin
  CheckBounds(Index);

  Result := FData[Index];
end;

//function TStringBuilder.GetMaxCapacity: Integer;
//begin
//  Result := FMaxCapacity;
//end;

function TStringBuilder.Insert(Index: NativeUInt; const Value: TObject): TStringBuilder;
begin
{$if CompilerVersion >= 19}
  Result:= Insert(Index, Value.ToString());
{$else}
  Result:= Insert(Index, IntToStr(Integer(Value)));
{$ENDIF}
end;

function TStringBuilder.Insert(Index: NativeUInt; const Value: Int64): TStringBuilder;
begin
  Result:= Insert(Index, IntToStr(Value));
end;

function TStringBuilder.Insert(Index: NativeUInt; const Value: Single): TStringBuilder;
begin
  Result:= Insert(Index, FloatToStr(Value));
end;

function TStringBuilder.Insert(Index: NativeUInt; const Value: string): TStringBuilder;
begin
  if Index > Length then
    raise ERangeError.CreateResFmt(@SListIndexError, [Index]);

  Length := Length + System.Length(Value);
  Move(FData[Index], FData[Index + System.Length(Value)], (Length - System.Length(Value) - Index) * SizeOf(Char));
  Move(Value[Low(string)], FData[Index], System.Length(Value) * SizeOf(Char));
  Result := Self;
end;

function TStringBuilder.Insert(Index: NativeUInt; const Value: Word): TStringBuilder;
begin
  Result:= Insert(Index, IntToStr(Value));
end;

function TStringBuilder.Insert(Index: NativeUInt; const Value: Shortint): TStringBuilder;
begin
  Result:= Insert(Index, IntToStr(Value));
end;

function TStringBuilder.Insert(Index: NativeUInt;
  const Value: TCharArray): TStringBuilder;
begin
  if Index > Length then
    raise ERangeError.CreateResFmt(@SListIndexError, [Index]);

  Length := Length + System.Length(Value);
  Move(FData[Index], FData[Index + System.Length(Value)], System.Length(Value) * SizeOf(Char));
  Move(Value[0], FData[Index], System.Length(Value) * SizeOf(Char));
  Result := Self;
end;

function TStringBuilder.Insert(Index: NativeUInt; const Value: Currency): TStringBuilder;
begin
  Result:= Insert(Index, CurrToStr(Value));
end;

function TStringBuilder.Insert(Index: NativeUInt; const Value: Char): TStringBuilder;
begin
  if Index > Length then
    raise ERangeError.CreateResFmt(@SListIndexError, [Index]);

  Length := Length + 1;
  Move(FData[Index], FData[Index + 1], (Length - Index - 1) * SizeOf(Char));
  FData[Index] := Value;
  Result := Self;
end;

function TStringBuilder.Insert(Index: NativeUInt; const Value: Byte): TStringBuilder;
begin
  Result:= Insert(Index, IntToStr(Value));
end;

function TStringBuilder.Insert(Index: NativeUInt; const Value: Double): TStringBuilder;
begin
  Result:= Insert(Index, FloatToStr(Value));
end;

function TStringBuilder.Insert(Index: NativeUInt; const Value: Integer): TStringBuilder;
begin
  Result:= Insert(Index, IntToStr(Value));
end;

function TStringBuilder.Insert(Index: NativeUInt; const Value: Smallint): TStringBuilder;
begin
  Result:= Insert(Index, IntToStr(Value));
end;

function TStringBuilder.Insert(Index: NativeUInt; const Value: Boolean): TStringBuilder;
begin
  Result:= Insert(Index, BoolToStr(Value, True));
end;

function TStringBuilder.Insert(Index: NativeUInt; const Value: string;
  Count: NativeUInt): TStringBuilder;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Insert(Index, Value);
  Result := Self;
end;

function TStringBuilder.Insert(Index: NativeUInt; const Value: TCharArray; StartIndex,
  CharCount: NativeUInt): TStringBuilder;
begin
  if Index - 1 >= Length then
    raise ERangeError.CreateResFmt(@SListIndexError, [Index]);
  if StartIndex + CharCount > System.Length(Value) then
    raise ERangeError.CreateResFmt(@SInputBufferExceed,
      ['StartIndex', StartIndex, 'CharCount', CharCount]);

  Length := Length + CharCount;

  if Length - Index > 0 then
    Move(FData[Index], FData[Index + CharCount], (Length - Index) * SizeOf(Char));
  Move(Value[StartIndex], FData[Index], CharCount * SizeOf(Char));
  Result := Self;
end;

function TStringBuilder.Insert(Index: NativeUInt; const Value: Cardinal): TStringBuilder;
begin
  Result:= Insert(Index, IntToStr(Value));
end;

function TStringBuilder.Insert(Index: NativeUInt; const Value: UInt64): TStringBuilder;
begin
  Result:= Insert(Index, UIntToStr(Value));
end;

procedure TStringBuilder.ReduceCapacity;
var
  NewCapacity: Integer;
begin
  if Length > Capacity div 4 then
    Exit;

  NewCapacity := Capacity div 2;
  if NewCapacity < Length then
    NewCapacity := Length;
  Capacity := NewCapacity;
end;

function TStringBuilder.Remove(StartIndex, RemLength: Integer): TStringBuilder;
begin
  if RemLength <> 0 then
  begin
    CheckBounds(StartIndex);
    CheckBounds(StartIndex + RemLength - 1);

    if (Length - (StartIndex + RemLength)) > 0 then
      Move(FData[StartIndex + RemLength], FData[StartIndex], (Length - (StartIndex + RemLength)) * SizeOf(Char));
    Length := Length - RemLength;

    ReduceCapacity;
  end;
  Result := Self;
end;

function TStringBuilder.Replace(const OldValue, NewValue: string; StartIndex,
  Count: NativeUInt): TStringBuilder;
var
  CurPtr: PChar;
  EndPtr: PChar;
  Index: Integer;
  EndIndex: Integer;
  OldLen, NewLen: Integer;
begin
  Result := Self;

  if Count <> 0 then
  begin
    if StartIndex + Count > Length then
      raise ERangeError.CreateResFmt(@SInputBufferExceed,
        ['StartIndex', StartIndex, 'Count', Count]);

    OldLen := System.Length(OldValue);
    NewLen := System.Length(NewValue);
    Index := StartIndex;
    CurPtr := @FData[StartIndex];
    EndIndex := StartIndex + Count - OldLen;
    EndPtr := @FData[EndIndex];

    while CurPtr <= EndPtr do
    begin
      if CurPtr^ = OldValue[Low(string)] then
      begin
        if StrLComp(CurPtr, PChar(OldValue), OldLen) = 0 then
        begin
          if _Replace(Index, OldValue, NewValue) then
          begin
            CurPtr := @FData[Index];
            EndPtr := @FData[EndIndex];
          end;
          Inc(CurPtr, NewLen - 1);
          Inc(Index, NewLen - 1);
          Inc(EndPtr, NewLen - OldLen);
          Inc(EndIndex, NewLen - OldLen);
        end;
      end;

      Inc(CurPtr);
      Inc(Index);
    end;
  end;
end;

function TStringBuilder.Replace(const OldChar, NewChar: Char; StartIndex,
  Count: NativeUInt): TStringBuilder;
var
  Ptr: PChar;
  EndPtr: PChar;
begin
  if Count <> 0 then
  begin
    CheckBounds(StartIndex);
    CheckBounds(StartIndex + Count - 1);

    EndPtr := @FData[StartIndex + Count - 1];
    Ptr := @FData[StartIndex];
    while Ptr <= EndPtr do
    begin
      if Ptr^ = OldChar then
        Ptr^ := NewChar;
      Inc(Ptr);
    end;
  end;
  Result := Self;
end;

function TStringBuilder.Replace(const OldChar, NewChar: Char): TStringBuilder;
var
  Ptr: PChar;
  EndPtr: PChar;
begin
  EndPtr := @FData[Length - 1];
  Ptr := @FData[0];
  while Ptr <= EndPtr do
  begin
    if Ptr^ = OldChar then
      Ptr^ := NewChar;
    Inc(Ptr);
  end;
  Result := Self;
end;

function TStringBuilder.Replace(const OldValue, NewValue: string): TStringBuilder;
begin
  Result := self;
  Replace(OldValue, NewValue, 0, Length);
end;

procedure TStringBuilder.SetCapacity(Value: NativeUInt);
begin
  if Value < Length then
    raise ERangeError.CreateResFmt(@SListCapacityError, [Value]);
  if Value > FMaxCapacity then
    raise ERangeError.CreateResFmt(@SListCapacityError, [Value]);

  System.SetLength(FData, Value);
  FCapacity:= Value;
end;

procedure TStringBuilder.SetChars(Index: NativeUInt; Value: Char);
begin
  CheckBounds(Index);

  FData[Index] := Value;
end;


function TStringBuilder.ToString: string;
begin
  SetString(Result, MarshaledString(FData), Length);
end;

function TStringBuilder.ToString(StartIndex: NativeUInt; StrLength: Cardinal): string;
begin
  if StrLength <> 0 then
  begin
    CheckBounds(StartIndex);
    CheckBounds(StartIndex + StrLength - 1);

    Result := string.Create(FData, StartIndex, StrLength);
  end else
    Result := '';
end;

function TStringBuilder._Replace(Index: NativeUInt; const Old, New: string): Boolean;
var
  OldLength: Integer;
  OldCapacity: Integer;
  SizeChange: Integer;
begin
  Result := False;
  SizeChange := System.Length(New) - System.Length(Old);

  if SizeChange = 0 then
  begin
    Move(New[Low(string)], FData[Index], System.Length(New) * SizeOf(Char));
  end
  else
  begin
    OldLength := Length;
    if SizeChange > 0 then
    begin
      OldCapacity := Capacity;
      Length := Length + SizeChange;
      if OldCapacity <> Capacity then
        Result := True;
    end;

    Move(FData[Index + System.Length(Old)], FData[Index + System.Length(New)],
      (OldLength - (System.Length(Old) + Index)) * SizeOf(Char));
    Move(New[Low(String)], FData[Index], System.Length(New) * SizeOf(Char));

    if SizeChange < 0 then
      Length := Length + SizeChange;
  end;
end;

end.
