unit QuickerSort;

interface

{$T+}

uses
  System.Generics.Collections,
  FastDefaults;

type
  TArrayHelper = class helper for System.Generics.Collections.TArray
  strict private
    class procedure Swap<T>(var Values: array of T; L,R: integer); static;
  private
    class procedure QuickSort<T>(var Values: array of T;
                                 const Compare: TComparison<T>; L, R: Integer); static;
    class procedure BubbleSort<T>(var Values: array of T;
                                 const Compare: TComparison<T>; L, R: Integer); static;
  public
    class procedure Sort<T>(var Values: array of T); overload; static;
    class procedure Sort<T>(var Values: array of T;
                            const Compare: TComparison<T>); overload; static;
    class procedure Sort<T>(var Values: array of T;
                            const Comparer: IComparer<T>); overload; static;
    class procedure Sort<T>(var Values: array of T;
                            const Compare: TComparison<T>; Index, Count: Integer); overload; static;
    class procedure Sort<T>(var Values: array of T;
                            const Comparer: IComparer<T>; Index, Count: Integer); overload; static;
  end;

implementation

uses
  System.SysUtils, System.RTLConsts;

{ TArrayHelper }

class procedure TArrayHelper.Sort<T>(var Values: array of T;
  const Compare: TComparison<T>; Index, Count: Integer);
begin
  if (Index < Low(Values)) or ((Index > High(Values)) and (Count > 0))
    or (Index + Count - 1 > High(Values)) or (Count < 0)
    or (Index + Count < 0) then
    raise EArgumentOutOfRangeException.CreateRes(NativeInt(@SArgumentOutOfRange));
  if Count <= 1 then Exit;
  QuickSort<T>(Values, Compare, Index, Index + Count - 1);
end;

class procedure TArrayHelper.Sort<T>(var Values: array of T;
  const Comparer: IComparer<T>; Index, Count: Integer);
begin
  Sort<T>(Values, TComparison<T>(@Comparer.Compare), Index, Count);
end;

class procedure TArrayHelper.Sort<T>(var Values: array of T;
  const Compare: TComparison<T>);
begin
  QuickSort<T>(Values, Compare, Low(Values), High(Values));
end;

class procedure TArrayHelper.Sort<T>(var Values: array of T;
  const Comparer: IComparer<T>);
begin
  Sort<T>(Values, TComparison<T>(@Comparer.Compare), Low(Values), High(Values));
end;

class procedure TArrayHelper.Sort<T>(var Values: array of T);
begin
  Sort<T>(Values, TComparison<T>(@TComparer<T>.Default.Compare), Low(Values), High(Values));
end;

{TODO -oJohan -cPivot : Choose best of 3 for the pivot}
{TODO -oJohan -cDutchFlag : Change quicksort to DutchFlagVersion}

class procedure TArrayHelper.QuickSort<T>(var Values: array of T;
  const Compare: TComparison<T>; L, R: Integer);
var
  I, J: Integer;
  pivot, temp: T;
begin
  if (Length(Values) = 0) or ((R - L) <= 0) then Exit;
  repeat
    I:= L;
    J:= R;
    pivot:= Values[L + (R - L) shr 1];
    repeat
      while Compare(Values[I], pivot) < 0 do Inc(I);
      while Compare(Values[J], pivot) > 0 do Dec(J);
      if I <= J then begin
        if I <> J then begin
          if (SizeOf(T) > SizeOf(NativeInt)) then Swap(Values, I, J)
          else begin
            temp:= Values[I];
            Values[I]:= Values[J];
            Values[J]:= temp;
          end;
        end;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then begin
      if ((J - L) <= 7) then BubbleSort<T>(Values, Compare, L, J)
      else QuickSort<T>(Values, Compare, L, J);
    end;
    L:= I;
  until I >= R;
end;


class procedure TArrayHelper.BubbleSort<T>(var Values: array of T;
                const Compare: TComparison<T>; L, R: Integer);
var
  I, J: Integer;
  Temp: T;
begin
  for I:= R downto L do begin
    for J:= L to R - 1 do begin
      if (Compare(Values[J], Values[J + 1]) > 0) then begin
        if (SizeOf(T) > SizeOf(NativeInt)) then Swap(Values, I, J)
        else begin
          temp:= Values[J];
          Values[J]:= Values[J + 1];
          Values[J + 1]:= temp;
        end;
      end;
    end;
  end;
end;

{TODO -oJohan -cImplement : Put sensible code in swap}
class procedure TArrayHelper.Swap<T>(var Values: array of T; L, R: integer);
var
  Temp: T;
begin
  Temp:= Values[L];
  Values[L]:= Values[R];
  Values[R]:= Temp;
end;

end.
