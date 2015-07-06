unit QuickerSort;

interface

{$T+}

uses
  System.Generics.Collections,
  FastDefaults;

type
  TArrayHelper = class helper for System.Generics.Collections.TArray
  strict private
  type
    TSort<T> = record
    type
      PT = ^T;
    public
      class procedure InsertionSort(const Left, Right: PT); static;
      class procedure QuickSort3(Left, Right: PT); static;
    end;
  type
    TNormalArray<T> = array [0 .. 0] of T;
    TDynArray<T> = array of T;
  private
    class procedure QuickSortOld<T>(var Values: array of T; const Comparer: IComparer<T>;L, R: Integer); static;
    //class procedure QuickSort3<T>(var a: array of T; L, R: Integer); static;
    class procedure QuickSort<T>(var Values: array of T; L, R: Integer); static;
    class procedure DualPivotQuicksort<T>(var a: array of T; const Compare: TComparison<T>;
      Left, Right, divider: Integer); static;
    class procedure InsertionSort<T>(var Values: array of T; L, R: Integer); static;
  public
    class procedure Sort2<T>(var Values: array of T); overload; static;
    class procedure SortStackOverflow<T>(var Values: array of T); overload; static;
    class procedure SortEmba<T>(var Values: array of T); overload; static;
    class procedure Sort<T>(var Values: array of T); overload; static;
    class procedure Sort<T>(var Values: array of T; const Compare: TComparison<T>); overload; static;
    class procedure Sort<T>(var Values: array of T; const Comparer: IComparer<T>); overload; static;
    class procedure Sort<T>(var Values: array of T; const Compare: TComparison<T>; Index, Count: Integer);
      overload; static;
    class procedure Sort<T>(var Values: array of T; const Comparer: IComparer<T>; Index, Count: Integer);
      overload; static;
  end;

implementation

uses
  System.SysUtils, System.RTLConsts;

{ TArrayHelper }

class procedure TArrayHelper.Sort<T>(var Values: array of T; const Compare: TComparison<T>; Index, Count: Integer);
begin
  if (index < low(Values)) or ((index > high(Values)) and (Count > 0)) or (index + Count - 1 > high(Values)) or
    (Count < 0) or (index + Count < 0) then
      raise EArgumentOutOfRangeException.CreateRes(NativeInt(@SArgumentOutOfRange));
  if Count <= 1 then Exit;
  DualPivotQuicksort<T>(Values, Compare, index, index + Count - 1, 3);
end;

class procedure TArrayHelper.Sort<T>(var Values: array of T; const Comparer: IComparer<T>; Index, Count: Integer);
begin
  Sort<T>(Values, TComparison<T>(@Comparer.Compare), index, Count);
end;

class procedure TArrayHelper.Sort<T>(var Values: array of T; const Compare: TComparison<T>);
begin
  Sort<T>(Values, Compare, low(Values), high(Values) + 1);
end;

class procedure TArrayHelper.Sort<T>(var Values: array of T; const Comparer: IComparer<T>);
begin
  Sort<T>(Values, TComparison<T>(@Comparer.Compare), low(Values), high(Values));
end;

class procedure TArrayHelper.Sort<T>(var Values: array of T);
begin
  Sort<T>(Values, TComparison<T>(@TComparer<T>.Default.Compare), low(Values), high(Values) + 1);
end;

class procedure TArrayHelper.Sort2<T>(var Values: array of T);
begin
  QuickSort<T>(Values, low(Values), High(Values));
end;

class procedure TArrayHelper.SortStackOverflow<T>(var Values: array of T);
begin
  if Length(Values) <= 1 then exit;
  TSort<T>.QuickSort3(@Values[0], (@Values[High(Values)]));
end;


class procedure TArrayHelper.SortEmba<T>(var Values: array of T);
begin
  QuickSortOld<T>(Values, FastDefaults.TComparer<T>.Default, 0, High(Values));
end;



{TODO -oJohan -cPivot : Choose best of 3 for the pivot}
{TODO -oJohan -cDutchFlag : Change quicksort to DutchFlagVersion}

class procedure TArrayHelper.QuickSort<T>(var Values: array of T; L, R: Integer);
const
  BubbleLimit = 27;
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
      while true do begin
        if TComparer<T>.Default.Compare(Values[I], pivot) < 0 then Inc(I)
        else break;
      end;
      while true do begin
        if TComparer<T>.Default.Compare(Values[J], pivot) > 0 then Dec(J)
        else break;
      end;
      if I <= J then begin
        if I <> J then begin
          temp:= Values[I];
          Values[I]:= Values[J];
          Values[J]:= temp;
        end;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then begin
      if ((J - L) <= BubbleLimit) then TSort<T>.InsertionSort(@Values[L], @Values[J])  //InsertionSort<T>(Values, L, J)
      else QuickSort<T>(Values, L, J);
    end;
    L:= I;
  until I >= R;

end;

procedure QuickSort3(var A: array of integer; Left, Right: integer);
var
  Hi, Lo, Temp, Pivot: integer;
begin
  repeat
    if (Right-Left) > 16 then begin
      Pivot := A[(Right + Left) shr 1];
      Lo := Left;
      Hi := Right;
      repeat
        while A[Lo] < Pivot do inc(Lo);
        while a[Hi] > pivot do Dec(Hi);
        if Lo <= Hi then begin
          if Lo <> Hi then begin
            temp:= a[Lo];
            a[Lo]:= a[Hi];
            a[Hi]:= temp;
          end;
          Inc(Lo);
          Dec(Hi);
        end;
      until Hi < Lo;
      if Hi > Left then QuickSort3(a, Left, Hi);
      Left:= Lo;
    end else begin
      for Lo:= Left + 1 to Right do begin
        temp:= a[Lo];
        Hi:= Lo;
        while (Hi > Left) and (a[Hi - 1] > temp) do begin
          a[Hi]:= a[Hi - 1];
          Dec(Hi);
        end;
        a[Hi]:= temp;
      end;
      Exit;
    end;
  until Right <= Lo;
end;

class procedure TArrayHelper.TSort<T>.QuickSort3(Left, Right: PT);
const
  forever = false;
  InsertionLimit = 16;
type
  nu = NativeUInt;
var
  Lo, Hi, Pivot: PT;
  Temp: T;
begin
  if nu(Left) >= nu(Right) then exit;
  repeat
    //if (Right-Left) > 16 then
    if (nu((nu(Right) - nu(Left))) div SizeOf(T)) > InsertionLimit then begin
      //Pivot := A[(Right + Left) shr 1];
      nu(Pivot):= (nu((nu(Left) shr 1) + (nu(Right) shr 1)));
      Assert((nu(Pivot) > nu(Left)) and (nu(Pivot) < nu(Right)));
//      Lo := Left;
//      Hi := Right;
      Lo:= Left;
      Hi:= Right;
      repeat
        //while A[Lo] < Pivot do inc(Lo);
        repeat
          if TComparer<T>.Default.Compare(Lo^,Pivot^) < 0 then Inc(Lo)
          else Break;
        until forever;
        Assert((nu(Pivot) > nu(Left)) and (nu(Pivot) < nu(Right)));
        //while A[Hi] > Pivot do dec(Hi);
        repeat
          if TComparer<T>.Default.Compare(Hi^,Pivot^) > 0 then Dec(Hi)
          else Break;
        until forever;
        Assert((nu(Pivot) > nu(Left)) and (nu(Pivot) < nu(Right)));
        //if Lo <= Hi then begin
        if nu(Lo) <= nu(Hi) then begin
          //if Lo <> Hi then begin
          if nu(Lo) <> nu(Hi) then begin
            Temp:= lo^;
            Lo^:= Hi^;
            Hi^:= Temp;
          end;
          //Inc(Lo);
          //Dec(Hi);
          Inc(Lo);
          Dec(Hi);
        end;
        Assert((nu(Pivot) > nu(Left)) and (nu(Pivot) < nu(Right)));
      //until Hi < Lo;
      until (nu(Hi) < nu(lo));
      //if Hi > Left then QuickSort3(a, Left, Hi);
      if nu(Hi) > nu(Left) then QuickSort3(Left, Hi);
      //Left:= Lo;
      Left:= Lo;
    end else begin
//      //Insertion sort
//      for Lo:= Left + 1 to Right do begin
//        temp:= a[Lo];
//        Hi:= Lo;
//        repeat
//          if (Hi > Left) and (TComparer<T>.Default.Compare(a[Hi - 1], temp) > 0) then begin
//            a[Hi]:= a[Hi - 1];
//            Dec(Hi);
//          end
//          else break;
//        until forever;
//        a[Hi]:= temp;
//      end;
      InsertionSort(Left,Right);
      Exit;
    end;
  //until Right <= Lo;
  until Nu(Right) <= Nu(Lo);
end;


class procedure TArrayHelper.InsertionSort<T>(var Values: array of T; L, R: Integer);
var
  i,j: integer;
  x: T;
begin
  for I:= L + 1 to R do begin
    x:= Values[I];
    J:= I;
    //Allow inlining
    while true do begin
      if (J > L) and (TComparer<T>.Default.Compare(Values[J - 1], x) > 0) then begin
        Values[J]:= Values[J - 1];
        Dec(J);
      end
      else break;
    end;
    Values[J]:= x;
  end;
end;

{ TArrayHelper.TSort<T> }

class procedure TArrayHelper.TSort<T>.InsertionSort(const Left, Right: PT);
var
  Lo,Hi,Hi_1: PT;
  Temp: T;
begin





  //  for Lo:= Left + 1 to Right do begin
  Lo:= Left;
  Inc(Lo);
  while NativeUInt(Lo) <= NativeUInt(Right) do begin
    //temp:= a[Lo];
    Temp:= Lo^;
    //        Hi:= Lo;
    Hi:= Lo;
    //        while (Hi > Left) and (a[Hi - 1] > temp) do begin
    Hi_1:= Hi; Dec(Hi_1);
    //Allow inlining
    while true do begin
      if (NativeUInt(Hi) > NativeUInt(Left)) and (TComparer<T>.Default.Compare(Hi_1^, Temp) > 0) then begin
      //          a[Hi]:= a[Hi - 1];
        Hi^:= Hi_1^;
        //Dec(Hi);
        Dec(Hi);
      end
      else break;
    end; //while
    //a[Hi]:= temp;
    Hi^:= Temp;
    //End; {for}
    Inc(Lo);
  end;
end;

class procedure TArrayHelper.DualPivotQuicksort<T>(var a: array of T; const Compare: TComparison<T>;
  Left, Right, divider: Integer);
const
  BubbleLimit = 15;
  BubbleHalf = 8;
var
  len, third: Integer;
  m1, m2: Integer;
  pivot1, pivot2: T;
  less, great: Integer;
  dist: Integer;
  k: Integer;
  Temp: T;
begin
  len:= Right - Left;

  if (len < BubbleLimit) then begin
    TSort<T>.InsertionSort(@a[Left], @a[Right]);    //InsertionSort<T>(a, Left, Right);
    Exit;
  end;
  third:= len div divider;

  // "medians"
  m1:= Left + third;
  m2:= Right - third;

  if (m1 <= Left) then begin
    m1:= Left + 1;
  end;
  if (m2 >= Right) then begin
    m2:= Right - 1;
  end;
  if (TComparer<T>.Default.Compare(a[m1], a[m2]) < 0) then begin
    Temp:= a[m1]; a[m1]:= a[left]; a[left]:= Temp; //Swap<T>(a[m1], a[Left]);
    Temp:= a[m2]; a[m2]:= a[Right]; a[Right]:= Temp; //Swap<T>(a[m2], a[Right]);
  end else begin
    Temp:= a[m1]; a[m1]:= a[Right]; a[Right]:= Temp; //Swap<T>(a[m1], a[Right]);
    Temp:= a[m2]; a[m2]:= a[Left]; a[Left]:= Temp; //Swap<T>(a[m2], a[Left]);
  end;
        // pivots
  pivot1:= a[Left];
  pivot2:= a[Right];

        // pointers
  less:= Left + 1;
  great:= Right - 1;

        // sorting
  //for (int k = less; k <= great; k++)
  //for k:= less to great do begin
  k:= less;
  while (k <= great) do begin

    if (TComparer<T>.Default.Compare(a[k], pivot1) < 0) then begin
      Temp:= a[k]; a[k]:= a[less]; a[less]:= Temp; //Swap<T>(a[k], a[less]);
      Inc(less);
    end else if (TComparer<T>.Default.Compare(a[k], pivot2) > 0) then begin
      while (k < great) and (TComparer<T>.Default.Compare(a[great], pivot2) > 0) do begin
        Dec(great);
      end;
      Temp:= a[k]; a[k]:= a[great]; a[great]:= Temp; //Swap<T>(a[k], a[great]);
      Dec(great);

      if (TComparer<T>.Default.Compare(a[k], pivot1) < 0) then begin
        Temp:= a[k]; a[k]:= a[less]; a[less]:= Temp; //Swap<T>(a[k], a[less]);
        Inc(less);
      end;
    end;
    Inc(k);
  end;
        // swaps
  dist:= great - less;

  if (dist < BubbleHalf) then begin
    Inc(divider);
  end;
  Temp:= a[less - 1]; a[less - 1]:= a[left]; a[left]:= Temp; //Swap<T>(a[less - 1], a[Left]);
  Temp:= a[great + 1]; a[great + 1]:= a[Right]; a[Right]:= Temp; //Swap<T>(a[great + 1], a[Right]);

        // subarrays
  DualPivotQuicksort(a, Compare, Left, less - 2, divider);
  DualPivotQuicksort(a, Compare, great + 2, Right, divider);

        // equal elements
  if (dist > (len - BubbleHalf)) and (TComparer<T>.Default.Compare(pivot1, pivot2) <> 0) then begin
    //for (int k = less; k <= great; k++)
    k:= less;
    while (k <= great) do begin
    //for k:= less to great do begin

      if (TComparer<T>.Default.Compare(a[k], pivot1) = 0) then begin
        Temp:= a[k]; a[k]:= a[less]; a[less]:= Temp; //Swap<T>(a[k], a[less]);
        Inc(less);
      end else if (TComparer<T>.Default.Compare(a[k], pivot2) = 0) then begin
        Temp:= a[k]; a[k]:= a[great]; a[great]:= Temp; //Swap<T>(a[k], a[great]);
        Dec(great);

        if (TComparer<T>.Default.Compare(a[k], pivot1) = 0) then begin
          Temp:= a[k]; a[k]:= a[less]; a[less]:= Temp; //Swap<T>(a[k], a[less]);
          Inc(less);
        end;
      end;
      Inc(k);
    end;
  end;
        // subarray
  if (TComparer<T>.Default.Compare(pivot1, pivot2) < 0) then begin
    DualPivotQuicksort(a, Compare, less, great, divider);
  end;
end;


class procedure TArrayHelper.QuickSortOld<T>
  (var Values: array of T; const Comparer: IComparer<T>; L, R: Integer);
var
  I, J: Integer;
  pivot, temp: T;
begin
  if (Length(Values) = 0) or ((R - L) <= 0) then
    Exit;
  repeat
    I := L;
    J := R;
    pivot := Values[L + (R - L) shr 1];
    repeat
      while True do begin
        if TComparer<T>.Default.Compare(Values[I], pivot) < 0 then Inc(i)
        else break
      end;
      while true do begin
        if TComparer<T>.Default.Compare(Values[J], pivot) > 0 then Dec(J)
        else break;
      end;
      if I <= J then begin
        if I <> J then begin
          temp := Values[I];
          Values[I] := Values[J];
          Values[J] := temp;
        end;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then QuickSortOld<T>(Values, Comparer, L, J);
    L := I;
  until I >= R;
end;




end.

(*
/**
 *  <at> author Vladimir Yaroslavskiy
 *  <at> version 2009.09.10 m765
 */
public class DualPivotQuicksort {

    public static void sort(int[] a) {
        sort(a, 0, a.length);
    }

    public static void sort(int[] a, int fromIndex, int toIndex) {
        rangeCheck(a.length, fromIndex, toIndex);
        dualPivotQuicksort(a, fromIndex, toIndex - 1, 3);
    }

    private static void rangeCheck(int length, int fromIndex, int toIndex) {
        if (fromIndex > toIndex) {
            throw new IllegalArgumentException("fromIndex(" + fromIndex + ") > toIndex(" + toIndex + ")");
        }
        if (fromIndex < 0) {
            throw new ArrayIndexOutOfBoundsException(fromIndex);
        }
        if (toIndex > length) {
            throw new ArrayIndexOutOfBoundsException(toIndex);
        }
    }

    private static void swap(int[] a, int i, int j) {
        int temp = a[i];
        a[i] = a[j];
        a[j] = temp;
    }

    private static void dualPivotQuicksort(int[] a, int left, int right, int div) {
        int len = right - left;

        if (len < 27) { // insertion sort for tiny array
            for (int i = left + 1; i <= right; i++) {
                for (int j = i; j > left && a[j] < a[j - 1]; j--) {
                    swap(a, j, j - 1);
                }
            }
            return;
        }
        int third = len / div;

        // "medians"
        int m1 = left  + third;
        int m2 = right - third;

        if (m1 <= left) {
            m1 = left + 1;
        }
        if (m2 >= right) {
            m2 = right - 1;
        }
        if (a[m1] < a[m2]) {
            swap(a, m1, left);
            swap(a, m2, right);
        }
        else {
            swap(a, m1, right);
            swap(a, m2, left);
        }
        // pivots
        int pivot1 = a[left];
        int pivot2 = a[right];

        // pointers
        int less  = left  + 1;
        int great = right - 1;

        // sorting
        for (int k = less; k <= great; k++) {
            if (a[k] < pivot1) {
                swap(a, k, less++);
            }
            else if (a[k] > pivot2) {
                while (k < great && a[great] > pivot2) {
                    great--;
                }
                swap(a, k, great--);

                if (a[k] < pivot1) {
                    swap(a, k, less++);
                }
            }
        }
        // swaps
        int dist = great - less;

        if (dist < 13) {
           div++;
        }
        swap(a, less  - 1, left);
        swap(a, great + 1, right);

        // subarrays
        dualPivotQuicksort(a, left,   less - 2, div);
        dualPivotQuicksort(a, great + 2, right, div);

        // equal elements
        if (dist > len - 13 && pivot1 != pivot2) {
            for (int k = less; k <= great; k++) {
                if (a[k] == pivot1) {
                    swap(a, k, less++);
                }
                else if (a[k] == pivot2) {
                    swap(a, k, great--);

                    if (a[k] == pivot1) {
                        swap(a, k, less++);
                    }
                }
            }
        }
        // subarray
        if (pivot1 < pivot2) {
            dualPivotQuicksort(a, less, great, div);
        }
    }
}


Technical discussion about the development of the core libraries ()
headers
Vladimir Yaroslavskiy | 11 Sep 12:35 2009
Picon
Replacement of Quicksort in java.util.Arrays with new Dual-Pivot Quicksort

Hello All,

I'd like to share with you new Dual-Pivot Quicksort which is
faster than the known implementations (theoretically and
experimental). I'd like to propose to replace the JDK's
Quicksort implementation by new one.

Description
-----------
The classical Quicksort algorithm uses the following scheme:

1. Pick an element P, called a pivot, from the array.
2. Reorder the array so that all elements, which are less than
    the pivot, come before the pivot and all elements greater than
    the pivot come after it (equal values can go either way). After
    this partitioning, the pivot element is in its final position.
3. Recursively sort the sub-array of lesser elements and the
    sub-array of greater elements.

The invariant of classical Quicksort is:

[ <= p | >= p ]

There are several modifications of the schema:

[ < p | = p | > p ]  or  [ = p | < p | > p | = p ]

But all of them use *one* pivot.

The new Dual-Pivot Quicksort uses *two* pivots elements in this manner:

1. Pick an elements P1, P2, called pivots from the array.
2. Assume that P1 <= P2, otherwise swap it.
3. Reorder the array into three parts: those less than the smaller
    pivot, those larger than the larger pivot, and in between are
    those elements between (or equal to) the two pivots.
4. Recursively sort the sub-arrays.

The invariant of the Dual-Pivot Quicksort is:

[ < P1 | P1 <= & <= P2 } > P2 ]

The new Quicksort is faster than current implementation of Quicksort
in JDK (L. Bentley and M. Douglas McIlroy) and classical Quicksort.

The full description of the Dual-Pivot Quicksort you can find
on my page: http://iaroslavski.narod.ru/quicksort

Performance tests
-----------------
Here is result of running on different types of input data:

Client VM                          all    85%  organ  0..1          0..4
              random ascend descend equal  equal pipes random 010101 random
Dual-Pivot:  16.83  5.31    5.47   0.35  0.68  10.59  1.06   1.02   2.18
   Bentley's:  19.77  9.08   10.13   0.63  1.12  13.22  1.63   1.08   2.49

Server VM                          all    85%  organ  0..1          0..4
              random ascend descend equal  equal pipes random 010101 random
Dual-Pivot:  23.94  6.68    6.63   0.43  0.62  17.14  1.42   1.96   3.41
   Bentley's:  25.20 10.18   10.32   2.07  1.33  16.72  2.95   1.82   3.39

The a lot of other tests have been run under client and server mode.
The most interesting is BentleyBasher test framework. It runs battery
of tests for all cases:

{ 100, 1000, 10000, 1000000 } x
{ sawtooth, rand, stagger, plateau, shuffle } x
{ ident, reverse, reverse_front, reverse_back, sort, dither}

where

100, ... , 1000000 - array length

sawtooth: x[i] =i%m
rand: x[i] = rand() % m
stagger: x[i] = (i*m + i) % n
plateau: x[i] = min(i, m)
shuffle: x[i] = rand()%m? (j+=2): (k+=2)

ident(x) - a copy of x
reverse(x, 0, n) - reversed copy
reverse_front(x, 0, n/2) - front half reversed
reverse_back(x, n/2, n) - back half reversed
sort(x) - an ordered copy
dither(x) - add i%5 to x[i]

Here is the result of execution:
Server VM: http://spreadsheets.google.com/pub?key=t_EAWUkQ4mD3BIbOv8Fa-AQ&output=html
Client VM: http://spreadsheets.google.com/pub?key=tdiMo8xleTxd23nKUObcz0Q&single=true&gid=0&output=html

Mathematical investigations
---------------------------
It is proved that for the Dual-Pivot Quicksort the average number of
comparisons is 2*n*ln(n), the average number of swaps is 0.8*n*ln(n),
whereas classical Quicksort algorithm has 2*n*ln(n) and 1*n*ln(n)
respectively. Full mathematical proof see in attached proof.txt
and proof_add.txt files. Theoretical results are also confirmed
by experimental counting of the operations.

Diff between current and new implementation of Quicksort
--------------------------------------------------------

Here is the link to the diff for java.util.Arrays class:
http://cr.openjdk.java.net/~alanb/6880672/webrev.00

If you like to look and play with new algorithm,
please, take attached class DualPivotQuicksort.java

Feedback
--------

Also I'd like to share a feedback from Joshua Bloch and
Jon Bentley who spent a lot of time investigating this
algorithm, who gave me many advices and tips how to
make new Quicksort better.

-------- Original Message --------
Subject: Re: Integration of new Dual-Pivot Quicksort into JDK 7
Date: Thu, 10 Sep 2009 07:20:11 -0700
From: Joshua Bloch <jjb@...>

Jon also says that Vladimir should make every reasonable improvement to
the basic method before checking in the code. In his words, "It would be
horrible to put the new code into the library, and then have someone
else come along and speed it up by another 20% by using standard
techniques." I believe it's not unlikely that this code may end up
getting ported to many languages and widely deployed in much the manner
of Bentley and McIlroy's fine sort (which is nearing 20 successful years
in the field). Jon will help Vladimir do this.

-------- Original Message --------
Subject: Dual-Pivot Quicksort: Next Steps
Date: Wed, 09 Sep 2009 15:02:25 -0400
From: Jon Bentley <jbentley@...>

Vladimir, Josh,
      I *finally* feel like I understand what is going on.  Now that I
(think that) I see it, it seems straightforward and obvious.
      Tony Hoare developed Quicksort in the early 1960s. I was very
proud to make minor contributions to a particularly clean (binary)
quicksort in the mid 1980s, to a relatively straightforward, industrial
strength Quicksort with McIlroy in the early 1990s, and then to
algorithms and data structures for strings with Sedgewick in the mid 1990s.
      I think that Vladimir's contributions to Quicksort go way beyond
anything that I've ever done, and rank up there with Hoare's original
design and Sedgewick's analysis. I feel so privileged to play a very,
very minor role in helping Vladimir with the most excellent work!

-----------------------------------------------

Let me know, if you have any questions/comments.

Thank you,
Vladimir

Here is the details between steps (1) and (2) in the case of Dual-Pivot Quicksort:
-----------------------------------------------------------------------------

>From the algorithm above, the average number of comparisons C_n as a
function of the number of elements may be represented by the equation:

(1)  C_n = 1 + 2/(n*(n-1)) * sum_{i=0}^{n-2} sum_{j=i+1}^{n-1} {C_i + 1*i + C_{j-i-1} + 2*(j-i- 1) +
C_{n-j-1} + 2*(n-j-1)}

Equation (1) means that total number is the sum of the comparison numbers of
all cases of partitions into 3 parts plus number of comparisons for elements
from left part (one comparison), center and right parts (2 comparisons).

!!! It can be rewritten in other way: !!!

(2)  C_n = 1 + R*(n-2) + 2/(n(n-1)) * sum_{i=0}^{n-2} sum_{j=i+1}^{n-1} {C_i + C_{j-i-1} + C_{n-j-1}}

where R is the average number of comparisons during one iteration. This
constant R can be found as average value (1 + 2 + 2) / 3 = 5/3 ~ 1.6666.

-----------------------------------------------------------------------------

How it can be rewritten:

We should show that 2/(n*(n-1)) * sum_{i=0}^{n-2} sum_{j=i+1}^{n-1} {i + 2*(j-i- 1) + 2*(n-j-1)} equals
to 5/3 * (n-2). Let's consider the double sum:

sum_{i=0}^{n-2} sum_{j=i+1}^{n-1} {i + 2*(j-i- 1) + 2*(n-j-1)} =

= sum_{i=0}^{n-2} sum_{j=i+1}^{n-1} {2*n - 4} - sum_{i=0}^{n-2} sum_{j=i+1}^{n-1} {i} =

= 2*(n-2)*sum_{i=0}^{n-2} {n-1-i} - sum_{i=0}^{n-2} sum_{j=i+1}^{n-1} {i} =

= 2*(n-2)*((n-1)^2 - (n-1)*(n-2)/2) - (n-1)*sum_{i=0}^{n-2} {i} + sum_{i=0}^{n-2} {i*i} =

------------------------------------------------------------------
here we use the property: sum_{k=1}{n} {k^2} = n^3/3 + n^2/2 + n/6
------------------------------------------------------------------

= 2*(n-2)*((n-1)^2 - (n-1)*(n-2)/2) - (n-1)*(n-1)*(n-2)/2 + (n-2)^3/3 + (n-2)^2/2 + (n-2)/6 =

= 1/6 * (12*(n-1)^2*(n-2) - 6*(n-1)*(n-2)^2 - 3*(n-1)^2*(n-2) + 2*(n-2)^3 + 3*(n-2)^2 + (n-2)) =

= 1/6 * (3*(n-1)*(n-2)*(3*(n-1) - 2*(n-2)) + (n-2)*(2*(n-2)^2 + 3*(n-2) + 1))) =

= 1/6 * (3*(n-1)*(n-2)*(n+1) + (n-2)*(2*n^2 - 5*n + 3)) =

= 1/6 * (n-2)*(5*n^2 - 5*n) = 5/6 * n*(n-1)*(n-2)

Substitute the result into equation (1):

(1.1)  C_n = 1 + 2/(n*(n-1)) * 5/6 * n*(n-1)*(n-2) + 2/(n*(n-1)) * sum_{i=0}^{n-2} sum_{j=i+1}^{n-1} {C_i +
C_{j-i-1} + C_{n-j-1}}

or

(1.2)  C_n = 1 + 5/3*(n-2) + 2/(n*(n-1)) * sum_{i=0}^{n-2} sum_{j=i+1}^{n-1} {C_i + C_{j-i-1} + C_{n-j-1}}

We see that (1.2) is same as (2).

/**
 *  <at> author Vladimir Yaroslavskiy
 *  <at> version 2009.09.10 m765
 */
public class DualPivotQuicksort {

    public static void sort(int[] a) {
        sort(a, 0, a.length);
    }

    public static void sort(int[] a, int fromIndex, int toIndex) {
        rangeCheck(a.length, fromIndex, toIndex);
        dualPivotQuicksort(a, fromIndex, toIndex - 1, 3);
    }

    private static void rangeCheck(int length, int fromIndex, int toIndex) {
        if (fromIndex > toIndex) {
            throw new IllegalArgumentException("fromIndex(" + fromIndex + ") > toIndex(" + toIndex + ")");
        }
        if (fromIndex < 0) {
            throw new ArrayIndexOutOfBoundsException(fromIndex);
        }
        if (toIndex > length) {
            throw new ArrayIndexOutOfBoundsException(toIndex);
        }
    }

    private static void swap(int[] a, int i, int j) {
        int temp = a[i];
        a[i] = a[j];
        a[j] = temp;
    }

    private static void dualPivotQuicksort(int[] a, int left, int right, int div) {
        int len = right - left;

        if (len < 27) { // insertion sort for tiny array
            for (int i = left + 1; i <= right; i++) {
                for (int j = i; j > left && a[j] < a[j - 1]; j--) {
                    swap(a, j, j - 1);
                }
            }
            return;
        }
        int third = len / div;

        // "medians"
        int m1 = left  + third;
        int m2 = right - third;

        if (m1 <= left) {
            m1 = left + 1;
        }
        if (m2 >= right) {
            m2 = right - 1;
        }
        if (a[m1] < a[m2]) {
            swap(a, m1, left);
            swap(a, m2, right);
        }
        else {
            swap(a, m1, right);
            swap(a, m2, left);
        }
        // pivots
        int pivot1 = a[left];
        int pivot2 = a[right];

        // pointers
        int less  = left  + 1;
        int great = right - 1;

        // sorting
        for (int k = less; k <= great; k++) {
            if (a[k] < pivot1) {
                swap(a, k, less++);
            }
            else if (a[k] > pivot2) {
                while (k < great && a[great] > pivot2) {
                    great--;
                }
                swap(a, k, great--);

                if (a[k] < pivot1) {
                    swap(a, k, less++);
                }
            }
        }
        // swaps
        int dist = great - less;

        if (dist < 13) {
           div++;
        }
        swap(a, less  - 1, left);
        swap(a, great + 1, right);

        // subarrays
        dualPivotQuicksort(a, left,   less - 2, div);
        dualPivotQuicksort(a, great + 2, right, div);

        // equal elements
        if (dist > len - 13 && pivot1 != pivot2) {
            for (int k = less; k <= great; k++) {
                if (a[k] == pivot1) {
                    swap(a, k, less++);
                }
                else if (a[k] == pivot2) {
                    swap(a, k, great--);

                    if (a[k] == pivot1) {
                        swap(a, k, less++);
                    }
                }
            }
        }
        // subarray
        if (pivot1 < pivot2) {
            dualPivotQuicksort(a, less, great, div);
        }
    }
}

At first consider the classic Quicksort scheme and find the average
number of comparisons and swaps for it. We assume that input data is
random permutation of n numbers from the range [1..n].

Classic Quicksort
=================

1. Choose a pivot element (take random),
2. Compare each (n-1) elements with the pivot
3. and swap it, if necessary, to have the partitions:
   [ <= pivot | >= pivot ]
4. Sort recursively left and right parts.

>From the algorithm above, the average number of comparisons C_n as a
function of the number of elements may be represented by the equation:

(1)  C_n = (n-1) + 1/n * sum_{k=0}^{n-1} {C_k + C_{n-k-1}}

and last sum can be rewritten:

(2)  C_n = (n-1) + 2/n * sum_{k=0}^{n-1} C_k

Write formula (2) for n+1:

(3)  C_{n+1} = n + 2/(n+1) * sum_{k=0}^{n} C_k

Multiply (2) by n and (3) by (n+1) and subtract one from other, we have:

(4)  (n+1)*C_{n+1} - n*C_n = 2*n + 2*C_n

Sorting an array of n elements may be considered as selecting one permutation
of the n elements among all possible permutations. The number of possible
permutations of n elements is n!, so the task for any sorting algorithm is
to determine the one permutation out of n! possibilities. The minimum number
of operations (swap and comparisons) for sorting n elements is const*ln(n!).
>From the Stirling's formula the approximation of the number of operations
is A*n*ln(n) + B*n + C, where A, B and C are constant coefficients.
The coefficients B and C are not important for large n. Therefore,
the function C_n may be approximated by the equation:

(5)  C_n = A*n*ln(n)

The function C_n is substituted from equation (5) into equation (4),
which yields the following equation:

(6)  (n+1)*A*(n+1)*ln(n+1) - n*A*n*ln(n) = 2*n + 2*A*n*ln(n)

Using the properties of logarithms, equation (6) can then be reduced to:

(7)  n*ln(1+1/n) + 2*ln(1+1/n) + (1/n) * ln(n+1) = 2/A

Using a property of logarithm: ln(1 + x) -> x, if x -> 0, and other property:
ln(n) / n -> 0, when n -> +oo, equation (7) will be approximated by:

(8)  1 + 0 + 0 = 2/A

So, the coefficient A is equal to 2 and the average number of comparisons
in sorting of n size arrays is

(9) C_n = 2*n*ln(n).

To find the approximation of the average number of swaps, we use the similar
approach as in the case of comparisons. The average number of swaps S_n as a
function of the number of elements may be represented by the equation:

(10)  S_n = 1/2*(n-1) + 2/n * sum_{k=0}^{n-1} S_k

We assume that average number of swaps during one iteration is 1/2*(n-1).
It means that in average one half of elements is swapped only. Using the
same approach, we find that the coefficient A equals to 1. Therefore,
the function S_n may be approximated by the equation:

(11)  S_n = n*ln(n)

-------------------------------------------------------------------------------

Now consider the Dual-Pivot Quicksort scheme and find the average
number of comparisons and swaps for it. We assume that input data is
random permutation of n numbers from the range [1..n].

Dual-Pivot Quicksort
====================

1. Choose 2 pivot elements pivot1 and pivot2 (take random),
2. pivot1 must be less or equal than pivot2, otherwise they are swapped
3. Compare each (n-2) elements with the pivots
4. and swap it, if necessary, to have the partitions:
   [ <= p1 | p1 <= & <= p2 | >= p2 ]
5. Sort recursively left, center and right parts.

>From the algorithm above, the average number of comparisons C_n as a
function of the number of elements may be represented by the equation:

(1)  C_n = 1 + 2/(n*(n-1)) * sum_{i=0}^{n-2} sum_{j=i+1}^{n-1} {C_i + 1*i + C_{j-i-1} + 2*(j-i-1) +
C_{n-j-1} + 2*(n-j-1)}

Equation (1) means that total number is the sum of the comparison numbers of
all cases of partitions into 3 parts plus number of comparisons for elements
from left part (one comparison), center and right parts (2 comparisons).
It can be rewritten in other way:

(2)  C_n = 1 + R*(n-2) + 2/(n(n-1)) * sum_{i=0}^{n-2} sum_{j=i+1}^{n-1} {C_i + C_{j-i-1} + C_{n-j-1}}

where R is the average number of comparisons during one iteration. This
constant R can be found as average value (1 + 2 + 2) / 3 = 5/3, which
means that elements from left part are required only one comparison,
and elements form center and right parts - two comparisons.

The double sum in equation (2) can be reduced:

(3)  C_n = 1 + 5/3*(n-2) + 2/(n*(n-1)) * sum_{k=0}^{n-2} {3 * (n-k-1) * C_k}

Denote 1 + 5/3*(n-2) by f_n and multiply to n*(n-1):

(4)  n*(n-1)*C_n = n*(n-1)*f_n + 6 * sum_{k=0}^{n-2} {(n-k-1) * C_k}

Write formula (4) for n+1:

(5)  n*(n+1)*C_{n+1} = n*(n+1)*f_n + 6 * sum_{k=0}^{n-1} {(n-k) * C_k}

Subtract (4) from (5), we have:

(6)  n*(n+1)*C_{n+1} - n*(n-1)*C_n = n*(n+1)*f_n - n*(n-1)*f_n + 6 * sum_{k=0}^{n-2} C_k + 6*C_{n-1}

Denote n*(n+1)*C_{n+1} - n*(n-1)*C_n by X_n and n*(n+1)*f_n - n*(n-1)*f_n by F_n:

(7)  X_n = F_n + 6 * sum_{k=0}^{n-2} C_k + 6*C_{n-1}

Write formula (7) for n+1:

(8)  X_{n+1} = F_{n+1} + 6 * sum_{k=0}^{n-1} C_k + 6*C_n

Subtract (7) from (8), we have:

(9)  X_{n+1} - X_n = F_{n+1} - F_n + 6*C_n

Resolving of F_{n+1} - F_n gives:

(10)  X_{n+1} - X_n = 2 + 10*n + 6*C_n

The function X_n is substituted into equation (10), which yields
the following equation:

(11)  (n+1)*(n+2)C_{n+2} -2*n(n+1)*C_{n+1} + (n*(n-1) - 6)*C_n = 2 + 10*n

We will find the function C_n approximated by the equation:

(12)  C_n = A*n*ln(n)

The function C_n is substituted from equation (12) into equation (11),
which yields the following equation:

(13)  (n^3+5*n^2+8*n+4)*ln(n+2) - (2*n^3+4*n^2+2*n)*ln(n+1) + (n^3-n^2-6*n)*ln(n) = (10*n+2) / A

Using the properties of logarithms, equation (13) can then be reduced to:

(14)  n^3*(ln(n+2)-2*ln(n+1)+ln(n)) + n^2*(5*ln(n+2)-4*ln(n+1)-ln(n)) +
n*(8*ln(n+2)-2*ln(n+1)-6*ln(n)) + 4*ln(n+2) = (10*n+2) / A

Using a property of logarithm: ln(1 + x) -> x, if x -> 0, and other property:
ln(n) / n -> 0, when n -> +oo, equation (15) will be approximated by:

(15)  -1 + 4 + 2 + 0 + 0 = 10 / A

So, the coefficient A is equal to 2 and the average number of comparisons
in sorting of n size arrays is

(9) C_n = 2*n*ln(n).

To find the approximation of the average number of swaps, we use the similar
approach as in the case of comparisons. The average number of swaps S_n as a
function of the number of elements may be represented by the equation:

(10)  S_n = 4 + 2/3*(n-2) + 2/(n*(n-1)) * sum_{k=0}^{n-2} {(n-k-1)*S_k}

We assume that average number of swaps during one iteration is 2/3*(n-2).
It means that in average one third of elements is swapped only. Using the
same approach, we find that the coefficient A equals to 0.8. Therefore,
the function S_n may be approximated by the equation:

(11)  S_n = 0.8*n*ln(n)

-------------------------------------------------------------------------

And as summary:

The value of the coefficient A:

            dual-pivot   classic
comparison:    2.0         2.0
      swap:    0.8         1.0
