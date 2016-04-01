unit FastMath;

//note that this unit only contains optimized versions for X64.
//The other versions are already optimized.

interface

{ MinValue: Returns the smallest signed value in the data array (MIN) }
function MinValue(const Data: array of Single): Single; overload;
function MinValue(const Data: array of Double): Double; overload;
function MinValue(const Data: array of Extended): Extended; overload;
function MinIntValue(const Data: array of Integer): Integer;

function Min(const A, B: Integer): Integer; overload; {$ifdef PurePascal}inline;{$endif}
function Min(const A, B: Int64): Int64; overload; inline;
function Min(const A, B: UInt64): UInt64; overload; inline;
function Min(const A, B: Single): Single; overload; inline;
function Min(const A, B: Double): Double; overload; inline;
function Min(const A, B: Extended): Extended; overload; inline;

{ MaxValue: Returns the largest signed value in the data array (MAX) }
function MaxValue(const Data: array of Single): Single; overload;
function MaxValue(const Data: array of Double): Double; overload;
function MaxValue(const Data: array of Extended): Extended; overload;
function MaxIntValue(const Data: array of Integer): Integer;

function Max(const A, B: Integer): Integer; overload; inline;
function Max(const A, B: Int64): Int64; overload; inline;
function Max(const A, B: UInt64): UInt64; overload; inline;
function Max(const A, B: Single): Single; overload; inline;
function Max(const A, B: Double): Double; overload; inline;
function Max(const A, B: Extended): Extended; overload; inline;



implementation

{ MinValue: Returns the smallest signed value in the data array (MIN) }
function MinValue(const Data: array of Single): Single; overload;
asm
  or    rcx,rcx
  jz @error
  mov   eax,[rcx-4] //get length
  neg   eax
  sub   rcx,rax
  add   eax,32
  jns @tail
  movups xmm0,[rcx+rax-32]
@loop:
  minps  xmm0,[rcx+rax-32+16]
  add   eax,16
  js @loop
  movaps xmm1,xmm0
  shufps xmm1,xmm0,3+(2 shl 2)+(1 shl 4)+(0 shl 6)
  minps  xmm0,xmm1
  shufps xmm1,xmm1,2+(3 shl 2)+(0 shl 4)+(1 shl 6)
  minps  xmm0,xmm1
  movss  eax,xmm0
@tail:

@error:


end;

function MinValue(const Data: array of Double): Double; overload;
begin

end;

function MinValue(const Data: array of Extended): Extended; overload;
begin

end;

function MinIntValue(const Data: array of Integer): Integer;
begin

end;


function Min(const A, B: Integer): Integer; overload;
{$if defined(PurePascal)}
inline;
begin
  Result:= ((a-b)*(a>b))+b;
end;
{$elseif defined(CPUX86)}
asm
  sub a,b
  sbb ecx,ecx
  and a,ecx
  add a,b
end;
{$else}
asm
  sub a,b
  sbb eax,eax  //RAX = 0 if a>b else RAX=-1
  and eax,a    //RAX = 0 if a>b else RAX=(a-b)
  add eax,b    //rax = a-b+b = a if b>a else rax =0+b = b
end;
{$endif}

function Min(const A, B: Int64): Int64; overload; {$if defined(PurePascal)}
inline;
begin
  Result:= ((a-b)*(a>b))+b;
end;
{$elseif defined(CPUX86)}
asm
  sub a,b
  sbb ecx,ecx
  and a,ecx
  add a,b
end;
{$else}
asm
  sub a,b
  sbb rax,rax  //RAX = 0 if a>b else RAX=-1
  and rax,a    //RAX = 0 if a>b else RAX=(a-b)
  add rax,b    //rax = a-b+b = a if b>a else rax =0+b = b
end;
{$endif}


function Min(const A, B: UInt64): UInt64; overload; inline;
begin

end;

function Min(const A, B: Single): Single; overload; inline;
begin

end;

function Min(const A, B: Double): Double; overload; inline;
begin

end;

function Min(const A, B: Extended): Extended; overload; inline;
begin

end;


{ MaxValue: Returns the largest signed value in the data array (MAX) }
function MaxValue(const Data: array of Single): Single; overload;
begin

end;

function MaxValue(const Data: array of Double): Double; overload;
begin

end;

function MaxValue(const Data: array of Extended): Extended; overload;
begin

end;

function MaxIntValue(const Data: array of Integer): Integer;
begin

end;


function Max(const A, B: Integer): Integer; overload; inline;
begin

end;

function Max(const A, B: Int64): Int64; overload; inline;
begin

end;

function Max(const A, B: UInt64): UInt64; overload; inline;
begin

end;

function Max(const A, B: Single): Single; overload; inline;
begin

end;

function Max(const A, B: Double): Double; overload; inline;
begin

end;

function Max(const A, B: Extended): Extended; overload; inline;
begin

end;






//// func Abs(x float64) float64
//TEXT ·Abs(SB),NOSPLIT,$0
//	MOVQ   $(1<<63), BX
//	MOVQ   BX, X0 // movsd $(-0.0), x0
//	MOVSD  x+0(FP), X1
//	ANDNPD X1, X0
//	MOVSD  X0, ret+8(FP)
//	RET
//
//// func Frexp(f float64) (frac float64, exp int)
//TEXT ·Frexp(SB),NOSPLIT,$0
//	FMOVD   f+0(FP), F0   // F0=f
//	FXAM
//	FSTSW   AX
//	SAHF
//	JNP     nan_zero_inf
//	JCS     nan_zero_inf
//	FXTRACT               // F0=f (0<=f<1), F1=e
//	FMULD  $(0.5), F0     // F0=f (0.5<=f<1), F1=e
//	FMOVDP  F0, frac+8(FP)   // F0=e
//	FLD1                  // F0=1, F1=e
//	FADDDP  F0, F1        // F0=e+1
//	FMOVLP  F0, exp+16(FP)  // (int=int32)
//	RET
//nan_zero_inf:
//	FMOVDP  F0, frac+8(FP)   // F0=e
//	MOVL    $0, exp+16(FP)  // exp=0
//	RET
//
//
//#define Big		0x4330000000000000 // 2**52
//
//// func Floor(x float64) float64
//TEXT ·Floor(SB),NOSPLIT,$0
//	MOVQ	x+0(FP), AX
//	MOVQ	$~(1<<63), DX // sign bit mask
//	ANDQ	AX,DX // DX = |x|
//	SUBQ	$1,DX
//	MOVQ    $(Big - 1), CX // if |x| >= 2**52-1 or IsNaN(x) or |x| == 0, return x
//	CMPQ	DX,CX
//	JAE     isBig_floor
//	MOVQ	AX, X0 // X0 = x
//	CVTTSD2SQ	X0, AX
//	CVTSQ2SD	AX, X1 // X1 = float(int(x))
//	CMPSD	X1, X0, 1 // compare LT; X0 = 0xffffffffffffffff or 0
//	MOVSD	$(-1.0), X2
//	ANDPD	X2, X0 // if x < float(int(x)) {X0 = -1} else {X0 = 0}
//	ADDSD	X1, X0
//	MOVSD	X0, ret+8(FP)
//	RET
//isBig_floor:
//	MOVQ    AX, ret+8(FP) // return x
//	RET
//
//// func Ceil(x float64) float64
//TEXT ·Ceil(SB),NOSPLIT,$0
//	MOVQ	x+0(FP), AX
//	MOVQ	$~(1<<63), DX // sign bit mask
//	MOVQ	AX, BX // BX = copy of x
//	ANDQ    DX, BX // BX = |x|
//	MOVQ    $Big, CX // if |x| >= 2**52 or IsNaN(x), return x
//	CMPQ    BX, CX
//	JAE     isBig_ceil
//	MOVQ	AX, X0 // X0 = x
//	MOVQ	DX, X2 // X2 = sign bit mask
//	CVTTSD2SQ	X0, AX
//	ANDNPD	X0, X2 // X2 = sign
//	CVTSQ2SD	AX, X1	// X1 = float(int(x))
//	CMPSD	X1, X0, 2 // compare LE; X0 = 0xffffffffffffffff or 0
//	ORPD	X2, X1 // if X1 = 0.0, incorporate sign
//	MOVSD	$1.0, X3
//	ANDNPD	X3, X0
//	ORPD	X2, X0 // if float(int(x)) <= x {X0 = 1} else {X0 = -0}
//	ADDSD	X1, X0
//	MOVSD	X0, ret+8(FP)
//	RET
//isBig_ceil:
//	MOVQ	AX, ret+8(FP)
//	RET
//
//// func Trunc(x float64) float64
//TEXT ·Trunc(SB),NOSPLIT,$0
//	MOVQ	x+0(FP), AX
//	MOVQ	$~(1<<63), DX // sign bit mask
//	MOVQ	AX, BX // BX = copy of x
//	ANDQ    DX, BX // BX = |x|
//	MOVQ    $Big, CX // if |x| >= 2**52 or IsNaN(x), return x
//	CMPQ    BX, CX
//	JAE     isBig_trunc
//	MOVQ	AX, X0
//	MOVQ	DX, X2 // X2 = sign bit mask
//	CVTTSD2SQ	X0, AX
//	ANDNPD	X0, X2 // X2 = sign
//	CVTSQ2SD	AX, X0 // X0 = float(int(x))
//	ORPD	X2, X0 // if X0 = 0.0, incorporate sign
//	MOVSD	X0, ret+8(FP)
//	RET
//isBig_trunc:
//	MOVQ    AX, ret+8(FP) // return x
//	RET

end.
