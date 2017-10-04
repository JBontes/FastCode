unit HiResStopWatch;

interface

uses
  System.Diagnostics,
  System.SysUtils;

type
  THiResStopWatch = record
  private
    FMsStopWatch: TStopWatch;
    FRepeatCount: integer;
    FRunning: boolean;
    FStart: int64;
    FStop: int64;
  public
    class function NeedsManyIterations: boolean; static;
    class function StartNew(LowestTimingOfXAttempts: integer = 10)
      : THiResStopWatch; static;
    procedure Stop;
    function GetElapsedTicks: int64; overload;
    function GetElapsedTicks(divider: int64; SubtractLoop: boolean = true)
      : int64; overload;

    // Todo, make Sample generic, so it can accept Procs with arguments.
    class function Sample(A: TProc; Repeats: integer; EmptyProc: TProc = nil;
      LowestTimingOfXAttempts: integer = 10): int64; static;

    function GetElapsedMilliseconds: int64;
    property ElapsedTicks: int64 read GetElapsedTicks;
    property ElapsedMilliseconds: int64 read GetElapsedMilliseconds;
  end;


//{$define ForceRDTSCP}
///  uncomment if you want extra accuracy in x86.
///  should run on all modern processors.



//{$define AllowOutOfOrder}
///  useful if you want to time the effect of mispredicted brances
///  make sure to execute the timing code as soon as possible after the
///  branch.
{$ifdef AllowOutOfOrder}
  {$undef ForceRDTSCP}
{$endif}




implementation

function Min(A, b: int64): int64; inline;
begin
  if A > b then
    Result := b
  else
    Result := A;
end;
{ THiResStopWatch }


function IsTimerBroken: boolean;
{$ifdef CPUX86}
asm
  //Check to see if RDTSC runs at fixed or variable frequency
  push ebx
  mov eax,$80000007  //Has TSC Invariant support?
  cpuid
  pop ebx
  xor eax,eax        //Assume no
  and edx,$10        //test TSC_invariant bit
  setnz al           //If TSC = broken, return true.
end;
{$endif}
  //Check to see if RDTSC runs at fixed or variable frequency
{$ifdef CPUX64}
asm
  mov r8,rbx
  mov eax,$80000007  //TSC Invariant support?
  cpuid
  mov rbx,r8
  xor eax,eax
  and edx,$10 //test bit 8
  setnz al
end;
{$endif}

function RDTSC: int64;
{$IFDEF CPUX64}
asm
  {$IFDEF AllowOutOfOrder}
  rdtsc
  {$ELSE}
    rdtscp       //rdstcp is read serialized, it will not execute too early.
    //also ensure it does not execute too late
    mov r8,rdx   //rdtscp changes rdx and rax, force dependency chain on rdx
    xor r8,rbx   //push rbx, do not allow push rbx to execute OoO
    xor rbx,rdx  //rbx=r8
    xor rbx,r8   //rbx = 0
    push rdx
    push rax
    mov rax,rbx  //rax = 0, but in a way that excludes OoO execution.
    cpuid
    pop rax
    pop rdx
    mov rbx,r8
    xor rbx,rdx  //restore rbx
  {$ENDIF AllowOutOfOrder}
  shl rdx,32
  or rax,rdx
  {$else}
{$IFDEF CPUX86}
asm
  {$IFNDEF AllowOutOfOrder}
    {$IFDEF ForceRDTSCP}
      db $0F, $01, $F9   //rdtscp
      //rdtscp       //rdstcp is read serialized, it will not execute too early.
      //also ensure it does not execute too late
      mov ecx,edx   //rdtscp changes rdx and rax, force dependency chain on rdx
      xor ecx,ebx   //push ebx, do not allow push ebx to execute OoO
      xor ebx,edx   //ebx=ecx
      xor ebx,ecx   //ebx = 0
      push edx
      push ecx
      push eax
      mov eax,ebx  //rax = 0, but in a way that excludes OoO execution.
      cpuid
      pop eax
      pop ecx
      pop edx
      mov ebx,ecx
      xor ebx,edx  //restore rbx
      {$else}
      xor eax,eax
      push ebx
      cpuid         // On x86 we can't assume the existance of RDTSP
      rdtsc
      pop ebx       // so use CPUID to serialize
      {$ENDIF}
      {$ELSE}
      rdtsc
    {$ELSE}
  error !
{$ENDIF}
{$ENDIF}
{$ENDIF}
end;

function THiResStopWatch.GetElapsedMilliseconds: int64;
begin
  Assert(not(FRunning), 'Do not call while running, you''ll skew the timings');
  Result := FMsStopWatch.ElapsedMilliseconds;
end;

function THiResStopWatch.GetElapsedTicks: int64;
begin
  if FRunning then
    FStop := RDTSC;
  Result := FStop - FStart;
end;

function THiResStopWatch.GetElapsedTicks(divider: int64;
  SubtractLoop: boolean = true): int64;
var
  Timer: THiResStopWatch;
  EmptyTicks: int64;
  A, i, d: integer;
  NormalTime, EmptyLoopTime: int64;
begin
  NormalTime := (FStop - FStart) div divider;
  if SubtractLoop then
  begin
    EmptyTicks := MaxInt;
    // Time empty loop.
    for A := 1 to FRepeatCount do
    begin // always lowest of 10 times.
      Timer := THiResStopWatch.StartNew;
      for i := 1 to divider do
      begin
        d := not(d);
      end;
      Timer.Stop;
      EmptyTicks := Min(EmptyTicks, Timer.GetElapsedTicks);
    end;
    EmptyLoopTime := EmptyTicks div divider;
  end
  else
    EmptyLoopTime := 0;
  Result := NormalTime - EmptyLoopTime;
end;

class function THiResStopWatch.NeedsManyIterations: boolean;
begin
  Result:= IsTimerBroken;
end;

class function THiResStopWatch.Sample(A: TProc; Repeats: integer;
  EmptyProc: TProc = nil; LowestTimingOfXAttempts: integer = 10): int64;
var
  i, j: integer;
  MyStopWatch: THiResStopWatch;
  LowestTime, CurrentTime: int64;
begin
  // First warm up the cache using a few trail runs outside the timing.
  if Repeats > 100 then
  begin
    for i := 1 to 100 do
      A;
  end;

  LowestTime := UInt64(-1) shr 1; // Maxint64
  for j := 1 to LowestTimingOfXAttempts do
  begin
    MyStopWatch := THiResStopWatch.StartNew(LowestTimingOfXAttempts);
    for i := 1 to Repeats do
      A;
    MyStopWatch.Stop;
    CurrentTime := MyStopWatch.GetElapsedTicks(Repeats,
      not(Assigned(EmptyProc)));
    LowestTime := Min(LowestTime, CurrentTime);
  end;
  Result := LowestTime;
  if Assigned(EmptyProc) then
  begin
    // Warm up the cache
    if Repeats > 100 then
    begin
      for i := 1 to 100 do
        EmptyProc;
    end;

    LowestTime := UInt64(-1) shr 1;
    for j := 1 to LowestTimingOfXAttempts do
    begin
      MyStopWatch := THiResStopWatch.StartNew(LowestTimingOfXAttempts);
      for i := 1 to Repeats do
        EmptyProc;
      MyStopWatch.Stop;
      CurrentTime := MyStopWatch.GetElapsedTicks(Repeats);
      LowestTime := Min(LowestTime, CurrentTime);
    end;
    Result := Result - LowestTime;
  end;
end;



class function THiResStopWatch.StartNew(LowestTimingOfXAttempts: integer = 10)
  : THiResStopWatch;
begin
  Result.FStop := 0;
  Result.FRunning := true;
  Result.FRepeatCount := LowestTimingOfXAttempts;
  Result.FMsStopWatch := TStopWatch.StartNew;
  Result.FStart := RDTSC;
end;

procedure THiResStopWatch.Stop;
begin
  FStop := RDTSC;
  FMsStopWatch.Stop;
  FRunning := false;
end;



end.
