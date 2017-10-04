(*******************************************************
 System.FastSystem
 A fast drop-in addition to speed up function in system.pas
 It should compile and run in XE2 and beyond.

 Alpha version 0.5, fully tested in Win64

 (c) Copyright 2016 J. Bontes

   This Source Code Form is subject to the terms of the
   Mozilla Public License, v. 2.0.
   If a copy of the MPL was not distributed with this file,
   You can obtain one at http://mozilla.org/MPL/2.0/.

********************************************************
FillChar code is an altered version FillCharsse2 SynCommons.pas
which is part of Synopse framework by Arnaud Bouchez

********************************************************
Changelog

0.5 Initial version:

********************************************************)

unit FastSystem;

interface

procedure FillChar(var Dest; Count: NativeInt; Value: ansichar); inline; overload;
procedure FillChar(var Dest; Count: NativeInt; Value: Byte); overload;
procedure FillMemory(Destination: Pointer; Length: NativeUInt; Fill: Byte); inline;
{$EXTERNALSYM FillMemory}
procedure ZeroMemory(Destination: Pointer; Length: NativeUInt); inline;
{$EXTERNALSYM ZeroMemory}

implementation

procedure FillChar(var Dest; Count: NativeInt; Value: ansichar); inline; overload;
begin
  FillChar(Dest, Count, byte(Value));
end;

procedure FillMemory(Destination: Pointer; Length: NativeUInt; Fill: Byte);
begin
  FillChar(Destination^, Length, Fill);
end;

procedure ZeroMemory(Destination: Pointer; Length: NativeUInt); inline;
begin
  FillChar(Destination^, Length, 0);
end;

//This code is 3x faster than System.FillChar on x64.

{$ifdef CPUX64}
procedure FillChar(var Dest; Count: NativeInt; Value: Byte);
//rcx = dest
//rdx=count
//r8b=value
asm
              .noframe
              .align 16
              movzx r8,r8b           //There's no need to optimize for count <= 3
              mov rax,$0101010101010101
              mov r9,rdx
              imul rax,r8            //fill rax with value.
              cmp edx,59             //Use simple code for small blocks.
              jl  @Below32
@Above32:     mov r11,rcx
              rep mov r8b,7          //code shrink to help alignment.
              lea r9,[rcx+rdx]       //r9=end of array
              sub rdx,8
              rep mov [rcx],rax
              add rcx,8
              and r11,r8             //and 7 See if dest is aligned
              jz @tail
@NotAligned:  xor rcx,r11            //align dest
              lea rdx,[rdx+r11]
@tail:        test r9,r8             //and 7 is tail aligned?
              jz @alignOK
@tailwrite:   mov [r9-8],rax         //no, we need to do a tail write
              and r9,r8              //and 7
              sub rdx,r9             //dec(count, tailcount)
@alignOK:     mov r10,rdx
              and edx,(32+16+8)      //count the partial iterations of the loop
              mov r8b,64             //code shrink to help alignment.
              mov r9,rdx
              jz @Initloop64
@partialloop: shr r9,1              //every instruction is 4 bytes
              lea r11,[rip + @partial +(4*7)] //start at the end of the loop
              sub r11,r9            //step back as needed
              add rcx,rdx            //add the partial loop count to dest
              cmp r10,r8             //do we need to do more loops?
              jmp r11                //do a partial loop
@Initloop64:  shr r10,6              //any work left?
              jz @done               //no, return
              mov rdx,r10
              shr r10,(19-6)         //use non-temporal move for > 512kb
              jnz @InitFillHuge
@Doloop64:    add rcx,r8
              dec edx
              mov [rcx-64+00H],rax
              mov [rcx-64+08H],rax
              mov [rcx-64+10H],rax
              mov [rcx-64+18H],rax
              mov [rcx-64+20H],rax
              mov [rcx-64+28H],rax
              mov [rcx-64+30H],rax
              mov [rcx-64+38H],rax
              jnz @DoLoop64
@done:        rep ret
              //db $66,$66,$0f,$1f,$44,$00,$00 //nop7
@partial:     mov [rcx-64+08H],rax
              mov [rcx-64+10H],rax
              mov [rcx-64+18H],rax
              mov [rcx-64+20H],rax
              mov [rcx-64+28H],rax
              mov [rcx-64+30H],rax
              mov [rcx-64+38H],rax
              jge @Initloop64        //are we done with all loops?
              rep ret
              db $0F,$1F,$40,$00
@InitFillHuge:
@FillHuge:    add rcx,r8
              dec rdx
              db $48,$0F,$C3,$41,$C0 // movnti  [rcx-64+00H],rax
              db $48,$0F,$C3,$41,$C8 // movnti  [rcx-64+08H],rax
              db $48,$0F,$C3,$41,$D0 // movnti  [rcx-64+10H],rax
              db $48,$0F,$C3,$41,$D8 // movnti  [rcx-64+18H],rax
              db $48,$0F,$C3,$41,$E0 // movnti  [rcx-64+20H],rax
              db $48,$0F,$C3,$41,$E8 // movnti  [rcx-64+28H],rax
              db $48,$0F,$C3,$41,$F0 // movnti  [rcx-64+30H],rax
              db $48,$0F,$C3,$41,$F8 // movnti  [rcx-64+38H],rax
              jnz @FillHuge
@donefillhuge:mfence
              rep ret
              db $0F,$1F,$44,$00,$00  //db $0F,$1F,$40,$00
@Below32:     and  r9d,not(3)
              jz @SizeIs3
@FillTail:    sub   edx,4
              lea   r10,[rip + @SmallFill + (15*4)]
              sub   r10,r9
              jmp   r10
@SmallFill:   rep mov [rcx+56], eax
              rep mov [rcx+52], eax
              rep mov [rcx+48], eax
              rep mov [rcx+44], eax
              rep mov [rcx+40], eax
              rep mov [rcx+36], eax
              rep mov [rcx+32], eax
              rep mov [rcx+28], eax
              rep mov [rcx+24], eax
              rep mov [rcx+20], eax
              rep mov [rcx+16], eax
              rep mov [rcx+12], eax
              rep mov [rcx+08], eax
              rep mov [rcx+04], eax
              mov [rcx],eax
@Fallthough:  mov [rcx+rdx],eax  //unaligned write to fix up tail
              rep ret

@SizeIs3:     shl edx,2           //r9 <= 3  r9*4
              lea r10,[rip + @do3 + (4*3)]
              sub r10,rdx
              jmp r10
@do3:         rep mov [rcx+2],al
@do2:         mov [rcx],ax
              ret
@do1:         mov [rcx],al
              rep ret
@do0:         rep ret
end;
{$endif}

initialization
  //Patch the RTL.
end.
