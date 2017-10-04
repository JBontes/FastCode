unit FastRTLPatch;

interface

//There is no interface.

implementation

uses
  WinAPI.Windows;

{$ifdef CPUX86}
procedure RecordClear(var Dest; TypeInfo: pointer);
asm // faster version by AB (direct call to finalization procedures)
        { ->    EAX pointer to record to be finalized   }
        {       EDX pointer to type info                }
        { <-    EAX pointer to record to be finalized   }
        movzx ecx,byte ptr [edx].TFieldTable.NameLen
        push ebx
        mov ebx,eax
        push esi
        push edi
        mov edi,[edx+ecx].TFieldTable.ManagedCount
        lea esi,[edx+ecx].TFieldTable.ManagedFields
        test edi,edi
        jz @@end
@@loop: mov edx,[esi].TFieldInfo.TypeInfo
        mov eax,[esi].TFieldInfo.&Offset
        mov edx,[edx]
        lea esi,[esi+8]
        movzx ecx,[edx].TFieldTable.Kind
        lea eax,eax+ebx // eax=data to be initialized
        sub cl,tkLString
        {$ifdef UNICODE}
        cmp cl,tkUString-tkLString+1
{$else} cmp cl,tkDynArray-tkLString+1
{$endif}jnb @@err
        call dword ptr [@@Tab+ecx*4]
        dec edi
        jg @@loop
@@end:  mov eax,ebx // keep eax at return (see e.g. TObject.CleanupInstance)
        pop edi
        pop esi
        pop ebx
        ret
        nop; nop; nop // align @@Tab
@@Tab:  dd System.@LStrClr
{$IFDEF LINUX} // under Linux, WideString are refcounted as AnsiString
        dd System.@LStrClr
{$else} dd System.@WStrClr {$endif}
{$ifdef LVCL}
        dd @@err
{$else} dd System.@VarClr  {$endif}
        dd @@Array
        dd RecordClear
        dd System.@IntfClear
        dd @@err
        dd System.@DynArrayClear
        {$ifdef UNICODE}
        dd System.@UStrClr
        {$endif}
@@err:  mov al,reInvalidPtr
        pop edi
        pop esi
        pop ebx
        jmp System.Error
@@array:movzx ecx,[edx].TFieldTable.NameLen
        add ecx,edx
        mov edx,dword ptr [ecx].TFieldTable.ManagedFields[0] // Fields[0].TypeInfo^
        mov ecx,[ecx].TFieldTable.ManagedCount
        mov edx,[edx]
        call System.@FinalizeArray
        // we made Call @@Array -> ret to continue
end;
{$elseif defined(CPUX64}
procedure RecordClear(var Dest; TypeInfo: pointer);
asm // faster version by AB (direct call to finalization procedures)
        { ->    -EAX- RCX pointer to record to be finalized   }
        {       -EDX- RDX pointer to type info                }
        { <-    RAX pointer to record to be finalized   }
        movzx r8d,byte ptr [rdx].TFieldTable.NameLen
        push rbx
        mov rbx,rcx
        push rsi
        push rdi
        mov edi,[rdx+r8].TFieldTable.ManagedCount
        lea rsi,[rdx+r8].TFieldTable.ManagedFields
        test edi,edi
        jz @@end
@@loop: mov rdx,[rsi].TFieldInfo.TypeInfo
        mov eax,[rsi].TFieldInfo.&Offset
        mov edx,[rdx]
        lea rsi,[rsi+8]
        movzx ecx,[rdx].TFieldTable.Kind
        lea rax,rax+rbx // eax=data to be initialized
        sub cl,tkLString
        {$ifdef UNICODE}
        cmp cl,tkUString-tkLString+1
{$else} cmp cl,tkDynArray-tkLString+1
{$endif}jnb @@err
        call dword ptr [@@Tab+rcx*4]
        dec edi
        jg @@loop
@@end:  mov rax,rbx // keep eax at return (see e.g. TObject.CleanupInstance)
        pop rdi
        pop rsi
        pop rbx
        ret
        nop; nop; nop // align @@Tab
@@Tab:  dd System.@LStrClr
{$IFDEF LINUX} // under Linux, WideString are refcounted as AnsiString
        dd System.@LStrClr
{$else} dd System.@WStrClr {$endif}
{$ifdef LVCL}
        dd @@err
{$else} dd System.@VarClr  {$endif}
        dd @@Array
        dd RecordClear
        dd System.@IntfClear
        dd @@err
        dd System.@DynArrayClear
        {$ifdef UNICODE}
        dd System.@UStrClr
        {$endif}
@@err:  mov al,reInvalidPtr
        pop rdi
        pop rsi
        pop rbx
        jmp System.Error
@@array:movzx r8d,[rdx].TFieldTable.NameLen
        add rcx,rdx
        mov edx,dword ptr [rcx].TFieldTable.ManagedFields[0] // Fields[0].TypeInfo^
        mov ecx,[rcx].TFieldTable.ManagedCount
        mov rdx,[rdx]
        {TODO -oJB -cCheck the parameters of FinalizeArray : initial write}
        //procedure FinalizeArray(P, TypeInfo: Pointer; Count: NativeUInt);
        xchg rcx,rax
        xchg r8, rcx
        call System.@FinalizeArray
        // we made Call @@Array -> ret to continue
end;
{$endif}

{$ifdef CPUX86}
procedure RecordCopy(var Dest; const Source; TypeInfo: pointer);
asm  // faster version of _CopyRecord{dest, source, typeInfo: Pointer} by AB
        { ->    EAX pointer to dest             }
        {       EDX pointer to source           }
        {       ECX pointer to typeInfo         }
        push ebp
        push ebx
        push esi
        push edi
        movzx ebx,byte ptr [ecx].TFieldTable.NameLen
        mov esi,edx                     // esi = source
        mov edi,eax                     // edi = dest
        add ebx,ecx                     // ebx = TFieldTable
        xor eax,eax                     // eax = current offset
        mov ebp,[ebx].TFieldTable.ManagedCount // ebp = TFieldInfo count
        mov ecx,[ebx].TFieldTable.Size
        test ebp,ebp
        jz @fullcopy
        push ecx                        // sizeof(record) on stack
        add ebx,offset TFieldTable.ManagedFields[0] // ebx = first TFieldInfo
@next:  mov ecx,[ebx].TFieldInfo.&Offset
        mov edx,[ebx].TFieldInfo.TypeInfo
        sub ecx,eax
        mov edx,[edx]
        jle @nomov
        lea esi,[esi+ecx]
        lea edi,[edi+ecx]
        neg ecx
@mov1:  mov al,[esi+ecx] // fast copy not destructable data
        mov [edi+ecx],al
        inc ecx
        jnz @mov1
@nomov: mov eax,edi
        movzx ecx,[edx].TFieldTable.Kind
        cmp ecx,tkLString
        je @@LString
        jb @@err
{$ifdef UNICODE}
        cmp ecx,tkUString
        je @@UString
{$else} cmp ecx,tkDynArray
        je @@DynArray
{$endif}ja @@err
        jmp dword ptr [ecx*4+@@tab-tkWString*4]
@@Tab:  dd @@WString,@@Variant,@@Array,@@Record,@@Interface,@@err
        {$ifdef UNICODE}dd @@DynArray{$endif}
@@errv: mov al,reVarInvalidOp
        jmp @@err2
@@err:  mov al,reInvalidPtr
@@err2: pop edi
        pop esi
        pop ebx
        pop ebp
        jmp System.Error
        nop // all functions below have esi=source edi=dest
@@Array:
        movzx ecx,byte ptr [edx].TFieldTable.NameLen
        push dword ptr [edx+ecx].TFieldTable.Size
        push dword ptr [edx+ecx].TFieldTable.ManagedCount
        mov ecx,dword ptr [edx+ecx].TFieldTable.ManagedFields[0] // Fields[0].TypeInfo^
        mov ecx,[ecx]
        mov edx,esi
        call System.@CopyArray
        pop eax // restore sizeof(Array)
        jmp @@finish
@@Record:
        movzx ecx,byte ptr [edx].TFieldTable.NameLen
        mov ecx,[edx+ecx].TFieldTable.Size
        push ecx
        mov ecx,edx
        mov edx,esi
        call RecordCopy
        pop eax // restore sizeof(Record)
        jmp @@finish
        nop; nop; nop
@@Variant:
{$ifdef NOVARCOPYPROC}
        mov edx,esi
        call System.@VarCopy
{$else} cmp dword ptr [VarCopyProc],0
        mov edx,esi
        jz @@errv
        call [VarCopyProc]
{$endif}mov eax,16
        jmp @@finish
{$ifdef DELPHI6OROLDER} nop; nop; {$endif}
@@Interface:
        mov edx,[esi]
        call System.@IntfCopy
        jmp @@fin4
        nop; nop; nop
@@DynArray:
        mov ecx,edx // ecx=TypeInfo
        mov edx,[esi]
        call System.@DynArrayAsg
        jmp @@fin4
@@WString:
{$ifndef LINUX}
        mov edx,[esi]
        call System.@WStrAsg
        jmp @@fin4
{$endif}
@@LString:
        mov edx,[esi]
        call System.@LStrAsg
{$ifdef UNICODE}
        jmp @@fin4
        nop; nop
@@UString:
        mov edx,[esi]
        call System.@UStrAsg
{$endif}
@@fin4: mov eax,4
@@finish:
        add esi,eax
        add edi,eax
        add eax,[ebx].TFieldInfo.&Offset
        dec ebp    // any other TFieldInfo?
        lea ebx,[ebx+8] // next TFieldInfo
        jnz @next
        pop ecx // ecx= sizeof(record)
@fullcopy:
        mov edx,edi
        sub ecx,eax
        mov eax,esi
        jle @nomov2
        call dword ptr [MoveFast]
@nomov2:pop edi
        pop esi
        pop ebx
        pop ebp
end;
{$elseif defined(CPUX64}
procedure RecordCopy(var Dest; const Source; TypeInfo: pointer);
asm  // faster version of _CopyRecord{dest, source, typeInfo: Pointer} by AB
        { ->    EAX pointer to dest             }
        {       EDX pointer to source           }
        {       ECX pointer to typeInfo         }
        push rbp
        push rbx
        push rsi
        push rdi
        movzx rbx,byte ptr [r8].TFieldTable.NameLen
        mov rsi,rdx                     // esi = source
        mov rdi,rcx                     // edi = dest
        add rbx,r8                     // ebx = TFieldTable
        xor eax,eax                     // eax = current offset
        mov ebp,[rbx].TFieldTable.ManagedCount // ebp = TFieldInfo count
        mov ecx,[rbx].TFieldTable.Size
        test ebp,ebp
        jz @fullcopy
        push rcx                        // sizeof(record) on stack
        add rbx,offset TFieldTable.ManagedFields[0] // ebx = first TFieldInfo
@next:  mov ecx,[rbx].TFieldInfo.&Offset
        mov rdx,[rbx].TFieldInfo.TypeInfo
        sub ecx,eax
        mov rdx,[rdx]
        jle @nomov
        lea rsi,[rsi+rcx]
        lea rdi,[rdi+rcx]
        neg rcx
@mov1:  mov al,[rsi+rcx] // fast copy not destructable data
        mov [rdi+rcx],al
        inc ecx
        jnz @mov1
@nomov: mov rax,rdi
        movzx ecx,[rdx].TFieldTable.Kind
        cmp ecx,tkLString
        je @@LString
        jb @@err
{$ifdef UNICODE}
        cmp ecx,tkUString
        je @@UString
{$else} cmp ecx,tkDynArray
        je @@DynArray
{$endif}ja @@err
        jmp dword ptr [rcx*4+@@tab-tkWString*4]
@@Tab:  dd @@WString,@@Variant,@@Array,@@Record,@@Interface,@@err
        {$ifdef UNICODE}dd @@DynArray{$endif}
@@errv: mov cl,reVarInvalidOp
        jmp @@err2
@@err:  mov cl,reInvalidPtr
@@err2: pop rdi
        pop rsi
        pop rbx
        pop rbp
        jmp System.Error
        nop // all functions below have esi=source edi=dest
@@Array:
        movzx ecx,byte ptr [rdx].TFieldTable.NameLen
        push dword ptr [rdx+rcx].TFieldTable.Size
        push dword ptr [rdx+rcx].TFieldTable.ManagedCount
        mov rcx,qword ptr [rdx+rcx].TFieldTable.ManagedFields[0] // Fields[0].TypeInfo^
        mov ecx,[rcx]
        mov rdx,rsi
        {TODO -oJB -cCorrect parameters for CopyArray : Correct for 64-bit}
        //                     EAX   EDX     EXC                stack
        //                     RCX,  RDX,    R8,                R9
        //procedure _CopyArray(Dest, Source, TypeInfo: Pointer; Count: NativeUInt);
        call System.@CopyArray
        pop rax // restore sizeof(Array)
        jmp @@finish
@@Record:
        movzx ecx,byte ptr [edx].TFieldTable.NameLen
        mov ecx,[edx+ecx].TFieldTable.Size
        push ecx
        mov ecx,edx
        mov edx,esi
        call RecordCopy
        pop eax // restore sizeof(Record)
        jmp @@finish
        nop; nop; nop
@@Variant:
{$ifdef NOVARCOPYPROC}
        mov edx,esi
        call System.@VarCopy
{$else} cmp dword ptr [VarCopyProc],0
        mov edx,esi
        jz @@errv
        call [VarCopyProc]
{$endif}mov eax,16
        jmp @@finish
{$ifdef DELPHI6OROLDER} nop; nop; {$endif}
@@Interface:
        mov edx,[esi]
        call System.@IntfCopy
        jmp @@fin4
        nop; nop; nop
@@DynArray:
        mov ecx,edx // ecx=TypeInfo
        mov edx,[esi]
        call System.@DynArrayAsg
        jmp @@fin4
@@WString:
{$ifndef LINUX}
        mov edx,[esi]
        call System.@WStrAsg
        jmp @@fin4
{$endif}
@@LString:
        mov edx,[esi]
        call System.@LStrAsg
{$ifdef UNICODE}
        jmp @@fin4
        nop; nop
@@UString:
        mov edx,[esi]
        call System.@UStrAsg
{$endif}
@@fin4: mov eax,4
@@finish:
        add esi,eax
        add edi,eax
        add eax,[ebx].TFieldInfo.&Offset
        dec ebp    // any other TFieldInfo?
        lea ebx,[ebx+8] // next TFieldInfo
        jnz @next
        pop ecx // ecx= sizeof(record)
@fullcopy:
        mov edx,edi
        sub ecx,eax
        mov eax,esi
        jle @nomov2
        call dword ptr [MoveFast]
@nomov2:pop edi
        pop esi
        pop ebx
        pop ebp
end;
{$endif}

//RTL copyrecord
{TODO -oJB -cOptimization: Eliminate case statement with jumptable}
{TODO -oJB -c64bit : Rewrite for 64-bit}
procedure _CopyRecord{ dest, source, typeInfo: Pointer };
const
  FldPtr = 8;
  EndPtr = 4;
  RecSize = 0;
  TFieldInfoSize = SizeOf(TFieldInfo);
asm
        { ->    EAX pointer to dest             }
        {       EDX pointer to source           }
        {       ECX pointer to typeInfo         }

        PUSH    EBX                      { 12 -> 8  }
        PUSH    ESI                      { 8  -> 4  }
        PUSH    EDI                      { 4  -> 0  }
        PUSH    EBP                      { 0  -> 12 }
        PUSH    0                        { 12 -> 8  use [ESP+FldPtr] for the "saved" Field pointer }
        PUSH    0                        { 8  -> 4  use [ESP+EndPtr] for the "end" of the field info array }

        MOV     EBX,EAX
        MOV     ESI,EDX

        XOR     EAX,EAX
        MOV     AL,[ECX].TTypeInfo.Name.Byte

        LEA     EDI,[ECX+EAX+2+8]
        MOV     EBP,[EDI-4] //TFieldTable.Count
        XOR     EAX,EAX
        MOV     ECX,[EDI-8] //TFieldTable.Size
        TEST    EBP,EBP
        JZ      @@moveWhole
        PUSH    ECX                      { 4  -> 0  }
{$IFDEF WEAKREF}
        MOV     ECX,EBP
        LEA     EDX,[EDI+ECX*TFieldInfoSize]
        MOV     [ESP+EndPtr],EDX
        MOV     [ESP+FldPtr],EDX
@@findWeak:
        CMP     [EDI+ECX*TFieldInfoSize-TFieldInfoSize].TFieldInfo.TypeInfo,0
        JE      @@hasWeak
        DEC     ECX
        JNZ     @@findWeak
        JMP     @@loop
@@hasWeak:
        LEA     ECX,[EDI+ECX*8]
        MOV     [ESP+FldPtr],ECX
        DEC     EBP     { Remove the sentinal from consideration }
{$ENDIF}
@@loop:
{$IFDEF WEAKREF}
        MOV     EDX,[ESP+FldPtr]
        CMP     EDX,[ESP+EndPtr]
        JE      @@noWeak
        CMP     [EDX].TFieldInfo.TypeInfo,0
        JE      @@noWeak
        CMP     [EDI].TFieldInfo.TypeInfo,0
        JE      @@doneStrong  { found the sentinal, so the rest are all weak }
        MOV     ECX,[EDX+4] // TFieldInfo.Offset
        CMP     ECX,[EDI+4] // TFieldInfo.Offset Compare the offsets
        JA      @@noWeak
@@doneStrong:
        XCHG    EDI,[ESP+FldPtr] // Swap the Weak/noWeak pointers
@@noWeak:
{$ENDIF}
        MOV     ECX,[EDI+4] //TFieldInfo.Offset
        SUB     ECX,EAX
        JLE     @@nomove1
        MOV     EDX,EAX
        ADD     EAX,ESI
        ADD     EDX,EBX
        CALL    Move
@@noMove1:
        MOV     EAX,[EDI+4] //TFieldInfo.Offset
        MOV     EDX,[EDI].TFieldInfo.TypeInfo
        MOV     EDX,[EDX]
        MOV     CL,[EDX].TTypeInfo.Kind

{$IFDEF WEAKREF}
        CMP     CL,tkMethod
        JE      @@Method
{$ENDIF}
{$IFDEF AUTOREFCOUNT}
        CMP     CL,tkClass
        JE      @@Class
{$ENDIF}
        CMP     CL,tkInterface
        JE      @@Interface
{$IFDEF WEAKREF}
        CMP     EDI,[ESP+FldPtr]
        JA      @@error
{$ENDIF}
        CMP     CL,tkLString
        JE      @@LString
        CMP     CL,tkWString
        JE      @@WString
        CMP     CL,tkUString
        JE      @@UString
        CMP     CL,tkVariant
        JE      @@Variant
        CMP     CL,tkArray
        JE      @@Array
        CMP     CL,tkRecord
        JE      @@Record
        CMP     CL,tkDynArray
        JE      @@DynArray
@@error:
        MOV     AL,reInvalidPtr
        POP     ECX                      { 0  -> 4  }
        ADD     ESP, 8                   { 4  -> 12 }
        POP     EBP                      { 12 -> 0  }
        POP     EDI                      { 0  -> 4  }
        POP     ESI                      { 4  -> 8  }
        POP     EBX                      { 8  -> 12 }
        JMP     Error

@@LString:
        MOV     EDX,[ESI+EAX]
        ADD     EAX,EBX
        CALL    _LStrAsg
        MOV     EAX,4
        JMP     @@common

@@UString:
        MOV     EDX,[ESI+EAX]
        ADD     EAX,EBX
        CALL    _UStrAsg
        MOV     EAX,4
        JMP     @@common

@@WString:
        MOV     EDX,[ESI+EAX]
        ADD     EAX,EBX
        CALL    _WStrAsg
        MOV     EAX,4
        JMP     @@common

@@Variant:
        LEA     EDX,[ESI+EAX]
        ADD     EAX,EBX
        CALL    _VarCopy
        MOV     EAX,16
        JMP     @@common

@@Array:
{$IFDEF ALIGN_STACK}
        SUB     ESP, 8                   { 0  -> 8  }
{$ENDIF ALIGN_STACK}
        XOR     ECX,ECX
        MOV     CL,[EDX+1]
        PUSH    dword ptr [EDX+ECX+2]    { 8  -> 4  }
        PUSH    dword ptr [EDX+ECX+2+4]  { 4  -> 0  }
        MOV     ECX,[EDX+ECX+2+8]
        MOV     ECX,[ECX]
        LEA     EDX,[ESI+EAX]
        ADD     EAX,EBX
        CALL    _CopyArray
        POP     EAX
{$IFDEF ALIGN_STACK}
        ADD     ESP, 8
{$ENDIF ALIGN_STACK}
        JMP     @@common

@@Record:
{$IFDEF ALIGN_STACK}
        SUB     ESP, 12                  { 0  -> 4  }
{$ENDIF ALIGN_STACK}
        XOR     ECX,ECX
        MOV     CL,[EDX+1]
        MOV     ECX,[EDX+ECX+2]
        PUSH    ECX                      { 4  -> 0  }
        MOV     ECX,EDX
        LEA     EDX,[ESI+EAX]
        ADD     EAX,EBX
        CALL    _CopyRecord
        POP     EAX                      { 0  -> 4  }
{$IFDEF ALIGN_STACK}
        ADD     ESP, 12                  { 4  -> 0  }
{$ENDIF ALIGN_STACK}
        JMP     @@common

@@Interface:
        MOV     EDX,[ESI+EAX]
        ADD     EAX,EBX
{$IFDEF WEAKINTFREF}
        CMP     EDI,[ESP+FldPtr]                { if Weak then _IntfWeakCopy else _IntfCopy }
        JB      @@intfCopy

        CALL    _IntfWeakCopy
        MOV     EAX,4
        JMP     @@common
@@intfCopy:
{$ENDIF}
        CALL    _IntfCopy
        MOV     EAX,4
        JMP     @@common

{$IFDEF WEAKREF}
@@Method:
        LEA     EDX,[ESI+EAX]
        ADD     EAX,EBX
        CALL    _CopyClosure
        MOV     EAX,8
        JMP     @@common
{$ENDIF}

{$IFDEF AUTOREFCOUNT}
@@Class:
        MOV     EDX,[ESI+EAX]
        ADD     EAX,EBX
{$IFDEF WEAKINSTREF}
        CMP     EDI,[ESP+FldPtr]                { if Weak then _InstWeakCopy else _InstCopy }
        JB      @@instCopy

        CALL    _InstWeakCopy
        MOV     EAX,4
        JMP     @@common
@@instCopy:
{$ENDIF}
        CALL    _InstCopy
        MOV     EAX,4
        JMP     @@common
{$ENDIF}

@@DynArray:
        MOV     ECX,EDX
        MOV     EDX,[ESI+EAX]
        ADD     EAX,EBX
        CALL    _DynArrayAsg
        MOV     EAX,4

@@common:
        ADD     EAX,[EDI+4]
        ADD     EDI,8
        DEC     EBP
        JNZ     @@loop
        POP     ECX                      { 0  -> 4  }

@@moveWhole:
        SUB     ECX,EAX
        JLE     @@noMove2
        LEA     EDX,[EBX+EAX]
        ADD     EAX,ESI
{$IFDEF ALIGN_STACK}
        SUB     ESP, 4                   { 4  -> 0  }
{$ENDIF ALIGN_STACK}
        CALL    Move
{$IFDEF ALIGN_STACK}
        ADD     ESP, 4                   { 0  -> 4  }
{$ENDIF ALIGN_STACK}
@@noMove2:

        ADD     ESP, 8                   { 4  -> 12 }
        POP     EBP                      { 12 -> 0  }
        POP     EDI                      { 0  -> 4  }
        POP     ESI                      { 4  -> 8  }
        POP     EBX                      { 8  -> 12 }
end;                                     { 12 -> 0  RET }

{$IFDEF PurePascal}

procedure PatchMove(Old, New: pointer; Size: integer); {Patch System.Move to Divert Calls to New Move Procedure}
const
  JumpFarId = $E9;
var
  Protect, OldProtect: DWORD;
begin
  VirtualProtect(Old, 256, PAGE_EXECUTE_READWRITE, @OldProtect);
  if PByte(Old)^ <> JumpFarId then begin {Check if Already Patched}
    PByte(Old)^:= JumpFarId;
    PInteger(Integer(Old) + 1)^:= Integer(New) - Integer(Old) - 5; {Change Destination}
  end;
  VirtualProtect(Old, 256, OldProtect, @Protect);
  FlushInstructionCache(GetCurrentProcess, Old, 256);
end; {PatchMove}

{$ELSE}

procedure PatchMove(Old, New: pointer; Size: integer); {Overwrite System.Move with Main Procedure of New Move}
const
  JumpFarId = $E9;
  JumpPtrId = $25FF;
var
  I, Offset: Integer;
  Src, Dest: PByte;
  Protect, OldProtect: DWORD;
begin
  VirtualProtect(Old, 256, PAGE_EXECUTE_READWRITE, @OldProtect);
  if PByte(Old)^ <> JumpFarId then begin{Check if Already Patched}
    if PWord(Old)^ = JumpPtrId then begin {System.Move Starts JMP DWORD PTR [XXXXXXXX] (ie. Using Packages)}
      PByte(Old)^:= JumpFarId;
      PInteger(Integer(Old) + 1)^:= Integer(New) - Integer(Old) - 5; {Change Destination}
    end else begin {Patch RTL function}
      Move(New^, Old^, Size);
    end;
  end;
  VirtualProtect(Old, 256, OldProtect, @Protect);
  FlushInstructionCache(GetCurrentProcess, Old, Size);
end; {PatchMove}

{$ENDIF}



procedure PatchRTL;
begin

end;


initialization
  PatchRTL;
finalization
end.
