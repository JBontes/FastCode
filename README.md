# FastCode
Fast replacements for Embarcadero's standard libs for Delphi XE7 and above.  
Note that this is still early alpha code and needs to be tested.  

# System.Generics.FastDefaults  
Speeds up comparisons by using static class functions instead of interfaces.  
The functions are inline and inject only the short snippet of code need to compare the types in use.  

It does this by resolving the type at compile-time using the new `GetTypeKind` compiler intrinsic in XE7.

#Example

**FastDefault**

    var
      I1, I2: integer;
      RFast, RSlow: integer;
    begin
      I1:= 1;
      I2:= 2;
      RFast:= System.Generics.FastDefaults.TComparer<integer>.Default.Compare(i1,i2);
      if RFast = 0 then Readln;
    end.

Assembly generated:

```
Project43.dpr.130: I1:= 1;
004365CC B801000000       mov eax,$00000001
Project43.dpr.131: I2:= 2;
004365D1 BA02000000       mov edx,$00000002
Project43.dpr.132: RFast:= System.Generics.FastDefaults.TComparer<integer>.Default.Compare(i1,i2);
004365D6 8945EC           mov [ebp-$14],eax
004365D9 8955E8           mov [ebp-$18],edx
004365DC 8B45EC           mov eax,[ebp-$14]
004365DF 2B45E8           sub eax,[ebp-$18]
Project43.dpr.133: if RFast = 0 then ReadLn;
004365E2 85C0             test eax,eax
004365E4 750F             jnz $004365f5
004365E6 A1FC954300       mov eax,[$004395fc]
004365EB E848F2FCFF       call @ReadLn
004365F0 E84FE6FCFF       call @_IOTest
Project43.dpr.139: end.
004365F5 E8060CFDFF       call @Halt0
```

**Default Default**
```
var
  I1, I2: integer;
  RFast, RSlow: integer;
begin
  I1:= 1;
  I2:= 2;
  RSlow:= System.Generics.Defaults.TComparer<integer>.Default.Compare(i1,i2);
  if RSlow = 0 then Readln;
end.
```

Assembly generated:
```
004365D3 33C0             xor eax,eax
004365D5 55               push ebp
004365D6 682D664300       push $0043662d
004365DB 64FF30           push dword ptr fs:[eax]
004365DE 648920           mov fs:[eax],esp
Project43.dpr.130: I1:= 1;
004365E1 BB01000000       mov ebx,$00000001
Project43.dpr.131: I2:= 2;
004365E6 BE02000000       mov esi,$00000002
Project43.dpr.132: RSlow:= System.Generics.Defaults.TComparer<integer>.Default.Compare(i1,i2);
004365EB 8D55EC           lea edx,[ebp-$14]
004365EE A1B8564300       mov eax,[$004356b8]
004365F3 E868F2FFFF       call {System.Generics.Defaults}TComparer<System.Integer>.Default
004365F8 8B45EC           mov eax,[ebp-$14]
004365FB 8BCE             mov ecx,esi
004365FD 8BD3             mov edx,ebx
004365FF 8B18             mov ebx,[eax]
00436601 FF530C           call dword ptr [ebx+$0c]
Project43.dpr.133: if RSlow = 0 then ReadLn;
00436604 85C0             test eax,eax
00436606 750F             jnz $00436617
00436608 A1FC954300       mov eax,[$004395fc]
0043660D E826F2FCFF       call @ReadLn
00436612 E82DE6FCFF       call @_IOTest
Project43.dpr.139: end.
00436617 33C0             xor eax,eax
00436619 5A               pop edx
0043661A 59               pop ecx
0043661B 59               pop ecx
0043661C 648910           mov fs:[eax],edx
0043661F 6834664300       push $00436634
00436624 8D45EC           lea eax,[ebp-$14]
00436627 E8BC3FFDFF       call @IntfClear
0043662C C3               ret 
0043662D E9C204FDFF       jmp @HandleFinally
00436632 EBF0             jmp $00436624
00436634 5E               pop esi
00436635 5B               pop ebx
```
