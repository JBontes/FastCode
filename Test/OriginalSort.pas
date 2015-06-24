unit OriginalSort;

interface

uses
  System.Generics.Collections;

type
  TOriginalArray = class
    class procedure Sort<T>(var Values: array of T); overload; static;
  end;

implementation

class procedure TOriginalArray.Sort<T>(var Values: array of T);
begin
  TArray.Sort<T>(Values);
end;

end.
