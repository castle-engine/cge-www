{$ifdef FPC} {$mode objfpc}{$H+}{$J-} {$endif}
{$ifdef MSWINDOWS} {$apptype CONSOLE} {$endif}
{$R+} // range checking on - nice for debugging

var
  MyArray: array [0..9] of Integer;
  I: Integer;
begin
  // initialize
  for I := 0 to 9 do
    MyArray[I] := I * I;

  // show
  for I := 0 to 9 do
    WriteLn('Square is ', MyArray[I]);

  // does the same as above
  for I := Low(MyArray) to High(MyArray) do
    WriteLn('Square is ', MyArray[I]);

  // does the same as above
  I := 0;
  while I < 10 do
  begin
    WriteLn('Square is ', MyArray[I]);
    I := I + 1; // or "I += 1", or "Inc(I)"
  end;

  // does the same as above
  I := 0;
  repeat
    WriteLn('Square is ', MyArray[I]);
    Inc(I);
  until I = 10;

  // does the same as above
  // note: here I enumerates MyArray values, not indexes
  for I in MyArray do
    WriteLn('Square is ', I);
end.