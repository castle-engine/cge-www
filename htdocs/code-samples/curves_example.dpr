program curves_example;

{$apptype console}

uses SysUtils, CastleVectors, CastleFilesUtils, CastleCurves, CastleLog;
var
  FirstCurve: TCurve;
  Curves: TCurveList;
begin
  InitializeLog;

  FirstCurve := TCurve.LoadFromFile('castle-data:/my_curves.xml');
  try
    // That's it, you loaded the 1st curve from XML file.
    // Write some initial curve points.
    WritelnLog(FirstCurve.Point(0.0).ToString);
    WritelnLog(FirstCurve.Point(0.1).ToString);
  finally FreeAndNil(FirstCurve) end;

  { in more complicated scenarios, my_curves.xml may keep many curves
    inside. Load them like this: }

  Curves := TCurveList.Create(true { free objects });
  try
    Curves.LoadFromFile('castle-data:/my_curves.xml');
    if Curves.Count = 0 then
      raise Exception.Create('No curves defined in file');
    FirstCurve := Curves[0];
    // That's it, you have the 1st curve from XML file.
    // Write some initial curve points.
    WritelnLog(FirstCurve.Point(0.0).ToString);
    WritelnLog(FirstCurve.Point(0.1).ToString);
    WritelnLog(FirstCurve.Point(0.2).ToString);
  finally FreeAndNil(Curves) end;
end.
