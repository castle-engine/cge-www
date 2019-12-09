{$ifdef MSWINDOWS} {$apptype GUI} {$endif}

uses CastleLog, CastleWindow, CastleColors, CastleControls;
var
  Window: TCastleWindowBase;
  Lab: TCastleLabel;
begin
  InitializeLog;

  WritelnLog('My Log Message');
  WritelnLog('My Category', 'My Log Message');
  WritelnWarning('My Warning');

  // display the LogOutput value in a window
  Window := TCastleWindowBase.Create(Application);

  Lab := TCastleLabel.Create(Application);
  Lab.Caption := 'Logging to ' + LogOutput;
  Lab.Color := White;
  Window.Controls.InsertFront(Lab);

  Window.Open;
  Application.Run;
end.
