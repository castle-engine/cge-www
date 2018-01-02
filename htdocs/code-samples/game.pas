{ Game initialization and logic. }
unit Game;

interface

implementation

uses SysUtils,
  CastleWindow, CastleControls, CastleLog, CastleUIControls;

var
  Window: TCastleWindow;
  Status: TCastleLabel;

{ One-time initialization of resources. }
procedure ApplicationInitialize;
begin
  { For a scalable UI (adjusts to any window size in a smart way), use UIScaling }
  Window.Container.UIReferenceWidth := 1024;
  Window.Container.UIReferenceHeight := 768;
  Window.Container.UIScaling := usEncloseReferenceSize;

  Status := TCastleLabel.Create(Application);
  Status.Anchor(vpMiddle);
  Status.Anchor(hpMiddle);
  Status.Caption := 'Hello world!';
  Window.Controls.InsertFront(Status);
end;

function MyGetApplicationName: string;
begin
  Result := 'my_fantastic_game';
end;

initialization
  { Set OnGetApplicationName, to determine the ApplicationName value.
    Our log uses ApplicationName, so it's useful to set OnGetApplicationName early. }
  OnGetApplicationName := @MyGetApplicationName;

  { Start logging. Do this as early as possible,
    to log information and eventual warnings during initialization. }
  InitializeLog;

  { Initialize Application.OnInitialize. }
  Application.OnInitialize := @ApplicationInitialize;

  { Create and assign Application.MainWindow. }
  Window := TCastleWindow.Create(Application);
  Application.MainWindow := Window;

  { You should not need to do *anything* more in the unit "initialization" section.
    Most of your game initialization should happen inside ApplicationInitialize.
    In particular, it is not allowed to read files before ApplicationInitialize
    is called (in case of non-desktop platforms, some necessary things
    may not be prepared yet). }
end.
