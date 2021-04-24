{ Game initialization and logic. }
unit GameInitialize;

interface

implementation

uses CastleWindow, CastleControls, CastleLog, CastleUIControls,
  CastleApplicationProperties, CastleColors;

var
  Window: TCastleWindowBase;
  Status: TCastleLabel;

{ One-time initialization of resources. }
procedure ApplicationInitialize;
begin
  { For a scalable UI (adjusts to any window size in a smart way), use UIScaling.

    Note: In a real application, it is better to load these settings by
    Window.Container.LoadSettings('castle-data:/CastleSettings.xml') }
  Window.Container.UIReferenceWidth := 1024;
  Window.Container.UIReferenceHeight := 768;
  Window.Container.UIScaling := usEncloseReferenceSize;

  { Add some UI to the window.

    Note: In a real application, it is better to divide the application into a number
    of UI states, and states should load a UI designed using the editor. }
  Status := TCastleLabel.Create(Application);
  Status.Anchor(vpMiddle);
  Status.Anchor(hpMiddle);
  Status.Color := White;
  Status.Caption := 'Hello world!';
  Window.Controls.InsertFront(Status);
end;

initialization
  { Initialize Application.OnInitialize. }
  Application.OnInitialize := @ApplicationInitialize;

  { Create and assign Application.MainWindow. }
  Window := TCastleWindowBase.Create(Application);
  Application.MainWindow := Window;

  { You should not need to do *anything* more in the unit "initialization" section.
    Most of your game initialization should happen inside ApplicationInitialize.
    In particular, it is not allowed to read files before ApplicationInitialize
    is called (in case of non-desktop platforms, some necessary things
    may not be prepared yet). }
end.
