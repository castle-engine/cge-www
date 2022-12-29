{ Game initialization and logic. }
unit GameInitialize;

interface

implementation

uses SysUtils,
  CastleWindow, CastleLog,
  GameViewMain;

var
  Window: TCastleWindow;

{ One-time initialization of resources. }
procedure ApplicationInitialize;
begin
  { Adjust container settings for a scalable UI (adjusts to any window size in a smart way). }
  Window.Container.LoadSettings('castle-data:/CastleSettings.xml');

  { Create TViewMain that will handle "main" view of the game.
    Larger games may use multiple views,
    e.g. TViewMainMenu ("main menu view"),
    TViewPlay ("playing the game view"),
    TViewCredits ("showing the credits view") etc. }
  ViewMain := TViewMain.Create(Application);
  Window.Container.View := ViewMain;
end;

initialization
  { Initialize Application.OnInitialize. }
  Application.OnInitialize := @ApplicationInitialize;

  { Create and assign Application.MainWindow. }
  Window := TCastleWindow.Create(Application);
  Window.ParseParameters; // allows to control window size / fullscreen on the command-line
  Application.MainWindow := Window;

  { You should not need to do *anything* more in the unit "initialization" section.
    Most of your game initialization should happen inside ApplicationInitialize.
    In particular, it is not allowed to read files before ApplicationInitialize
    is called (in case of non-desktop platforms, some necessary things
    may not be prepared yet). }
end.
