uses CastleWindow, CastleUIControls, CastleOnScreenMenu;

var
  Window: TCastleWindow;
  OnScreenMenu1: TCastleOnScreenMenu;

type
  TEventHandler = class
    class procedure NewGameClick(Sender: TObject);
    class procedure LoadGameClick(Sender: TObject);
  end;

class procedure TEventHandler.NewGameClick(Sender: TObject);
begin
  // ... new game
end;

class procedure TEventHandler.LoadGameClick(Sender: TObject);
begin
  // ... load game from disk
end;

begin
  Window := TCastleWindow.Create(Application);

  OnScreenMenu1 := TCastleOnScreenMenu.Create(Application);
  OnScreenMenu1.Add('New game', @TEventHandler(nil).NewGameClick);
  OnScreenMenu1.Add('Load game', @TEventHandler(nil).LoadGameClick);
  OnScreenMenu1.Anchor(hpMiddle);
  OnScreenMenu1.Anchor(vpMiddle);
  Window.Controls.InsertFront(OnScreenMenu1);

  Window.OpenAndRun;
end.