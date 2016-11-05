uses SysUtils, CastleColors, CastleVectors, CastleWindow, CastleUIControls,
  CastleControls, CastlePlayer, CastleRectangles, CastleGLUtils;

var
  Window: TCastleWindow;
  Player: TPlayer;

type
  TMyPlayerHUD = class(TUIControl)
  public
    procedure Render; override;
  end;

procedure TMyPlayerHUD.Render;
begin
  inherited;
  UIFont.Print(20, 20, Yellow,
    Format('Player life: %f / %f', [Player.Life, Player.MaxLife]));
end;

var
  PlayerHUD: TMyPlayerHUD;
begin
  Window := TCastleWindow.Create(Application);

  Player := TPlayer.Create(Window.SceneManager);
  Player.Life := 75; // just to make things interesting
  Window.SceneManager.Items.Add(Player);
  Window.SceneManager.Player := Player;

  { When starting your game, create TMyPlayerHUD instance
    and add it to Window.Controls }
  PlayerHUD := TMyPlayerHUD.Create(Application);
  Window.Controls.InsertFront(PlayerHUD);

  Window.OpenAndRun;
end.