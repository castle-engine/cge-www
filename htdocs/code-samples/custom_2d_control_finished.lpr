uses SysUtils, CastleColors, CastleVectors, CastleWindow, CastleUIControls,
  CastleControls, CastlePlayer, CastleRectangles, CastleGLUtils,
  Classes, CastleFilesUtils, CastleGLImages;

var
  Window: TCastleWindow;
  Player: TPlayer;

type
  TMyPlayerHUD = class(TCastleUserInterface)
  private
    FMyImage: TDrawableImage;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Render; override;
  end;

constructor TMyPlayerHUD.Create(AOwner: TComponent);
begin
  inherited;
  FMyImage := TDrawableImage.Create('castle-data:/face.png');
end;

destructor TMyPlayerHUD.Destroy;
begin
  FreeAndNil(FMyImage);
  inherited;
end;

procedure TMyPlayerHUD.Render;
var
  R: TRectangle;
begin
  inherited;

  R := Rectangle(10, 10, 400, 50);
  { draw background of health bar with a transparent red }
  DrawRectangle(R, Vector4(1, 0, 0, 0.5));
  { calculate smaller R, to only include current life }
  R := R.Grow(-3);
  R.Width := Round(R.Width * Player.Life / Player.MaxLife);
  { draw the inside of health bar with an opaque red }
  DrawRectangle(R, Vector4(1, 0, 0, 1));

  UIFont.Print(20, 20, Yellow,
    Format('Player life: %f / %f', [Player.Life, Player.MaxLife]));

  FMyImage.Draw(420, 10);
end;

var
  PlayerHUD: TMyPlayerHUD;
begin
  Window := TCastleWindow.Create(Application);
  Window.Open;

  Player := TPlayer.Create(Window.SceneManager);
  Player.Life := 75; // just to make things interesting
  Window.SceneManager.Items.Add(Player);
  Window.SceneManager.Player := Player;

  { When starting your game, create TMyPlayerHUD instance
    and add it to Window.Controls }
  PlayerHUD := TMyPlayerHUD.Create(Application);
  Window.Controls.InsertFront(PlayerHUD);

  Application.Run;
end.
