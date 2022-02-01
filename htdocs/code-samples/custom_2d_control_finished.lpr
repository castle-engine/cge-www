uses SysUtils, Classes,
  CastleColors, CastleVectors, CastleWindow, CastleUIControls,
  CastleControls, CastleRectangles, CastleGLUtils, CastleFilesUtils, CastleGLImages;

type
  TPlayerInformation = class(TComponent)
  public
    Life, MaxLife: Single;
  end;

var
  Window: TCastleWindow;
  PlayerInformation: TPlayerInformation;

type
  TPlayerHud = class(TCastleUserInterface)
  private
    FMyImage: TDrawableImage;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Render; override;
  end;

constructor TPlayerHud.Create(AOwner: TComponent);
begin
  inherited;
  FMyImage := TDrawableImage.Create('castle-data:/face.png');
end;

destructor TPlayerHud.Destroy;
begin
  FreeAndNil(FMyImage);
  inherited;
end;

procedure TPlayerHud.Render;
var
  R: TFloatRectangle;
begin
  inherited;

  R := FloatRectangle(10, 10, 400, 50);
  { draw background of health bar with a transparent red }
  DrawRectangle(R, Vector4(1, 0, 0, 0.5));
  { calculate smaller R, to only include current life }
  R := R.Grow(-3);
  R.Width := R.Width * PlayerInformation.Life / PlayerInformation.MaxLife;
  { draw the inside of health bar with an opaque red }
  DrawRectangle(R, Vector4(1, 0, 0, 1));

  UIFont.Print(20, 20, Yellow, Format('Player life: %f / %f', [
    PlayerInformation.Life,
    PlayerInformation.MaxLife
  ]));

  FMyImage.Draw(420, 10);
end;

var
  PlayerHud: TPlayerHud;
begin
  Window := TCastleWindow.Create(Application);
  Window.Open;

  PlayerInformation := TPlayerInformation.Create(Application);
  PlayerInformation.Life := 75;
  PlayerInformation.MaxLife := 100;

  PlayerHud := TPlayerHud.Create(Application);
  Window.Controls.InsertFront(PlayerHud);

  Application.Run;
end.
