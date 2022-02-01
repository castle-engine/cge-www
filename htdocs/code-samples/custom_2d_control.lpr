uses SysUtils, Classes,
  CastleColors, CastleVectors, CastleWindow, CastleUIControls,
  CastleControls, CastleRectangles;

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
  public
    procedure Render; override;
  end;

procedure TPlayerHud.Render;
begin
  inherited;
  UIFont.Print(20, 20, Yellow, Format('Player life: %f / %f', [
    PlayerInformation.Life,
    PlayerInformation.MaxLife
  ]));
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
