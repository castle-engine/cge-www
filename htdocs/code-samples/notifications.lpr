uses CastleWindow, CastleGameNotifications, CastleColors, CastleUIControls;

var
  Window: TCastleWindowBase;
begin
  Window := TCastleWindowBase.Create(Application);
  Window.Open;

  Notifications.TextAlignment := hpMiddle;
  Notifications.Anchor(hpMiddle);
  Notifications.Anchor(vpBottom, 10);
  Notifications.Color := Yellow;
  Window.Controls.InsertFront(Notifications);

  Notifications.Show('You picked 10 ammo!');

  Application.Run;
end.
