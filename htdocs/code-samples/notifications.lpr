uses CastleWindow, CastleNotifications, CastleColors, CastleUIControls;

var
  Window: TCastleWindow;
  Notifications: TCastleNotifications;
begin
  Window := TCastleWindow.Create(Application);
  Window.Open;

  Notifications := TCastleNotifications.Create(Application);
  Notifications.TextAlignment := hpMiddle;
  Notifications.Anchor(hpMiddle);
  Notifications.Anchor(vpBottom, 10);
  Notifications.Color := Yellow;
  Window.Controls.InsertFront(Notifications);

  Notifications.Show('You picked 10 ammo!');

  Application.Run;
end.
