uses CastleWindow, CastleGameNotifications, CastleColors, CastleUIControls;

var
  Window: TCastleWindow;
begin
  Window := TCastleWindow.Create(Application);

  Notifications.TextAlignment := hpMiddle;
  Notifications.Anchor(hpMiddle);
  Notifications.Anchor(vpBottom, 10);
  Notifications.Color := Yellow;
  Window.Controls.InsertFront(Notifications);

  Notifications.Show('You picked 10 ammo!');

  Window.OpenAndRun;
end.