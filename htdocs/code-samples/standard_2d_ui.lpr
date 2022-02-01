uses CastleWindow, CastleUIControls, CastleControls, CastleColors;

var
  Window: TCastleWindow;
  MyLabel: TCastleLabel;
  MyButton: TCastleButton;
begin
  Window := TCastleWindow.Create(Application);
  Window.Open;

  MyButton := TCastleButton.Create(Application);
  MyButton.Caption := 'Click me!';
  MyButton.Anchor(hpMiddle);
  MyButton.Anchor(vpBottom, 10);
  Window.Controls.InsertFront(MyButton);

  MyLabel := TCastleLabel.Create(Application);
  MyLabel.Caption := 'Click on the button!';
  MyLabel.Color := White;
  MyLabel.Anchor(hpMiddle);
  { position label such that it's over the button }
  MyLabel.Anchor(vpBottom, 10 + MyButton.EffectiveHeight + 10);
  Window.Controls.InsertFront(MyLabel);

  Application.Run;
end.
