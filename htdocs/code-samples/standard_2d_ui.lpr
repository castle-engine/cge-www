uses CastleWindow, CastleUIControls, CastleControls;

var
  Window: TCastleWindow;
  MyLabel: TCastleLabel;
  MyButton: TCastleButton;
begin
  Window := TCastleWindow.Create(Application);

  MyButton := TCastleButton.Create(Application);
  MyButton.Caption := 'Click me!';
  MyButton.Anchor(hpMiddle);
  MyButton.Anchor(vpBottom, 10);
  Window.Controls.InsertFront(MyButton);

  MyLabel := TCastleLabel.Create(Application);
  MyLabel.Caption := 'Click on the button!';
  MyLabel.Anchor(hpMiddle);
  { position label such that it's over the button }
  MyLabel.Anchor(vpBottom, 10 + MyButton.CalculatedHeight + 10);
  Window.Controls.InsertFront(MyLabel);

  Window.OpenAndRun;
end.