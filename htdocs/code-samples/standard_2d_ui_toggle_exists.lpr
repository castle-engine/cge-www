uses CastleWindow, CastleUIControls, CastleControls, CastleColors;

var
  Window: TCastleWindow;
  MyRect: TCastleRectangleControl;
  MyButton: TCastleButton;

type
  TEventHandler = class
    class procedure ButtonClick(Sender: TObject);
  end;

class procedure TEventHandler.ButtonClick(Sender: TObject);
begin
  MyRect.Exists := not MyRect.Exists;
end;

begin
  Window := TCastleWindow.Create(Application);
  Window.Open;

  MyButton := TCastleButton.Create(Application);
  MyButton.Caption := 'Toggle rectangle';
  MyButton.Anchor(hpMiddle);
  MyButton.Anchor(vpBottom, 10);
  { use a trick to avoid creating a useless instance
    of the TEventHandler class. }
  MyButton.OnClick := @TEventHandler(nil).ButtonClick;
  Window.Controls.InsertFront(MyButton);

  MyRect := TCastleRectangleControl.Create(Application);
  MyRect.Color := Yellow;
  MyRect.Width := 200;
  MyRect.Height := 200;
  MyRect.Anchor(hpMiddle);
  MyRect.Anchor(vpMiddle);
  Window.Controls.InsertFront(MyRect);

  Application.Run;
end.
