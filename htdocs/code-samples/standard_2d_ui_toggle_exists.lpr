uses CastleWindow, CastleUIControls, CastleControls, CastleColors;

var
  Window: TCastleWindow;
  MyRect: TCastleRectangleControl;
  HideRectButton: TCastleButton;

type
  TEventHandler = class
    class procedure ButtonClick(Sender: TObject);
  end;

class procedure TEventHandler.ButtonClick(Sender: TObject);
begin
  MyRect.Exists := not MyRect.Exists;
  HideRectButton.Pressed := not MyRect.Exists;
end;

begin
  Window := TCastleWindow.Create(Application);
  Window.Open;

  HideRectButton := TCastleButton.Create(Application);
  HideRectButton.Caption := 'Hide rectangle';
  HideRectButton.Toggle := true;
  HideRectButton.Anchor(hpMiddle);
  HideRectButton.Anchor(vpBottom, 10);
  { use a trick to avoid creating a useless instance
    of the TEventHandler class. }
  HideRectButton.OnClick := @TEventHandler(nil).ButtonClick;
  Window.Controls.InsertFront(HideRectButton);

  MyRect := TCastleRectangleControl.Create(Application);
  MyRect.Color := Yellow;
  MyRect.Width := 200;
  MyRect.Height := 200;
  MyRect.Anchor(hpMiddle);
  MyRect.Anchor(vpMiddle);
  Window.Controls.InsertFront(MyRect);

  Application.Run;
end.
