uses CastleWindow, CastleConfig, CastleApplicationProperties, CastleControls,
  CastleColors;
var
  Window: TCastleWindow;
  LabelForParameter: TCastleLabel;
  MyParameter: string;
begin
  { set ApplicationName,
    this is used by UserConfig.Load to determine config file location. }
  ApplicationProperties.ApplicationName := 'my_game_name';

  { open Window }
  Window := TCastleWindow.Create(Application);
  Window.Container.BackgroundColor := White;
  Window.Open;

  { load UserConfig }
  UserConfig.Load;

  { load parameter from UserConfig }
  MyParameter := UserConfig.GetValue('my_parameter', 'default_value');

  { create UI and show show parameter from UserConfig }
  LabelForParameter := TCastleLabel.Create(Application);
  LabelForParameter.Caption := 'My parameter is now equal: ' + MyParameter;
  Window.Controls.InsertFront(LabelForParameter);

  { do the main part of your program }
  // MyParameter := 'some other value'; // test
  Application.Run;

  { save parameter }
  UserConfig.SetValue('my_parameter', MyParameter);
  // or like this:
  UserConfig.SetDeleteValue('my_parameter', MyParameter, 'default_value');

  { save UserConfig }
  UserConfig.Save;
end.
