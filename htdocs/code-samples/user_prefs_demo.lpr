uses CastleWindow, CastleConfig, CastleApplicationProperties, CastleControls;
var
  Window: TCastleWindow;
  LabelForParameter: TCastleLabel;
  MyParameter: string;
begin
  { make sure application name is correct,
    this is used by UserConfig.Load to determine config file location. }
  ApplicationProperties.ApplicationName := 'my_game_name';

  { load config from file }
  UserConfig.Load;

  { load data }
  MyParameter := UserConfig.GetValue('my_parameter', 'default_value');

  { do the main part of your program }
  Window := TCastleWindow.Create(Application);

  LabelForParameter := TCastleLabel.Create(Application);
  LabelForParameter.Caption := 'My parameter is now equal: ' + MyParameter;
  Window.Controls.InsertFront(LabelForParameter);

  // MyParameter := 'some other value'; // test

  Window.OpenAndRun;

  { save data }
  UserConfig.SetValue('my_parameter', MyParameter);
  // or like this:
  UserConfig.SetDeleteValue('my_parameter', MyParameter, 'default_value');

  { save config to file }
  UserConfig.Save;
end.
