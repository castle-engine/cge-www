<?php
  require_once 'tutorial_common.php';
  tutorial_header('User preferences');
?>

Easy: Use CastleConfig. Various engine components automatically register that they can load/save user preferences. So

[[
{ make sure you have created all stuff that registers user preferences first }
SoundEngine; // if you want to save sound config
RecentMenu := TCastleRecentFiles.Create(nil); // if you use "recent files" menu

{ make sure application name is correct if you want by setting OnGetApplicationName, like this : }

function MyGetApplicationName: string;
begin
  Result := 'glplotter';
end;

...
  OnGetApplicationName := @MyGetApplicationName;

{ load config from file }
Config.Load;

... { do your program }

{ save config to file }
Config.Save;
]]

<?php
  tutorial_footer();
?>
