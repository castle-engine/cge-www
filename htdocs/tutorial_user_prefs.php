<?php
require_once 'castle_engine_functions.php';
tutorial_header('User preferences');
?>

<p>That's easy: Use <?php api_link('CastleConfig', 'CastleConfig.html'); ?>.
Various engine components automatically register their callbacks there,
so they can load/save user preferences. The code:</p>

<?php echo pascal_highlight(
'function MyGetApplicationName: string;
begin
  Result := \'my_game_name\';
end;

{ your main program body: }
begin
  { make sure application name is correct by setting OnGetApplicationName: }
  OnGetApplicationName := @MyGetApplicationName;

  { make sure you have created all stuff that registers user preferences first }
  SoundEngine; // if you want to save sound config
  RecentMenu := TCastleRecentFiles.Create(nil); // if you use "recent files" menu

  { load config from file }
  Config.Load;

  ... { do your program }

  { save config to file }
  Config.Save;
end.'); ?>

<?php
tutorial_footer();
?>
