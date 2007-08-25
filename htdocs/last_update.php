<?php
/*
language: update
  l-bin = recompile under Linux and update Linux binary archive, tar.gz,
  f-bin = same thing for FreeBSD,
  w-bin = recompile under Windows and update Windows binary archive, zip,
  src = update (using update_pascal_src.sh) sources specific to a program
  units-src = update units sources, i.e. `update_pascal_src.sh units'
----------------------------------------
Text for next update:
----------------------------------------
Blockers before next WWW update:
after update:
*/
?>

<p><b>Latest update:</b>

<div class="latest_update_description">
<p><b>August 25, 2007:</b>
<ul>
  <li><?php echo a_href_page('view3dscene 2.2.0', 'view3dscene') ?> release:
    view3dscene can display animations now (for now in <?php echo a_href_page(
    "Kanim (Kambi VRML engine animations) format", 'kanim_format'); ?> and
    MD3).</li>
  <li><?php echo a_href_page('Kambi VRML test suite 1.1.0',
    'kambi_vrml_test_suite'); ?> release: many kanim demos added.</li>
  <li><?php echo a_href_page('Kambi VRML game engine 1.1.0',
    'kambi_vrml_game_engine'); ?> release: many changes, for animations
    in view3dscene, also GLMenu and GameSoundEngine units added
    (some "The Castle" code improved and moved to a generally-usefull
    units area), bugfixes to MD3 texture handling.</li>
</ul>
</div>

<p>See <?php echo
a_href_page('the log of previous changes to these pages', 'changes_log') ?>.
