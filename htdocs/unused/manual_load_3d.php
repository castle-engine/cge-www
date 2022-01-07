<p>You can also download
<?php echo a_href_page('our demo models', 'demo_models'); ?>.
And you can open all the models with
<?php echo a_href_page('view3dscene', 'view3dscene'); ?>,
to see how do they look like before loading them into your game.</p>

<p>And if you want to make your own 3D model, go ahead &mdash; generally use any
3D modeler and export to any 3D format we can handle, preferably X3D.
See <?php echo a_href_page('our guide to creating game data', 'creating_data_intro'); ?> for more information about preparing 3D data.</p>

------------------------------------------------------------------------------

<?php echo $toc->html_section(); ?>

<p>An important strength of our engine is that you can express a lot
of stuff inside your data, that is inside
<?php echo a_href_page('VRML/X3D', 'vrml_x3d'); ?> models.
So many features of our engine
(<?php echo a_href_page('shaders','x3d_implementation_shaders'); ?>,
 <?php echo a_href_page('screen effects', 'x3d_extensions_screen_effects'); ?>,
 <?php echo a_href_page('mirrors', 'x3d_implementation_cubemaptexturing'); ?>
 and many many more) don't have any special Object Pascal examples,
because they are simply not needed. For simple uses, you just define what you
need inside VRML/X3D file (of course, for advanced usage you can do a lot more
with Object Pascal code, and you can always build/modify VRML/X3D graph
by Object Pascal code).
So <b>be sure to grab <?php echo a_href_page('our demo VRML/X3D models', 'demo_models'); ?></b>
 and try opening them with any engine example program (like the one you just created,
or even our <?php echo a_href_page('view3dscene', 'view3dscene'); ?>)
&mdash; you will find that everything just works,
not requiring a single line of Object Pascal code.</p>

<p><b>Our model viewer <?php echo a_href_page('view3dscene', 'view3dscene'); ?></b>
 allows to test your models before loading them to your games.
