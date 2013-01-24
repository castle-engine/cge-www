<?php
require_once 'castle_engine_functions.php';
tutorial_header('Scene manager', '3D world information');
?>

<p>Actually, this step of the tutorial is already done for you: scene
manager is already created and ready for use, in SceneManager property
of the TCastleControl or TCastleWindow instance. But let's stop for a
second to understand <b>what scene manager is</b>, as it's quite
central idea to how you work with the engine.</p>

<p>Scene manager is a single TCastleSceneManager class instance that
knows literally everything about your 3D world. It is essential to
have it, and add all your 3D stuff to it.</p>

<p>In the simple scenario, by default TCastleSceneManager also acts as a
viewport filling the whole window. So the whole OpenGL context is
drawn to show your 3D world. In more complex scenarios you can have
many smaller viewports inside your window using TCastleViewport
controls (we'll not discuss it in this tutorial, see <a
href="http://castle-engine.sourceforge.net/vrml_engine_doc/output/xsl/html/section.custom_viewports.html">custom
viewports notes</a>, and <tt>multiple_viewports</tt> example in engine
sources). You can also turn off scene manager from being a viewport
(DefaultViewport := false), and then scene manager is really
<b>only</b> something that keeps track of 3D world, and nothing more.</p>

<p>As I said, you actually already have SceneManager property of your
window/control, so the work is done for you. If more advanced
scenarios, you may want to create and manage scene manager yourself.</p>

<?php
tutorial_footer();
?>
