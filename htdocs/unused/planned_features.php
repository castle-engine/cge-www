<?php /*
  <li><p><b>Blender X3D exporter improvements</b>

    <p><i>Rejected. Just export to glTF, the exporter is way better than X3D exporter ever was. glTF has more momentum as an interchange format than X3D.</i>

    <p>Current Blender X3D exporter doesn't support animations,
    configuring collisions (X3D <?php api_link('Collision', 'X3DNodes.TCollisionNode.html'); ?> node),
    3D sound sources and more.
    We would like to fix it!:) This will be useful for everyone using Blender and X3D,
    not only with our engine.

    <p>See also our page about <a href="creating_data_blender.php">creating data in Blender</a>
    and <a href="https://github.com/castle-engine/cge-www/blob/master/htdocs/doc/obsolete/blender_x3d_exporter.adoc">hints
    about exporting from Blender to X3D</a>.
*/ ?>

------------------------------------------------------------------------------

<!--
    <p>Maybe Metal renderer. Only <i>maybe</i>, as it's an API used by only one platform &mdash; iOS. So far, OpenGLES serves us good on iOS. In practice, this depends on the future, how much will Metal matter in a few years.

    <p>Maybe Direct3D renderer. Only <i>maybe</i>, as it's an API used only on Windows. So far, OpenGL serves us good. The rare platforms where OpenGL had problems on Windows are 1. really old right now (really old Intel GPUs), 2. we can consider using an OpenGLES->Direct3D bridge, like ANGLE, for them.
-->

------------------------------------------------------------------------------

    <!--
    supports WebGL, and has generics and <code>Generics.Collections</code> unit, and in general should be able to consume CGE code. It remains to actually do it, i.e. give our code to pas2js and go through all necessary changes. <a href="https://castle-engine.io/wp/2020/07/20/pas2js-with-generics-rocks-and-makes-castle-game-engine-for-webgl-possible/">See this news post.</a>
    -->

------------------------------------------------------------------------------

  <!--
    Priority lowered.
    We don't care so much about X3D interoperation, as most of people use glTF.
    We don't need JS.

------------------------------------------------------------------------------

  <li>
    <p><b>Scripting in JavaScript</b></p>

    <p>Allow to use JavaScript (ECMAScript) directly inside VRML/X3D files (in Script nodes). This will follow VRML/X3D specification. Implementation will be through <a href="http://besen.sourceforge.net/">besen</a> (preferably, if it will work well enough), SpiderMonkey, or maybe some other JS library.</p>
  </li-->

------------------------------------------------------------------------------
    <!--
    Maybe following the X3D "particle system" component, so it will be saved and loaded as an X3D file.

    Note: My *highest* priority is to have particle system that has a great visual designer.
    Having something conforming to X3D "particle system" component would be great,
    but it is not highest priority.
    Having the visual designer available in CGE editor would also be great,
    but it is not the most critical priority.

    It would be nice to be able to export Blender particle engine to it, but possibly it's not really doable (different particle physics in Blender than X3D, we should go with own designer following more X3D).
    -->

    <!-- Use ARB_point_sprite? -->
