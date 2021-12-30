<?php echo $toc->html_section(); ?>

<p>Blender versions earlier than 2.80
included a working X3D exporter out-of-the-box.
So you can export your content from Blender to X3D open it with our engine.</p>

<div class="download jumbotron">
    <a class="btn btn-primary btn-lg" href="https://github.com/castle-engine/castle-engine/wiki/Blender">Some hints about exporting from Blender to X3D are here</a>
</div>

<p>We have our own X3D exporter version, which is based on the Blender X3D exporter
and adds some fixes and also <b>support for <a href="x3d_implementation_texturing_extensions.php#section_ext_common_surface_shader">CommonSurfaceShader</a> exporting</b>.

<div class="download jumbotron">
    <a class="btn btn-primary btn-lg" href="https://raw.githubusercontent.com/castle-engine/cge-blender/master/x3d_exporter/castle_engine_x3d.zip"><span class="glyphicon glyphicon-download" aria-hidden="true"></span><br>Download our X3D exporter</a>
</div>

<p>A short installation instruction: just install it like any other Blender add-on (<i>"Install from file..."</i> on <i>"Add-ons"</i> tab).

<p>A <i>very detailed</i> installation instruction:

<ol>
  <li>In Blender, click the <i>"File -&gt; User Preferences"</i> menu item.
  <li>Go to the <i>"Add-ons"</i> tab.
  <li>Click the button <i>"Install from file..."</i> at the bottom of the window that appears.
  <li>Select the <code>castle_engine_x3d.zip</code> file you downloaded, and confirm by clicking <i>"Install from file..."</i> near the top-right corner.
  <li>Find the script box e.g. by searching for <code>"castle"</code> in the search box. You should see a script named <i>"Web3D X3D/VRML2 format (Castle Game Engine Importer/Exporter)"</i>. Activate this script by ticking the checkbox right near it's name.
  <li>Optionally, you can click now <i>"Save User Settings"</i> at the bottom to have this script automatically enabled next time you open Blender.
  <li>Make sure it works: look into the <i>"File -&gt; Export"</i> submenu, it should have now a new option <i>"X3D Extensible 3D (.x3d) (Castle Game Engine Exporter)"</i>
</ol>
