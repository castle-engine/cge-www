<?php
  require_once 'x3d_implementation_common.php';
  require_once 'x3d_extensions_functions.php';
  x3d_status_header('Programmable shaders', 'shaders',
    'This component defines nodes for using high-level shading languages
     available on modern graphic cards.'
  );

  echo castle_thumbs(array(
    array('filename' => 'glsl_teapot_demo.png', 'titlealt' => 'Teapot X3D model rendered with toon shading in GLSL'),
    array('filename' => 'glsl_flutter.png', 'titlealt' => 'GLSL demo &quot;flutter&quot; (from FreeWRL examples)'),
    array('html' =>
      '<div class="thumbs_cell_with_text_or_movie">This movie shows GLSL shaders by our engine. You can also '
      . current_www_a_href_size('get AVI version with much better quality', 'movies/2.avi')
      . (!HTML_VALIDATION ?
      '<object class="youtube_thumbnail_video"><param name="movie" value="http://www.youtube.com/v/ag-d-JGvHfQ&hl=en"></param><param name="wmode" value="transparent"></param><embed src="http://www.youtube.com/v/ag-d-JGvHfQ&hl=en" type="application/x-shockwave-flash" wmode="transparent" width="200" height="167"></embed></object>'
      : '')
      . '</div>'),
  ));

  $toc = new TableOfContents(
    array(
      new TocItem('Demos', 'demos'),
      new TocItem('Support', 'support'),
      new TocItem('Features and examples', 'examples'),
      new TocItem('Basic example', 'basic', 1),
      new TocItem('Inline shader source code', 'inline', 1),
      new TocItem('Passing values to GLSL shader uniform variables', 'uniforms', 1),
      new TocItem('Passing textures to GLSL shader uniform variables', 'uniforms_tex', 1),
      new TocItem('Passing attributes to GLSL shader', 'attributes', 1),
      new TocItem('Geometry shaders', 'geometry', 1),
      new TocItem('Geometry shaders before GLSL 1.50 not supported', 'geometry_old', 2),
      new TocItem('TODOs', 'todos', 1),
    ));
  $toc->echo_numbers = true;
?>

<p>Contents:
<?php echo $toc->html_toc(); ?>

<?php echo $toc->html_section(); ?>

<p>For complete demos and tests of these features,
see the <tt>shaders</tt> subdirectory inside <?php
echo a_href_page('our VRML/X3D demo models', 'demo_models'); ?>.</p>

<?php echo $toc->html_section(); ?>

<p><?php echo x3d_node_link('ComposedShader'); ?> and
<?php echo x3d_node_link('ShaderPart'); ?> nodes
allow you to write shaders in the <a href="http://www.opengl.org/documentation/glsl/">OpenGL shading language (GLSL)</a>.

<?php echo $toc->html_section(); ?>

<?php echo $toc->html_section(); ?>

<p>Examples below are in the classic (VRML) encoding.
Add inside the <tt>Appearance</tt> node code like</p>

<pre class="vrml_code">
shaders ComposedShader {
  language "GLSL"
  parts [
    ShaderPart { type "VERTEX"   url "my_shader.vs" }
    ShaderPart { type "FRAGMENT" url "my_shader.fs" }
  ]
}
</pre>

<?php echo $toc->html_section(); ?>

<p>You can directly write the shader source code inside an URL field
(instead of putting it in an external file).
The best way to do this, following the standards, is to use
the <a href="http://en.wikipedia.org/wiki/Data_URI_scheme">data URI</a>.
In the simplest case, just start the URL with "<tt>data:text/plain,</tt>"
and then write your shader code.</p>

<p><a href="http://svn.code.sf.net/p/castle-engine/code/trunk/demo_models/shaders/shaders_inlined.x3dv">Example: shaders_inlined.x3dv</a>.</p>

<p>Only in the X3D XML encoding: you can also place
shader source code inside the CDATA.</p>

<p>As an extension (but compatible at least with
<a href="http://instant-reality.com/">InstantPlayer</a>)
we also recognize URL as containing direct shader source if it
has any newlines and doesn't start with any URL protocol.
But it's better to use "<tt>data:text/plain,</tt>" mentioned above.

<?php echo $toc->html_section(); ?>

<p>You can set uniform variables for your shaders from VRML/X3D,
just add lines like</p>

<pre class="vrml_code">
inputOutput SFVec3f UniformVariableName 1 0 0
</pre>

to your ComposedShader node. These uniforms may also be modified by
VRML/X3D events (when they are <tt>inputOutput</tt> or <tt>inputOnly</tt>),
for example here's a simple way to pass the current time (in seconds)
to your shader:

<pre class="vrml_code">
# somewhere within Appearance:
shaders DEF MyShader ComposedShader {
  language "GLSL"
  parts [
    ShaderPart { type "VERTEX" url "my_shader.vs" }
    ShaderPart { type "FRAGMENT" url "my_shader.fs" }
  ]
  inputOnly SFTime time
}

# somewhere within grouping node (e.g. at the top-level of VRML/X3D file) add:
DEF MyTimer TimeSensor { loop TRUE }
ROUTE MyTimer.time TO MyShader.time

# Note that by default, TimeSensor.time values will be huge,
# which will cause precision problems for shaders.
# You can use our extension:
KambiNavigationInfo { timeOriginAtLoad TRUE }
# or use a different TimeSensor field to measure time.
</pre>

<p>Many VRML/X3D field types may be passed to appropriate GLSL uniform
values. You can even set GLSL vectors and matrices.
You can use VRML/X3D multiple-value fields to set
GLSL array types.</p>

<p>TODO: we support all mappings between VRML/X3D and GLSL types
for uniform values (that are mentioned in X3D spec),
except <tt>SFImage</tt> and <tt>MFImage</tt>.</p>

<a name="glsl_passing_uniform_textures"></a>
<?php echo $toc->html_section(); ?>

<p>You can also specify texture node (as <tt>SFNode</tt> field, or an array
of textures in <tt>MFNode</tt> field) as a uniform field value.
Engine will load and bind the texture and pass to GLSL uniform variable
bound texture unit. This means that you can pass in a natural way
VRML texture node to a GLSL <tt>sampler2D</tt>, <tt>sampler3D</tt>,
<tt>samplerCube</tt>, <tt>sampler2DShadow</tt> and such.</p>

<pre class="vrml_code">
shaders ComposedShader {
  language "GLSL"
  parts [
    ShaderPart { type "FRAGMENT" url
    "data:text/plain,

     uniform sampler2D texture_one;
     uniform sampler2D texture_two;

     void main()
     {
       gl_FragColor = gl_Color *
         max(
           texture2D(texture_one, gl_TexCoord[0].st),
           texture2D(texture_two, gl_TexCoord[1].st));
     }
    " }
  ]
  initializeOnly SFNode texture_one ImageTexture { url "one.png" }
  initializeOnly SFNode texture_two ImageTexture { url "two.png" }
}
</pre>

<p>A full working version of this example can be found
in <?php echo a_href_page('our VRML/X3D demo models', 'demo_models'); ?>
 (look for file <tt>shaders/simple_multitex_shaders.x3dv</tt>),
<a href="http://svn.code.sf.net/p/castle-engine/code/trunk/demo_models/shaders/simple_multitex_shaders.x3dv">or see it here</a>.
</p>

<p>When using GLSL shaders in X3D you should pass all
needed textures to them this way. Normal <tt>appearance.texture</tt>
is ignored when using shaders. However, in our engine,
we have a special case to allow you to specify textures also
in traditional <tt>appearance.texture</tt> field: namely,
when <tt>ComposedShader</tt> doesn't contain any texture nodes,
we will still bind <tt>appearance.texture</tt>. This e.g. allows
you to omit declaring texture nodes in <tt>ComposedShader</tt>
field if you only have one texture, it also allows renderer to
reuse OpenGL shader objects more (as you will be able to DEF/USE
in X3D <tt>ComposedShader</tt> nodes even when they use different
textures). But this feature should
not be used or depended upon in the long run.</p>

<p>Note that for now you have to pass textures in VRML/X3D fields
(<tt>initializeOnly</tt> or <tt>inputOutput</tt>).
TODO: Using <tt>inputOnly</tt> event to pass texture node to GLSL shader
does not work.</p>

<?php echo $toc->html_section(); ?>

<p>You can also pass per-vertex attributes to your shader.
You can pass floats, vectors and matrices.
The way do use this of course follows X3D specification,
see <?php echo x3d_node_link('FloatVertexAttribute'); ?>,
<?php echo x3d_node_link('Matrix3VertexAttribute'); ?>,
<?php echo x3d_node_link('Matrix4VertexAttribute'); ?> nodes.
You can place them in the <tt>attrib</tt> field of most geometry nodes
(like <tt>IndexedFaceSet</tt>).</p>

<p><a href="http://svn.code.sf.net/p/castle-engine/code/trunk/demo_models/shaders/attributes.x3dv">Example attributes.x3dv</a>,
showing how to pass elevation grid heights by the shader attributes.</p>

<?php echo $toc->html_section(); ?>

<?php
  echo castle_thumbs(array(
    array('filename' => 'geometry_shader_fun_smoothing.png', 'titlealt' => 'Geometry shaders fun smoothing demo'),
  ));
?>

<p><b>(This feature is in development now.
You need view3dscene from <a href="http://michalis.ii.uni.wroc.pl/castle-engine-snapshots/">snapshots</a>
or <a href="http://castle-engine.sourceforge.net/engine.php#section_download_src">SVN sources</a>
to test this.)</b></p>

<p>As an extension, we also have <i>geometry shaders</i>
(in addition to standard <i>vertex</i> and <i>fragment shaders</i>).
To use them, you simply set <tt>ShaderPart.type</tt> to <tt>"GEOMETRY"</tt>,
and put code of your geometry shader inside <tt>ShaderPart.url</tt>.</p>

<p><b>What is a geometry shader?</b>
A geometry shader is executed once for each primitive, like a single triangle.
Geometry shader works <i>between</i> the vertex shader and fragment shader
&mdash; it knows all the outputs from the vertex shader.
<!--
and it generates inputs to the fragment shader.
(More precisely, geometry shader generates inputs for the rasterizer,
and the interpolated values will be given to the fragment shader.)
-->
Geometry shader can use the information about given primitive (vertex positions
from vertex shader &mdash; in any space, and all vertex attributes) and can generate other primitives.
A single geometry shader may generate any number of primitives
(separated by the <tt>EndPrimitive</tt> call), so you can easily "explode"
a simple input primitive into a number of others
(or delete some original primitives based on some criteria).
The type of the output primitive may be different
than input &mdash; for example, you can change triangles into
points or the other way around.</p>

<p>Examples of geometry shaders with <tt>ComposedShader</tt>:</p>

<ul>
  <li><a href="http://svn.code.sf.net/p/castle-engine/code/trunk/demo_models/shaders/geometry_shader.x3dv">Download
    a basic example X3D file with geometry shaders</a></li>
  <li><a href="http://svn.code.sf.net/p/castle-engine/code/trunk/demo_models/shaders/geometry_shader_fun_smoothing.x3dv">Another
    example of geometry shaders: geometry_shader_fun_smoothing</a>.</li>
</ul>

<p>We have also a more flexible approach to geometry shaders
as part of our <?php echo a_href_page('compositing shaders', 'compositing_shaders'); ?>
 extensions. The most important advantage is that you can implement
only the geometry shader, and use the default vertex and fragment shader code
(that will do the boring stuff like texturing, lighting etc.).
Inside the geometry shader you have functions <tt>geometryVertexXxx</tt>
to pass-through or blend input vertexes in any way you like.
Everything is described in detail in our
<?php echo a_href_page('compositing shaders documentation', 'compositing_shaders'); ?>,
 in particular see the <a href="http://castle-engine.sourceforge.net/compositing_shaders_doc/html/chapter.geometry_shaders.html">the chapter "Extensions for geometry shaders"</a>.</p>

<p>Examples of geometry shaders with <tt>Effect</tt>:</p>

<ul>
  <li><a href="http://svn.code.sf.net/p/castle-engine/code/trunk/demo_models/compositing_shaders/geometry_shader_simple.x3dv">geometry_shader_simple</a></li>
  <li><a href="http://svn.code.sf.net/p/castle-engine/code/trunk/demo_models/compositing_shaders/geometry_shader_effects.x3dv">geometry_shader_effects</a></li>
  <li><a href="http://svn.code.sf.net/p/castle-engine/code/trunk/demo_models/compositing_shaders/geometry_shader_optional.x3dv">geometry_shader_optional</a></li>
</ul>

<?php echo $toc->html_section(); ?>

<p>Our implementation of geometry shaders is directed only at geometry
shaders as available in the OpenGL core 3.2 and later (GLSL version is 1.50 or later).</p>

<!-- http://www.lighthouse3d.com/tutorials/glsl-core-tutorial/geometry-shader/ -->

<p>Earlier OpenGL and GLSL versions had geometry shaders
only available through extensions:
<a href="http://www.opengl.org/registry/specs/ARB/geometry_shader4.txt">ARB_geometry_shader4</a>
or <a href="http://www.opengl.org/registry/specs/EXT/geometry_shader4.txt">EXT_geometry_shader4</a>.
They had the same purpose, but the syntax and calls were different and incompatible.
For example, vertex positions were in <tt>gl_PositionIn</tt> instead of <tt>gl_in</tt>.

<p>The most important incompatibility was that the <i>input
and output primitive types</i>, and the <i>maximum number of vertices
generated</i>, were specified outside of the shader source code.
To handle this, an X3D browser would have to do special OpenGL calls
(<tt>glProgramParameteriARB/EXT</tt>),
and these additional parameters must be placed inside the special
fields of the <tt>ComposedShader</tt>.
<a href="http://doc.instantreality.org/documentation/nodetype/ComposedShader/">InstantReality
ComposedShader</a> adds additional fields <tt>geometryInputType</tt>,
<tt>geometryOutputType</tt>, <tt>geometryVerticesOut</tt> specifically
for this purpose
(see also the bottom of <a href="http://doc.instantreality.org/tutorial/shader-programs/">InstantReality
shaders overview</a>).</p>

<!--
http://www.opengl.org/wiki/Geometry_Shader
http://www.opengl.org/wiki/Tutorial4:_Using_Indices_and_Geometry_Shaders_%28C_/SDL%29
 -->

<p>See <a href="http://en.wikipedia.org/wiki/GLSL#A_sample_trivial_GLSL_geometry_shader">simple
example on Wikipedia of GLSL geometry shader differences before and after GLSL 1.50</a>.</p>

<p>We have decided to <b>not implement the old style geometry shaders</b>.
The implementation would complicate the code
(need to handle new fields of the <tt>ComposedShader</tt> node),
and have little benefit (usable only for old OpenGL versions;
to make geometry shaders work with both old and new OpenGL versions,
authors would have to provide two separate versions of their geometry shaders).</p>

<p>So we just require new geometry shaders to conform to GLSL &gt;= 1.50
syntax.
On older GPUs, you will not be able to use geometry shaders at all.</p>

<?php echo $toc->html_section(); ?>

<p>TODO: <tt>activate</tt> event doesn't work to relink the GLSL
program now. (<tt>isSelected</tt> and <tt>isValid</tt> work perfectly for any
X3DShaderNode.)

<?php
  x3d_status_footer();
?>
