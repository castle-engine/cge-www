<?php
  require_once 'vrml_implementation_common.php';
  x3d_status_header('Programmable shaders', 'shaders',
    'This component defines nodes for using high-level shading languages
     available on modern graphic cards.'
  );

  echo vrmlengine_thumbs(array(
    array('filename' => 'glsl_teapot_demo.png', 'titlealt' => 'Teapot VRML model rendered with toon shading in GLSL'),
    array('filename' => 'glsl_flutter.png', 'titlealt' => 'GLSL demo &quot;flutter&quot; (from FreeWRL examples)'),
    array('html' =>
      '<div class="thumbs_cell_with_text_or_movie">This movie shows GLSL shaders by our engine. You can also '
      . current_www_a_href_size('get AVI version with much better quality', 'movies/2.avi')
      . (!HTML_VALIDATION ?
      '<object class="youtube_thumbnail_video"><param name="movie" value="http://www.youtube.com/v/ag-d-JGvHfQ&hl=en"></param><param name="wmode" value="transparent"></param><embed src="http://www.youtube.com/v/ag-d-JGvHfQ&hl=en" type="application/x-shockwave-flash" wmode="transparent" width="200" height="167"></embed></object>'
      : '')
      . '</div>'),
  ));
?>

<p><?php echo x3d_node_link('ComposedShader'); ?> and
<?php echo x3d_node_link('ShaderPart'); ?> nodes
allow you to write shaders in the <a href="http://www.opengl.org/documentation/glsl/">OpenGL shading language (GLSL)</a>.

<ul>
  <li><p><b>Basic example.</b></p>

    <p>Add inside <tt>Appearance</tt> node VRML code like</p>

<pre class="vrml_code">
shaders ComposedShader {
language "GLSL"
parts [
  ShaderPart { type "VERTEX" url "glsl_phong_shading.vs" }
  ShaderPart { type "FRAGMENT" url "glsl_phong_shading.fs" }
]
}
</pre>

    <p>See <?php echo a_href_page("Kambi VRML test suite",
    "kambi_vrml_test_suite"); ?>, directory <tt>shaders/</tt>
    for working demos of this.</p>
  </li>

  <li><p><b>Inline shader source code.</b></p>

    <p>You can directly write the shader source code inside an URL field
    (instead of putting it in an external file).
    The best way to do this, following the standards, is to use
    the <a href="http://en.wikipedia.org/wiki/Data_URI_scheme">data URI</a>.
    In the simplest case, just start the URL with "<tt>data:text/plain,</tt>"
    and then write your shader code.</p>

    <p>Only in the XML encoding: you can also just place
    shader source code inside the CDATA.</p>

    <p>As a non-standard extension (but compatible at least with
    <a href="http://instant-reality.com/">InstantPlayer</a>)
    we also recognize URL as containing direct shader source if it
    has any newlines
    and doesn't start with any URL protocol, <a href="https://vrmlengine.svn.sourceforge.net/svnroot/vrmlengine/trunk/kambi_vrml_test_suite/shaders/shaders_inlined.x3dv">example: shaders_inlined.x3dv</a>.</p>
  </li>

  <li><p><b>Passing values to GLSL shader uniform variables.</b></p>

    <p>You can also set uniform variables for your shaders from VRML,
    just add lines like</p>

<pre class="vrml_code">
inputOutput SFVec3f UniformVariableName 1 0 0
</pre>

    to your ComposedShader node. These uniforms may also be modified by
    VRML events (when they are <tt>inputOutput</tt> or <tt>inputOnly</tt>),
    for example here's a simple way to pass current VRML time (in seconds)
    to your shader:

<pre class="vrml_code">
# ......
# somewhere within Appearance:
shaders DEF MyShader ComposedShader {
language "GLSL"
parts [
  ShaderPart { type "VERTEX" url "my_shader.vs" }
  ShaderPart { type "FRAGMENT" url "my_shader.fs" }
]
inputOnly SFTime time
}

# ......
# somewhere within grouping node (e.g. at top-level of VRML file)
DEF MyTimer TimeSensor { loop TRUE }
ROUTE MyTimer.time TO MyShader.time
</pre>

    <p>Setting uniform values this way, from VRML fields/events,
    is supported for all required by spec types.
    So you can use VRML/X3D vector/matrix types to
    set GLSL vectors/matrices, you can use VRML/X3D
    multiple-value fields to set GLSL array types and such.</p>

    <p>TODO: <tt>SFImage</tt>, <tt>MFImage</tt> field types are
    not supported yet.</p>
  </li>

  <li><p><b><a name="glsl_passing_uniform_textures">Passing textures to GLSL shader uniform variables.</a></b></p>

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
    in <?php echo a_href_page('Kambi VRML test suite', 'kambi_vrml_test_suite'); ?>
    (look for file <tt>shaders/simple_multitex_shaders.x3dv</tt>),
    <a href="https://vrmlengine.svn.sourceforge.net/svnroot/vrmlengine/trunk/kambi_vrml_test_suite/shaders/simple_multitex_shaders.x3dv">or see it here</a>.
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

    <p>Note that for now you have to pass textures in VRML/X3D events.
    Using <tt>inputOnly</tt> event to pass texture node to GLSL shader
    will not work.</p>
  </li>

  <li><p><b>Passing attributes to GLSL shader uniform variables.</b></p>

    <p>You can also pass per-vertex attributes to your shader.
    You can pass floats, vectors and matrices.
    The way do use this of course follows X3D specification,
    see <?php echo x3d_node_link('FloatVertexAttribute'); ?>,
    <?php echo x3d_node_link('Matrix3VertexAttribute'); ?>,
    <?php echo x3d_node_link('Matrix4VertexAttribute'); ?> nodes.
    You can place them in the <tt>attrib</tt> field of most geometry nodes
    (like <tt>IndexedFaceSet</tt>).</p>

    <p><a href="https://vrmlengine.svn.sourceforge.net/svnroot/vrmlengine/trunk/kambi_vrml_test_suite/shaders/attributes.x3dv">Example attributes.x3dv</a>,
    showing how to pass elevation grid heights by the shader attributes.</p>

  <li><p>TODO: <tt>activate</tt> event doesn't work to relink the GLSL
    program now. (<tt>isSelected</tt> and <tt>isValid</tt> work perfectly for any
    X3DShaderNode.)
  </li>
</ul>

<?php
  x3d_status_footer();
?>
