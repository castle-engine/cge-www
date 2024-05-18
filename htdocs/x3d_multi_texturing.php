<?php
require_once 'castle_engine_functions.php';
castle_header('X3D MultiTexturing problems');

global $toc;
$toc = new TableOfContents(
  array(
    new TocItem('Future: where do we go with MultiTexture in X3D > 4.0', 'future'),
    new TocItem('Tests results', 'tests'),
      new TocItem('Modes and sources', NULL, 1),
      new TocItem('Modes blend', NULL, 1),
      new TocItem('Modes modulate add order', NULL, 1),
      new TocItem('Primitives', NULL, 1),
      new TocItem('Functions', NULL, 1),
      new TocItem('Transform and coordinates faces', NULL, 1),
      new TocItem('Transform and coordinates quads', NULL, 1),
      new TocItem('Image with movie multi texture', NULL, 1),
      new TocItem('Material color mixed with texture color', NULL, 1),
      new TocItem('Subtract and force alpha', NULL, 1),
      new TocItem('Subtract rgb various sources', NULL, 1),
    new TocItem('About tests', 'about_tests'),
      new TocItem('License', NULL, 1),
      new TocItem('How these files were created', NULL, 1),
      new TocItem('Details about browsers tested', 'tests_details', 1),
    new TocItem('Problems and proposed solutions', 'problems_solutions'),
    new TocItem('Proposed improved MultiTexture.mode specification', 'proposed_mode'),
    new TocItem('Proposed MultiTexture.source extension', 'proposed_source'),
    new TocItem('(FIXED IN X3D 4) Related single-texturing problem: RGB texture color by default modulates material color', 'default_texture_mode'),
  ));

$tests_url = 'https://raw.githubusercontent.com/castle-engine/demo-models/master/multi_texturing/';
?>

<?php echo pretty_heading('X3D MultiTexturing problems and proposed solutions'); ?>

<p>The X3D specification has unfortunately
some problems related to it's multi-texturing nodes,
mostly <code>MultiTexture</code> node.
I have documented these problems below, along with the tests on various
X3D browsers, and with the proposed solutions.

<p>Contents:
<?php echo $toc->html_toc(); ?>

<?php echo $toc->html_section(); ?>

<p><a href="https://github.com/michaliskambi/x3d-tests/wiki/Deprecate-some-unused-and-badly-specified-MultiTexturing-specification-pieces">See my wiki page "Deprecate some unused and badly specified MultiTexturing specification pieces"</a> about future multi-texturing changes I would like to see in X3D.

<p>In short &mdash; I think we should just deprecate (a subset of) <code>MultiTexture</code> node. It does not have much practical usage, it does not have much testcases, there seems to be little desire in Web3D community to fix it &mdash; likely because not many people use it. Deprecating/removing some pieces of X3D spec around <code>MultiTexture</code> node seems like the most efficient thing to do.

<?php echo $toc->html_section(); ?>

<p>X3D multi-texturing tests are available <a href="https://github.com/castle-engine/demo-models/tree/master/multi_texturing">inside the Castle Game Engine "Demo Models" repository</a>. It's easiest to just download whole <a href="https://github.com/castle-engine/demo-models">demo-models</a> repo to your disk, and then explore <code>multi_texturing</code> subdirectory. All tests are in X3D classic and XML encodings (equivalent), and with reference screenshots.

<p>All tests are also listed below, roughly in the order basic -&gt; advanced.
Click on the image to view reference rendering
(matches <?php echo a_href_page("view3dscene", "view3dscene") ?>
 from snapshots result, as our view3dscene implements all proposed specification
fixes). Download X3D in classic or XML version and open with X3D browser of your
choice. Files in <i>X3D classic (VRML) encoding</i> contain many comments,
read them to know what the test is about!

<?php
$test_number = 1;
function tests_row($name, $description, $test_results)
{
  global $tests_url, $test_number, $toc;

  echo $toc->html_section();

  echo cgeImg('block', array(
    array(
      'url_full' => "${tests_url}${name}_screen.png",
      'url_thumb' => "${tests_url}${name}_screen_thumb.png",
      'titlealt' => "${name}"
    ),
  ));

  echo "<p>${description}</p>

  <ul>
    <li><a href=\"${tests_url}${name}.x3dv\">X3D (classic) with comments inside</a>
    <li><a href=\"${tests_url}${name}.x3d\">X3D (XML)</a>
  </ul>

  <p>Test results:";

  if (isset($test_results['special'])) {
    echo '<p style="padding: 0.25em">' . $test_results['special'] . '</p>';
  } else {
    echo "
      <ul>
        <li><i>FreeWRL</i>: ${test_results['freewrl']}
        <li><i>BS Contact</i> (<a href=\"https://raw.github.com/wildpeaks/x3d-multi-texture/master/BS%20Contact/${name}.png\">Screen</a>) ${test_results['bscontact']}
        <li><i>Instant Player</i> (<a href=\"https://raw.github.com/wildpeaks/x3d-multi-texture/master/Instant%20Player/${name}.png\">Screen</a>) ${test_results['instantplayer']}
        <li><i>Octaga Player</i>: (<a href=\"https://raw.github.com/wildpeaks/x3d-multi-texture/master/Octaga%20Player/${name}.png\">Screen</a>) ${test_results['octaga']}
        ";
    if (array_key_exists('x_ite', $test_results)) {
      echo "<li><i>X_ITE</i>: ${test_results['x_ite']}";
    }
    if (array_key_exists('x3dom', $test_results)) {
      echo "<li><i>X3DOM</i>: ${test_results['x3dom']}";
    }
    echo "</ul>";
  }

  $test_number++;
}
tests_row('modes_and_sources',
  'Test various values for <code>MultiTexture.mode</code> and <code>MultiTexture.source</code>.',
  array(
    'freewrl' => 'Incorrect. MultiTexture.source seems ignored by FreeWRL. MultiTexture.mode = SUBTRACT is weird (not sure what is does). MultiTexture.mode = SELECTARG2 seems equal to MultiTexture.mode = SELECTARG1. Other MultiTexture.mode values seem Ok.',
    'bscontact' => 'Incorrect MultiTexture.mode = SUBTRACT column (all the other columns are correct, so BS Contact is closest to reference/view3dscene). Looks like BS Contact subtracts only RGB channel, does not touch alpha. This contradicts our proposed clarifications. The X3D spec is ambigous about this, see problem 4.',
    'instantplayer' => 'Incorrect, many problems. MultiTexture.source does something weird (not sure why it changes the result like that). Various MultiTexture.mode values incorrectly handled. Note MultiTexture.mode = SUBTRACT is correct (subtract alpha 1-1 makes invisible surface).',
    'octaga' => 'Incorrect, many problems.',
  ));
tests_row('modes_blend',
  'Test of <code>MultiTexture</code> special modes <code>"BLENDxxx"</code>.',
  array(
    'freewrl' => 'All incorrect. Not sure how FreeWRL interprets MultiTexture.mode = BLENDxxx, it doesn\'t seem to follow spec or match reference images.',
    'bscontact' => 'Ok.',
    'instantplayer' => 'All incorrect.',
    'octaga' => 'Incorrect 2nd, 3rd and 4th quads. So only BLENDDIFFUSEALPHA is correct.',
  ));
tests_row('modes_modulate_add_order',
  'Test of multitexturing <code>MODULATE</code> and <code>ADD</code> modes used together.
   Shows that <code>A * B + C &lt;&gt; A * C + B</code>
   (compare 3rd and 4th box).

   <p><small>Note that whether the 1st and 2nd cube should be yellowish or not is a separate question, related to the <a href="#section_default_texture_mode">default mode when using single-texturing (see below on this page, and test 9)</a>. For this test, we accepted the test as "Ok" regardless if the 1st and 2nd cubes are yellowish or not. Only 3rd and 4th cubes of this test were taken into account when judging if test passed/failed.</small></p>',
  array(
    'freewrl' => 'Ok.',
    'bscontact' => 'Possibly incorrect? 3rd cube should look <i>a little</i> different than 4th, but it <i>seems</i> exactly the same (unless it is just a lighting playing tricks).',
    'instantplayer' => 'Incorrect (3rd and 4th cube). Not really sure what is happening there, why the effet is like it is.',
    'octaga' => 'Ok.',
  ));
tests_row('primitives',
  'Test <code>MultiTexture</code> on primitives (<code>Box</code>, <code>Sphere</code>, <code>Cone</code>, <code>Cylinder</code>).',
  array(
    'freewrl' => 'Ok.',
    'bscontact' => 'Incorrect. Texture transformation is not applied, but at least two textures are mixed Ok.',
    'instantplayer' => 'Incorrect. Multi-texturing is not used (squirrel textue is not mixed with brick texture). But texture transformation for squirrel is applied.',
    'octaga' => 'Ok.',
  ));
tests_row('functions',
  'Test <code>MultiTexture.function</code>.',
  array(
    'freewrl' => 'All incorrect. It seems FreeWRL doesn\'t handle MultiTexture.function.',
    'bscontact' => 'Incorrect: 2nd and 3rd cube invalid. It looks like BS Contact supports MultiTexture.function (COMPLEMENT and ALPHAREPLICATE), but the COMPLEMENT support is buggy.',
    'instantplayer' => 'Incorrect 2nd and 3rd, it doesn\'t seem to apply MultiTexture.function at the right place. This is actually similar to BS Contact, but definitely contradicts the spec.',
    'octaga' => 'Incorrect. Looks like COMPLEMENT negates the alpha as well, which may be caused by specification problem 5.',
  ));
tests_row('transform_and_coordinates_faces',
  'Test various <code>MultiTextureTransform</code> and <code>MultiTextureCoordinate</code> values.',
  array(
    'freewrl' => 'Incorrect, various problems. It seems FreeWRL doesn\'t honor the multi-texture transformation properly, it also makes warnings "not enough textures in MultiTextureTransform...." instead of following the spec that says when identity matrices are assumed for transformation. Possibly caused by spec problem 6. below.',
    'bscontact' => 'Incorrect, various problems (but *different* than e.g. FreeWRL problems).',
    'instantplayer' => 'Incorrect, various problems, but *different* than BS Contact and FreeWRL.',
    'octaga' => 'Incorrect, various problems.',
  ));
tests_row('transform_and_coordinates_quads',
  'Test <code>MultiTexture</code> together with <code>IndexedQuadSet</code> from CAD component.
   Very similar to <code>transform_and_coordinates_faces.x3dv</code>
   (in fact the result should look <b>exactly</b> the same) but now uses
   <code>IndexedQuadSet</code> instead of <code>IndexedFaceSet</code>.',
  array(
    'freewrl' => 'FreeWRL doesn\'t support CADGeometry component. Results are incorrect (you see nothing), but that\'s somewhat acceptable since the console warns that CADGeometry level support is 0 (none) in FreeWRL.',
    'bscontact' => 'BS Contact doesn\'t seem to support CADGeometry component.',
    'instantplayer' => 'Incorrect. Result equal to 6. That\'s good, this means that CADGeometry quads correctly work with multi-texturing. But, since results of test 6. were not correct, results for test 7. show exactly the same problems.',
    'octaga' => 'Incorrect. Equal to 6. Which is good, it means CADGeometry quads work with multi-texturing. But, since result 6. was incorrect, this is incorrect too.',
  ));
tests_row('image_with_movie_multi_texture',
  'Test <code>ImageTexture</code> and <code>MovieTexture</code> mixing using <code>MultiTexture</code>.',
  array(
    'freewrl' => 'Incorrect. FreeWRL doesn\'t seem to support MovieTexture (although it doesn\'t complain when we request Texturing component at level 3, so it <i>should</i> support MovieTexture). Also makes warnings "not enough textures in MultiTextureTransform....", so probably would also exhibit problems from test 6.',
    'bscontact' => 'Incorrect. MovieTexture support is weird (movie seems played in a separate window instead of as a texture). Also, transformation of squirrel texture is wrong.',
    'instantplayer' => 'Incorrect. MovieTexture does not seem supported, at least for multi-texturing.',
    'octaga' => 'Incorrect. MovieTexture not supported? At least for multi-texturing.',
  ));
tests_row('material_color_mixed_with_texture_color',
  'This is not a <code>MultiTexture</code> test, but it tests a feature related
   to some multi-texturing problems: how various X3D browsers mix (single)
   texture with <code>Material.diffuseColor</code> and <code>Color</code> node.
   See <a href="#section_default_texture_mode">lower on this page for details
   why this is tested</a>.

   <p>The reference of this test (and view3dscene result) follows X3D 4.0
   and our idea to <i>always</i> modulate by default.
   This was different in X3D 3 specification.
   See <a href="https://github.com/michaliskambi/x3d-tests/wiki/Make-RGB-and-grayscale-textures-treatment-consistent">Make RGB and grayscale textures treatment consistent</a>.</p>
   ',
  array(
    'freewrl' => '<p>Version: 1.22.13: Seems to never mix texture color with Material.diffuseColor (for both RGB (correct) and grayscale (incorrect) textures), and always mixes texture color with Color node (for both RGB (incorrect) and grayscale (correct) textures).<br>
<p>Version: 4.0.0: RGB texture is multiplied with Material.diffuseColor (incorrect, although this is how we propose to change X3D spec), grayscale texture is multiplied with Material.diffuseColor (correct), RGB texture overrides Color node (correct) and grayscale texture multiplies Color node (correct).',
    'bscontact' => '<p>Version: 8.101: RGB texture replaces Material.diffiseColor (correct). Grayscale texture is <i>replaced by</i> Material.diffuseColor (incorrect and weird). RGB texture modulates with Color node (incorrect according to spec). Grayscale texture modulates Color node (correct).
<p>Version: 8.300: RGB texture overrides Material.diffuseColor (correct), grayscale texture is multiplied with Material.diffuseColor (correct), RGB texture multiplies Color node (incorrect) and grayscale texture multiplies Color node (correct).',
    'instantplayer' => '<p>Version: 2.1.0: Equal to BS Contact 8.101 result for this test, which means incorrect (but at least, this time, consistent with BS Contact). 2.2.0: it seems it changed to be better (but still not exactly spec-complaing): InstantPlayer 2.2.0 doesn\'t mix texture color with <code>Material.diffuseColor</code> for RGB textures (correct) and does mix with grayscale textures (correct). However, it always mixes texture color with <code>Color</code> (for both RGB (incorrect) and grayscale (correct) textures).
<p>Version: 2.8.0: Equal to BS Contact 8.300 result for this test (so almost correct, only RGB texture incorrectly multiplies Color node).',
    'octaga' => '<p>Version: 4.0.3: RGB texture overrides Material.diffuseColor (correct). Grayscale texture is overridden by Material.diffuseColor (incorrect and weird, seems to match BS Contact 8.101). RGB texture overrides Color node (correct). Grayscale texture modules with Color node (correct).
<p>Version: 5.0.0: All correct! Congrats, <b>this is the only browser that handles all 4 cases following the specification</b>: RGB texture overrides Material.diffuseColor (correct), grayscale texture is multiplied with Material.diffuseColor (correct), RGB texture overrides Color node (correct) and grayscale texture multiplies Color node (correct).',
    'x_ite' => '<p>Version 4.1.4-200: In all cases, texture color is multiplied by Material.diffuseColor or Color. This contradicts the X3D specification, but matches view3dscene, and is exactly as we propose to require in future X3D versions.',
    'x3dom' => '<p>Version 1.7.2: In all cases, texture color <i>overrides</i> Material.diffuseColor or Color, even when the texture is grayscale. This is incorrect.',
  ));
tests_row('subtract_and_force_alpha',
  'Test <code>MultiTexture</code> with separate modes and sources for RGB/alpha,
   see below for our proposal to allow separate RGB/alpha specification
   for modes and sources (problem 1.), and <a href="#section_proposed_mode">proposed extended MultiTexture.mode table</a>.',
  array(
    'special' => 'Testing this is not fair. All VRML/X3D browsers except view3dscene fail on this, because this tests a proposed (not yet part of X3D spec) extension to specify separate modes and sources for RGB/Alpha (and clear some of the confusion around modes spec along the way). See <a href="#section_proposed_mode">lower on this page about proposed separate MultiTexture.mode</a> and <a href="#section_proposed_source">lower on this page about proposed separate MultiTexture.source</a>',
  ));
tests_row('subtract_rgb_various_sources',
  'One more test of <code>MultiTexture</code> with separate modes and sources for RGB/alpha.
   Similar to "subtract" column of <code>modes_and_sources</code>,
   but showing what happens when we subtract only RGB.',
  array(
    'special' => 'Testing this is not fair. See previous test for more comments.',
  ));
?>

<?php echo $toc->html_section(); ?>

<?php echo $toc->html_section(); ?>

<p>For the widest possible use, consider these files
public domain, you're welcome to copy them to other examples
repositories etc. Yes, the sample textures/movies inside are in public
domain too (see <code>data/AUTHORS.txt</code> inside for details).

<?php echo $toc->html_section(); ?>

<p>All the X3D test files were written
manually in X3D classic encoding. XML encoding versions were automatically
generated from classic encoding by <a href="castle-model-converter">Castle Model Converter</a>.
The reference images were generated by
<a href="castle-model-viewer">Castle Model Viewer</a>
(using <code>--screenshot</code> option to make screenshots in batch mode).

<?php echo $toc->html_section(); ?>

<p>Unless otherwise specified above, we tested on:

<ul>
  <li>FreeWRL 1.22.13 (on Debian testing 32-bit, NVidia GeForce GPU)
  <li>BS Contact 8.101
  <li>Instant Player 2.1.0
  <li>Octaga Player 4.0.3
  <li>view3dscene (version right before 3.13.0) (on Debian testing 32-bit, NVidia GeForce GPU). view3dscene is Michalis' own browser, so the implementation 100% matches the reference images and all proposed clarifications/solutions mentioned on this page.
</ul>

<p>Screenshots obtained from BS Contact, Instant Player, Octaga Player
are available <a href="https://github.com/wildpeaks/x3d-multi-texture/archive/master.zip">as a zip file</a>
or <a href="https://github.com/wildpeaks/x3d-multi-texture">or just browse/clone the GIT repository</a>.
Many, many thanks to Cecile Muller for testing!

<?php echo $toc->html_section(); ?>

<p>X3D specification about multi-texturing has a couple of problems.
Below is a list of spotted problems, and an explanation how we handle it in our
engine (<?php echo a_href_page("Castle Game Engine", "engine"); ?>
 and <?php echo a_href_page("view3dscene", "view3dscene") ?>)
 and how we propose to fix X3D specification.
You probably want to read this along with
<a href="http://www.web3d.org/files/specifications/19775-1/V3.2/Part01/components/texturing.html#MultiTexture">MultiTexture
specification in X3D 3.2</a>
(or
<a href="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/texturing.html#MultiTexture">MultiTexture
specification in X3D 3.3</a>, there weren't any important changes since X3D 3.2).

<p>Please report any comments, preferably
to <a href="http://web3d.org/mailman/listinfo/x3d-public_web3d.org">x3d-public
mailing list</a>.

<?php
/*
<p>Please report if any other X3D browser treats it differently.
Unfortunately, existing browsers already show incompatibilities around
multi-texturing (for example, Octaga seems to revert the order of textures
compared to all other browsers).
Our implementation tries to follow common sense (do what is useful for authors and maps
naturally on GPUs) and majority of existing implementations.

<p>(If you have any power over the X3D spec, please fix issues mentioned
below. I posted about this on Web3D forums (no longer accessible) and x3d-public
mailing list and to "X3D specification comment form",
without any answer so far.)
*/ ?>

<p>Specification problems and our solutions:

<ol>
  <li><p><i>The mode field may contain an additional blending mode
    for the alpha channel.</i> This is the most troublesome
    sentence of the <code>MultiTexture</code> specification. It contradicts most of the remaining
    specification for <code>MultiTexture</code> node. Other spec parts clearly
    suggest that exactly one mode string corresponds to one texture unit,
    for example 1. it's mentioned explicitly that
    if the <code>mode.length</code> is less than <code>texture.length</code>,
    remaining modes should be assumed as "modulate" 2. many modes
    are clearly used over both RGB and Alpha channels, and they
    specify results for both RGB and Alpha channels.

    <p>This means that the meaning of <code>mode=["MODULATE","REPLACE"]</code>
    is not clear.

    <p>What did the authors meant by the word <b>may</b>
    in the sentence "may contain an additional blending mode"?

    <ul>
      <li>Expecting two mode strings for one texture unit
        clearly contradicts the spec.
      <li>Expecting a single mode string for one texture unit means that
        no mode specific for alpha channel is available.
      <li>Smart detection when to expect the next mode to be
        for alpha channel (for example expect the additional mode for alpha channel
        only when texture image has alpha channel) is also a bad idea.
        First, because the specification
        says absolutely nothing about it.
        Second, because operating on alpha channel
        makes sense even if the image in the current texture unit
        doesn't have alpha channel (because alpha may come from previous
        texture unit, or from a constant).
    </ul>

    <p>Also, some modes are clearly not possible (or sensible) for
    the alpha channel alone. For example, it doesn't make much sense to apply
    modes like <code>DOTPRODUCT3</code> or <code>BLEND*</code> only to
    the alpha channel.

    <p><i>Proposed clarification: a single string inside mode field
    <b>always</b> corresponds to exactly <b>one</b> texture unit</i>.
    This string may be a simple name of the mode (like <code>"MODULATE"</code>),
    in which case it describes behavior for both RGB and alpha channel.
    This string may also contain two mode names,
    separated by a comma or slash (like <code>"MODULATE / REPLACE"</code>),
    in which case a separate behavior is specified for RGB channels and
    for alpha channel.

    <p>The table in section
    <a href="#section_proposed_mode">Proposed improved MultiTexture.mode specification</a>
    contains the exact equations for all the modes,
    when used for both RGB and alpha or when used for only RGB
    or only alpha.

    <p><b>Relevant section of latest X3D specification:</b>
    <a href="<?php echo x3d_spec_latest_url('texturing', 'MultiTexture'); ?>">MultiTexture</a>.

    <p><b>Status: not fixed</b>.

  <li><p>In <i>Table 18.3 - Multitexture modes</i>, "REPLACE" mode
    is specified as "Arg2", which makes no sense. Arg2 comes
    by default from previous unit (or material color),
    this is implicated by the sentence "The source field
    determines the colour source for the second argument".
    So <code>mode "REPLACE"</code> interpreted as "Arg2"
    would then 1. completely ignore current
    texture unit 2. contradict the normal meaning of "REPLACE",
    which is explicitly mentioned in specification at paragraph
    before this table ("REPLACE for unlit appearance").
    An example with alpha (although ambiguous on it's own,
    more about this in previous point) clearly shows that
    "REPLACE" takes from 1st argument.

    <p><i>Proposed clarification:</i> "REPLACE" copies the "Arg1" (that is,
    current texture unit values). IOW, it's equivalent to "SELECTARG1".

    <p>To make it absolutely clear, it would also help if the spec
    would clearly say something along
    the lines <i>"Arg1 is the current texture unit, Arg2 is what is determined
    by the source field (by default, it's previous texture unit (or material color
    for 1st texture unit))"</i>. This would also make it clear what is the
    order of calculation for texture units
    (and would clarify that Octaga "reversed order"
    is incorrect &mdash; everyone else does it correctly).

    <p><b>Relevant section of latest X3D specification:</b>
    <a href="<?php echo x3d_spec_latest_url('texturing', 'MultiTexture'); ?>">MultiTexture</a>.

    <p><b>Status: not fixed</b>.

  <li><p>The meaning of <code>ADDSIGNED</code> and <code>ADDSIGNED2X</code> modes is not clear.
    Spec doesn't give the exact equation, and from the wording description
    it's not clear whether the -0.5 bias is applied to the sum
    (<code>Arg1 + Arg2 - 0.5</code>),
    or each component
    (<code>Arg1 - 0.5 + Arg2 - 0.5 = Arg1 + Arg2 - 1.0</code>).
    The first interpretation seems more reasonable,
    and it follows OpenGL <code>GL_ADD_SIGNED</code> behavior.

    <p>Neither interpretation results in the output
    range of values in -0.5 ... 0.5.
    The claim <i>making the effective range of values from −0.5 through 0.5</i>
    (at the <code>ADDSIGNED</code> value in table 18.3) doesn't seem to make
    any sense, regardless how you try to interpret it.

    <p><i>Proposed clarification:</i> I interpret it
    as "-0.5 bias is added to the sum",
    this follows OpenGL <code>GL_ADD_SIGNED</code> constant, so I guess this
    was the intention of the spec.

    <p><b>Relevant section of latest X3D specification:</b>
    <a href="<?php echo x3d_spec_latest_url('texturing', 'MultiTexture'); ?>">MultiTexture</a>.

    <p><b>Status: not fixed</b>.
    <a href="https://github.com/michaliskambi/x3d-tests/wiki/Deprecate-some-unused-and-badly-specified-MultiTexturing-specification-pieces">My current opinion: fix this by just deprecating ADDSIGNED, ADDSIGNED2X in X3D &gt; 4</a>.

  <li><p>Some modes say explicitly what happens with
    alpha channel, but some do not. This is especially troublesome
    in case of the "subtract" mode, that will subtract alphas making resulting
    alpha = 0 (invisible) for the most common situation when both textures
    have alpha = 1 (opaque).

    <p><i>Proposed clarification:</i>  See point 1.
    If you specify a simple mode name,
    then it applies to <i>both</i> RGB and alpha channels.
    Comparing with Octaga, our results
    for "subtract" are equal this way: with default alphas = 1,
    result gets alpha = 0.

    <p>This interpretation is consistent.
    In most cases, it also matches "what the author expects".
    The one exception is the "subtract" operation,
    when you usually do not want to subtract alphas &mdash;
    authors should just remember that <i>usually</i>
    they want subtract only RGB, using mode like
    <code>"SUBTRACT / MODULATE"</code>.

    <p>The table in section above
    (<i>Precise and corrected MultiTexture.mode specification</i>)
    makes it clear how to use each mode for only RGB, or only alpha, or both.

    <p><b>Relevant section of latest X3D specification:</b>
    <a href="<?php echo x3d_spec_latest_url('texturing', 'MultiTexture'); ?>">MultiTexture</a>.

    <p><b>Status: not fixed</b>.

  <li><p>It's not specified what channels are inverted by
    the <code>function="COMPLEMENT"</code> value. Well, obviously RGB are inverted,
    but is alpha channel inverted too?

    <p>Tests show that view3dscene, Instant Player, BS Contact do it on RGB
    (not alpha).
    Octaga does it on RGBA (it negates alpha channel too).
    Other tested browsers do not support this operation.

    <p><i>Proposed clarification:</i> <code>function="COMPLEMENT"</code>
    works only on RGB, does not touch alpha channel.
    This seems more suitable for usual cases, and follows the majority
    of implementations.

    <p><b>Relevant section of latest X3D specification:</b>
    <a href="<?php echo x3d_spec_latest_url('texturing', 'MultiTexture'); ?>">MultiTexture</a>.

    <p><b>Status: not fixed</b>.
    <a href="https://github.com/michaliskambi/x3d-tests/wiki/Deprecate-some-unused-and-badly-specified-MultiTexturing-specification-pieces">My current opinion: fix this by just deprecating function="COMPLEMENT" in X3D &gt; 4</a>.

  <li><p>The paragraphs for <code>MultiTextureTransform</code>
    (<i>texture coordinates for channel 0 are replicated...</i>)
    and <code>MultiTextureCoordinate</code>
    (<i>identity matrices are assumed...</i>) should be swapped in
    the spec.

    <p><b>Relevant section of latest X3D specification:</b>
    <a href="<?php echo x3d_spec_latest_url('texturing', 'MultiTextureTransform'); ?>">MultiTextureTransform</a>.

    <p><b>Status: fixed in X3D 4.0</b>.

  <li><p><code>MODULATEINVCOLOR_ADDALPHA</code> refers
    to non-existing mode
    <code>MODULATECOLOR_ADDALPHA</code> (that doesn't invert the color).

    <p><b>Relevant section of latest X3D specification:</b>
    <a href="<?php echo x3d_spec_latest_url('texturing', 'MultiTexture'); ?>">MultiTexture</a>.

    <p><b>Status: not fixed</b>.

  <li><p>Specification has 2 <code>source</code> values that refer to Phong lighting model ("DIFFUSE", "SPECULAR") and Gouraud shading:

<pre>
"DIFFUSE"  | The texture argument is the diffuse color interpolated from vertex components during Gouraud shading.
"SPECULAR" | The texture argument is the specular color interpolated from vertex components during Gouraud shading.
</pre>

    <p>But

    <ol>
      <li>
        <p>Browsers don't have to do Gouraud shading (Most browsers now allow both Phong and Gouraud shading).

      <li>
        <p>In X3D 4 we have new lighting models (physical, unlit) that don't even have diffuse/specular factors.
    </ol>

    <p><b>Relevant section of latest X3D specification:</b>
    <a href="<?php echo x3d_spec_latest_url('texturing', 'MultiTexture'); ?>">MultiTexture</a>.

    <p><b>Status: not fixed</b>.
    <a href="https://github.com/michaliskambi/x3d-tests/wiki/Deprecate-some-unused-and-badly-specified-MultiTexturing-specification-pieces">My current opinion: fix this by just deprecating source="DIFFUSE/SPECULAR" in X3D &gt; 4</a>.

<?php /*
  I deleted this section, and keep it in comments only to remind myself
  that I'm sometimes not very smart :)
  The spec of
  <code>source="DIFFUSE"</code> and <code>source="SPECULAR"</code>
  merely says that
  """
  The texture argument is the diffuse/specular color interpolated from vertex components during Gouraud shading.
  """

  They just want to interpolate Material.diffuseColor/specularColor field,
  from vertex to fragment.
  This doesn't force Gouraud shading,
  this doesn't force lighting calculation to be done at any point.

  <li><p>The definition of <code>source="DIFFUSE"</code>
    and <code>source="SPECULAR"</code> doesn't play nicely with lighting.

    <p>Reading the definitions of
    <code>source="DIFFUSE"</code>
    and <code>source="SPECULAR"</code>
     it would seem that X3D specification
    forces the Gouraud shading (calculate lighting at each vertex,
    not pixel). Which is unacceptable, and I'm absolutely sure that all X3D
    browsers ignore it. Most browsers, including ours,
    allow to choose Gouraud shading or Phong shading.
    The default shading depends on hardware capabilities, user settings,
    and maybe other X3D features (like our extensions to
    <?php echo a_href_page_hashlink('force Phong shading', 'x3d_extensions',
    'section_ext_shading'); ?>
    or <?php echo a_href_page_hashlink('use bump mapping', 'x3d_extensions',
    'section_ext_bump_mapping'); ?>).
    In any case, the shading definitely should <b>not</b> depend on whether
    we use multi-texturing or not.

    <p>Also, reading their descriptions it would seem that texture is applied
    <b>after</b> performing lighting calculation. Which contradicts
    the lighting equations in
    X3D spec <i>"17.2.2.4 Lighting equations"</i>,
    that clearly say that textures affect the diffuse color which is then
    used for lighting.

    <p><i>Proposed solution:</i>

    <ol>
      <li><p>At the very least, just change description of these source values
        to not talk about Gouraud shading.
        Just say for <code>source="DIFFUSE"</code>,
        <i>"The texture argument is the interpolated material diffuse color."</i>.
        And analogously for specular.
        <b>Do not talk about Gouraud shading here</b>, because
        1. you do not want to force Gouraud shading and
        2. according to X3D lighting spec, the texture color calculation
        should happen before the shading.

    ... rest of old notes removed, not relevant.
*/
?>

  <li><p>The default mode is always modulate, for both RGB and grayscale textures.
    This is inconsistent with single-texturing (using normal
    <code>ImageTexture</code> instead of <code>MultiImageTexture</code>),
    when the default mode is to <i>modulate</i> for grayscale textures,
    but <i>replace</i> for RGB textures. This means that you cannot blindly
    change <code>ImageTexture</code> node into a <code>MultiImageTexture</code> node
    (with a single <code>ImageTexture</code> inside): because the default mode
    (possibly) changed.

    <p><i>Proposed solution:</i> In this case, I propose to change the
    specification parts related to single-texturing (<code>ImageTexture</code>),
    and leave existing multi-texturing spec unchanged.
    That is, always <i>modulate</i> by default (regardless if texture
    is RGB or grayscale).

    <p>See <a href="#section_default_texture_mode">RGB texture color by default
    modulates material color</a> for a more detailed description of this problem.
    Existing browsers already disagree on this. Changing the spec to say
    <i>"we always modulate by default"</i> would greatly simplify the situation.

    <p><b>Status: fixed in X3D 4.0</b>.
  </li>

  <li><p>It would be useful to clarify what happens with grayscale texture
    images and images without alpha channel. Following the GPU behaviors
    (and common sense), we propose to add such statement to X3D specification:

    <p><i>For the purpose of multitexturing calculations,</i>
    <ol>
      <li><i>Grayscale texture is equivalent to an RGB texture
        with all color components (red, green, blue) equal.</i>
      <li><i>Texture without an alpha channel is equivalent to a texture with
        alpha channel filled with value 1.0 (completely opaque).</i>
    </ol>

    <p><b>Status: fixed in X3D 4.0</b>.
</ol>

<?php echo $toc->html_section(); ?>

<p>To allow different texture modes for RGB and for alpha channel,
you should just write two mode names inside one string, and separate
them by a comma or slash (additional whitespace around is allowed).
For example, <code>mode [ "MODULATE / REPLACE" ]</code>
means that on the 1st texture unit, RGB is modulated and alpha is replaced.
Contrast this with <code>mode [ "MODULATE" "REPLACE" ]</code>, that means
to modulate (both RGB and alpha) on the 1st texture unit,
and then to replace (both RGB and alpha) on the 2nd texture unit.

<p>This way we keep the interpretation that "one string on the mode field
always describes full behavior of exactly one texture unit".
Of course, some modes are not available for alpha channel (these are
the OpenGL constraints).

<p>Table below describes precise behavior and disallowed
situations for all mode names. Treat this as a corrected and precise version
of the similar table in X3D spec of <code>MultiTexture</code>
(see text down for details where and why it's corrected,
short version: specification is simply poor and inconsistent).
In table below,

<ol>
  <li><b>Arg1 is the current texture unit,</b></li>
  <li><b>Arg2 is determined by the source field</b>. By default,
    it's the result of previous texture stage, or (for the 1st stage)
    it's interpolated material*lighting.</li>
</ol>

<div style="overflow-x:auto;"> <!-- avoids adding horizontal scrolling for whole page on narrow mobile screens -->
<table class="specification">
  <tr>
    <th>Mode name</th>
    <th>Behavior when used alone<br/>
      (like "REPLACE")</th>
    <th>Behavior when used for only RGB channel<br/>
      (like "REPLACE / ...")</th>
    <th>Behavior when used for only alpha channel<br/>
      (like "... / REPLACE")</th>
  </tr>

  <tr>
    <td>MODULATE</td>
    <td>Output.RGBA := Arg1.RGBA * Arg2.RGBA</td>
    <td>Output.RGB  := Arg1.RGB  * Arg2.RGB </td>
    <td>Output.A    := Arg1.A    * Arg2.A   </td>
  </tr>

  <tr>
    <td>MODULATE2X</td>
    <td>Output.RGBA := Arg1.RGBA * Arg2.RGBA * 2</td>
    <td>Output.RGB  := Arg1.RGB  * Arg2.RGB  * 2</td>
    <td>Output.A    := Arg1.A    * Arg2.A    * 2</td>
  </tr>

  <tr>
    <td>MODULATE4X</td>
    <td>Output.RGBA := Arg1.RGBA * Arg2.RGBA * 4</td>
    <td>Output.RGB  := Arg1.RGB  * Arg2.RGB  * 4</td>
    <td>Output.A    := Arg1.A    * Arg2.A    * 4</td>
  </tr>

  <tr>
    <td>REPLACE or SELECTARG1</td>
    <td>Output.RGBA := Arg1.RGBA</td>
    <td>Output.RGB  := Arg1.RGB </td>
    <td>Output.A    := Arg1.A   </td>
  </tr>

  <tr>
    <td>SELECTARG2</td>
    <td>Output.RGBA := Arg2.RGBA</td>
    <td>Output.RGB  := Arg2.RGB </td>
    <td>Output.A    := Arg2.A   </td>
  </tr>

  <tr>
    <td>ADD</td>
    <td>Output.RGBA := Arg1.RGBA + Arg2.RGBA</td>
    <td>Output.RGB  := Arg1.RGB  + Arg2.RGB </td>
    <td>Output.A    := Arg1.A    + Arg2.A   </td>
  </tr>

  <tr>
    <td>ADDSIGNED</td>
    <td>Output.RGBA := Arg1.RGBA + Arg2.RGBA - 0.5</td>
    <td>Output.RGB  := Arg1.RGB  + Arg2.RGB  - 0.5</td>
    <td>Output.A    := Arg1.A    + Arg2.A    - 0.5</td>
  </tr>

  <tr>
    <td>ADDSIGNED2X</td>
    <td>Output.RGBA := (Arg1.RGBA + Arg2.RGBA - 0.5) * 2</td>
    <td>Output.RGB  := (Arg1.RGB  + Arg2.RGB  - 0.5) * 2</td>
    <td>Output.A    := (Arg1.A    + Arg2.A    - 0.5) * 2</td>
  </tr>

  <tr>
    <td>SUBTRACT</td>
    <td>Output.RGBA := Arg1.RGBA - Arg2.RGBA</td>
    <td>Output.RGB  := Arg1.RGB  - Arg2.RGB </td>
    <td>Output.A    := Arg1.A    - Arg2.A   </td>
  </tr>

  <tr>
    <td>OFF</td>
    <td>Texture stage is simply turned off.</td>
    <!-- What actually happens there? Previous takes value from previous tex unit? -->
    <td colspan="2">Not allowed.</td>
  </tr>

  <tr>
    <td>DOTPRODUCT3</td>
    <td>NewArg1.RGB := (Arg1.RGB - 0.5) * 2;<br/>
        NewArg2.RGB := (Arg2.RGB - 0.5) * 2;<br/>
        Output.RGBA := dot(NewArg1.RGB, NewArg2.RGB)</td>
    <td>... (calculate NewArg* same as on the left)...<br/>
        Output.RGB := dot(NewArg1.RGB, NewArg2.RGB)</td>
    <td>Not allowed.</td>
  </tr>

  <tr>
    <td>BLENDDIFFUSEALPHA</td>
    <td>Output.RGBA :=<br/>
        &nbsp;&nbsp;Arg1 * PRIMARY_COLOR.Alpha +<br/>
        &nbsp;&nbsp;Arg2 * (1 - PRIMARY_COLOR.Alpha)</td>
    <td>Output.RGB :=<br/>
        &nbsp;&nbsp;Arg1.RGB * PRIMARY_COLOR.Alpha +<br/>
        &nbsp;&nbsp;Arg2.RGB * (1 - PRIMARY_COLOR.Alpha)</td>
    <td>Not allowed.</td>
  </tr>

  <tr>
    <td>BLENDTEXTUREALPHA</td>
    <td>Output.RGBA :=<br/>
        &nbsp;&nbsp;Arg1 * Arg1.A +<br/>
        &nbsp;&nbsp;Arg2 * (1 - Arg1.A)</td>
    <td>Output.RGB :=<br/>
        &nbsp;&nbsp;Arg1.RGB * Arg1.A +<br/>
        &nbsp;&nbsp;Arg2.RGB * (1 - Arg1.A)</td>
    <td>Not allowed.</td>
  </tr>

  <tr>
    <td>BLENDFACTORALPHA</td>
    <td>Output.RGBA :=<br/>
        &nbsp;&nbsp;Arg1 * MULTI_TEXTURE_CONSTANT.Alpha +<br/>
        &nbsp;&nbsp;Arg2 * (1 - MULTI_TEXTURE_CONSTANT.Alpha)</td>
    <td>Output.RGB :=<br/>
        &nbsp;&nbsp;Arg1.RGB * MULTI_TEXTURE_CONSTANT.Alpha +<br/>
        &nbsp;&nbsp;Arg2.RGB * (1 - MULTI_TEXTURE_CONSTANT.Alpha)</td>
    <td>Not allowed.</td>
  </tr>

  <tr>
    <td>BLENDCURRENTALPHA</td>
    <td>Output.RGBA :=<br/>
        &nbsp;&nbsp;Arg1 * PREVIOUS_STAGE.Alpha +<br/>
        &nbsp;&nbsp;Arg2 * (1 - PREVIOUS_STAGE.Alpha)</td>
    <td>Output.RGB :=<br/>
        &nbsp;&nbsp;Arg1.RGB * PREVIOUS_STAGE.Alpha +<br/>
        &nbsp;&nbsp;Arg2.RGB * (1 - PREVIOUS_STAGE.Alpha)</td>
    <td>Not allowed.</td>
  </tr>
</table>
</div>

<?php echo $toc->html_section(); ?>

<p>In the same spirit, you can specify separate sources for RGB and alpha
channels, just separate them by comma or slash within a single string.
For example, source string <code>"DIFFUSE / FACTOR"</code> says to take diffuse
color as a source for Arg2.RGB and constant factor (<code>MultiTexture.alpha</code> field)
for Arg2.Alpha.

<p>Note that the empty string is also a source name (it means to take
color from previous texture stage). So source string like <code>"/ FACTOR"</code>
is also Ok (takes RGB from previous stage, and alpha from constant factor),
and <code>"FACTOR /"</code> is Ok (takes RGB from constant factor
<code>MultiTexture.color</code>, and alpha from previous stage).

<p>An example: suppose you have two textures that you want to subtract
on RGB (tex2 - tex1) channel, and you want to set resulting alpha channel
to 1.0 (regardless of any texture value). This will work:

<pre class="vrml_code">
MultiTexture {
  texture [
    ImageTexture { url "tex1.png" }
    ImageTexture { url "tex2.png" }
  ]
  mode [ "REPLACE" "SUBTRACT / SELECTARG2" ]
  source [ "" " / FACTOR" ]
  alpha 1.0
}

# Calculations on texture unit 1:
#   Stage1Output.RGBA := Tex1.RGBA;
# Calculations on texture unit 2:
#   Output.RGB := Tex2.RGB - Stage1Output.RGB;
#   Output.A := Arg2.A := 1.0;
</pre>

<?php echo $toc->html_section(); ?>

<p>This section documents a problem related to single-texturing behavior,
that is connected with some multi-texturing troubles.
Note that (at the time of X3D 3) this was the <i>one and only place</i>
where our engine deliberately did something different than X3D specification,
because we felt that the X3D specification behavior is really not useful.
<b>In our engine, the texture color is by default multiplied by the material color.
This issue is
<a href="https://github.com/michaliskambi/x3d-tests/wiki/Make-RGB-and-grayscale-textures-treatment-consistent">fixed in X3D 4.0</a>,
where I introduced prose/equations that match my recommended behavior (and match what CGE is doing).</b>

<p>VRML 2 / X3D specifications
say that RGB textures should by default <code>REPLACE</code> the material color (as opposed
to <code>MODULATE</code>).
This is when no multi-texturing is used.
This is said by the specification at tables
<i>"Table 17.2 — Unlit colour and alpha mapping"</i> and
<i>"Table 17.3 — Lit colour and alpha mapping"</i>: note that RGB and RGBA texture
colors are not multiplied by color from <code>Material.diffuseColor</code>
or <code>Color</code> nodes.
Also spec about Color nodes (11.4.2 Color, 11.4.3 ColorRGBA) says explicitly
that <i>"RGB or RGBA textures take precedence over colours; specifying both
an RGB or RGBA texture and a Color* node for geometric shape will
result in the Color* node being ignored."</i>.

<p>Problems with the specification text:

<ol>
  <li>It makes <code>Material.diffuseColor</code> and <code>Color</code>
    useless with RGB textures, which is a shame. GPUs do not have such limitations.
  <li>It is inconsistent with <code>MultiTexture</code> behavior,
    when the modulate mode is the default &mdash; regardless if we have
    RGB or grayscale texture.
  <li>In case of our Gouraud shading,
    the texture color has to be mixed with the whole lighting calculation.
    Using the "replace" mode by default would mean that shapes are unlit
    when you use RGB textures.
    Thus the problem would be very noticeable in Gouraud shading (if only X3D browsers would actually honor
    this part of X3D specification to the letter).
</ol>

<p>A separate problem is that browsers are already
inconsistent in the implementation of this rule, see test results.
That's understandable, because the spec behavior is a little useless.

<p>That's why we propose to change the specification:
simply always <code>MODULATE</code> (component-wise multiply on RGBA channels).
In other words, treat a grayscale texture exactly like an RGB texture
with all color components (red, green, blue) equal.
Our engine and view3dscene already implement this behavior.

<?php /*

<p>I didn't decide it lightly (noone likes
to deliberately contradict the specification...), but I think this case
is justified &mdash; <code>MODULATE</code> behavior is much more useful and usually
desired, IMO. Feel welcome to send me emails and argument against this.
After all, I'm trying to fit the needs of most people with default
behavior. If many people think that specification is right and I'm dumb,
and the default behavior should follow the spec and be <code>REPLACE</code>,
I'll obey :)

<p>You have menu item in view3dscene <i>RGB Textures Color Mode -&gt;
GL_REPLACE</i> to change this (from code, use
<code>Scene.Attributes.TextureModeRGB := GL_REPLACE;</code>).
But note that this doesn't give you spec-complaint behavior,
as the shapes with RGB textures in this case will become simply unlit.

*/ ?>

<?php
  castle_footer();
?>
