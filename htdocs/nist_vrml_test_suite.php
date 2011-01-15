<?php
  require_once 'vrmlengine_functions.php';
  require_once 'vrml_implementation_common.php';

  vrmlx3d_header("NIST conformance test suite results");
?>

<?php echo pretty_heading($page_title); ?>

<p>Results of testing our engine on <i>NIST Conformance Test Suite</i>.
<a href="http://www.web3d.org/x3d/content/examples/Conformance/">NIST
test suite is maintained and available currently on web3d.org</a>.
For historical purposes, you can also see at
<a href="http://xsun.sdct.itl.nist.gov/~mkass/vts/html/vrml.html">the old and original version of NIST VRML Test Suite</a>
(but not that the links to actual models are broken there).</p>

<p>Each test was classified as "pass" only if it passed fully.
Which is a good objective measure,
but also means that many tests failed
because unrelated features are not implemented. For example,
don't be discouraged by many failures in <tt>PROTO</tt> category.
Prototypes were 100% working in all tests, and I consider their
implementation as practically finished.
But unrelated things like missing <tt>Script</tt> support for ECMAScript
prevented the tests in <tt>PROTO</tt> category from passing completely.</p>

<p>Cases are marked above as "success" (+) only if they succeed
completely.
The style of table below was modeled after similar page
<a href="http://www.openvrml.org/doc/conformance.html">
OpenVRML Conformance Test Results</a>. <!-- See there also
for some  remarks about invalid tests included in
NIST test suite. -->

<?php
function pass($count, $comment = '')
{
  global $current_test_number;
  for ($i = 0; $i < $count; $i ++)
  {
    echo '
    <tr>
      <td>' . $current_test_number . '</td>
      <td class="pass">+</td>';
    if ($comment != '')
    {
      echo '<td rowspan="' . $count . '">' . $comment . '</td>';
      $comment = '';
    }
    echo '</tr>';
    $current_test_number++;
  }
}

function fail($count, $comment = '')
{
  global $current_test_number;
  for ($i = 0; $i < $count; $i ++)
  {
    echo '
    <tr>
      <td>' . $current_test_number . '</td>
      <td class="fail">-</td>';
    if ($comment != '')
    {
      echo '<td rowspan="' . $count . '">' . $comment . '</td>';
      $comment = '';
    }
    echo '</tr>';
    $current_test_number++;
  }
}

$default_texture_mode_modulate_disclaimer = a_href_page_hashlink(
  'You have to set <i>RGB Textures Color Mode -&gt; GL_REPLACE</i> to get 100% correct result.',
  'vrml_implementation_texturing', 'default_texture_mode_modulate');
?>

<table class="nist_test_suite">
  <tr>
    <th>Node Group</th>
    <th>Node</th>
    <th>Test Number</th>
    <th>Result</th>
    <th>Notes</th>
  </tr>
  <tr>
    <td rowspan="166">Appearance</td>
    <td rowspan="12">Appearance</td>
    <td>1</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>2</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>3</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>4</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>5</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>6</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>7</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>8</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>9</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>10</td>
    <td class="pass">+</td>
    <td><?php echo $default_texture_mode_modulate_disclaimer; ?></td>
  </tr>
  <tr>
    <td>11</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>12</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td rowspan="7">FontStyle</td>
    <td>1</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>2</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>3</td>
    <td class="pass">+</td>
    <td>Note that the test looks strange because the X axis line
      starts at X = -200. This is an error in the test file.
  </tr>
  <tr>
    <td>4</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>5</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>6</td>
    <td class="fail">-</td>
    <td>For horizontal text test passes, but vertical text
      is not implemented yet.
  </tr>
  <tr>
    <td>7</td>
    <td class="fail">-</td>
    <td>Handling ECMAScript not implemented yet.
  </tr>
  <tr>
    <td rowspan="34">ImageTexture</td>
    <td>1</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>2</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>3</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>4</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>5</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>6</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>7</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>8</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>9</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>10</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>11</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>12</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>13</td>
    <td class="fail">-</td>
    <td>The texture top is not aligned precisely with text top.
  </tr>
  <tr>
    <td>14</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>15</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>16</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>17</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>18</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>19</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>20</td>
    <td class="fail">-</td>
    <td>Like case 13: The texture top is not aligned precisely with text top.
  </tr>
  <tr>
    <td>21</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>22</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>23</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>24</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>25</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>26</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>27</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>28</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>29</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>30</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>31</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>32</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>33</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>34</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td rowspan="29">Material</td>
    <td>1</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>2</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>3</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>4</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>5</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>6</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>7</td>
    <td class="pass">+</td>
    <td rowspan="3"><?php echo $default_texture_mode_modulate_disclaimer; ?></td>
  </tr>
  <tr>
    <td>8</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>9</td>
    <td class="pass">+</td>
  </tr>
<?php

$current_test_number = 10;
pass(20);

?>

  <tr>
    <td rowspan="19">MovieTexture</td>
    <td>1</td>
    <td class="pass">+</td>
  </tr>

<?php
$current_test_number = 2;
fail(2, 'Audio from MovieTexture is not played yet');
pass(12);
pass(1, 'The movie text.mpg is still (5 identical frames, according to ffmpeg,
gstreamer and xine)');
pass(3);
?>

  <tr>
    <td rowspan="17">PixelTexture</td>
    <td>1</td>
    <td class="pass">+</td>
  </tr>

<?php
$current_test_number = 2;
pass(10);
fail(1, 'Texture top is not aligned precisely with Text top');
pass(5);
?>

  <tr>
    <td rowspan="48">TextureTransform</td>
    <td>1</td>
    <td class="pass">+</td>
  </tr>

<?php
$current_test_number = 2;
pass(11);
pass(2, 'Results look a little different, but matching precisely Xj3D and OpenVRML results.');
pass(32);
pass(2, 'Results look slightly incorrect, but matching precisely Xj3D and OpenVRML results. I think this is a shortcoming of my GPU (<i>ATI Mobility Radeon X1600</i>), precisely transforming small textures may make small errors?');
?>

  <tr>
    <td colspan="5"><i>...here I skipped some tests, to be checked later...</i></td>
  </tr>

  <tr>
    <td rowspan="2">Grouping</td>
    <td>Transform</td>
    <td>all 25</td>
    <td class="pass">+</td>
    <td>Note that "scaleorient Y" and "scaleorient Z" text descriptions are incorrect. Our results are correct (and match at least InstantPlayer).</td>
  </tr>

  <tr>
    <td>Billboard</td>
    <td>all 6</td>
    <td class="pass">+</td>
    <td>Note that "axis - rot Z" doesn't make much sense, when axisOfRotation is Z then rotating around it doesn't change the local Z vector, so the results are undefined... We do just nothing in this case. Results in other browsers (at least InstantPlayer) are weird, and it's also OK &mdash; it's undefined situation.</td>
  </tr>

  <tr>
    <td colspan="5"><i>...here I skipped some tests, to be checked later...</i></td>
  </tr>

  <tr>
    <td rowspan="113">Geometry</td>
    <td rowspan="6">Box</td>
    <td>1</td>
    <td class="pass">+</td>
    <td>This links to <tt>Text</tt> test, that passes (but has nothing
      to do with <tt>Box</tt>)
  </tr>

<?php
$current_test_number = 2;
pass(5);
?>

  <tr>
    <td rowspan="8">Cone</td>
    <td>1</td>
    <td class="pass">+</td>
    <td>This links to <tt>Text</tt> test, that passes (but has nothing
      to do with <tt>Cone</tt>)
  </tr>

<?php
$current_test_number = 2;
pass(4, 'Again, tests linking to unrelated testcases for <tt>Box</tt> node (that pass)');
pass(2);
pass(1, 'Unrelated <tt>Box</tt> test... (that passes)');
?>

  <tr>
    <td rowspan="9">Cylinder</td>
    <td>1</td>
    <td class="pass">+</td>
    <td>Unrelated <tt>Text</tt> test again...
  </tr>

<?php
$current_test_number = 2;
pass(4, 'Unrelated tests for <tt>Box</tt> again...');
pass(2);
pass(1, 'Unrelated <tt>Cone</tt> test...');
pass(1, 'Unrelated <tt>Box</tt> test...');
?>

  <tr>
    <td rowspan="14">ElevationGrid</td>
    <td>1 (default - grid)</td>
    <td class="pass">+</td>
    <td>Note that by default ElevationGrid is not smoothed (creaseAngle = 0),
      this is following the spec.
  </tr>
  <tr>
    <td>2</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>3</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>4</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>5</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>6</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>7</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>8</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>9 (test - normalfaces)</td>
    <td class="pass">+</td>
    <td>The reference image of the test is bad. The result should
      be more obvious (whole rows of quads have the same normal),
      and it is &mdash; with our engine.
  </tr>
  <tr>
    <td>10</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>11</td>
    <td class="pass">+</td>
    <td>Although we use two-sided lighting.
  </tr>
  <tr>
    <td>12</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>13</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>14</td>
    <td class="pass">+</td>
  </tr>

  <tr>
    <td rowspan="17">Extrusion</td>
    <td>1</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>2</td>
    <td class="pass">+</td>
    <td>Reference images show the incorrect non-uniform scaling
      of the caps. We handle it right.
  </tr>
  <tr>
    <td>3</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>4</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>5</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>6</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>7</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>8</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>9</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>10</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>11</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>12</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>13</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>14</td>
    <td class="fail">-</td>
    <td>While generally looks Ok, it seems that our triangulating
      algorithm can't handle this particular shape perfectly.
  </tr>
  <tr>
    <td>15</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>16</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>17</td>
    <td class="pass">+</td>
  </tr>

  <tr>
    <td rowspan="21">IndexedFaceSet</td>
    <td>1</td>
    <td class="pass">+</td>
  </tr>

<?php
$current_test_number = 2;
pass(10);
pass(3, $default_texture_mode_modulate_disclaimer);
pass(7);
?>

  <tr>
    <td rowspan="10">IndexedLineSet</td>
    <td>1</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>2</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>3</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>4</td>
    <td class="pass">+</td>
    <td rowspan="2">(These tests have nothing to do with IndexedLineSet,
      they are for IndexedFaceSet.)</td>
  </tr>
  <tr>
    <td>5</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>6</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>7</td>
    <td class="pass">+</td>
    <td rowspan="2">(These tests have nothing to do with IndexedLineSet,
      they are for IndexedFaceSet.)</td>
  </tr>
  <tr>
    <td>8</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>9</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>10</td>
    <td class="pass">+</td>
  </tr>

  <tr>
    <td rowspan="5">PointSet</td>
    <td>1</td>
    <td class="pass">+</td>
  </tr>

<?php
$current_test_number = 2;
pass(4);
?>

  <tr>
    <td rowspan="5">Shape</td>
    <td>1</td>
    <td class="pass">+</td>
  </tr>

<?php
$current_test_number = 2;
pass(4);
?>

  <tr>
    <td rowspan="6">Sphere</td>
    <td>1</td>
    <td class="pass">+</td>
    <td>Unrelated <tt>Text</tt> tests...
  </tr>

<?php
$current_test_number = 2;
pass(5, 'Unrelated <tt>Box</tt> tests...');
?>

  <tr>
    <td rowspan="12">Text</td>
    <td>1</td>
    <td class="pass">+</td>
  </tr>

<?php
$current_test_number = 2;
pass(3);
fail(3, 'Text.length is not supported yet');
fail(2, 'Text.maxExtent is not supported yet');
pass(2);
fail(1, 'Texture mapping is a little incorrect, text is too small');
?>

  <tr>
    <td colspan="5"><i>...here I skipped some tests, to be checked later...</i></td>
  </tr>

  <tr>
    <td rowspan="50">Misc</td>
    <td rowspan="18">EXTERNPROTO</td>
    <td>1</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>2</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>3</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>4</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>5</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>6</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>7</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>8</td>
    <td class="fail">-</td>
    <td>Currently, base URL for EXTERNPROTO is from the file where EXTERNPROTO
      is written, not from the file where it's instantiated.
  </tr>
  <tr>
    <td>9</td>
    <td class="fail">-</td>
    <td rowspan="2">
      ECMAScript is not supported yet. Also, the DEF declaration inside
      a script causes known problem with cycles in VRML graph.</td>
  </tr>
  <tr>
    <td>10</td>
    <td class="fail">-</td>
  </tr>
  <tr>
    <td>11</td>
    <td class="fail">-</td>
    <td rowspan="2">ECMAScript is not supported yet.</td>
  </tr>
  <tr>
    <td>12</td>
    <td class="fail">-</td>
  </tr>
  <tr>
    <td>13</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>14</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>15</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>16</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>17</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>18</td>
    <td class="pass">+</td>
  </tr>

  <tr>
    <td rowspan="30">PROTO</td>
    <td>1</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>2</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>3</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>4</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>5</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>6</td>
    <td class="pass">+</td>
    <td>(It links to unrelated Text test that works?)</td>
  </tr>
  <tr>
    <td>7A</td>
    <td class="pass">+</td>
    <td>Result is Ok, but we do not handle
      Sound, AudioClip nodes (yet).</td>
  </tr>
  <tr>
    <td>7B</td>
    <td class="fail">-</td>
    <td>Static result seems Ok, but we do not handle VisibilitySensor (yet).</td>
  </tr>
  <tr>
    <td>7C</td>
    <td class="fail">-</td>
    <td>Static result seems Ok, but we do not handle VisibilitySensor
      and Collision.collideTime is not generated (yet).</td>
  </tr>
  <tr>
    <td>7D</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>7E</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>7F</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>7G</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>7H</td>
    <td class="pass">+</td>
    <td>Result is Ok, although actually we do not handle ECMAScript (yet).</td>
  </tr>
  <tr>
    <td>7I</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>7J</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>7K</td>
    <td class="pass">+</td>
  </tr>

  <tr>
    <td>8</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>9</td>
    <td class="fail">-</td>
    <td>Tested features work perfectly. But VisibilitySensor
    is not handled (yet), so animation doesn't start (you can replace
    it by e.g. ProximitySensor with large sizes, and animation will work).
  </tr>
  <tr>
    <td>10</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>11</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>12</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>13</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>14</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>15</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>16</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>17</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>18</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>19</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>20</td>
    <td class="pass">+</td>
  </tr>

  <tr>
    <td rowspan="2">WorldInfo</td>
    <td>1</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>2</td>
    <td class="pass">+</td>
  </tr>

  <tr>
    <td rowspan="14">Sensors

      <p>(Actually tested on <a href="http://www.web3d.org/x3d/content/examples/Conformance/Sensors/">X3DV versions here</a>.)
    </td>
    <td rowspan="3">CylinderSensor</td>
    <td class="testnr">all except below</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td class="testnr">minmax disk</td>
    <td class="fail">?</td>
    <td>We do honor min/maxAngle. However, this test uses min/max values of -2Pi/+2Pi, effectively allowing any rotation value. The test requirement ("one full rotation") assumes that browser calculates following rotations as angles outside of [-2Pi, 2Pi] range, which is not required by the specification (and not really clearly doable, when you receive discrete mouse events you cannot 100% reliably detect when "full rotation" occurs.)</td>
  </tr>
  <tr>
    <td class="testnr">multisensor</td>
    <td class="fail">-</td>
    <td>We do not "see" the higher TouchSensor so it doesn't even get isOver events.</td>
  </tr>

  <tr>
    <td rowspan="3">PlaneSensor</td>
    <td class="testnr">1 .. 11</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td class="testnr">12 (multisensor)</td>
    <td class="fail">-</td>
    <td>We do not "see" the higher TouchSensor so it doesn't even get isOver events.</td>
  </tr>
  <tr>
    <td class="testnr">13 .. 14</td>
    <td class="pass">+</td>
  </tr>

  <tr>
    <td>ProximitySensor</td>
    <td class="testnr">1 .. 10 (all)</td>
    <td class="pass">+</td>
  </tr>

  <tr>
    <td rowspan="2">SphereSensor</td>
    <td class="testnr">all except multisensor</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td class="testnr">multisensor</td>
    <td class="fail">-</td>
    <td>We do not "see" the higher TouchSensor so it doesn't even get isOver events.</td>
  </tr>

  <tr>
    <td>TimeSensor</td>
    <td class="testnr">1 .. 19 (all)</td>
    <td class="pass">+</td>
  </tr>

  <tr>
    <td rowspan="3">TouchSensor</td>
    <td class="testnr">1 .. 6</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td class="testnr">7 (hittexcoord)</td>
    <td class="fail">-</td>
    <td>TouchSensor.hitTexCoord_changed not implemented yet.</td>
  </tr>
  <tr>
    <td class="testnr">8..9</td>
    <td class="pass">+</td>
  </tr>

  <tr>
    <td>VisibilitySensor</td>
    <td class="testnr">1 .. 9 (all)</td>
    <td class="fail">-</td>
    <td>VisibilitySensor is not implemented yet.</td>
  </tr>

  <tr>
    <td colspan="5"><i>... here I again skipped some tests ...</i></td>
  </tr>

  <tr>
    <td rowspan="12">Special_Groups</td>
    <td rowspan="6">LOD</td>
    <td>1</td>
    <td class="pass">+</td>
  </tr>

<?php
$current_test_number = 2;
pass(5, "Note that switching between Viewpoints in these tests has very strange VRML code. Namely there are interpolators with two <i>equal</i> keys (so they don't actually make any change, and this is correctly optimized in the engine). Moreover, they are connected to time sensors with 2 seconds cycle. This causes strange effects when clicking fast on various touch sensors, as many interpolators conquer to change the same Transform.position values. I'll emphasize: we handle it correctly, and optimize correctly, we have to evaluate simultaneous changes to the same field from various routes... The test is just strange, without any purpose.");
?>

  <tr>
    <td rowspan="6">Switch</td>
    <td>1</td>
    <td class="pass">+</td>
    <td>This is actually an <tt>Anchor</tt> bound-500 test, that passes.
      (Possibly, wget messed up my local copy of NIST tests...
      Online server with NIST tests is broken, so I can't check).
  </tr>

<?php
$current_test_number = 2;
pass(5);
?>

  <tr>
    <td colspan="5"><i>That's enough for now...
      I don't have time to check all the tests.
      If someone wants to do the work and do the remaining
      tests (and document results just like above),
      please contact us by
      <?php echo MAILING_LIST_LINK; ?>.</i>
</table>

<?php
  vrmlx3d_footer();
?>
