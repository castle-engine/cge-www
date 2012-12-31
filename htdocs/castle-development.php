<?php
  require_once 'castle_engine_functions.php';

  castle_header("The Castle &mdash; development", NULL, array('castle'));

  $toc = new TableOfContents(
    array(
      new TocItem('TODO things', 'todo'),
      new TocItem('Feedback', 'feedback'),
      new TocItem('Creating 3d objects', 'creating_new_3d'),
      new TocItem('3D model formats', '3d_formats', 1),
      new TocItem('Debug menu, debug options', 'debug_menu_options', 1),
      new TocItem('Levels', 'levels', 1),
      new TocItem('Items', 'items', 1),
      new TocItem('Creatures', 'creatures', 1),
      new TocItem('Advanced effects', 'effects', 1),
      new TocItem('Creating sounds', 'creating_sounds'),
      new TocItem('Compiling', 'compiling'),
      new TocItem('Game goals', 'goals'),
    )
  );
?>

<h1><?php echo a_href_page('The Castle', 'castle'); ?> &mdash; development</h1>

<div style="
  background: #E8D59A;
  margin-left: 4em;
  margin-right: 4em;
  border: thin dashed black;
/*  font-weight: bold; */
  font-style: italic;
  padding: 1em;
  ">
  <p style="margin-top: 0px">Please note that I consider "The Castle", as a game, finished now.
  Hey, it was fun, it was great, and thank you for everyone that helped!
  Now let's make something even better :)</p>

  <p>New game, using much improved "The Castle" engine, is&nbsp;planned &mdash;
  this new game will be technically "The Castle 2.0" (although actual name planned
  now is "Human Programming").
  I&nbsp;don't plan to further extend "The Castle" data (no new levels,
  no new creatures etc.). It makes more sense for me to focus on new game data,
  that will use many engine features added after "The Castle" release.</p>

  <p style="margin-bottom: 0px">This isn't supposed to discourage you from dabbling in "The Castle"
  data :). On the contrary, feel free to experiment even more, create
  levels / creatures not necessarily tied to the current "The Castle" mood.
  Hey, you're welcome to start a new game on your own, using "The Castle" engine.
  (Almost) everything documented
  here will also apply to the new game, so information on this page
  will certainly remain relevant.</p>
</div>

<p><?php echo a_href_page('"The Castle"', 'castle') ?> is developed in
an open-source manner, which means that if anyone wants to help &mdash;
you're more than welcome. You don't even have to be a programmer &mdash;
3D level/creature designers are wanted (alive!),
and sending simple feedback is definitely useful too.
This page collects various developer information how you can
help, and how you can compile and extend the game's code and data.</p>

<p>All questions, feedback, etc. may be posted to
<?php echo a_href_page('our forum or mailing list', 'forum'); ?>.

<p>Of course, any help with designing levels, items, monsters, creating
sounds and / or providing significant feedback will be mentioned in the
<?php echo a_href_page('credits list', 'castle-credits') ?>
 and in-game credits dialog. You want some fame and fortune ? We got it.

<p>Contents:
<?php echo $toc->html_toc(); ?>

<?php echo $toc->html_section(); ?>

<p>Most important TODOs for now are:
<ul>
  <li><p>Make bow much more interesting. (I had a nice idea how to make
    aiming from the bow difficult, but it was not implemented for PGD compo,
    not enough time. But it will be done.)

  <li>Visual effects (on creature wound (some blood etc.), ball missile
    better rendering (some pulsating flare etc.), ball missile explosion),
    decals on walls.
</ul>

More detailed list is inside game archive in <tt>TODO</tt> file.

<?php echo $toc->html_section(); ?>

<p>Any kind of feedback will be useful. This includes:

<ul>
  <li><p>Bug reports. If anything goes wrong, I obviously
    want to know about it. Describe how to reproduce the bug &mdash;
    i.e. what you did, what happened, and what you expected to happen.
    If you think that something works strange, this also qualifies as
    a bug. If some text inside the documentation (these HTML pages)
    is unclear, this also qualifies as a bug.

    <p>Note that this game requires rather good graphic card available.
    If the game is awfully slow, then (probably) it's not a bug &mdash;
    it's just the way it is, and I will not be able to help much.
    Make sure you have absolutely newest drivers (including OpenGL
    library) for your graphic card.

    <p>Bugs should be submitted to our
    <a href="<?php echo TICKETS_TRACKER_URL; ?>">tickets tracker</a>,
    or <?php echo a_href_page('our forum or mailing list', 'forum'); ?>.
    </li>

  <li><p>Features. Do you think that anything should be added ?
    Maybe something should be changed ? Any ideas may be useful here
    &mdash; new key / mouse commands, changing the behavior of
    present commands, displaying some things in a different way, etc.

    <p>Feature requests should also be submitted to
    the <a href="<?php echo TICKETS_TRACKER_URL; ?>">tickets tracker</a>
    or <?php echo a_href_page('our forum or mailing list', 'forum'); ?>.
    </li>
</ul>

<?php echo $toc->html_section(); ?>

<p>Below I describe how you can create new levels, items or monsters for
this game. This requires at least some knowledge of a 3d modelling program
(I use <a href="http://www.blender3d.org">Blender</a>).
Depending on what exactly you will want to do, this may also
require some programming knowledge &mdash; of course I will help
here, which means that you can just design 3d model and tell me
what it's supposed to do in the game (e.g. if it's an item then
what happens when player uses it ?), and I will write appropriate code.

<p>You don't have to make all images, textures, 3d parts yourself
&mdash; you can freely use things available on the Internet.
Just be sure that what you take is available on GNU General Public License
(or anything that can be treated like a super-set of GNU GPL, like LGPL
or public domain), as I will want to include
this in the game licensed on GNU GPL. For a list of various sites
with useful programs and collections of textures etc. see
<ul>
  <li><?php echo a_href_page_hashlink('list
    of things used when developing this game', 'castle-credits',
    'section_graphics'); ?>
  <li><a href="http://www.pascalgamedevelopment.com/archive/index.php/t-2847.html">
    "Where can I find free repeatable textures?" thread on PGD forums</a>
  <li><?php echo a_href_page_hashlink('list
    of things used when developing malfunction', 'malfunction',
    'section_external_resources'); ?>
  <li><?php echo a_href_page_hashlink('list
    of things used when developing lets_take_a_walk', 'lets_take_a_walk',
    'section_credits'); ?>
</ul>

<?php echo $toc->html_section(); ?>

<p><i>Note:</i> the description below is written for "any 3D game using
our engine". For "The Castle 2" submissions, we actually require
you to use:

<ol>
  <li>X3D as a final format (because this is most feature-rich
    and handled perfectly with our engine),
  <li>and you have to use some open-source modeller.
    Preferably <a href="http://www.blender3d.org/">Blender</a>, but other
    open-source are also fine. That's because many people may want to further
    work on and improve your model &mdash; this isn't really possible
    if you used a proprietary modeller.
</ol>

<p>The short 1-sentence introduction is: you can use any 3D modelling program
able to create <?php echo a_href_page('VRML or X3D', 'vrml_x3d'); ?> files.</p>

<p>VRML and X3D formats have official specifications
and our engine is capable of handling most of their features
(actually,
<?php echo a_href_page('even more than that', 'x3d_extensions') ?>).
So the engine is not tied to any particular modelling program.
Use <?php echo a_href_page('view3dscene', 'view3dscene') ?>
 to view the various models outside of the game.</p>

<p>You may want to view in view3dscene some of our
<?php echo a_href_page("VRML/X3D demo models",
  "demo_models"); ?> &mdash;
there are many testcases and demos of what can be expressed in VRML / X3D
and what our engine can understand and render.</p>

<p>My favourite 3d modeller is <a href="http://www.blender3d.org/">Blender</a>.
It's open source, it's available for Linux (yeah, Windows and others too
if you insist :), and it has an enormous set of features.
Including good export for VRML / X3D, of course.
Basically there are no special
rules for designing in Blender models for our engine &mdash;
just do whatever you like (and whatever can be exported to VRML / X3D).</p>

<p>The engine also handles perfectly VRML/X3D files exported from <i>3ds Max</i>.
So you can use <i>3ds Max</i> to make models too. Also VRML/X3D produced
by open-source modeller <a href="http://www.artofillusion.org/">Art of Illusion</a>
are perfect. I suspect that all significant 3D modelling tools are able
to export decently to VRML or X3D, so you can probably use any modeller you like.
(This was one of the reasons that I chose VRML / X3D format as my basic format
&mdash; it's nice to see that it pays off :) ).
Note that I'd like to make the game using as much open-source tools as possible,
so using open-source modeller is strongly favored over proprietary modellers.

<p>Some detailed hints / tricks:</p>

<ul>
  <li><p>I often "wrap" (using <tt>WWWInline</tt> VRML 1.0 or
    <tt>Inline</tt> VRML 2.0 / X3D node) models produced by
    Blender inside some small VRML files written by hand.
    For example, <tt>data/levels/castle_hall_final.wrl</tt> is a small VRML file that is
    maintained by hand. It sets up some things that were uncomfortable
    (or impossible, due to lacks of VRML 1.0 exporter) to set up from Blender,
    and includes (using <tt>WWWInline</tt> node) other VRML / X3D file
    (<tt>data/levels/castle_hall_processed.wrl</tt> in this case)
    that defines geometry,
    materials and textures of the level.</p></li>

  <li><p>Moreover, I sometimes want to post-process VRML / X3D files generated from
    Blender. I use EmacsLisp macros for this (since this is a great language
    for text operations, and it's built in my favourite editor :) ).
    For example <tt>data/levels/castle_hall.wrl</tt> in VRML file
    exported from Blender (from <tt>data/levels/castle_hall.blend</tt>),
    and it's processed to <tt>data/levels/castle_hall_processed.wrl</tt>.</p></li>

  <li><p><i>X3D and VRML 2.0 (aka 97) notes:</i><br />
    It's advised to use <?php echo a_href_page("our customized exporter", "blender"); ?>.</li>

  <li><p><i>Collada/ 3DS / MD3 / other formats notes:</i><br/>
    Actually, you can also use models in other formats instead
    of VRML / X3D. Our engine can handle other formats, see
    <?php echo a_href_page('view3dscene', 'view3dscene') ?>
    for more detailed info.</p>

    <p>So if your favourite 3d modelling program is not able to export
    to VRML / X3D but it's able to export to e.g. Collada &mdash;
    you can use it too. We support quite a lot of Collada and other
    3D formats features.</p>

    <p>Still, note that <em>everything</em> that can be expressed
    in other 3D formats handled and that is understood
    by our engine can also be expressed in VRML / X3D.
    But not vice-versa, i.e. there are some features that
    are available with my engine when using VRML / X3D but are not available
    when using e.g. Collada.
    So the bottom line is: well, you can use Collada or some other formats
    with my engine, it works OK, but VRML / X3D format is just way better :)</p></li>
</ul>

<?php echo $toc->html_section(); ?>

<p>If you want to modify game content heavily, I advice to get
familiar with our <i>debug menu</i>. The debug menu contains
many useful commands that will make your life much easier. By default,
debug menu is activated by backquote key (the one with tilde, the same
key used to bring console in many FPS games).

<p>Using the debug menu
you can turn on some special features useful for designers/debugging
(e.g. to see bounding volumes of objects) or turn off normal game features
that may be annoying when designing (e.g. stop time for creatures).

<p>You can also request a reload of particular creature/item/level VRML/XML
etc. files. This is extremely useful when you changed some data file
and you would like to quickly reload just this one file,
to see how things work now &mdash; without exiting the game.

<p>There are also some command-line debug options (but not too much &mdash;
it's always more flexible to have things available at runtime instead
of only at the start). Run the program with <tt>--help</tt>
to get their list.
<?php /*
<tt>--debug-no-creatures</tt> is one such useful option:
it avoids this lengthy "Loading creatures" loading time, which is useful
when you're not interested in testing creatures
(e.g. when you only design new level geometry).
*/ ?>

<?php echo $toc->html_section(); ?>

<ul>
  <li><p>The very concept of this game (much like any other modern game)
    is that you can design new levels without writing a single line
    of "real" (in this case: ObjectPascal) code.
    Basically, you design your level, then you add it to
    <tt>data/levels/index.xml</tt> file, and it's done.</p>

    <p>There's a special level called "Hello world" in the game data.
    This is a level that is intentionally an ultra-simple example
    of how a level can be done. If you want to create your
    own level, you can start by modifying this level.
    See files in <tt>data/level/hello_world/</tt> subdirectory.</p>

    <p>Note that "Hello world" was done in VRML 1.0. For a sample
    level done in pure VRML 2.0, look at "Fountain" level.</p></li>

  <li><p>To place items and creatures (collectively called "3D resources")
    on the level you place a special "placeholder" objects on the level.
    Every placeholder object will be removed
    when the level is loaded (so you can use any shape for it &mdash;
    I usually use wire cubes). Placeholder object position and name will
    determine the actual item/creature position, resource (this refers
    to a resource defined by resource.xml file, which in turn defines
    the ObjectPascal class (determining the behavior of creature/item)
    and a lot of properties) and quantity
    (in case of items) or initial life (in case of creatures).
    Name of the placeholder is
    <tt>CasRes&lt;resource-name&gt;[&lt;resource-number&gt;][_&lt;ignored&gt;]</tt>:
    <ul>
      <li><tt>CasRes</tt> is just a shortcut for "<b>Cas</b>tle Game Engine <b>Res</b>ource".
      <li><tt>&lt;resource-name&gt;</tt> is one of the resource names,
        see available names in <tt>resource.xml</tt> files inside <tt>creatures/</tt>
        and <tt>items/</tt> subdirectories.
      <li><tt>&lt;resource-number&gt;</tt> (optional) is an integer
        specifying item quantity (default 1 if not given) or initial creature
        life (default is taken from <tt>resource.xml</tt> file, specific
        to this creature).
      <li>Anything after underscore is ignored. You can use this to make
        object name unique, e.g. all Blender objects must be unique.
    </ul>

  <li><p>When loading level, we search for Blender object named <tt>CasMoveLimit</tt>.
    Our code is adapted to standard VRML/X3D exporters from Blender such that
    we know how the name is stored inside VRML/X3D and we can retrieve it.

    <p>Such object (if found) is removed from level 3D model,
    and it's bounding box is used to limit allowed player positions.
    This can be used to disallow player to move to the very edge of
    the level.

    <p>Usually you will add <tt>CasMoveLimit</tt> object using Blender, you can also
    use <?php echo a_href_page('view3dscene', 'view3dscene') ?> &mdash;
    see command 'Console -&gt; Print scene bounding box'.

  <li><p>Blender object named <tt>CasWater</tt> indicates water volume.
    At some point this will be extended (when I'll need it) to
    include every 'CasWater[_&lt;ignored&gt;]', so that you will be able to
    define water by a sum of shapes. Right now, it's only a single shape,
    and we look only at it's bounding box.

  <li><p>Levels must be oriented such that +Z is "up".
    While all things in my general units are flexible
    and allow any up vector to be used, I made in "The Castle" a couple
    of assumptions that really need +Z to be up.

  <li><p>Typical VRML / X3D nodes that I write "by hand"
    (in <tt>level_final.wrl</tt> file) are:

    <ul>
      <li><tt>Background</tt>, <tt>NavigationInfo</tt> nodes

        <p>These nodes were introduced in VRML 2.0 (although I implemented them
        for VRML 1.0 also). But Blender exporter to VRML 1.0 does not write
        these nodes. So I just write them by hand. Blender exporter for VRML 2.0
        does write them, but still it's usually easier to set them up by hand.
      </li>

      <li><tt>PerspectiveCamera</tt> / <tt>Viewpoint</tt> node

        <p>VRML 1.0 Blender exporter: exports camera settings,
        but in an uncomfortable
        format (as a transformation, instead of as VRML camera node).
        This is OK for simple viewing of VRML models, but it's not OK
        if you want to "tweak" VRML models. In particular, it's bad
        when you want to add Background node (see above) and light nodes
        (see hints below) by hand. Then you have to either insert it
        into the middle of Blender's generated VRML file, or use awkward
        coordinates (because of additional transformation).
        So I usually generate camera node by opening model in my view3dscene
        (see <?php echo a_href_page('view3dscene', 'view3dscene') ?>)
        and using "Print current camera node (Ctrl+C)" feature.

        <p>VRML 2.0 Blender exporter generates better <tt>Viewpoint</tt>
        from camera, but still it doesn't have gravityUp like we want,
        so it's much better to use
        <?php echo a_href_page('view3dscene', 'view3dscene') ?>
        "Print current camera node (Ctrl+C)" feature.
      </li>

      <li><tt>Light</tt> nodes that are "global" (will shine
        on other objects (like enemies) that are not part of the level
        mesh). While Blender X3D exporter can create X3D lights
        from Blender lights, it doesn't allow me to use all features
        of X3D lights. So you can configure lights by hand,
        or by using <?php echo a_href_page('view3dscene', 'view3dscene') ?>
        <i>Edit -&gt; Lights Editor</i> feature.
      </li>
    </ul>

  <li><p>All levels should keep the same general sizes. In other words,
    it's <i>not good</i> to just make your new level of arbitrary
    size and then set player's avatarSize such that things "look sensible".
    Why ? Because I place items and enemies on the level.
    So size of all these various models must match.

    <p>In other words: just make sure you use
<pre class="vrml_code">
  NavigationInfo {
    avatarSize [ 0.5, 2 ]
    speed 20
    ...
    type "WALK" # "WALK" is not really needed for castle,
                # but useful for testing
                # with general VRML browsers like view3dscene
  }
</pre>
    in your level. And force yourself to design a level that "fits"
    such avatarSize.

    <p>As for <i>speed</i>: I know that the speed 20 is quite fast,
    like "always running". On some levels it feels good (e.g. large, outdoor
    "The Gate" level), on some levels ("Fountain") it would be awkward to
    always run. So you can set the speed to <tt>10</tt> if your level
    should feel more like walking (this usually means smaller, "indoor feeling"
    level).

  <li><p>Sectors and waypoints (aka portals) for the level:

    <ul>
      <li><p>Placeholders named CasSector&lt;index&gt;[_&lt;ignored&gt;]
        define sectors.
        Placeholders named CasWaypoint[_&lt;ignored&gt;] define sectors.
        See TGameSceneManager.LoadLevel API docs for details.

      <li><p>Sectors of waypoints (and reverse property, Waypoints
        of sectors) are automatically calculated, by looking how waypoints
        bounding boxes collide with sectors.
        This is the only moment when waypoint's bounding volume are considered,
        for all other purposes waypoints are simply points.
        So you place boxes that indicate waypoints
        between two neighboring sectors, such that the bounding box
        of the waypoint is partially inside both sectors.

        <p>Sectors boxes need <i>not</i> be strictly separated.
        When 3D object, like player or creature, is within two sectors,
        it's arbitrarily assigned to any of the possible sectors.
        However, for creature AI, this may cause some awkward movement
        (when the creature goes to a waypoint, instead of directly to the target),
        so try to set sectors that don't overlap (much).

      <li><p>You don't have to cover whole level with sectors.
        If creature (or it's target) is not inside any sector, then
        the move direction is simply set to go to the target directly.

      <li><p>Sectors and waypoints are used for creature AI.
        Each sector occupies some space in 3D (like a room).
        Each waypoint indicates a place where you can move
        from one sector to another (like a narrow door between two rooms).
        Sectors create a graph, with waypoints indicating the graph
        connections.
        If the creature is in a different sector then it's target,
        it walks through appropriate waypoints.


        <p>We may also use sectors to speed up rendering in the future.
        For now, it doesn't seem needed &mdash; frustum culling,
        and hardware occlusion query optimize the rendering set good enough,
        without any difficult configuration.

<!--
        It's assumed that there will not be too many sectors on the level
        (100 sectors is <i>really</i> around maximum. 10-20 is reasonable.)
        So sectors are <i>not</i> (at least right now) stored in any "intelligent"
        structure (like an octree) and creature moving AI uses
        the simplest search on the graph to find a satisfiable path
        (no A* algorithm or anything; it's simply not needed for the kind of
        sectors layouts that I will typically use).
-->
      </li>
    </ul>
  </li>

  <li><p>If you made a completely new level, you want to add it to the game.
    That's easy: just add a subdirectory with <tt>level.xml</tt> file inside
    the game data.
    See <a href="http://svn.code.sf.net/p/castle-engine/code/trunk/castle_game_engine/doc/README_about_index_xml_files.txt">level.xml and resource.xml files documentation</a>.
    You can switch to the level by debug menu "Change to level" command
    (or even you can start "new game" from this level, if you set it's
    <tt>default_played</tt> to <tt>true</tt>).

    <!--
      You can add somewhere the call to LevelFinished(TYourLevel.Create),
      so that player is in some situation transferred from other level
      to your level.
    -->

    <p>Note that when starting "New Game" player can choose to start
    from any level that he (she ? :) previously visited ("visited"
    either as part of normal game story of through debug command
    "Change to level"). This feature may be removed in the future when
    real "Save game" / "Load game" feature will be implemented.
  </li>

  <li><p>With a little programming, you can add your own level logic.
    This allows to implement in ObjectPascal special behaviors on the level.
    You can also add other full-featured 3D objects and animations to the level
    (although the level itself may also contain animated things).</p>
  </li>
</ul>

<?php echo $toc->html_section(); ?>

<ul>
  <li><p>Item up vector must be +Z.

  <li><p>Item should be oriented such that:

    <p>Z = 0 plane is the base plane of the item. It will be aligned with the base
    (i.e. lowest Z) of "placeholder" object placed on the level.

    <p>X = 0 and Y = 0 is the line around which object will rotate
    when shown on the level. It will be aligned with the X, Y middle
    of "placeholder" object placed on the level.

    <p>I had an idea to just automatically take item's bounding box,
    it's middle X, Y and lowest Z and automatically adjust to this.
    But this would disable some possible visual tricks.

  <li><p>Size of item model is exactly the size that will be used to display
    it on level, so set this to look good. Sensible default
    is size around 1.0.

  <li><p>About the "placeholder" objects on the level:

    <p>You can place items on the level by placing a "placeholder" object
    on the level with appropriate name.

    <p>When loading, we search for shape nodes that have a parent node
    named like "CasRes&lt;item-resource-name&gt;&lt;quantity&gt;_&lt;ignored&gt;".
    Where "&lt;item-resource-name&gt;" is "LifePotion" or "Sword" or any
    other TItemResource.Name value (see CastleItems unit),
    "&lt;quantity&gt;" is, well, the integer quantity
    ("1" is assumed if "&lt;quantity&gt;" is omitted), and "&lt;ignored&gt;"
    is just anything that will be ignored (you can use this
    to make object names in your model unique,
    which is obviously desirable).

<!--
(new Blender allows longer names.

    <p>Some reasoning about convention above: Blender's names
    have quite limited length, that's why CamelCase is used
    for "&lt;item-resource-name&gt;" and components are generally "glued"
    without any "_" or "-" or " " between.
-->

    <p>Such "placeholder" object is removed from the actual level and
    instead I insert into level an item. Item position is determined
    by placeholder lowest Z and middle X,Y line (see above).

    <p>You can easily insert such "placeholder" with Blender &mdash; just insert
    any shape (I usually insert Cube, and set it's rendering to
    "wireframe" in Blender), and then edit Blender's mesh name
    as appropriate.

  <li><p>Prepare appropriate 2D image of item (to be shown in inventory slots
    and such).
    <!--
    (Once I tried just automatically rendering models inside inventory slots
    but this doesn't look good enough). You can do it however you like.
    -->
    For example, you can do this by opening
    the model in <?php echo a_href_page('view3dscene', 'view3dscene') ?>,
    setting your camera as desired and taking a screenshot
    (see "Display -> Screenshot ..." menu options).
    Or use "Display -> Raytrace".
    It's a very good idea to store the screenshot camera for the future
    (in case we need to redo the image later),
    by using "Console -> Print Current Camera (Viewpoint)..."
    and adding it to your 3D model file.

    <p><?php echo a_href_page('"The Castle"', 'castle') ?> game
    requires all item images to have a size 95 x 95, but that is a specific
    requirement of this game (because of how it displays the inventory).
    Each game has it's own way to display the inventory, so these requirements
    may be different for other games.

    <p>This image filename should be referenced by the <tt>image="xxx"</tt>
    attribute in the root element of item <tt>resource.xml</tt> file.

  <li><p>For an item that can be equipped as a weapon, you also have
    to prepare two scenes showing the weapon in "ready" state (equipped)
    and "attack" (attack animation). Both of these 3D objects may be static
    or animated (through precalculated animation or (TODO) VRML/X3D events).
    You will almost always want to make the "attack" model animated,
    but you can also make the "ready" model animated (e.g. to show some fire
    flickering on the weapon or such; just don't overdo it, as such animations
    can become distracting from the player).

  <li><p>Finally, to really add the item to the game add it's <tt>resource.xml</tt>
    file. See README_about_index_xml_files.txt for more documentation
    about what is possible there. New item can be automatically placed
    on a level, using placeholders named "CasRes" + item name.
    There is no need to modify the game executable.
</ul>

<?php echo $toc->html_section(); ?>

<ul>
  <li><p>Many notes about items apply also to creatures:

    <ul>
      <li>Up vector must be +Z.
        Moreover, for creatures: the looking direction
        (i.e. whatever should be considered the "front" of the creature)
        should be +X.
      <li>Z = 0 is the base plane (ground) of the creature
      <li>X = 0 and Y = 0 is the middle of the creature on XY plane
      <li>Size of creature's model as rendered in the game is exactly
        the size of it's model. A good reference is player's camera
        height: 2. So creatures have generally height around 2.

      <li>Like with items, initial creatures positions can be set
        by placing a "placeholder" on the level.
        Creature position is determined by placeholder lowest Z and
        middle X,Y line.

        <p>If <tt>&lt;creature-life&gt;</tt> part is not present, the default
        MaxLife value (taken from this creature's <tt>resource.xml</tt> file,
        attribute <tt>default_max_life</tt>) will be used.
        You can also use this feature to place already dead corpses on the level
        (e.g. the Doom E1M1 level uses this):
        just specify <tt>&lt;creature-life&gt;</tt> as 0 (zero).

        <p>Initial creature looking direction
        is determined by the transformation of the placeholder object.
        See placeholder_default_direction in README_about_index_xml_files.txt
        for details.

      <li>For more customizations, see developers documentation
        about CastleCreatures unit.

      <li>For simple customizations you can set various creature
        parameters by editing <tt>data/creatures/xxx/resource.xml</tt> file.
    </ul>
  </li>

  <li><p>When you want to use this creature on particular level(s),
    you should add it to <tt>prepare_resources</tt> in <tt>level.xml</tt>
    file. Alternatively, if the creature should be always loaded
    when the player is present (for example, special creature <tt>Arrow</tt>
    should be always loaded, as the player may come to any level with a bow
    and shoot some arrows), then it should have <tt>always_prepare</tt> in
    it's <tt>resource.xml</tt>.</p></li>

  <li><p>For testing various animations of creatures (and adjusting their time
    parameters) you can use <?php echo a_href_page('view3dscene', 'view3dscene') ?>.

  <li><p>If you want to use <?php echo a_href_page_hashlink('shadow volumes', 'x3d_extensions',
    'section_ext_shadows'); ?>, and you want the creature to cast shadows
    by shadow volumes, the creature model must be a correct 2-manifold.
    This means that every edge must have exactly two neighboring faces,
    so that the shape is a correct closed volume.
    Also, faces must be oriented consistently (e.g. CCW outside).
    This requirement is usually quite naturally satisfiable for creature
    models, and consistent ordering allows you to use backface culling which
    is a good thing on it's own.</p>

    <p>You can inspect whether your model is detected as a 2-manifold
    by <?php echo a_href_page('view3dscene', 'view3dscene'); ?>:
    see menu item <i>Help -&gt; Manifold Edges Information</i>.
    To check which edges are actually detected as border you can use
    <i>View -&gt; Fill mode -&gt; Silhouette and Border Edges</i>,
    manifold silhouette edges are displayed yellow and border edges
    (you want to get rid of them) are blue.</p>

    <p>In Blender, you can easily detect why the mesh is not
    manifold by <i>Select non-manifold</i> command (in edit mode).
    Also, remember that faces must be ordered consistently CCW
    &mdash; I think that in some cases <i>Recalculate normals outside</i>
    may be needed to reorder them properly.

  <li><p>Notes for animations: beware. Animations are done with a method
    that essentially makes their rendering very fast (rendering each
    animation frame should be as fast as if it was a still scene).
    However it costs much in terms of memory consumption (and in
    "Loading creatures" time). So be sure to test how much memory is consumed
    by the game after your new animation is loaded. (using system monitors
    or such).</p>
  </li>
</ul>

<?php echo $toc->html_section(); ?>

<p>As a final note, our engine supports some nice graphic features, and you
may want to use them to make your models nicer. Some of the more important
eye-candy features are:</p>

<ul>
  <li><p><?php echo a_href_page(
    'Write shaders in GLSL, and connect them to objects in your VRML / X3D models',
    'x3d_implementation_shaders'); ?>.</p></li>

  <li><p><?php echo a_href_page_hashlink('Bump mapping',
    'x3d_extensions', 'section_ext_bump_mapping'); ?>.
    See "The Fountain" level for example.</p></li>

  <li><p>You can make your level geometry cast dynamic shadows (on everything,
    including itself). Just make it 2-manifold (see above notes about creatures
    for instructions how to check this).
</ul>

<?php echo $toc->html_section(); ?>

<p>I need sounds (effects and music) for my game. If you can
produce good sounds &mdash; send them to me.

<p>If you cannot make your own sounds but you're willing to spend
some time just finding the sounds on the Internet, that's also great.
There are many places from where you can download free sounds on the Internet
(so many that it actually takes some time to find the good sounds).
So your help here is welcomed as well.
(Still, remember that sounds have to be available under GNU GPL or some
super-set of it, like LGPL or public domain.)
Some useful links to start searching for sounds can be found at
<a href="http://www.pascalgamedevelopment.com/showthread.php?2278-Where-do-you-get-your-sounds-and-samples">"Where
do you get your sounds and samples ?" thread on PGD</a>.
In particular
<ul>
  <li>Sounds found by
    <a href="http://www.flashkit.com/soundfx/">FlashKit (soundfx)</a> and
    <a href="http://www.flashkit.com/loops/">FlashKit (loops)</a>
    are OK when marked as <i>freeware</i>. This is a great resource
    of sounds.</li>
  <li>Sounds found by <a href="http://www.findsounds.com/">FindSounds</a>
    are sometimes OK &mdash; click on "Show page" for each sound to
    see what the author of the sound allow you.</li>
</ul>

<p>What sounds are wanted ?
<ul>
  <li><p>Basically, any sound that fits into the game is wanted.
    This includes "environmental" sounds that could match some
    levels (e.g. wind blowing, birds singing, river flowing &mdash;
    many ideas; such sounds add a great "atmosphere" to the levels.).
    This also includes sounds for some player's action, and sounds for some
    creature's actions.

    <p>In general, don't feel limited.
    If you see some place in my game that could deserve playing
    some nice sound, feel free to create it and I will put it inside
    my game. The more good quality sounds,
    the more realistic the game experience will be.
  </li>

  <li><p>Now, if you really want concrete examples of sounds that are
    needed: Look into the data file <tt>data/sounds.xml</tt>,
    search there for sounds with empty filename (<tt>file_name=""</tt>).
    They indicate missing sound effects. These are places where I imagined
    playing some sound &mdash; but I couldn't make/find any good sound
    for it.

    <p>The names should be
    more-or-less self explanatory, e.g. <tt>player_sudden_pain</tt>
    means a sound that will be played when player's life points
    will significantly drop down (this will be played along with
    showing "red-out" effect). If you have doubts what given sound
    is intended to be, just ask on
    <?php echo a_href_page('our forum or mailing list', 'forum'); ?>.
  </li>
</ul>

<p>Notes about sound files:

<ul>
  <li><p>Sound format requirements: my engine can play WAV
    and OggVorbis files. Short sounds should be stored as WAV,
    long sounds (like level music) may be stored as OggVorbis files.

  <li><p>Do not make your sounds more silent
    just because you're recording some "silent" thing.
    For example, <tt>mouse_squeek.wav</tt> should be as loud
    as <tt>plane_engines_starting.wav</tt>. The fact that mouse squeek
    is in reality much more quiet than plane engine doesn't matter here.
    You should always make your sound files with maximum quality,
    and this means that they should use all the available sound range.

  <li><p>Music: as of 2006-04-25, music is done and it's great.
    To create a music I just need a sound file that can be nicely
    played in a loop.

  <li>Special notes for creating footsteps sound:
    <ul>
      <li>Don't make the footsteps sound too long.
        Preferably you should put there only 2 footsteps. Reason ?
        When progress is displayed (e.g. because player changes levels),
        or when player enters the game menu, footsteps sound is not
        immediately stopped &mdash; it's just played until the end.
        Yes, this is desired, as it makes better effect than suddenly
        stopping all the sounds.

      <li>These 2 footsteps should take about 1 second. This is the amount
        of time that "feels good" with head bobbing.
        (See the <tt>data/player.xml</tt> file, <tt>head_bobbing_time = 0.5</tt>
        there means that 1 footstep = 0.5 of the second for head bobbing.)
    </ul>

  <li><p>Remember that if sounds are supposed to be spatialized (i.e. played
    by Sound3d procedures), then you must make them mono (never stereo!).
    That's because Windows OpenAL will never spatialize stereo sounds.

    <p>You can use any editor you like to convert your sounds to mono.
    I like this sox command-line:
    <pre>  sox input.wav -c 1 output.wav</pre>
    See also <tt>data/sounds/scripts/example_make_mono.sh</tt>
</ul>

<?php echo $toc->html_section(); ?>

<p><?php echo a_href_page_hashlink('Download game sources', 'engine',
'section_download_src'); ?>. From this page, grab both
<i>Castle Game Engine</i> sources and <i>castle</i> sources.
Unpack them in such way that <tt>castle_game_engine/</tt>
and <tt>castle/</tt> directories are together (siblings) under the same directory.

<p>Compile by running <tt>make</tt> inside the <tt>castle/</tt> directory.
Note that you must use GNU make.
You must have <a href="http://www.freepascal.org/">FPC (Free Pascal Compiler)</a>
installed (you can look at <?php echo a_href_page_hashlink(
  'notes about FPC version required', 'engine', 'section_fpc_ver'); ?>,
  in short: use latest stable FPC version).
There is no Delphi support &mdash; I live in the open source world.

<!--
<div style="border: thin solid gray; margin-left: 3em; padding: 0.5em;">
<p><i>Old notes for compiling with FPC &lt;= 2.0.4 :</i>

<p>Sometimes FPC 2.0.4 fails with <tt>Fatal: Internal error 200310221</tt>.
This is a bug in FPC 2.0.4, fixed since a long time in trunk 2.1.1 and later.
I don't know the exact cause of it, but it has
something to do with FPC 2.0.4 inability to handle unit dependencies in
"The Castle" (no, there are no circular interface dependencies, but still there is
something that confuses FPC 2.0.4). The workaround was usually to do
<tt>make clean</tt> before each compilation (so that each FPC run starts
from a "clean" state and has to recompile all castle units), fortunately
FPC is so fast that this wasn't a big problem. Sometimes also retrying the
compilation (i.e. without <tt>make clean</tt>) pushed it to "move forward".</p>

<p>Best workaround is of course to upgrade to FPC 2.2.0.
I don't plan to keep FPC 2.0.4 compilation supported much longer,
now that stable FPC 2.2.0 is released.</p></div>
-->

<p>Before you run the compiled executable, make sure that it's properly
installed: Unix users can make a symlink
<tt>$HOME/.castle.data</tt> (you can easily
make this symlink by <tt>make install</tt>), unless you know you will
always run the game with current directory = castle directory.
And on all OSes make sure that you have the appropriate libraries
installed. Windows developers may be comfortable with simply copying
appropriate DLL files from precompiled version of the game,
or download and use <?php echo current_www_a_href_size('these DLLs',
  'miscella/win32_dlls.zip'); ?>.</p>

<p>In the game sources archive I include all the sources needed to compile
the game. Also everything needed to tweak with various parts of the game
(e.g. GIMP *.xcf files, that are the "source" version of *.png images,
and Blender's *.blend files that are the "source" version of some *.wrl files).

<p>Everything is open source,
<a href="http://www.gnu.org/licenses/gpl.html">GNU GPL</a>.

<?php echo $toc->html_section(); ?>

<p><i>Below are some long-term plans. The game I'm dreaming about here
is something like "The Castle 2.0", or even "The Castle 3.0",
and probably will not be actually named "The Castle" anymore.
Still, technically it will be "The Castle" continuation, based
on next-generation of our VRML/X3D engine.</i></p>

<p>Beware, I'm going to be dreaming in the next paragraph.

<p>Basically the game is intended to be like FPS but with some nice storyline.
This also means that it should feel more like RPG (in more-or-less
fantasy setting) &mdash; large world, many items, weapons, some character
stats and levels etc. Also I want to utilize my engine to make
levels more "interactive" &mdash; some objects on the level
are able to move etc. There are many such games, some very old, some quite new.
I'm just going to do this once again, in the way I want,
and make it really good :)

<p>Now, going back to reality: for PGD competition, no storyline,
no real character stats were done (player has just Life and MaxLife &mdash;
that's it), and the world is small (3 levels)
and there are not so many items and creatures (as of 0.6.2 version,
there are 5 creatures and 7 items). Basically
my intention for PGD compo was to do something like
"Quake, but with more short-range weapons and only 3-4 levels".
(yeah, very innovative, I know :).

<p>As for the development style &mdash; this game is developed
fully open-source (including the game data). Part of the game
sources is a general-purpose 3D game engine
(I used it already to make small games and tools
like
<?php echo a_href_page('malfunction', 'malfunction'); ?>,
<?php echo a_href_page('lets_take_a_walk', 'lets_take_a_walk'); ?> and
<?php echo a_href_page('view3dscene', 'view3dscene'); ?>).
Everything should run on any modern OS/processor supported by
<a href="http://www.freepascal.org/">FreePascal</a> compiler,
see <?php echo a_href_page('Castle Game Engine', 'engine'); ?>
 for exact listing (if you want to port
it to other system, you're welcome to provide the patches and compiled binaries).
And I'm trying to do all game data using open-source things,
most important here is <a href="http://www.blender3d.org/">Blender</a>
used to make all the models (see
<?php echo a_href_page('credits page', 'castle-credits'); ?> for
full list of things and resources used).

<?php
  castle_footer();
?>
