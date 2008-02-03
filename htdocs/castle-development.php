<?php
  require_once 'vrmlengine_functions.php';

  common_header("The Castle &mdash; development", LANG_EN, '');

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
      new TocItem('Some implementation notes (interesting only' .
        ' for programmers probably)', 'implementation'),
    )
  );
?>

<h1><?php echo a_href_page('The Castle', 'castle'); ?> &mdash; development</h1>

<p><?php echo a_href_page('"The Castle"', 'castle') ?> is developed in
an open-source manner, which means that if anyone wants to help &mdash;
you're more than welcome. You don't even have to be a programmer &mdash;
3D level/creature designers are wanted (alive!),
and sending simple feedback is definitely useful too.
This page collects various developer information how you can
help, and how you can compile and extend the game's code and data.

<p>All questions, feedback, etc. may be posted to
<?php echo MAILING_LIST_LINK; ?>.

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

    <p>Bugs should be submitted to
    <a href="<?php echo BUGS_TRACKER_URL; ?>">bug tracker</a>,
    if you're unsure is it a "real" bug you can also post to
    <?php echo MAILING_LIST_LINK; ?>.
    </li>

  <li><p>Features. Do you think that anything should be added ?
    Maybe something should be changed ? Any ideas may be useful here
    &mdash; new key / mouse commands, changing the behavior of
    present commands, displaying some things in a different way, etc.

    <p>Feature requests should be submitted to
    <a href="<?php echo FEATURE_REQUESTS_TRACKER_URL; ?>">feature request
    tracker</a> or to <?php echo MAILING_LIST_LINK; ?>.
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
  <li><a href="http://pascalgamedevelopment.com/viewtopic.php?p=21080">
    "Where can I find free repeatable textures?" thread on PGD forums</a>
  <li><?php echo a_href_page_hashlink('list
    of things used when developing malfunction', 'malfunction',
    'section_external_resources'); ?>
  <li><?php echo a_href_page_hashlink('list
    of things used when developing lets_take_a_walk', 'lets_take_a_walk',
    'section_credits'); ?>
</ul>

<?php echo $toc->html_section(); ?>

<p>The short 1-sentence introduction is: you can use any 3D modelling program
able to create VRML 1.0 or 2.0 (aka 97) files.</p>

<p>VRML 1.0 and 2.0 formats have official specifications
and our engine is capable
of handling any VRML 1.0 valid files and most VRML 2.0 files (actually,
<?php echo a_href_page('even more than that', 'kambi_vrml_extensions') ?>).
So the engine is not tied to any particular modelling program.
Use <?php echo a_href_page('view3dscene', 'view3dscene') ?>
 to view the various models outside of the game.</p>

<p>You may want to view in view3dscene some of the models from
<?php echo a_href_page("Kambi VRML test suite",
  "kambi_vrml_test_suite"); ?> &mdash;
there are many testcases and demos of what can be expressed in VRML 1.0 and 2.0
and what our engine can understand and render.</p>

<p>My favourite 3d modeller is <a href="http://www.blender3d.org/">Blender</a>.
It's open source, it's available for Linux (yeah, Windows too
if you insist :), and it has an enormous set of features.
Including good export for VRML 1.0 and 2.0, of course.
Basically there are no special
rules for designing in Blender models for our engine &mdash;
just do whatever you like (and whatever can be exported to VRML 1.0 or 2.0).</p>

<p>The engine also handles perfectly VRML files exported from <i>3ds Max</i>.
So you can use <i>3ds Max</i> to make models too. Also VRMLs produced
by open-source modeller <a href="http://www.artofillusion.org/">Art of Illusion</a>
are perfect. I suspect that all significant 3D modelling tools are able
to export decently to VRML, so you can probably use any modeller you like.
(This was one of the reasons that I chose VRML format as my basic format
&mdash; it's nice to see that it pays off :) ).
Note that I'd like to make the game using as much open-source tools as possible,
so using open-source modeller is strongly favored over proprietary modellers.

<p>Some detailed hints / tricks:</p>

<ul>
  <li><p>I often "wrap" (using <tt>WWWInline</tt> VRML 1.0 or
    <tt>Inline</tt> VRML 2.0 node) models produced by
    Blender inside some small VRML files written by hand.
    For example, <tt>data/levels/castle_hall_final.wrl</tt> is a small VRML file that is
    maintained by hand. It sets up some things that were uncomfortable
    (or impossible, due to lacks of VRML 1.0 exporter) to set up from Blender,
    and includes (using <tt>WWWInline</tt> node) other VRML file
    (<tt>data/levels/castle_hall_processed.wrl</tt> in this case)
    that defines geometry,
    materials and textures of the level.</p></li>

  <li><p>Moreover, I sometimes want to post-process VRML files generated from
    Blender. I use EmacsLisp macros for this (since this is a great language
    for text operations, and it's built in my favourite editor :) ).
    For example <tt>data/levels/castle_hall.wrl</tt> in VRML file
    exported from Blender (from <tt>data/levels/castle_hall.blend</tt>),
    and it's processed to <tt>data/levels/castle_hall_processed.wrl</tt>.</p></li>

  <li><p><i>VRML 2.0 (aka 97) notes:</i><br />
    For Blender, it's adviced to use our customized VRML 97 exporter,
    see <?php echo a_href_page("Blender VRML stuff", "blender_stuff"); ?>.</li>

  <li><p><i>3DS / MD3 / Collada / other formats notes:</i><br/>
    Actually, you can also use models in other formats instead
    of VRML. Our engine can handle other formats, see
    <?php echo a_href_page('view3dscene', 'view3dscene') ?>
    for more detailed info.</p>

    <p>So if your favourite 3d modelling program is not able to export
    to VRML but it's able to export to e.g. 3DS &mdash; you can use it too.
    But, since 3DS is a closed format, it will never be supported
    completely by our engine (just like by any other program, since everyone
    has to "reverse engineer" 3DS).</p>

    <p><i>Collada</i> format is a positive exception to this. It's an XML format with
    an open specification, pretty much like X3D / VRML.
    So using 3D models is Collada format is actually not a bad idea &mdash;
    except that we do not handle all features from Collada files currently.
    But Collada support can (and will be, if needed) easily improved &mdash;
    don't hesitate to report if some Collada feature is not handled, we'll fix it.</p>

    <p>Still, note that <em>everything</em> that can be expressed
    in other 3D formats handled and that is understood
    by our engine can also be expressed in VRML.
    But not vice-versa, i.e. there are some features that
    are available with my engine when using VRML but are not available
    when using e.g. 3DS.
    So the bottom line is: well, you can use 3DS or some other formats
    with my engine, it works OK, but VRML format is just way better :)</p></li>
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
to get their list. <tt>--debug-no-creatures</tt> is one such useful option:
it avoids this lengthy "Loading creatures" loading time, which is useful
when you're not interested in testing creatures
(e.g. when you only design new level geometry).

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

  <li><p>To place items and creatures on the level you place
    a special "stub" objects on the level. Every stub object will be removed
    when the level is loaded (so you can use any shape for it &mdash;
    I usually use wire cubes). Stub object position and name will
    determine the actual item/creature position, kind and quantity
    (quantity in case of items).
    Name format for items is
    <tt>Item&lt;item-kind-name&gt;&lt;quantity&gt;_&lt;ignored&gt;</tt>.
    Name format for creatures is
    <tt>Crea&lt;creature-kind-name&gt;_&lt;ignored&gt;</tt>.
    See lower in this file for more details.</p></li>

  <li><p>When loading level, we search for node named <tt>LevelBox</tt>.</p>

    <p>In VRML 1.0, such node should be a parent of some shape
    (e.g. <tt>IndexedFaceSet</tt>) node.
    In VRML 2.0, such node should be within <tt>Shape</tt> node that
    has parent named <tt>ME_LevelBox</tt>.
    What this means in practice, is that in Blender you can simply
    name the Blender's mesh <tt>LevelBox</tt> and it will be correctly
    recognized by our engine if you export to VRML 1.0 or 2.0 using
    standard Blender exporters.

    <p>When such node is found, we calculate it's BoundingBox and remove
    it from the scene. Calculated BoundingBox becomes Level.LevelBox
    value. And Level.LevelBox is used to limit allowed player positions.
    This can be used to disallow player to move to the very edge of
    the level.

    <p>Usually you will add <tt>LevelBox</tt> node using Blender, you can also
    use <?php echo a_href_page('view3dscene', 'view3dscene') ?> &mdash;
    see command 'Console -> Print scene bounding box as VRML node'.

  <li><p>Similar to <tt>LevelBox</tt>, I do identical trick to calculate
    water boxes. Just place a mesh with name 'WaterBox'.
    At some point this will be extended (when I'll need it) to
    include every 'WaterBox_&lt;ignored&gt;', so that you will be able to
    define water by a sum of boxes.

    <p>On some particular levels I use similar trick with <tt>XxxBox</tt>
    for other other purposes too.
    For example "Castle Hall" level has <tt>HintButtonBox</tt> to indicate
    where to show "<i>Hint: press this button by the "p" key</i>".

  <li><p>Levels must be oriented such that +Z is "up".
    While all things in my general units are flexible
    and allow any up vector to be used, I made in "The Castle" a couple
    of assumptions that really need +Z to be up.
    (like <tt>TItemOnLevel.PlayerCollision</tt>).

  <li><p>Typical VRML nodes that I write "by hand"
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

      <li><tt>Light</tt> nodes

        <p>I usually write light nodes in a separate VRML file
        (that contains only light nodes; see e.g. <tt>data/levels/castle_hall_lights.wrl</tt>).
        This is comfortable, because I usually want to use level lights
        also on other objects (like enemies) that are not part of the level
        object.

        <p>(Also, while Blender exporters can export Blender lights to VRML,
        it doesn't allow me to use all features of VRML lights.)

        <p>Since version 0.5.7 you can also edit the lights from the game
        &mdash; see "Debug options" menu for "Edit lights" command.
        So you have to add appropriate nodes to xxx_lights.wrl
        file by hand, and then you can just run the game and configure lights
        properties from the game.
      </li>
    </ul>

  <li><p>All levels should keep the same general sizes. In other words,
    it's <i>not good</i> to just make your new level of arbitrary
    size and then set player's avatarSize such that things "look sensible".
    Why ? Because I place items and enemies on the level.
    So size of all these various models must match.

    <p>In other words: just make sure you use
    <pre>
      NavigationInfo {
        avatarSize [ 0.5, 2 ]
        speed 20
        ...
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
      <li><p>Shapes placed under the name Sector&lt;index&gt;_&lt;ignored&gt;
        are removed from the real level in TLevel constructor.
        The geometry of given sector is understood to be the sum
        of all Sector&lt;index&gt; boxes.

        <p>Also VisibleSectors for each sector are coded in CastleLevel.pas unit.

        <p>For programmers: in cases when the sum of bounding boxes
        is not flexible enough to define a geometry,
        you can define any kind of geometry in Pascal code,
        by overriding TSceneSector.IsPointInside.
        In the future, treatment of the shape inside Sector&lt;index&gt;_&lt;ignored&gt;
        may change, so that any kind of closed shape will be allowed there
        and it will be stored and used precisely (not only as it's bounding box).

        <p>Remember that sectors are numbered starting from 0.

      <li><p>Shapes placed under the name Waypoint&lt;index&gt;_&lt;ignored&gt;
        are also removed from the real level in TLevel constructor.
        Middle point of bounding box of such shape determines
        waypoint's Posiiton.
        Remember that waypoints are numbered starting from 0.

      <li><p>Sectors of waypoints (and reverse property, Waypoints
        of sectors) are automatically calculated, by looking for waypoints
        that have a Position that falls inside Sector's BoundingBoxes
        enlarged by margin 0.5.

        <p>Note that sectors' BoundingBoxes need <i>not</i> be strictly separated.
        When object (like player or creature) is within two sectors,
        it's arbitrarily assigned to any allowed sector.
        And for the rendering optimization, such sectors would work fine.
        However, for monster AI, sectors should not have any common
        large non-zero volume.

        <p>So in practice, each sector should usually place
        waypoint's Position at it's border. Sectors may overlap but only
        for a minimal distance (otherwise awkward movement may happen
        when creature is in the common part).

      <li><p>You don't have to cover whole level with sectors.
        If some object (like player or creature) is not within any defined
        sector, it's considered to be inside the "implicit whole sector",
        that is a little special. It's geometry is considered
        infinte (every 3D point belongs to it, if it doesn't belong to any
        of the explicitly defined scetors). VisibleSectors
        is treated like filled with values "true" (so all other sectors
        are assumed to be visible).

        For programmers: SectorWithPoint returns nil when no sector found,
        and this indicates such "implicit whole sector".
        FindWay accepts SectorBegin and SectorEnd as nil
        (and always returns then false).

      <li><p>Sectors and waypoints are used for 2 things:

        <ol>
          <li>To speed up rendering: When player's CameraPos is within
            a given sector (not nil), we have to render only the sectors
            for which VisibleSectors is true.
            TODO: right now it's not used to speed up rendering,
            and VisibleSectors is ignored.

          <li>To make creature moving AI more intelligent:
            If a creature wants to move from SectionBegin to SectorEnd,
            and SectionBegin &lt;&gt; SectorEnd (and none of them is nil),
            creature knows that it must pass through appropriate waypoints.
        </ol>

        It's assumed that there will not be too many sectors on the level
        (100 sectors is <i>really</i> around maximum. 10-20 is reasonable.)
        So sectors are <i>not</i> (at least right now) stored in any "intelligent"
        structure (like an octree) and creature moving AI uses
        the simplest search on the graph to find a satisfiable path
        (no A* algorithm or anything; it's simply not needed for the kind of
        sectors layouts that I will typically use).
      </li>
    </ul>
  </li>

  <li><p>If you made a completely new level, you want to add it to the game.
    That's easy: just add new <tt>&lt;level&gt;</tt> element to
    <tt>data/levels/index.xml</tt> file. Now the level is known to the program.
    You can switch to the level by debug menu "Change to level" command
    (or even you can start "new game" from this level, if you set it's
    <tt>default_available_for_new_game</tt> to <tt>true</tt>).

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

    <p>If you don't mind a little programming, you may want to create
    new <tt>TLevel</tt> descendant in the program and use it for your level,
    this allows for many tricks.
    See existing level implementations in CastleLevelSpecific unit
    for examples what can be achieved.
    You may even want to move your new level class
    to a completely separate unit file if you want.
  </li>

  <li><p>With a little programming, you can add various dynamic parts
    to your level. I call these things "level objects".
    For example, this is used to make the moving cart (gate level), button
    (castle hall level), exit gate (cages level), elevator (tower level),
    doors (doom level)... and many more. Level objects can also cast shadows,
    see notes about shadows for creatures.</p>

    <p>It's expected that at some point it will be possible to add such
    level objects only by editing <tt>levels/index.xml</tt> file, with no need
    to modify game sources.</p></li>
</ul>

<?php echo $toc->html_section(); ?>

<ul>
  <li><p>Item up vector must be +Z.

  <li><p>Item should be oriented such that:

    <p>Z = 0 plane is the base plane of the item. It will be aligned with the base
    (i.e. lowest Z) of "stub" object placed on the level.

    <p>X = 0 and Y = 0 is the line around which object will rotate
    when shown on the level. It will be aligned with the X, Y middle
    of "stub" object placed on the level.

    <p>I had an idea to just automatically take item's bounding box,
    it's middle X, Y and lowest Z and automatically adjust to this.
    But this would disable some possible visual tricks.

  <li><p>Size of item model is exactly the size that will be used to display
    it on level, so set this to look good. Sensible default
    is size around 1.0.

  <li><p>About the "stub" objects on the level:

    <p>You can place items on the level by placing a "stub" object
    on the level with appropriate name.

    <p>When loading, I search for shape nodes that have a parent node
    named like "Item&lt;item-kind-name&gt;&lt;quantity&gt;_&lt;ignored&gt;".
    Where "&lt;item-kind-name&gt;" is "LifePotion" or "Sword" or any
    other TItemKind.VRMLNodeName value (see CastleItems unit),
    "&lt;quantity&gt;" is, well, the integer quantity
    ("1" is assumed if "&lt;quantity&gt;" is omitted), and "&lt;ignored&gt;"
    is just anything that will be ignored (you can use this
    to make object names in your model unique,
    which is obviously desirable).

    <p>Some reasoning about convention above: Blender's names
    have quite limited length, that's why CamelCase is used
    for "&lt;item-kind-name&gt;" and components are generally "glued"
    without any "_" or "-" or " " between.

    <p>Such "stub" object is removed from the actual level and
    instead I insert into level an item. Item position is determined
    by stub lowest Z and middle X,Y line (see above).

    <p>You can easily insert such "stub" with Blender &mdash; just insert
    any shape (I usually insert Cube, and set it's rendering to
    "wireframe" in Blender), and then edit Blender's mesh name
    as appropriate.

  <li><p>Prepare appropriate 2d image of item (to be shown in inventory slots etc.).
    (Once I tried just automatically rendering models inside inventory slots
    but this doesn't look good enough). You can do it however you like.

    <p>One way to do it is to use my raytracer, called
    <?php echo a_href_page('rayhunter', 'rayhunter') ?> :
    set up appropriate light and camera settings and then just render
    item's model to the image. <tt>data/items/sword/image.png</tt>
    is done like this.

    <p>Another is to open a model with
    <?php echo a_href_page('view3dscene', 'view3dscene') ?>,
    set your camera however you like and make a screenshot.
    Then you can edit it in whatever program you like
    (like GIMP) to suit your needs.

    <p>Note that all items' images must be of equal size.
    See <tt>data/items/Makefile.common</tt> for
    IMAGE_WIDTH and IMAGE_HEIGHT constants.

    <p>Traditionally this image is stored inside
    <tt>data/items/your_item/image.png</tt> image.
    You may also provide <tt>data/items/your_item/image.wrl</tt>
    VRML file, that includes item's model file and sets camera to
    the same viewport you used to render the image.
    This is essential for people that may later want to modify your model
    and remake the image.

  <li><p>If this is a weapon, then additionally you want to prepare what I call
    a ScreenImage of this weapon &mdash; this will be displayed on player's
    screen when it's equipped. See e.g. <tt>equipped.png</tt> images for
    sword or bow items.

    <p>Once again, you can do this image however you like.
    I used rayhunter to render screen for sword.
    In the future the need for this image will be removed &mdash;
    I'll render weapon's
    3D model in the game (probably using the 1st attack animation frame).

    <p>You must also prepare an animation of "swinging" the weapon.
    See e.g. <tt>attack_*.wrl</tt> files for sword or bow items.

  <li><p>Finally, to really add the item to the game add 1 line to
    CastleItems units in DoInitialization call creating your
    TItemKind instance.

  <li><p>For some items you may be able to just reuse some existing TItemKind
    class, for others you will want to derive new class from TItemKind
    and override there things you want.

  <li>For simple customizations you can set various item
    parameters by editing <tt>data/items/kinds.xml</tt> file.
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
        by placing a "stubs" on the level. Name of stub object is like
        "<tt>Crea&lt;creature-kind-name&gt;&lt;creature-life&gt;_&lt;ignored&gt;</tt>".
        Creature position is determined by stub lowest Z and
        middle X,Y line.

        <p>If <tt>&lt;creature-life&gt;</tt> part is not present, the default
        MaxLife value (taken from creatures <tt>kinds.xml</tt> file,
        attribute <tt>default_max_life</tt>) will be used.
        You can also use this feature to place already dead corpses on the level
        (e.g. the Doom E1M1 level uses this):
        just specify <tt>&lt;creature-life&gt;</tt> as 0 (zero).

        <p>Initial creature looking direction
        is determined by ... TODO: right now starting creature direction
        just points into player starting position.
        This is more-or-less sensible, usually.
        But it's meant to be comfortably configurable in scene file in the future.
        <i>Plan to fix this:</i> It's already possible to add a creature
        in game, using debug menu <i>Add creature</i> commands.
        Together with debug menu command <i>Time stop for creatures</i>,
        this allows you quite easily and comfortably place creatures on the level,
        and you're able to freely set both their position and direction
        then. This should be extended: 1. new VRML nodes specially for
        "The Castle" that express creatures on the level (as alternative
        to creatures "stub boxes", or maybe inside such stub boxes ?)
        2. debug menu command to dump current creatures to such VRML nodes,
        so that you can paste them to level <tt>xxx_final.wrl</tt> file.

      <li>To really add the creature to the game add 1 line to
        CastleCreatures unit in DoInitialization call creating your
        TCreatureKind instance.

      <li>For some creatures you may be able to just reuse some existing TCreatureKind
        and TCreature descendants, for others you will want to derive
        your own descendants.

      <li>For simple customizations you can set various creature
        parameters by editing <tt>data/creatures/kinds.xml</tt> file.
    </ul>
  </li>

  <li><p>When you want to use this creature on particular level(s),
    you should add it to <tt>required_resources</tt> in <tt>levels/index.xml</tt>
    file. Alternatively, if the creature should be always loaded
    when the player is present (for example, special creature <tt>Arrow</tt>
    should be always loaded, as the player may come to any level with a bow
    and shoot some arrows), then it can be added to <tt>required_resources</tt> in
    <tt>player.xml</tt>.</p></li>

  <li><p>For testing various animations of creatures (and adjusting their time
    parameters) you can use <?php echo a_href_page('view3dscene', 'view3dscene') ?>.

  <li><p>For shadows to work fast, creature model (all animation
    frames etc.) should be composed from a number of 2-manifold parts.
    It's allowed to not make them perfectly 2-manifold, but then
    in some cases, some artifacts are unavoidable &mdash; see
    <?php echo a_href_page("VRML engine documentation",'vrml_engine_doc'); ?>,
    chapter "Shadows" for description.
    To be manifold, edge must have exactly two neighboring faces,
    so that ideally the creature shape is a correct closed volume.
    Also, faces must be oriented consistently (e.g. CCW outside).
    This requirement is usually quite naturally satisfiable for creature
    models, and consistent ordering allows you to use backface culling which
    is a good thing on it's own.</p>

    <p>You can inspect whether your model is detected as a 2-manifold,
    run the game with <tt>--debug-log</tt> parameter (possibly redirecting
    stdout to a file then). Look there for lines that indicate loading
    of your creature, e.g. for <tt>Alien</tt> creature:</p>

<pre>
Animation info:                  Alien.Stand animation:    1 scenes *     4276 triangles
Animation info: Alien.Stand animation: 6414 manifold edges, 0 border edges
</pre>

    <p>... the 2nd line above tells you that Alien is a perfect manifold
    (0 border edges). So shadows will be fast.</p>

    <p>You can also run
    <tt>kambi_vrml_game_engine/3dmodels.gl/examples/shadow_volume_test/shadow_volume_test</tt>
    test program from my engine, setting your creature model as a shadow
    caster. This will tell you manifold / border edges count. Also you
    can display there your border edges (use the "View" menu), manifold silhouette
    edges are displayed yellow and border edges are blue.
    See <?php echo a_href_page("VRML engine documentation",'vrml_engine_doc'); ?>,
    chapter "Shadows" for example screenshots how this looks like.
    Useful if you want to see where the border edges are located, to fix them.</p>

    <p>In Blender, you can easily detect why the mesh is not
    manifold by <i>Select non-manifold</i> command (in edit mode).
    Also, remember that faces must be ordered consistently CCW
    &mdash; I think that in some cases <i>Recalculate normals outside</i>
    may be needed to reorded it properly.

    <p>This whole issue doesn't concern creatures with
    <tt>casts_shadow="False"</tt> in <tt>creatures/kinds.xml</tt> file.
    But remember that the default value of <tt>casts_shadow</tt> is
    <tt>"True"</tt> &mdash; to encourage you to make all creatures cast a shadow.
    Let's show people these beatiful shadows !

  <li><p>Notes for animations: beware. Animations are done with a method
    that essentially makes their rendering very fast (rendering each
    animation frame should be as fast as if it was a still scene).
    However it costs much in terms of memory consumption (and in
    "Loading creatures" time). Some features that should be avoided:

    <ul>
      <li>Different material for each vertex. This is taken from vertex painting
        in blender. I.e. you can leave vertex painting as generated (incorrectly)
        by blender, as PER_VERTEX_INDEXED. If you fix it to PER_VERTEX your
        models will take a lot of more memory space. E.g. for SpiderQueen
        this is 270 MB with vertex painting vs 80 MB without vertex painting
        (triangle count is 2715).
      </li>
    </ul>
  </li>
</ul>

<?php echo $toc->html_section(); ?>

<p>As a final note, our engine supports some nice graphic features, and you
may want to use them to make your models nicer. Some of the more important
eye-candy features are:</p>

<ul>
  <li><p><?php echo a_href_page_hashlink(
    'Write shaders in GLSL, and connect them to objects in your VRML models',
    'kambi_vrml_extensions', 'ext_shaders'); ?>.</p></li>

  <li><p><?php echo a_href_page_hashlink('Bump mapping',
    'kambi_vrml_extensions', 'ext_bump_mapping'); ?>.
    See "The Fountain" level for example.</p></li>

  <li><p>You can make your level geometry cast dynamic shadows (on everything,
    including itself) by setting
    <pre>  scene_dynamic_shadows="True"</pre>
    in <tt>levels.xml</tt> file. Note that this usually requires the level scene
    to be 2-manifold, just like creatures for shadows.
    See "The Fountain" level for example.</p></li>
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
<a href="http://www.pascalgamedevelopment.com/viewtopic.php?t=2564&amp;start=0">"Where
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
    is intended to be, just ask on <?php echo MAILING_LIST_LINK; ?>.
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
        of time that "feels good" with player speed and head bobbing.
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

<p><?php echo sf_download('Download game sources',
  'castle-' . VERSION_CASTLE . '-src.tar.gz'); ?>.
You will also need
<?php echo a_href_page('Kambi VRML game engine', 'kambi_vrml_game_engine'); ?>
 sources, unpack them in such way that <tt>kambi_vrml_game_engine/</tt>
and <tt>castle/</tt> directories are together within the same directory.

<p>Then compile inside <tt>castle/</tt> directory by simple
<pre>
  make build-unix
</pre>
under Unixes (Linux, FreeBSD, Mac OS X) or
<pre>
  make build-windows
</pre>
under Windows. Note that you must use GNU make.
Required compiler is
<a href="http://www.freepascal.org/">FreePascal</a>
(you can look at <?php echo a_href_page_hashlink(
  'notes about FPC version required', 'sources', 'section_fpc_ver'); ?>
 &mdash; usually this is "latest stable FPC version").
 There is no Delphi support &mdash; I live in the open source world.

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

<p>Before you run the compiled executable, make sure that it's properly
installed: Unix users should make sure that symlink
<tt>$HOME/.castle.data</tt> is appropriate (you can easily
make this symlink by <tt>make install</tt>).
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

<p>I feel obligated here to say what exactly this game is going to be.
So beware, I'm going to be dreaming in the next paragraph.

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
see <?php echo a_href_page('Kambi VRML game engine', 'kambi_vrml_game_engine'); ?>
 for exact listing (if you want to port
it to other system, you're welcome to provide the patches and compiled binaries).
And I'm trying to do all game data using open-source things,
most important here is <a href="http://www.blender3d.org/">Blender</a>
used to make all the models (see
<?php echo a_href_page('credits page', 'castle-credits'); ?> for
full list of things and resources used).

<?php echo $toc->html_section(); ?>

<p>Collisions:

<ul>
  <li><p>Everywhere in the game GetCameraHeight returns normal HeightAboveTheGround.
    Usually in my general units (kambi_vrml_game_engine units) I don't make
    any assumptions about HomeCameraUp, it can be any vector &mdash; which means
    that SqrHeightAboveTheGround is easier to calculate,
    and calculating actual HeightAboveTheGround costs us Sqrt call.
    But in this game, we know that HomeCameraUp is always (0, 0, 1),
    and this means that actual HeightAboveTheGround can be calculated
    easily (by TVRMLTriangleOctree.GetCameraHeightZ), without the cost
    of Sqrt.</p></li>

  <li><p>Creatures collisions are done sometimes by a Sphere and sometimes by
    the bounding Box.</p>

    <p>Sphere doesn't change. I.e. throughout entire creature life, it's
    bounding sphere has the same radius.</p>

    <p>Box does change. It always corresponds to the bounding box of the actual
    animation frame, so it closely represents current creature state.
    E.g. dead humanoid creature will have low and wide box, while alive
    will have tall and thin box.</p>

    <p>This means that sometimes Box is better, sometimes the Sphere.</p>

    <p>Sphere advantages:</p>

    <ol>
      <li><p>You cannot guarantee that creature Box will never collide
        with the level/player/other creatures etc. &mdash; because each
        animation frame change changes also the Box, so the creature's Box
        may suddenly enter collision with something just because the time
        changed and next animation frame has larger BoundingBox.
        I cannot just stop animation in the middle (preventing such collision),
        because I don't know what to do with such animation then...
        So I just allow such collisions. Code that uses creature collision
        by bounding box has to be prepared for this possibility.</p>

        <p>On the other hand, I can guarantee that creature's Sphere never
        enters colliding state. That's easy, since creature's Sphere moves
        only when creature Position changes.
        So I can prevent creature move before it enters colliding state.
        (well, actually bounding sphere center (MiddlePosition)
        also changes with time, as usually it's calculated
        looking at current bounding box... But this is not a problem as long
        as sphere radius is chosen to be small enough.)</p>

      <li><p>Also, for non-flying creatures, the Sphere is placed above the ground,
        and this allows creature to climb the stairs &mdash; just like the player.
        Creature just walks into the stairs, and then "growing up" mechanism
        (using GetCameraHeight) pushes them up. This creates a short time
        when creature's lower parts collide with the stairs, but this is
        almost unnoticeable for the player, since "growing up" works fast.</p></li>
    </ol>

    <p>Sphere disadvantage:</p>

    <ol>
      <li><p>Well, Sphere is far from perfect as bounding volume &mdash; it's too small,
        sometimes also too large, sometimes both at the same time...</p>

        <p>Since the Sphere radius remains always the same, it must be good
        for all creature animation frames. This problem is somewhat cured
        by new (as of this writing, 2007-03) UseBoundingSphere feature,
        that allows you to use bounding box when you know that you will
        not need Sphere advantages anymore. E.g. when creature is dead,
        we don't need the advantages 1. and 2. above --- dead creature
        may be stuck in a wall, and it doesn't have to climb stairs.
        This means that what happens during the creature's Dying animation
        doesn't affect CameraRadius.</p>

        <p>But still it may be a problem sometimes, if some creature states
        have entirely different animations and bounding boxes. Then you
        will be forced to choose one universal CameraRadius for all creature
        states.</p>

        <ul>
          <li>Obviously you can't set radius too small, because if it's much smaller
            than actual creature's geometry then the creature will noticeably collide
            with level geometry and other creatures.</li>

          <li>On the other hand, you can't set radius too large
            (or move creature's MiddlePosition, that decides where's the Sphere center,
            much lower). This would block stair climbing.</li>
        </ul>
      </li>
    </ol>
  </li>
</ul>

<?php
  if (!IS_GEN_LOCAL) {
    $counter = php_counter("castle-development", TRUE);
  };

  common_footer();
?>
