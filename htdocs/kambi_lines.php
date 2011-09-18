<?php
  require_once 'castle_engine_functions.php';

  castle_engine_header("kambi_lines",
    "kambi_lines - small game in OpenGL based on old DOS 'Color Lines'.",
    array('all_programs'));
?>

<?php
  echo pretty_heading("kambi_lines", VERSION_KAMBI_LINES);
  echo default_program_thumbnail("kambi_lines");

  $toc = new TableOfContents(
    array(
      new TocItem('Download', 'download'),
      new TocItem('Game rules', 'rules'),
      new TocItem('Some detailed game rules', 'detailed_rules'),
      new TocItem('Source code', 'source'),
    )
  );
  $toc->echo_numbers = true;
?>

<p>This is my version of an old game known as "Color Lines".

<?php echo $toc->html_toc(); ?>

<?php echo $toc->html_section(); ?>

<?php echo_standard_program_download(
  'kambi_lines', 'kambi_lines', VERSION_KAMBI_LINES,
  $std_releases_post_1_8_0); ?>

<p><i>Installation</i>: just extract the downloaded archive anywhere.
Run the game binary (<tt>./kambi_lines</tt> on Unix,
<tt>kambi_lines.exe</tt> on Windows).</p>

<p>On Unix, if you don't want to always run the binary from the game's
directory, you can extract the game (or make a symlink to extracted dir)
to <tt>$HOME/.kambi_lines.data/</tt>, or <tt>/usr/local/share/kambi_lines/</tt>,
or <tt>/usr/share/kambi_lines/</tt>. You can then
move or symlink the binary <tt>kambi_lines</tt> to any place
you like (e.g. <tt>$HOME/bin</tt> or <tt>/usr/local/bin</tt>).</p>

<p>You can run the program with <tt>--fullscreen</tt> command-line option,
the game will then change screen resolution to 640x480 and run in fullscreen
mode (this is what original DOS "Color Lines" were doing).

<?php echo $toc->html_section(); ?>

<p>The rules are simple:
<ol>
  <li>You play on a square board, with 9 x 9 fields.
    Every field is either empty or one colored ball is there.

  <li>In each your move, you choose one ball and move it
    from it's current place to some other place on the board.
    There must exist some clear way (not blocked by other balls)
    between the <i>current place</i> to <i>some other place</i> of the ball.
    After your move, new 3 balls will appear in random places
    on the board.

  <li>When you arrange 5 or more balls of the same color
    side-by-side in one row, column or diagonal, then the balls
    disappear, you get some points, and you get free move
    (i.e. no new balls will appear on the board after such
    "winning move").
</ol>

<p>So basically the board gets cluttered with balls more and more,
and you have to struggle to keep it clean. Your goal is to collect
as many points as you can, until the board is whole filled with
balls and then the game ends.

<p>In my version of this game, there are some addons: there
are two-colored balls and joker balls (can play a role of any
color), and you get bonus points for making one "winning move"
right after another.

<?php echo $toc->html_section(); ?>

<dl>
  <dt>Bonus for making one "winning move" right after another</dt>
  <dd>If you received some points in previous move
    (because you arranged some "matching line") then
    you will get <i>doubled</i> amount of points for arranging
    a matching line in this move. If you will manage
    to get these points, then in the next move
    you will get <i>tripled</i> amount of points for arranging
    a matching line. And so on, as long as in each successive
    move you will manage to arrange some matching line.
    In lower-left corner you will see the text "ACTIVE BONUS: x ..."
    that shows how the points for the current move will be multipled.</dd>

  <dt>Special balls</dt>
  <dd>If the "Special balls" mode is on, then you can get on the board
    two-colored balls (red-white
    <img src="images/kambi_lines/ball_red_white_1.png" alt="Red-white ball">
    and blue-yellow
    <img src="images/kambi_lines/ball_blue_yellow_1.png" alt="Blue-yellow ball">)
    and joker balls
    (<img src="images/kambi_lines/ball_joker_1.png" alt="Joker ball">).
    Two-colored balls can be used as a substitute of any of given two colors.
    Joker ball can be used as a substitute of any color.

    <p>Notes:
    <ol>
      <li>Still every matching line must have some concrete color.
        E.g. you can't use red-white ball to connect 2 white balls,
        1 red-white ball and 2 red balls into one matching line.
        On the other hand, these six balls...

        <p><img src="images/kambi_lines/red_white_combo.png"
          alt="Six balls: 1 x white, 4 x red-white ball, 1 x red ball">

        <p>... create a matching line. There is one matching line of white
        color, and one matching line of red color. You will get 12 points
        (6 balls * 2) for arranging such line on the board.

      <li>In each matching line there must be at least one
        normal (one-colored) ball. So e.g. you can't make a matching line
        using 5 joker balls.
    </ol>
  </dd>
</dl>

<?php echo $toc->html_section(); ?>

<p><?php echo SOURCES_OF_THIS_PROG_ARE_AVAIL; ?>

<p>Concept is closely based on original DOS "Color lines" game
by Olga Demin (programming), Igor Ivkin and Gennady Denisov (graphics).
Also most of graphics are copied from that game.
I used them without any permission, so, essentially, they are stolen,
but this game is "abandonware" since very long time, so I guess noone cares.
Anyway, credit for graphics goes to Igor Ivkin and Gennady Denisov,
who made graphics for original DOS "Color lines".

<p>The graphics for the default set of 7 balls are modified version
of "KDE Lines" graphics.

<p>Splash screen was made by
<?php echo a_href_page('rayhunter', 'rayhunter'); ?>.

<?php /* Too small:
  echo depends_par(array(
    DEPENDS_OPENGL,
    DEPENDS_LIBPNG_AND_ZLIB,
    DEPENDS_MACOSX)); */ ?>

<?php
  castle_engine_footer();
?>
