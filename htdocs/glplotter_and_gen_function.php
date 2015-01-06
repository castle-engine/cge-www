<?php
  require_once 'castle_engine_functions.php';

  castle_header("glplotter and gen_function",
    'glplotter &mdash; program for plotting graphs in OpenGL, and ' .
    'gen_function &mdash; for generating function graphs',
    array('all_programs'));
?>

<h1>glplotter
  <span style="font-size: small">(version <?php echo VERSION_GLPLOTTER; ?>)</span><br/>
and gen_function
  <span style="font-size: small">(version <?php echo VERSION_GEN_FUNCTION; ?>)</span></h1>

<?php
  echo castle_thumbs(array(
    array('filename' => 'glplotter_screen_demo_1.png', 'titlealt' => 'Screenshot from &quot;glplotter&quot;'),
    array('filename' => 'glplotter_screen_demo_2.png', 'titlealt' => 'Screenshot from &quot;glplotter&quot;'),
    array('filename' => 'glplotter_screen_demo_3.png', 'titlealt' => 'Screenshot from &quot;glplotter&quot;'),
  ));
?>

<p><i>glplotter</i> draws graphs. It can generate graphs
of various functions (you can specify any mathematical expression as function
expression, e.g. "<code>sin(x) + x^2</code>").</p>

<p><i>glplotter</i> can also load a graph from a file. This is useful
if your graph data doesn't come from any mathematical expression &mdash;
e.g. maybe it's some collected statistical data.
Maybe your data is not even a function at all &mdash;
actually any shape consisting of line segments may be displayed by glplotter.</p>

<p>That's pretty much everything you need to know... you can download and
run glplotter now, the interface should be self-explanatory.<!-- Most of
the documentation below deals with some advanced usage cases, probably not
interesting for most people (although you may be interested in the list
of keys).--></p>

<?php
  $toc = new TableOfContents(
    array(
      new TocItem('Download', 'download'),
      new TocItem('Command-line parameters', 'command_line'),
      new TocItem('Graph file format', 'graph_file_format'),
      new TocItem('Requirements', 'depends'),
      new TocItem('gen_function', 'gen_function'),
      new TocItem('Syntax of mathematical expressions', 'math_expr_syntax'),
    )
  );
?>

<?php echo $toc->html_toc(); ?>

<?php echo $toc->html_section(); ?>

<?php echo_standard_program_download('glplotter', 'glplotter',
  VERSION_GLPLOTTER); ?>

<p><?php echo S_INSTALLATION_INSTRUCTIONS_SHORT; ?></p>
<p><?php echo SOURCES_OF_THIS_PROG_ARE_AVAIL; ?></p>

<?php echo $toc->html_section(); ?>

<p>You can pass at command-line file names from which to load graphs.
Dash (<code>-</code>) as filename means "standard input".

<p>E.g. you could pipe the output of <code>gen_function</code> program
to glplotter, like
<pre>
  gen_function "sin(x)" -10 10 0.1 | glplotter -
</pre>
which will display the graph of sinus for X in [-10; 10]. Commands like
<pre>
  gen_function "sin(x)" -10 10 0.1 > plot.1
  gen_function "x^2" -10 10 0.1 > plot.2
  glplotter plot.1 plot.2
</pre>
will display graphs of sinus and x<sup>2</sup> at once.
Of course, in case of function expressions, it's usually more comfortable
to set them up inside glplotter using <i>"Functions"</i> menu.

<p>Options <code>--light</code> and <code>--dark</code> allow you to choose
appropriate color scheme.

<p>Option <code>--custom-size SIZE</code> (or <code>-c SIZE</code>) specifies size for
<ul>
  <li>grid shown when <code>--grid-custom</code> was used (or Ctrl + G pressed)
  <li>numbers scale shown when <code>--num-scale-custom</code> was used
    (or Ctrl + S pressed)
  <li>numbers shown when <code>--numbers-custom</code> was used (or
    Ctrl + N pressed)
</ul>

<p>See also <?php echo a_href_page(
"standard parameters of OpenGL programs", "opengl_options") ?> and
<?php echo a_href_page(
"standard parameters of all programs", "common_options") ?>.

<?php echo $toc->html_section(); ?>

<p>Graph for glplotter is actually just a set of line segments.
They don't have to correspond to any function &mdash; they can show
any shape, they can cross each other etc.

<p>File format:
<ul>
  <li>Lines starting with <code>#</code> (hash) are comments.
  <li>Lines with two float numbers (separated by any whitespae)
    represent another point of the graph. You can use decimal or scientific
    float format (like <code>3.14</code> or <code>10e-3</code>).
    A line segment will be drawn from this point to the next one
    (unless a <code>break</code> will occur).
  <li>Line with only <code>break</code> word means a break in a line segment
    sequence.
  <li>Line like <code>name=&lt;graph_name&gt;</code> specifies graph name
    (will be used in glplotter legend).
</ul>
Whitespace at the beginning and end of the line is always ignored.

<?php echo $toc->html_section(); ?>

<p>glplotter requires:

<?php echo depends_ul( array(
  DEPENDS_OPENGL,
  DEPENDS_LIBPNG_AND_ZLIB,
  DEPENDS_UNIX_CASTLE_WINDOW_GTK_2,
  DEPENDS_MACOSX) );
?>

<?php echo $toc->html_section(); ?>

<p><code>gen_function</code> generates graph file from given function expression.
It's seldom needed &mdash; glplotter can make a graph from function expression
on it's own, see menu <i>"Functions"</i>.

<!--
<p>Compiled binary not redistributed now, there was not enough interest
in this program to recompile and test it for latest updates.
You have to get the source code and compile it yourself for now,
report if this is a problem.</p>
-->

<?php
echo_standard_program_download('gen_function', 'gen_function',
  VERSION_GEN_FUNCTION);
?>

<p>Call like:
<pre>
  gen_function &lt;function&gt; &lt;x1&gt; &lt;x2&gt; &lt;xstep&gt;
</pre>

<p>This will write on standard output the graph of function
<code>&lt;function&gt;</code> for x in range <code>[&lt;x1&gt; ; &lt;x2&gt;]</code>
(with x sampled at each <code>&lt;xstep&gt;</code> distance).
The graph will be in format understood by glplotter.

<p>For example

<pre>
  gen_function "x^2" 0 5 1
</pre>

will write

<pre>
# File generated by gen_function on 14-3-2004 at 23:34:37
#   function = x^2
#   x1 = 0
#   x2 = 5
#   xstep = 1
name=x^2
 0.0000000000000000E+0000  0.0000000000000000E+0000
 1.0000000000000000E+0000  1.0000000000000000E+0000
 2.0000000000000000E+0000  4.0000000000000000E+0000
 3.0000000000000000E+0000  9.0000000000000000E+0000
 4.0000000000000000E+0000  1.6000000000000000E+0001
 5.0000000000000000E+0000  2.5000000000000000E+0001
</pre>

<?php
  /* Removed docs: to verbose, not useful for most people...

A następne linie możnaby zapisać w bardziej czytelnej (ale równoważnej)
postaci jako:
<pre>
  0 0
  1 1
  2 4
  3 9
  4 16
  5 25
</pre>

Czyli lewa kolumna to po kolei numbers od 0 do 5 (co 1) a prawa kolumna
to wartości funkcji <code>x^2</code> (czyli x<sup>2</sup>) gdzie x to wartość
w lewej kolumnie.

<p>Polecenia
<pre>
  gen_function "x^2" 0 5 1 > plik.plot
  glplotter plik.plot
</pre>

albo, krócej,

<pre>
  gen_function "x^2" 0 5 1 | glplotter -
</pre>

wyświetlą więc wykresik funkcji x<sup>2</sup> na przedziale <code>[0;5]</code>. */
?>

<p><?php echo S_INSTALLATION_INSTRUCTIONS_SHORT; ?></p>
<p><?php echo SOURCES_OF_THIS_PROG_ARE_AVAIL; ?></p>

<?php echo $toc->html_section(); ?>

<p>Short overview of mathematical expressions syntax: this is really
just normal syntax of mathematical expressions, as used in all software
and resembling normal mathematical notation.
When we deal with function expressions, then <code>x</code> represents the argument,
e.g. <code>(x+4)*3+2</code>, <code>sin(x)</code> etc.

<p>For detailed information about syntax and built-in functions,
see <?php echo a_href_page('CastleScript language reference', 'castle_script'); ?>.
We use a subset of CastleScript syntax, allowing only a simple expression
as a function expression, operating on argument "X".

<!--
- wylaczyc wykres 2, i 3
- podstawowe klawisze : +, - (skalowanie), strzalki (przesuwanie)
- wylaczyc siatke co 1, wlaczyc siatke + numbers co Pi
- potem wlaczyc 2, albo 3 i zaobserwowac ze wszystko sie zgadza -
  uzyc klawisza q aby zobaczyc Prawde
- reszta klawiszy : F10 (savescreen),
  pomocne zabaweczki - celownik, pktcoords, grid/ num-scale/ numbers custom,
    obracanie
- esc (wyjscie)

do tutoriala  notki o składni wyrażen. Notka o użyteczności notacji
  Iversona i ogólniej f-cji boolowskich np. mając daną funkcję
  f(x) i chcąc by była określona tylko gdy dane wyrażenie W boolowskie
  (a wiec 1 = true, 0 = false) bylo true wystarczy zrobic
  g(x) = f(x) / W (gdy W = 0 czyni funkcje nieokreslona,
  gdy W = 1 czyni g(x) = f(x))
-->

<?php
  castle_footer();
?>
