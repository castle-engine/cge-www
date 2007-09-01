<?php
  require_once 'vrmlengine_functions.php';

  common_header("glplotter and gen_function", LANG_EN,
    'glplotter &mdash; program for plotting graphs in OpenGL, and ' .
    'gen_function &mdash; for generating function graphs');
?>

<h1>glplotter
  <span style="font-size: small">(version <?php echo VERSION_GLPLOTTER; ?>)</span><br/>
and gen_function
  <span style="font-size: small">(version <?php echo VERSION_GEN_FUNCTION; ?>)</span></h1>

<?php
  echo '<table align="right">' .
    '<tr><td>' . medium_image_progs_demo("glplotter_screen_demo_1.png", "glplotter", false) .
    '<tr><td>' . medium_image_progs_demo("glplotter_screen_demo_2.png", "glplotter", false) .
    '<tr><td>' . medium_image_progs_demo("glplotter_screen_demo_3.png", "glplotter", false) .
    '</table>';
?>

<p><i>glplotter</i> draws graphs. It can generate graphs
of various functions (you can specify any mathematical expression as function
expression, e.g. "<tt>sin(x) + x^2</tt>").</p>

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
      new TocItem('Command-line parameters (graphs)', 'command_line_graphs'),
      new TocItem('Command-line parameters (others)', 'command_line_others'),
      new TocItem('Graph file format', 'graph_file_format'),
      new TocItem('Requirements', 'depends'),
      new TocItem('gen_function', 'gen_function'),
      new TocItem('Syntax of mathematical expressions', 'math_expr_syntax'),
    )
  );
?>

<?php echo $toc->html_toc(); ?>

<?php echo $toc->html_section(); ?>

<p>Here are the binaries of the program. No special installation
is required, just unpack these archives and run the program.
<?php echo_standard_program_download('glplotter', 'glplotter',
  VERSION_GLPLOTTER, true); ?>

<p><?php echo SOURCES_OF_THIS_PROG_ARE_AVAIL; ?>

<?php echo $toc->html_section(); ?>

<p>Uruchamiaj±c program you can pass as parameters file names
from which to load graphs, e.g.
<pre>
  glplotter file.plot
</pre>

<p>E.g. you can pipe the output of <tt>gen_function</tt> program
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

<?php echo $toc->html_section(); ?>

<p>Run as
<pre>
  glplotter [OPTION]... [FILE]...
</pre>

<p>Podaj dowolnie wiele nazw plików (nazwa pliku - (my¶lnik) oznacza standardowe
wej¶cie).

<p>Opcje kontroluj±ce jakie elementy wy¶wietlaæ:
<pre>
  --crosshair           --no-crosshair
  --point-coords        --no-point-coords
  --osie-xy             --no-osie-xy
  --map                 --no-map
  --grid-1              --no-grid-1
  --podzialka-1         --no-podzialka-1
  --liczby-1            --no-liczby-1
  --grid-pi             --no-grid-pi
  --podzialka-pi        --no-podzialka-pi
  --liczby-pi           --no-liczby-pi
  --grid-custom         --no-grid-custom
  --podzialka-custom    --no-podzialka-custom
  --liczby-custom       --no-liczby-custom
  --only-points         --no-only-points
</pre>

<p>Opcje <tt>--light</tt> i <tt>--dark</tt> okre¶laj± jasny lub ciemny schemat
kolorów.

<p>Opcja <tt>--custom-size SIZE</tt> (lub <tt>-c SIZE</tt>) podaje rozmiar dla
<ul>
  <li>siatki wy¶wietlanej po podaniu <tt>--grid-custom</tt>
    albo przyci¶niêciu Ctrl + G
  <li>podzia³ki wy¶wietlanej po podaniu <tt>--podzialka-custom</tt>
    albo przyci¶niêciu Ctrl + P
  <li>podzia³ki liczbowej wy¶wietlanej po podaniu <tt>--liczby-custom</tt>
    albo przyci¶niêciu Ctrl + L
</ul>

<p>Patrz tak¿e <?php echo a_href_page(
"standardowe parametry moich programów w OpenGL'u", "opengl_options") ?> i
<?php echo a_href_page(
"ogólne uwagi o parametrach dla moich programów", "common_options") ?>.

<?php echo $toc->html_section(); ?>

To co nazywam tu "wykresem" to po prostu zupe³nie swobodny zbiór odcinków.
Odcinki te nie musz± prezentowaæ wykresu jakiej¶ funkcji - mog± byæ dowolnie
po³o¿one wzglêdem siebie, dowolnie siê przecinaæ itp.

<p>Format pliku wykresu: ka¿da linia to
<ul>
  <li>Komentarz, gdy linia zaczyna siê znakiem <tt>#</tt> (hash).
  <li>Kolejny punkt na linii wykresu,
    gdy linia to dwie liczby rzeczywiste oddzielone bia³ymi znakami.
    Liczba rzeczywista mo¿e byæ zapisana w postaci dziesiêtnej lub wyk³adniczej,
    np. <tt>3.14</tt> lub <tt>10e-3</tt>.
  <li>Przerwa w linii wykresu,
    gdy linia zawiera tylko s³owo <tt>break</tt>.
  <li>Nazwa wykresu (u¿ywana do wy¶wietlania mapki w lewym-dolnym rogu okienka)
    w postaci <tt>name=&lt;nazwa_wykresu&gt;</tt>.
</ul>
Bia³e znaki na pocz±tku i na koñcu linii s± zawsze dozwolone i ignorowane.

<?php echo $toc->html_section(); ?>

<p>glplotter requires:

<?php echo depends_ul( array(
  DEPENDS_OPENGL,
  DEPENDS_LIBPNG_AND_ZLIB,
  DEPENDS_UNIX_GLWINDOW_GTK_2,
  DEPENDS_MACOSX) );
?>

<?php echo $toc->html_section(); ?>

<p><tt>gen_function</tt> generates graph file from given function expression.
It's seldom needed &mdash; glplotter can make a graph from function expression
on it's own, see menu <i>"Functions"</i>.

<p>Call like:
<pre>
  gen_function &lt;funkcja&gt; &lt;x1&gt; &lt;x2&gt; &lt;xstep&gt;
</pre>

<p>W odpowiedzi gen_function wypisze na standardowe wyj¶cie wykres
funkcji <tt>&lt;funkcja&gt;</tt> na przedziale <tt>[&lt;x1&gt; ; &lt;x2&gt;]</tt>
(ze wspó³rzêdn± x próbkowan± co <tt>&lt;xstep&gt;</tt>) w formacie zrozumia³ym dla
<?php echo a_href_page("glplottera", "glplotter"); ?>.

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

A nastêpne linie mo¿naby zapisaæ w bardziej czytelnej (ale równowa¿nej)
postaci jako:
<pre>
  0 0
  1 1
  2 4
  3 9
  4 16
  5 25
</pre>

Czyli lewa kolumna to po kolei liczby od 0 do 5 (co 1) a prawa kolumna
to warto¶ci funkcji <tt>x^2</tt> (czyli x<sup>2</sup>) gdzie x to warto¶æ
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

wy¶wietl± wiêc wykresik funkcji x<sup>2</sup> na przedziale <tt>[0;5]</tt>. */
?>

<p>Download gen_function:

<?php echo_standard_program_download('gen_function', 'gen_function',
  VERSION_GEN_FUNCTION, false); ?>

<p><?php echo SOURCES_OF_THIS_PROG_ARE_AVAIL; ?>

<?php echo $toc->html_section(); ?>

<p>Skrót specyfikacji: to jest normalny zapis wyra¿enia matematycznego.
Gdy mamy do czynienia z wyra¿eniem funkcji, zmienna <tt>x</tt> to argument
funkcji. Np. <tt>(x+4)*3+2</tt>, <tt>sin(x)</tt> itd.

<p><b>Czynnik</b> to
<ul>
  <li>Nazwa zmiennej (ci±g liter, podkre¶leñ i cyfr zaczynaj±cy siê liter±
    lub podkre¶leniem). W przypadku <tt>glplotter</tt> i <tt>gen_function</tt>
    dozwolona w tym momencie jest tylko zmienna <tt>x</tt>.
  <li>Sta³a: <tt>pi</tt>, <tt>enat</tt> lub liczba rzeczywista
    (np. <tt>3.14</tt>)
  <li>-czynnik (np. <tt>-x</tt>)
  <li>Wyra¿enie w nawiasach (np. <tt>(12+34)</tt>).
  <li>Wywo³anie funkcji, np. <tt>sin(x)</tt> lub <tt>power(3.0, x)</tt>.
    Znane funkcje to
    <ul>
      <li><tt>Sin</tt>, <tt>Cos</tt>, <tt>Tan</tt>, <tt>CoTan</tt>
      <li><tt>ArcSin</tt>, <tt>ArcCos</tt>, <tt>ArcTan</tt>, <tt>ArcCoTan</tt>
      <li><tt>SinH</tt>, <tt>CosH</tt>, <tt>TanH</tt>, <tt>CoTanH</tt>
      <li><tt>Log2</tt>, <tt>Ln</tt>, <tt>Log</tt>, <tt>Power2</tt>,
        <tt>Exp</tt>, <tt>Power</tt>, <tt>Sqr</tt>, <tt>Sqrt</tt><br>
        (<tt>Log2(x) = Log(2, x)</tt>,
         <tt>Power2(x) = Power(2, x) = 2^x</tt>,
         <tt>Exp(x) = Power(enat, x) = enat^x</tt>)
      <li><tt>Sgn</tt>, <tt>Abs</tt>, <tt>Ceil</tt>, <tt>Floor</tt>
      <li><tt>Greater</tt>, <tt>Lesser</tt>, <tt>GreaterEq</tt>,
        <tt>LesserEq</tt>, <tt>Equal</tt>, <tt>NotEqual</tt><br>
        (zwracaj± 0 (fa³sz) lub 1 (prawda))
      <li><tt>Or</tt>, <tt>And</tt>, <tt>Not</tt><br>
        (zwracaj± 0 (fa³sz) lub 1 (prawda), jako argumenty bior± dwie
        (<tt>Or</tt>, <tt>And</tt>) lub jedn± (<tt>Not</tt>) liczby i
        traktuj± 0 jako fa³sz i wszystko inne jako prawdê)
    </ul>
    <!--
    (sa to wszystkie nazwy funkcji poza tymi realizowanymi
    przez operatory 2-argumentowe +-*/ i 1-argumentowy -.
    Wszystkie one maja okreslona liczbe parametrow
    w odpowiednim FunctionKind[].argsCount.)
    -->
  <li>Porównanie w nawiasach klamrowych, tzn.
    <tt>[ wyra¿enie_1 operator wyra¿enie_2 ]</tt>,
    gdzie operator to <tt>&lt;, &gt;, &lt;=, &gt;=, = lub &lt;&gt;</tt>.
    Przyk³ad: <tt>[ x &gt; 3 ]</tt>. Warto¶ci± takiego czynnika jest 1
    gdy zale¿no¶c jest spe³niona lub 0 je¶li nie.
    To jest uproszczona postaæ konwencji Iversona.
    <!--
    Jest to uproszczona
    postac konwencji zapisu Kennetha E. Iversona (ktora zobaczylem
    w "Matematyce konkretnej" Grahama, Knutha i Patashnika).
    Notka - aby robic operacje w rodzaju not, or czy and uzywaj
    odpowiednich funkcji operujacych na liczbach.
    Powyzsza notacja tez jest zreszta tylko bardzo wygodnym skrotem
    dla odpowiednich funkcji Greater, Lesser, GreaterEq itd. )
    -->
</ul>

<p><b>Dwuargumentowe operatory <tt>/, *, ^, %</tt></b> wykonuj± odpowiednio
dzielenie, mno¿enie, potêgowanie i zwracaj± resztê z dzielenia (modulo).
Wszystkie maj± ten sam priorytet i ³±cz± w lewo.
Modulo jest liczone jako <tt>x % y = x - Floor(x/y) * y</tt>.
W wyra¿eniu <tt>x^y</tt> je¿eli y nie jest liczb± ca³kowit± to x musi byæ
&gt;=0.

<!--
(wszystko jest na wartosciach rzeczywistych;
pamietaj tez ze operator potegowania ma taki sam priorytet jak
np. mnozenie a operatory o rownym priorytecie sa obliczane od
lewej do prawej wiec np. 2*4^2 = 8^2, nie 2*16))
-->

<p><b>Dwuargumentowe operatory <tt>+ i -</tt></b> wykonuj± dodawanie i
odejmowanie. One te¿ ³±cz± w lewo. Maj± s³abszy priorytet od
operatorów multiplikatywnych powy¿ej, wiêc np.
<tt>12 + 3 * 4</tt> daje 24.

<p>Du¿e / ma³e litery w nazwach funkcji, sta³ych i zmiennych nie s±
rozró¿niane.

<p>Przyk³ady:
<pre>
  sin(x) ^ 10
  2 * (cos(ln(x)) - 1)
  [sin(x) > cos(x)]
  or( [sin(x) > cos(x)], [sin(x) > 0] )
</pre>

<!--
- wylaczyc wykres 2, i 3
- podstawowe klawisze : +, - (skalowanie), strzalki (przesuwanie)
- wylaczyc siatke co 1, wlaczyc siatke + liczby co Pi
- potem wlaczyc 2, albo 3 i zaobserwowac ze wszystko sie zgadza -
  uzyc klawisza q aby zobaczyc Prawde
- reszta klawiszy : F10 (savescreen),
  pomocne zabaweczki - celownik, pktcoords, grid/ podzialka/ liczby custom,
    obracanie
- esc (wyjscie)

do tutoriala  notki o sk³adni wyra¿en. Notka o u¿yteczno¶ci notacji
  Iversona i ogólniej f-cji boolowskich np. maj±c dan± funkcjê
  f(x) i chc±c by by³a okre¶lona tylko gdy dane wyra¿enie W boolowskie
  (a wiec 1 = true, 0 = false) bylo true wystarczy zrobic
  g(x) = f(x) / W (gdy W = 0 czyni funkcje nieokreslona,
  gdy W = 1 czyni g(x) = f(x))
-->

<?php
  if (!IS_GEN_LOCAL) {
    $counter = php_counter("glplotter_and_gen_function", TRUE);
  };

  common_footer();
?>