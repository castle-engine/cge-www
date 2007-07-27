<?php
  require_once 'vrmlengine_functions.php';

  camelot_header("glplotter", LANG_PL,
    "glplotter - program w OpenGLu do rysowania ró¿norakich figur, wykresów itp. " .
    "na dwuwymiarowej siatce.");
?>

<?php
  echo pretty_heading("glplotter", VERSION_GLPLOTTER);
  echo '<table align="right">' .
    '<tr><td>' . medium_image_progs_demo("glplotter_screen_demo_1.png", "glplotter", false) .
    '<tr><td>' . medium_image_progs_demo("glplotter_screen_demo_2.png", "glplotter", false) .
    '<tr><td>' . medium_image_progs_demo("glplotter_screen_demo_3.png", "glplotter", false) .
    '</table>';
?>

<p>glplotter to program rysuj±cy ró¿ne figury, wykresy funkcji, zbiory kresek itp.
na dwuwymiarowym uk³adzie wspó³rzêdnych. Uruchamiaj±c program podajesz mu jako
parametry nazwy plików z których ma odczytaæ wszystkie wykresy do
narysowania, np.
<pre>
  glplotter plik.plot
</pre>

<p>£±cz±c glplotter z programami które mog± automatycznie generowaæ ró¿ne
pliki z wykresami (jak <?php echo a_href_page("gen_funkcja", "gen_funkcja"); ?>)
mo¿na u¿ywaæ
glplottera jako programu do ogl±dania np. wykresów funkcji. Polecenie
<pre>
  gen_funkcja "sin(x)" -10 10 0.1 | glplotter -
</pre>
wy¶wietli wykres funkcji sinus na odcinku [-10; 10] a polecenia
<pre>
  gen_funkcja "sin(x)" -10 10 0.1 > plot.1
  gen_funkcja "x^2" -10 10 0.1 > plot.2
  glplotter plot.1 plot.2
</pre>
wy¶wietl± wykresy funkcji sinus i x<sup>2</sup> na jednym obrazku.

<p>Oto program. ¯adna instalacja nie jest potrzebna, po prostu rozpakuj archiwum
w jakim¶ katalogu i stamt±d uruchamiaj <tt>glplotter</tt>.
<?php echo_standard_program_download('glplotter', 'glplotter',
  VERSION_GLPLOTTER, true); ?>

<p><?php echo SOURCES_OF_THIS_PROG_ARE_AVAIL; ?>

<p>Dokumentacja:
<ol>
  <li><a href="#section_params">Parametry</a>
  <li><a href="#section_controls">Obs³uga</a>
  <li><a href="#section_plot_format">Format plików z wykresami</a>
  <li><a href="#section_depends">Wymagania</a>
</ol>

<h3><a name="section_params">Parametry</a></h3>

<p>Uruchom jako
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

<h3><a name="section_controls">Obs³uga</a></h3>

<p>Do dyspozycji masz ró¿ne polecenia w menu, przegl±daj±c menu
mo¿esz te¿ dowiedzieæ siê jakie skróty klawiszowe s± przypisane do
odpowiednich poleceñ. Nie bêdê tu wymienia³ wszystkich
dostêpnych poleceñ, po prostu
uruchom program i pobaw siê nimi - ich znaczenie powinno byæ
zazwyczaj jasne.

<p>Klawisze jakie nie s± obecne jako polecenia w menu:
<ul>
  <li>strza³ki : przesuwanie wykresu
  <li>+/- : skalowanie
    <!-- (z nieruchomym miejscem pod celownikiem, tzn. podczas skalowania
         punkt na ktory wskazuje "celownik" pozostaje ten sam) -->
  <li>PgUp / PgDown : obracaj wykres <!-- wzgledem srodka okienka -->
</ul>

<p>Trzymanie CTRL podczas przyciskania tych klawiszy spowoduje ¿e wykres
bêdzie 10 razy szybciej przesuwany / skalowany / obracany.
Trzymanie SHIFT oznacza "100 razy szybciej".
Konsekwentnie, trzymaj jednocze¶nie CTRL i SHIFT aby klawisze
dzia³a³y 1000 razy szybciej.

<p>Klawisz F10 (i odpowiednie polecenie menu) zapamiêtuj± aktualny
obraz wykresu do pliku o nazwie <tt>glplotter_screen_%d.png</tt>
w aktualnym katalogu, gdzie <tt>%d</tt> bêdzie pierwsz± woln± liczb±.

<p>Mo¿na przesuwaæ wykres przeci±gaj±c go myszk±, tzn. przesuwaj±c myszkê
przy wci¶nietym lewym klawiszu.

<h3><a name="section_plot_format">Format plików z wykresami</a></h3>

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

<h3><a name="section_depends">Wymagania</a></h3>

<?php echo depends_ul( array(
  DEPENDS_OPENGL,
  DEPENDS_LIBPNG_AND_ZLIB,
  DEPENDS_UNIX_GLWINDOW_GTK_2,
  DEPENDS_MACOSX) );
?>

<?php
  if (!IS_GEN_LOCAL) {
    $counter = php_counter("glplotter", TRUE);
  };

  camelot_footer();
?>