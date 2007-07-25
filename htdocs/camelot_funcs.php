<?php /* -*- mode: php -*- (This page should be edited in php mode,
                            mmm mode is too slowish for this page). */

  /* assert_options(ASSERT_ACTIVE, 1); */

  require "kambi-php-lib/funcs.php";
  require "kambi-php-lib/kambi_toc.php";
  require "generated_versions.php";

  define('COUNTER_DATA_PATH', '/tmp/persistent/vrmlengine/counters/');
  require "kambi-php-lib/counter.php";

  /* Some rules to follow when editing these pages:

     - The basic idea is that these pages may be processed in two
       various modes:
       - When IS_GEN_LOCAL = false, we make normal online version
         on the page, visible online on URL defined by CURRENT_URL.
       - When IS_GEN_LOCAL = true, we make special version of the page
         suitable for offline viewing. It differs from the online
         version in many details.

     - Nigdy nie rób bezpo¶rednich linków do stron za pomoc± <a href ...>,
       zawsze u¿ywaj funkcji a_href_page[_hashlink]. Ona zajmuje siê
       kilkoma rzeczami, np. dba o to aby wszystko dzia³a³o dobrze
       dla ka¿dej warto¶ci IS_GEN_LOCAL (i dla dowolnej kombinacji plików
       dostêpnych lokalnie w/g funkcji is_page_available_locally),
       IS_GEN_PAGE_HREFS_TO_HTML, CURRENT_URL, ona te¿ zajmuje siê
       automatycznie doklejeniem odpowiedniego sufixu jêzyka do nazwy strony.

       Wyj±tkiem s± hash-linki w obrêbie tej samej strony, tzn. linki
       a postaci <a href="[tutaj hash]hash_link_na_tej_samej_stronie">...</a>,
       to jest dozwolone.

     - Podobnie, nie rób bezpo¶rednich linków do innych plików.
       Co wiêcej, nie u¿ywaj a_href_size z funcs.php.
       Zamiast tego u¿ywaj current_www_a_href_size, z tych samych powodów
       co wy¿ej a_href_page[_hashlink].

     - Na pocz±tku ka¿dej strony rób
         require "camelot_funcs.php";
         camelot_header(...odpowiednie parametry...);
       a na koñcu camelot_footer()

     Ponadto nastêpuj±ce rzeczy s± sugerowane aby uzyskaæ spójny styl:
     - Tabelki dostêpnych klawiszy i operacji myszk± robiæ jako
       <table border="1" class="key_list"> (jak zawrzeæ border="1"
       w CSS ? Tak ¿eby do table nie by³o potrzebnych ¿adnych innych parametrów
       poza class ?)

     - Wa¿niejsze strony o stosunkowo krótkim tytule mog± siê zaczynaæ
       u¿ywaj±c echo pretty_heading(... tytu³ strony ..., wersja programu),
       w szczególno¶ci mo¿e byæ
         echo pretty_heading($page_title);

     - Przy opisie instalacji moich gier pod UNIXy home directory u¿ytkownika
       bêdê oznacza³ jako "$HOME" (a nie np. "~"). Mimo ¿e algorytm wyznaczania
       katalogu domowego jest nieco bardziej z³o¿ony (KambiUtils.HomeDir)
       ni¿ tylko GetEnvironmentVariable('HOME'), to jednak zapis $HOME
       uwa¿am za lepszy od ~ bo jest d³u¿szy.
  */

  /* Constants always defined by this script (so they are accessible after
       including this script) :

     IS_GEN_LOCAL : bool = true means we're generating local version of the
       page to be stored as usual HTML, not as part of the online
       pages on CURRENT_URL.
       It is set here to true if you give --gen-local param to php,
       else it's false.

       What is "local" page ? A better name would be a "separate" page;
       such pages can be distribited separately, i.e. without attaching
       to them default index.html page, images\default_background etc.
       This allows me to distribute some pages as documentation for
       my programs (e.g. lets_take_a_walk contains such lets_take_a_walk.php
       generated with --gen-local) while not being forced to distribute
       with them _all_ documentation from my site
       (e.g. lets_take_a_walk contains lets_take_a_walk[.pl].php,
       common_options.php, opengl_options[.pl].php. It does not
       contain some pages that I do not consider "part of documentation for
       lets_take_a_walk usage" like index.php/html).
       Some steps (in this file (camelot_funcs) and in all specific php
       pages) must to be taken when generating "local" pages:
       - links to main page (index[.pl].(php|html)) (like those
         in $s_quick_links and <link rel=Start ...>) must not be
         generated
       - default page background (specified by calling camelot_header())
         may not contain any url to image
       - every local page will have in it's footer a text stating that
           o. I'm the author of this page,
           o. link to CURRENT_URL
         (I want to display this information to everyone that sees my
         pages/programs; when not generating local pages, this info
         is listed on index.php)

       - links to files on WWW should be always written
         using current_www_a_href_size. This way when we generate local page
         those links will always have absolute URL starting with CURRENT_URL.
         That's needed since we don't provide anything besides HTML pages
         in locally generated versions.

     IS_GEN_PAGE_HREFS_TO_HTML : bool = true means we're generating
       a_href_page with extension HTML instead of default PHP extension
       (it does NOT mean any other changes in pages; i.e. everything will
       look as usual (links to index.html, default background contains link to
       bg image etc.); it was useful once when on camelot.homedns.org
       php was not available - I was then able to generate all pages locally
       with href's to htmls and everything (well, with the exception of
       counters in counter.php) worked as expected; now it is still useful
       since it is required by IS_GEN_LOCAL).

       Always true if IS_GEN_LOCAL, use command-line param --gen-page-hrefs-to-html
       to force it to be true even if not IS_GEN_LOCAL. If not IS_GEN_LOCAL
       and no command-line param "gen_page_hrefs_to_html" then it will be false.
  */

  /* Parsuj argv. ============================================================

     Dozwolone opcje:
     --gen-local : ustaw IS_GEN_LOCAL na true
     --gen-page-hrefs-to-html : ustaw IS_GEN_PAGE_HREFS_TO_HTML na true
     --locally-avail ARGS... :
       wszystkie parametry za --locally-avail zostan± potraktowane jako
       nazwy plików które deklarujemy jako dostêpne lokalnie w sensie funkcji
       is_file_available_locally.
     Wszystkie nieznane parametry spowoduj± exit() z odpowiednim komunikatem
     b³êdu.

     Tutaj ustawiamy sta³e IS_GEN_LOCAL, IS_GEN_PAGE_HREFS_TO_HTML
     oraz zmienn± $locally_available_files. Tutaj te¿ ustawiamy sobie
     prawid³owe current dir w przypadku IS_GEN_LOCAL=true.
  */

  /* Zmienna wewnêtrzna dla funkcji is_file_available_locally, ustawiana
     w kodzie poni¿ej. */
  $locally_available_files = array();

  for ($i=1; $i<$_SERVER['argc']; $i++)
  {
    if ($_SERVER['argv'][$i] == '--gen-local')
      define_if_needed('IS_GEN_LOCAL', true); else
    if ($_SERVER['argv'][$i] == '--gen-page-hrefs-to-html')
      define_if_needed('IS_GEN_PAGE_HREFS_TO_HTML', true); else
    if ($_SERVER['argv'][$i] == '--locally-avail')
    {
      $locally_available_files = array_slice($_SERVER['argv'], $i + 1);
      break;
    } else
      exit("Not recognized command-line parameter " . $_SERVER['argv'][$i]);
  }

  // testy: echo "Locally available are "; print_r($locally_available_files);

  /* Je¿eli sta³e nie zosta³y ustawione przed odpowiednie parametry
     to ustaw je teraz. */
  define_if_needed('IS_GEN_LOCAL', false);
  define_if_needed('IS_GEN_PAGE_HREFS_TO_HTML', IS_GEN_LOCAL);

  if (IS_GEN_LOCAL)
  {
    $dir = $_ENV['VRMLENGINE_LOCAL_PATH'];
    chdir($dir) or exit("Cannot change directory to \"$dir\"");
  }

/* ============================================================
   some functions related to IS_GEN_LOCAL */

/* Jako $file_name podaj nazwê pliku. Np. "view3dscene.php",
   "view3dscene.pl.php". Dozwolone jest poprzedzenie tego wzglêdnym katalogiem,
   jak np. "miscella/badBlaster.zip".

   Je¿eli IS_GEN_LOCAL i plik nie jest dostêpny lokalnie to zwróci false,
   wpp. zwróci true. Ujmuj±c to inaczej zwraca
   (!IS_GEN_LOCAL or plik jest dostêpny lokalnie).

   Np. gdy generujemy dokumentacjê lokaln± specjalnie do do³±czenia jej do
   programu rayhunter to mamy dostêpne pliki "rayhunter.html",
   "rayhunter.pl.html", "common_options.html"
   tzn. dla tych warto¶ci $file_name ta funkcja zwróci true;
   dla pozosta³ych stron, np. "view3dscene.html", ta funkcja
   zwróci false. Gdy generujemy strony nie-lokalnie (czyli IS_GEN_LOCAL=false)
   to zawsze zwraca true, bo wtedy zak³ada ¿e przecie¿ na serwerze CURRENT_URL
   s± dostêpne wszystkie pliki. */
function is_file_available_locally($file_name)
{
  global $locally_available_files;
  return (!IS_GEN_LOCAL || in_array($file_name, $locally_available_files));
}

/* Jak is_file_available_locally tyle ¿e tej funkcji mo¿esz u¿ywaæ tylko
   dla stron (tzn. HTML/PHP) i nie podajesz tutaj rozszerzenia strony
   (".html" lub ".php"). */
function is_page_available_locally($page_name)
{
  /* Mo¿emy tu zawsze doklejaæ suffix .html bo strony lokalne zawsze
     maj± rozszerzenie .html a dla stron nielokalnych funkcja
     is_file_available_locally dzia³a nie patrz±c w ogóle na $file_name. */
  return is_file_available_locally($page_name . '.html');
}

  /* Global consts ====================================================== */

  /* :string = I just wasn't sure how to say "here are binaries" in english */
  define('S_HERE_ARE_BINARIES', 'Here are the binaries. No special installation ' .
    'is required, just unpack these archives and run the program.');

  /* :string = URL do katalogu g³ownego tych wlasnie stron w sieci
     (zakonczony '/') */
  define('CURRENT_URL', 'http://vrmlengine.sourceforge.net/');
  /* Short server name, corresponding to CURRENT_URL,
     used only to show to humans (when making links
     from offline to online page). */
  define('CURRENT_URL_SHORT', 'vrmlengine.sf.net');

  /* sta³e do specyfikowania jêzyka, w zmiennej $page_lang i w parametrach
     wielu funkcji. Kiedy¶ u¿ywa³em tutaj stringów "en", "pl" - u¿ywanie
     sta³ych intów ma t± zaletê ¿e trudniej jest siê pomyliæ (w razie czego
     dostaniemy ewidentny b³±d - nieznany identyfikator) oraz ³atwiej
     przetwarzaæ (to tylko inty, nie jakie¶ stringi). */
  define('LANG_PL', 0);
  define('LANG_EN', 1);

  /* This is a language for filenames without language suffix.
     E.g. when this is LANG_EN, then page in Polish is 'view3dscene.pl.php',
     page in Greek is 'view3dscene.gr.php' but page in English is
     simply 'view3dscene.php' (NOT 'view3dscene.en.php').

     This is particularly important because 'index.php' is the default
     page loaded for CURRENT_URL,
     so LANG_WITHOUT_FILENAME_SUFFIX is the default language seen
     by everyone who visits my pages.

     This was Polish at first, now it's English. */
  define('LANG_WITHOUT_FILENAME_SUFFIX', LANG_EN);

  /* Stala tablica; przet³umacz moje sta³e LANG_xxx na lang u¿ywany w HTMLu
     (zreszt± to nie jest specyficzne dla HTMLa, to jaki¶ bardziej ogólny
     standard nazewnictwa jêzyków). */
  $lang_to_html_lang = array(LANG_PL => 'pl',
                             LANG_EN => 'en');

  /* in normal circumstances this should always be 'index' */
  define('MAIN_PAGE_BASENAME', 'index');

  /* Stala tablica; dla podanego basename (tzn. bez rozszerzenia (.php lub .html)
     i suffixu jezyka (np. ".pl")) strony podaj na jakie jezyki strona jest
     przetlumaczona. NIE bierze pod uwage IS_GEN_LOCAL, tzn. zaklada ze
     wszystko co w ogole jest -- jest dostepne.
     Kazdy element tej tablicy to tablica wartosci LANG_xxx.

     Taka globalna tablica jakie jezyki sa dostepne jest przydatna do
     a_href_page -- kiedy robimy link do jakiejs strony jest dobrze wiedziec
     jakie jezyki sa dostepne (i ew. wymusic link ze strony po angielsku do
     strony po polsku, bo docelowa strona w wersji angielskiej nie istnieje).

     This should be used only by supported_page_langs function.
     All other functions should use supported_page_langs.
     (I could make this var local to supported_page_langs,
     but it could cost some execution time). */
  $pages_langs = array(
    'gen_funkcja' => array(LANG_PL),
    'glplotter' => array(LANG_PL),
    'jamy_i_nory' => array(LANG_PL)
  );

  /* Global variables =======================================================

     ¯adna funkcja z tego pliku (poza camelot_header)
     nie mo¿e byæ wywo³ana przed zainicjowaniem
     wszystkich poni¿szych zmiennych (za wyj±tkiem kilku wyj±tków które bêd±
     mia³y to wyra¼nie stwierdzone w komentarzu; bêdzie wtedy wyra¼nie okre¶lone
     których zmiennych dana funkcja wymaga).

     Jest gwarantowane ¿e wywo³anie camelot_header zapewnia ¿e wszystkie te
     zmienne s± zainicjowane, wiêc po wywo³aniu camelot_header nie musisz siê
     ju¿ o nic martwiæ. */

  /* Poni¿sze zmienne s± inicjowane w camelot_header, wcze¶niej nale¿y je
     traktowaæ jako NIE zainicjowane (czyli nie nadaj±ce siê do u¿ycia). */
  $page_title = '';
  /* Jêzyk strony, po zainicjowaniu warto¶æ to jedna ze sta³ych LANG_. */
  $page_lang = -1;
  /* internal, used only in camelot_header/footer() */
  $s_quick_links = '';

  /* Poni¿sze zmienne s± zainicjowane na domy¶lne warto¶ci z chwil± w³±czenia
     tego pliku (tzn. po require "camelot_funcs.php";). Je¶li chcesz zmieniæ
     ich warto¶ci to musisz to zrobiæ rêcznie przed wywo³aniem jakiejkolwiek
     innej funkcji z tego pliku (tak¿e camelot_header). */
  $main_page = false;

/* functions ======================================================= */

/* Zwraca string = tagi htmla (block-level) ktore generuja ladny heading.
   Na pewno zdefiniuje min jeden element w stylu h1
   (wiec subsections na stronie powinienes tagowac jako h2).

   Kiedy¶ by³ tu heading z cieniem przy u¿yciu CSSa (ten sam napis
   wypisany dwa razy), ale usun±³em to -- to nie chcia³o wygl±daæ
   dobrze we wszystkich przegl±darkach przy dowolnych ustawieniach.

   You may supply $version_number, this is intended for pages
   that document functionality of some program. */
function pretty_heading($heading_text, $version_number = NULL)
{
  $result = "<h1>$heading_text";

  if (!is_null($version_number))
    $result .= " <span class=\"version_number\">(version $version_number)</span>";

  $result .= "</h1>";

  return $result;
}

/* jesli nie IS_GEN_LOCAL to nie rozni sie niczym od a_href_size
   z funcs.php. Wpp. poprzedza $f_name pelnym URL naszej
   strony (CURRENT_URL) i nie podaje size.

   (Kiedys przy IS_GEN_LOCAL podawalo size brany z lokalnej kopii tego pliku,
   $f_name; ale zmienilem to, teraz w ogole nie podaje size. To dlatego ze tego
   typu size moze szybko stac sie nieaktualny. W szczegolnosci, czesto
   wkladam strony wygenerowane z IS_GEN_LOCAL do srodka archiwum
   do ktorego jest odnosnik z tej samej strony (np. glviewimage.php
   zawiera odnosnik na glviewimage_linux.tar.gz a w srodku
   glviewimage_linux.tar.gz jest strona glviewimage.html ktora zawiera odnosnik
   do archiwum glviewimage_linux.tar.gz na WWW.).
   To powoduje ze w momencie generowania strony czesto nie jest jeszcze znany
   faktyczny rozmiar $f_name bo w momencie generowania strony z IS_GEN_LOCAL
   plik $f_name jest wlasnie w trakcie uaktualniania. Tego rodzaju rekurecyjna
   zaleznosc (strona wymaga archiwum ktore wymaga strony) sprawia ze w praktyce
   czesto musialbym wywolywac "update_archives" dwa razy pod rzad tylko po to
   aby za drugim razem dobrze zalapac rozmiary archiwum. A ciagle przeciez
   istnieje spora szansa ze rozmiary archiwow na moich stronach beda sie
   czesto zmieniac, wiec po co je umieszczac na stronie ?)

   Innymi slowy, $f_name powinien byc sciezka wzgledna do pliku na
   CURRENT_URL. Wtedy ta funkcja wygeneruje a_href_size lub,
   w przypadku IS_GEN_LOCAL, sensowny odpowiednik z linkiem http://...
*/
function current_www_a_href_size($link_title, $f_name)
{
  if (IS_GEN_LOCAL)
    $final_f_name = CURRENT_URL . $f_name; else
    $final_f_name = $f_name;

  return "<a href=\"$final_f_name\">$link_title"
    . (IS_GEN_LOCAL ? '': " (" . readable_file_size($f_name) . ")") . "</a>";
}

/* $image_name = nazwa obrazka istniej±ca w progs_demo/original_size/ i
   progs_demo/medium_size/.
   Zwróci odpowiednie tagi które zawieraj± <img...> do obrazka w medium_size
   i s± zawarte w <a href...> do obrazka w original_size.
   Jezeli $aligned to obrazek bêdzie dosuniêty do prawej, tzn. mieæ align="right".
   $prog_name zostanie u¿yte aby wygenerowaæ odpowiedni tekst dla atrybutu
   alt= obrazka, to bêdzie co¶ w rodzaju "Obrazek z &quot;$prog_name&quot;",
   odpowiednio przet³umaczone na $page_lang.

   Mo¿e zwróciæ '' je¶li obrazki nie s± dostêpne lokalnie. Mówi±c bardziej
   ogólnie, mo¿esz u¿ywaæ tej funkcji bez wzglêdu na warto¶æ IS_GEN_LOCAL -
   ona sobie poradzi tak czy tak. */
function medium_image_progs_demo($image_name, $prog_name, $aligned = true)
{
  $image_name_original_size = "images/progs_demo/original_size/$image_name";
  $image_name_medium_size   = "images/progs_demo/medium_size/$image_name";

  if (!is_file_available_locally($image_name_original_size) ||
      !is_file_available_locally($image_name_medium_size)) return '';

  global $page_lang;
  switch ($page_lang)
  {
    case LANG_PL: $alt = "Obrazek z &quot;$prog_name&quot;"; break;
    case LANG_EN: $alt = "Image from &quot;$prog_name&quot;"; break;
  }

  return
   "<a href=\"$image_name_original_size\">
      <img "
       . ($aligned ? "align=\"right\" " : "") .
       "src=\"$image_name_medium_size\" alt=\"$alt\">
    </a>";
}

/* To samo co medium_image_progs_demo tyle ¿e tutaj $image_name jest generowany
   automatycznie jako "$prog_name_screen_demo.png" */
function default_medium_image_progs_demo($prog_name)
{
  return medium_image_progs_demo("${prog_name}_screen_demo.png", $prog_name);
}

/* Returns $pages_langs[$page_basename] or just array(LANG_EN)
   if there is no such key $pages_langs.
   I.e. by default it's understood there page is available
   only in English. */
function supported_page_langs($page_basename)
{
  global $pages_langs;

  return (array_key_exists($page_basename, $pages_langs) ?
    $pages_langs[$page_basename] :
    array(LANG_EN) );
}

/* camelot header ============================================================ */

/* $meta_description :string/NULL = krótki opis strony,
   o ile nie bêdzie NULL bêdzie wypisany jako <meta name="Description" ...>

   $meta_keywords :string/NULL = dodatkowe keywords strony rozdzielone ",",
   o ile nie bêdzie NULL to bêdzie wypisany jako <meta name="Keywords" ...>.

   $bonus_header_tags = beda wypisane tuz przed </head>.
   Moga zawierac rozne rzeczy specyficzne dla strony,
   np. jej deklaracje <style>, <script> i inne (bede rozszerzal ta liste
   gdy znajde do tego powody).

   $a_page_lang musi byc elementem supported_page_langs($page_basename)
*/
function camelot_header($a_page_title, $a_page_lang,
  $meta_description = NULL, $meta_keywords = NULL, $bonus_header_tags = '')
{
  global $page_title, $page_lang, $s_quick_links, $main_page,
    $lang_to_html_lang, $this_page_name;

  $page_title = $a_page_title;
  $page_lang = $a_page_lang;

  /* calculate $this_page_name */
  /* Poprzez Apache'a (na moim Linuxie, moim Windowsie, i na camelot.homedns.org)
     dostajê dobre $_SERVER['PHP_SELF']. Uruchomiony z linii poleceñ (do --gen-local):
     pod Linuxem dostajê $_SERVER['PHP_SELF'], pod Windowsem nie (pod Windowsem
     dostajê $_SERVER['PHP_SELF'] ustawione na '' (ale ustawione, tzn. nie jest NULL).
     Wiêc pod Windowsem biorê je z $_SERVER['argv'][0]. */
  $this_page_name = $_SERVER['PHP_SELF'];
  if ($this_page_name == '')
    $this_page_name = $_SERVER['argv'][0];
  $this_page_name = basename($this_page_name);

  /* calculate $page_basename (requires $page_lang and $this_page_name) */
  $page_basename = $this_page_name;
  $page_basename = basename($page_basename, '.php');
  $page_basename = basename($page_basename, '.' . $lang_to_html_lang[$page_lang]);

  /* evaluate $supported_langs (requires $page_basename) */
  $supported_langs = supported_page_langs($page_basename);
  assert('in_array($a_page_lang, $supported_langs)');

  /* camelot specific functions =============================================

     Zdefiniowane w ¶rodku camelot_header ¿eby mog³y skorzystaæ z istnienia
     sta³ej PAGE_LANG jako domy¶lnej warto¶ci dla swoich parametrów. */

  define('PAGE_LANG', $page_lang);

  function page_url($page_name, $hash_link, $target_page_lang, &$url_comment)
  /* $page_name: string, nie zawiera extension, nie zawiera takze sufixu
     jezyka (".pl" lub innych, jesli kiedys je dodam).
     Czyli basename strony.

     $hash_link, je¿eli ró¿ny od '', to bêdzie dopisany (i poprzedzony hashem)
     do URLa strony.

     $target_page_lang: sta³a LANG_xxx, jêzyk docelowy strony.
     Mo¿esz tutaj podawaæ zmienn± globaln± $page_lang.
     Je¿eli dana strona w danym jêzyku nie istnieje (chwilowo sprawdzanie
     istnienia strony nie bierze pod uwage IS_GEN_LOCAL, ale powinno)
     to mo¿e wybraæ arbitralnie inny jêzyk. Chwilowo "zmiana jêzyka na inny"
     jest ³atwa do zdefiniowania bo mamy tylko dwa jêzyki (en i pl) wiêc
     je¶li jeden nie istnieje to bierzemy drugi. Zawsze przy takiej zmianie
     jêzyka (tzn. gdy dostajemy stronê w innym jêzyku ni¿ za¿±dany
     $target_page_lang)
     dostaniemy dodatkowo komentarz w url_comment (np. " (polish)" gdy
     ze strony w jêzyku LANG_EN za¿±dali¶my innej strony po angielsku
     ($target_page_lang = LANG_EN) ale dostali¶my stronê po polsku
     (np. dla 'glplottera' który nie ma wersji angielskiej)).

     Innymi s³owy, podaj±c $target_page_lang NIE my¶l o tym w jakim jêzyku
     dana strona jest dostêpna. My¶l o tym w jakim jêzyku CHCESZ dostaæ
     stronê (a wiêc zazwyczaj w takim samym jêzyku w jakim jest aktualna
     strona). Je¿eli strona w jêzyku jakiego chcesz jest niedostêpna to
     zostanie podana strona w jêzyku dostêpnym i odpowiednie ostrze¿enie
     dostaniesz w $url_comment. Ma to m.in. t± zaletê ¿e je¿eli kiedy¶ dodam
     np. stronê glplottera po angielsku to wystarczy ¿e uaktualniê tablicê
     $pages_langs i wszystkie linki do glplottera ze stron angielskich
     (które chwilowo s± linkami do strony glplottera po poslku) automatycznie
     zmieni± siê na linki do strony po angielsku.

     Ta funkcja zwraca url zadanej strony, czyli page_name z doklejonym suffixem
     jezyka i rozszerzeniem i ew. poprzedzone CURRENT_URL (je¶li strona
     generowana z IS_GEN_LOCAL odwo³uje siê do czego¶ niedostêpnego lokalnie).

     Pod $url_comment zwraca komentarz do URLa w postaci "" (pusty string) lub
     " <i>(komentarz)</i>" (zwróæ uwagê na spacjê na pocz±tku),
     np. " <i>(nie-lokalny link)</i>". Powiniene¶ gdzie¶ pokazaæ ten url_comment
     u¿ytkownikowi bo on zawiera generalnie wa¿ne informacje. */
  {
    global $page_lang, $lang_to_html_lang;

    /* init $url_comment to empty. Everywhere in this function we will
       append things to $url_comment using
         str_append_part_to1st($url_comment, ', ', $next_part) */
    $url_comment = '';

    /* optionally change $target_page_lang */
    if (!in_array($target_page_lang, supported_page_langs($page_name)))
    {
      /* tests:
      exit("No such lang: $page_name, " . $lang_to_html_lang[$target_page_lang]);
      */
      switch ($target_page_lang)
      {
        case LANG_PL:
          str_append_part_to1st($url_comment, ', ', 'po angielsku');
          $target_page_lang = LANG_EN;
          break;
        case LANG_EN:
          str_append_part_to1st($url_comment, ', ', 'in Polish');
          $target_page_lang = LANG_PL;
          break;
      }
    }

    if ($target_page_lang == LANG_WITHOUT_FILENAME_SUFFIX)
      $language_suffix = ''; else
      $language_suffix = '.' . $lang_to_html_lang[$target_page_lang];

    $result = $page_name . $language_suffix;

    if (!is_page_available_locally($result))
    {
      $result = CURRENT_URL . $result . '.php';
      switch ($page_lang)
      {
        case LANG_PL: str_append_part_to1st($url_comment, ', ',
          'odsy³acz do ' . CURRENT_URL_SHORT); break;
        case LANG_EN: str_append_part_to1st($url_comment, ', ',
          'link to ' . CURRENT_URL_SHORT); break;
      }
    } else
    {
      if (IS_GEN_PAGE_HREFS_TO_HTML)
        $result .= '.html'; else
        $result .= '.php';
    }

    if ($hash_link != '')
      $result .= '#' . $hash_link;

    if ($url_comment != '')
      $url_comment = " <i>($url_comment)</i>";

    return $result;
  }

  function a_href_page_core($link_title, $page_name, $hash_link,
    $target_page_lang)
  /* Internal, aby zapewniæ wspóln± implementacjê dla a_href_page i
     a_href_page_hashlink */
  {
    $page_url = page_url($page_name, $hash_link, $target_page_lang, $url_comment);
    return "<a href=\"$page_url\">$link_title$url_comment</a>";
  }

  function a_href_page($link_title, $page_name, $target_page_lang = PAGE_LANG)
  /* Zwraca href do tej strony, co¶ w rodzaju <a href=$page_url>$link_title</a>,
     gdzie $page_url = page_url($page_name, '', $target_page_lang). */
  {
    return a_href_page_core($link_title, $page_name, '', $target_page_lang);
  }

  function a_href_page_hashlink($link_title, $page_name, $hash_link,
    $target_page_lang = PAGE_LANG)
  /* Jak a_href_page ale page_url ma odpowiedni hash_link. */
  {
    return a_href_page_core($link_title, $page_name, $hash_link, $target_page_lang);
  }

  switch ($page_lang)
  {
    case LANG_PL:
      define('SOURCES_OF_THIS_PROG_ARE_AVAIL',
        '¬ród³a niniejszego programu s± dostepne ' .
        a_href_page('na tej stronie', 'sources') .
        ', je¶li jeste¶ zainteresowany.');
      break;
    case LANG_EN:
      define('SOURCES_OF_THIS_PROG_ARE_AVAIL',
        'Sources of this program are available from ' .
        a_href_page('this page', 'sources') .
        ', in case you\'re interested.');
      break;
  }

  /* DEPENDS_ consts and funcs  ===================================== */

  define('DEPENDS', 'Requirements');
  define('DEPENDS_OPENGL',
    '<a href="http://www.opengl.org/documentation/implementations/">OpenGL</a>
    <!--
    (on all modern OSes, OpenGL is probably already installed and working on
    your system) -->');
  define('DEPENDS_LIBPNG_AND_ZLIB',
    '<a href="http://www.libpng.org/">Libpng</a>,
     <a href="http://www.gzip.org/zlib/">Zlib</a>
     (under Windows appropriate DLL files are already included
     in program\'s archive, so you don\'t have to do anything)');
  define('SUGGESTS_OPENAL',
    a_href_page_hashlink('OpenAL', 'openal_notes', 'section_install') .
    ' is not strictly required, but is suggested
    (under Windows appropriate DLL files are already included
    in program\'s archive, so you don\'t have to do anything)');
  define('SUGGESTS_OPENAL_VORBISFILE',
    a_href_page_hashlink('OpenAL', 'openal_notes', 'section_install') .
    ' and <tt>vorbisfile</tt> library are not strictly required,
    but are suggested
    (under Windows appropriate DLL files are already included
    in program\'s archive, so you don\'t have to do anything)');
  define('DEPENDS_UNIX_GLWINDOW_GTK_1',
    'Under Unices (Linux, FreeBSD, Mac OS X):
    <a href="http://www.gtk.org/">GTK+</a> 1.x and gtkglarea');
  define('DEPENDS_UNIX_GLWINDOW_GTK_2',
    'Under Unix (Linux, FreeBSD, Mac OS X):
    <a href="http://www.gtk.org/">GTK+</a> >= 2.2 and
    <a href="http://gtkglext.sourceforge.net/">GtkGLExt</a> >= 1.0.6');
  define('DEPENDS_MACOSX',
    'Mac OS X users should look at the ' .
    a_href_page('list of dependencies on Mac OS X', 'macosx_requirements') );

  function depends_par($depends_array)
  {
    $result = '';
    foreach($depends_array as $item)
    {
      if ($result != '') $result .= ', ';
      $result .= $item;
    }

    $result = "<p><b>" . DEPENDS . ":</b> $result.";
    return $result;
  }

  function depends_ul($depends_array)
  {
    return array_to_ul($depends_array);
  }

  /* continuation of camelot_header ======================================== */

  /* evaluate $s_quick_links */
  $s_quick_links = '';

  $link_names = array(LANG_EN => 'English: this page in english',
                      LANG_PL => 'Polish: ta strona po polsku');
  foreach($supported_langs as $lang)
  {
    if ($lang != $page_lang)
      $s_quick_links = str_append_part($s_quick_links, ' | ',
        a_href_page($link_names[$lang], $page_basename, $lang));
  }

  if (!IS_GEN_LOCAL)
  {
    switch ($page_lang)
    {
      case LANG_PL: $SBackToMain = 'powrót do strony g³ównej'; break;
      case LANG_EN: $SBackToMain = 'back to main page'; break;
    }
    $s_quick_links = str_append_part($s_quick_links, ' | ', a_href_page(
      $SBackToMain, MAIN_PAGE_BASENAME));
  }

  if ($s_quick_links != '') $s_quick_links = '[' . $s_quick_links . ']';

?>
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN"
  "http://www.w3.org/TR/html4/loose.dtd">
<?php // I use HTML 4.01 with deprecated things ?>

<?php echo "<html lang=\"" .$lang_to_html_lang[$page_lang]. "\">\n"; ?>

<head>
<meta http-equiv="Content-Type" content="text/html; charset=ISO-8859-2">
<meta http-equiv="Content-Style-Type" content="text/css">
<meta name="Author" content="Michalis Kamburelis">

<?php
  if (!is_null($meta_keywords))
  { echo "<meta name=\"Keywords\" content=\"$meta_keywords\">\n"; }

  if (!is_null($meta_description))
  { echo "<meta name=\"Description\" content=\"$meta_description\">\n"; }

  $link_names = array(LANG_EN => 'This page in english',
                      LANG_PL => 'Ta strona po polsku');
  foreach($supported_langs as $lang)
  {
    if ($lang != $page_lang) {
      $page_url = page_url($page_basename, '', $lang, $url_comment);
      echo "<link rel=\"Alternate\"
                  type=\"text/html\"
                  href=\"$page_url\"
                  hreflang=\"" .$lang_to_html_lang[$lang]. "\"
                  lang=\"" .$lang_to_html_lang[$lang]. "\"
                  title=\"" .$link_names[$lang]. "$url_comment\">\n";
    }
  }

  if (! ($main_page || IS_GEN_LOCAL))
  {
    switch ($page_lang)
    {
      case LANG_PL: $main_page_title = 'Strona g³ówna'; break;
      case LANG_EN: $main_page_title = 'Main page'; break;
    }
    $page_url = page_url(MAIN_PAGE_BASENAME, '', $page_lang, $url_comment);
    echo "<link rel=\"Start\"
                type=\"text/html\"
                href=\"$page_url\"
                title=\"$main_page_title$url_comment\">\n";
  }

  switch ($page_lang)
  {
    case LANG_PL: echo "<meta name=\"Language\" content=\"Polish\">\n"; break;
    case LANG_EN: echo "<meta name=\"Language\" content=\"English\">\n"; break;
  }
?>

<title><?php echo $page_title ?></title>

<style type="text/css"><!--
  body { background: white; }
  table.key_list { width: 90%; }
  table.key_list th { background: #eeeee0; }
  table.key_list td { background: #ddddd0; }

  dt { font-weight: bold; }
  div.page_footer { }
  span.page_copyright { font-size: smaller }

  span.version_number { font-size: small }

  /* Adapted from my pasdoc.css */
  table.thin_borders { border-collapse: collapse; }
  table.thin_borders td { border: 1pt solid lightgray; padding: 0.3em; }
  table.thin_borders th { border: 1pt solid lightgray; padding: 0.3em; }

  dl.command_line_options_list > dt {
    font-family: monospace;
    font-weight: bold;
  }

  /* Leave default font-family for dt, and default font-weight.
     Use span with command_line_option class inside dt. */
  dl.command_line_options_list_custom > dt {
    font-weight: normal;
  }
  span.command_line_option {
    font-family: monospace;
    font-weight: bold;
  }

  div.quick_links_bottom_line { text-align: <?php
    echo (IS_GEN_LOCAL ? 'left' : 'center' ) ?>; }

  div.latest_update_description {
    margin-left: 3em;
    border: 1px solid gray;
    padding: 0.5em;
  }

  pre.vrml_code {
    border: 1px solid #aaaaaa;
    background-color: #f9f9f9;
    padding: 0.3em;
  }

  dl.vrml_ver_differences > dt {
    font-weight: normal;
    font-style: italic;
  }

  table.program_image_links {
    width: 100%;
  }

  td.program_image_link img { border: none; }

  td.program_image_link {
    background: #e8d59a;
    width: 50%;
    border: 1px solid black;
    text-align: center;
    padding: 0.5em;
  }

  .program_image_link_title { font-size: larger; }

  /* Style for note and note_title shamelessly stolen from CSS
     of the Subversion book. (from .sidebar and .sidebar .title) */
  .note
  {
      border-top: dotted 1px black;
      border-left: dotted 1px black;
      border-right: solid 2px black;
      border-bottom: solid 2px black;
      background: rgb(240,220,170);
      padding: 0 0.12in;
      margin: 0.5in;
  }

  .note .note_title {
    text-align: center;
    font-size: 125%;
  }
--></style>

<?php echo $bonus_header_tags; ?>
</head>
<body>

<?php
  if ( (!$main_page) && ($s_quick_links != '') ) { ?>
    <p align="right"><small> <?php echo $s_quick_links; ?> </small></p>
<?php };
}

/* camelot footer ============================================================ */

function camelot_footer()
{
  global $s_quick_links, $main_page;

  /* This is html text with copyright of these pages. */
  define('PAGE_COPYRIGHT',
  '<span class="page_copyright">Copyright Michalis Kamburelis.
This page is considered part of documentation of my programs,
and you are free to modify and further distribute it on terms of
<a href="http://www.gnu.org/licenses/gpl.html">GNU General Public License</a>.
</span>');

?>

<hr>

<div class="page_footer">

<?php
  if ( (!$main_page) && ($s_quick_links != '') ) { ?>
    <div class="quick_links_bottom_line"> <?php echo $s_quick_links; ?> </div>
<?php };

  if (IS_GEN_LOCAL) { ?>
    <address>
    By Michalis Kamburelis, as part of
    <?php echo "<a href=\"" . CURRENT_URL . "\">Kambi VRML game engine</a>"; ?>.
    </address>

    <p>
<?php
    echo PAGE_COPYRIGHT;
};

  if (! IS_GEN_LOCAL) { ?>
    <table>
      <tr><td>
      <a href="http://validator.w3.org/check/referer">
      <img border="0"
         src="images/valid-html401.png"
         alt="Valid HTML 4.01!" height="31" width="88"></a>
         <?php // style="padding-bottom: 3%" ?>
      <td>
      <?php echo PAGE_COPYRIGHT; ?>
    </table>

  <p>Services for the vrmlengine project provided by<br />
  <a href="http://sourceforge.net"><img src="http://sflogo.sourceforge.net/sflogo.php?group_id=200653&amp;type=3" width="125" height="37" border="0" alt="SourceForge.net Logo" /></a><br />
  See also <a href="http://sourceforge.net/projects/vrmlengine">vrmlengine
  project page on SourceForge</a>.</p>
<?php }; ?>

</div>

</body>
</html>

<?php
}

?>
