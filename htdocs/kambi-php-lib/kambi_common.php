<?php /* -*- mode: php -*- (This page should be edited in php mode,
                            mmm mode is too slowish for this page). */

/* Copyright 2001-2010 Michalis Kamburelis.

   This file is part of "Kambi PHP library".

   "Kambi PHP library" is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   "Kambi PHP library" is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with "Kambi PHP library"; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

   ============================================================

   This is a common functions library, shared by both
   http://vrmlengine.sourceforge.net/ and
   http://michalis.ii.uni.wroc.pl/~michalis/
   websites.

   Note that, contrary to other PHP files inside kambi-php-lib/
   directory, this file *does* make some assumptions about how your
   website looks and works like. In other words, this file is probably
   not directly usable for other projects. (Still, feel free
   to take parts of it or adjust it, if you want, of course).

   ============================================================

   Before including this file, you should define a couple of constants:
   - ENV_VARIABLE_NAME_LOCAL_PATH (needed only if you intend to make
     local HTML versions by command-line php)
   - CURRENT_URL (URL to main directory of these very web pages,
     must be finished by "/")
   - CURRENT_URL_SHORT (Short server name, corresponding to CURRENT_URL,
     used only to show to humans (when making links
     from offline to online page).
   - You may want to define KAMBI_NO_HOME_LINK (value is ignored,
     for now always define to true) to suppress automatic
     writing of main page link in common_header and common_footer.

   ============================================================

   Some rules to follow when editing these pages:

   - The basic idea is that these pages may be processed in two
     various modes:
     - When IS_GEN_LOCAL = false, we make normal online version
       on the page, visible online on URL defined by CURRENT_URL.
     - When IS_GEN_LOCAL = true, we make special version of the page
       suitable for offline viewing. It differs from the online
       version in many details.

   - Nigdy nie rób bezpośrednich linków do stron za pomocą <a href ...>,
     zawsze używaj funkcji a_href_page[_hashlink]. Ona zajmuje się
     kilkoma rzeczami, np. dba o to aby wszystko działało dobrze
     dla każdej wartości IS_GEN_LOCAL (i dla dowolnej kombinacji plików
     dostępnych lokalnie w/g funkcji is_page_available_locally),
     IS_GEN_PAGE_HREFS_TO_HTML, CURRENT_URL, ona też zajmuje się
     automatycznie doklejeniem odpowiedniego sufixu języka do nazwy strony.

     Wyjątkiem są hash-linki w obrębie tej samej strony, tzn. linki
     a postaci <a href="[tutaj hash]hash_link_na_tej_samej_stronie">...</a>,
     to jest dozwolone.

   - Podobnie, nie rób bezpośrednich linków do innych plików.
     Co więcej, nie używaj a_href_size z funcs.php.
     Zamiast tego używaj current_www_a_href_size, z tych samych powodów
     co wyżej a_href_page[_hashlink].

   - Use common_header(...odpowiednie parametry...) and
     common_footer() for page header/footer.
     Between them insert just the page's body.

   Ponadto następujące rzeczy są sugerowane aby uzyskać spójny styl:
   - Ważniejsze strony o stosunkowo krótkim tytule mogą się zaczynać
     używając echo pretty_heading(... tytuł strony ..., wersja programu),
     w szczególności może być
       echo pretty_heading($page_title);

   - Przy opisie instalacji moich gier pod UNIXy home directory użytkownika
     będę oznaczał jako "$HOME" (a nie np. "~"). Mimo że algorytm wyznaczania
     katalogu domowego jest nieco bardziej złożony (KambiUtils.HomeDir)
     niż tylko GetEnvironmentVariable('HOME'), to jednak zapis $HOME
     uważam za lepszy od ~ bo jest dłuższy.
  */

  /* assert_options(ASSERT_ACTIVE, 1); */

  require_once 'funcs.php';
  require_once 'kambi_toc.php';
  require_once 'counter.php';

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
       Some steps (in this file and in all specific php
       pages) must to be taken when generating "local" pages:
       - links to main page (index[.pl].(php|html)) (like those
         in $s_quick_links and <link rel=Start ...>) must not be
         generated
       - default page background (specified by calling common_header())
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
       wszystkie parametry za --locally-avail zostaną potraktowane jako
       nazwy plików które deklarujemy jako dostępne lokalnie w sensie funkcji
       is_file_available_locally.
     Wszystkie nieznane parametry spowodują exit() z odpowiednim komunikatem
     błędu.

     Tutaj ustawiamy stałe IS_GEN_LOCAL, IS_GEN_PAGE_HREFS_TO_HTML
     oraz zmienną $locally_available_files. Tutaj też ustawiamy sobie
     prawidłowe current dir w przypadku IS_GEN_LOCAL=true.
  */

  /* Zmienna wewnętrzna dla funkcji is_file_available_locally, ustawiana
     w kodzie poniżej. */
  $locally_available_files = array();

  if (array_key_exists('argc', $_SERVER))
  {
    for ($i = 1; $i < $_SERVER['argc']; $i++)
    {
      if ($_SERVER['argv'][$i] == '--gen-local')
        define_if_needed('IS_GEN_LOCAL', true); else
      if ($_SERVER['argv'][$i] == '--gen-page-hrefs-to-html')
        define_if_needed('IS_GEN_PAGE_HREFS_TO_HTML', true); else
      if ($_SERVER['argv'][$i] == '--html-validation')
        define_if_needed('HTML_VALIDATION', true); else
      if ($_SERVER['argv'][$i] == '--locally-avail')
      {
        $locally_available_files = array_slice($_SERVER['argv'], $i + 1);
        break;
      } else
        exit("Not recognized command-line parameter " . $_SERVER['argv'][$i]);
    }
  }

  // testy: echo "Locally available are "; print_r($locally_available_files);

  /* Jeżeli stałe nie zostały ustawione przed odpowiednie parametry
     to ustaw je teraz. */
  define_if_needed('IS_GEN_LOCAL', false);
  define_if_needed('IS_GEN_PAGE_HREFS_TO_HTML', IS_GEN_LOCAL);
  define_if_needed('HTML_VALIDATION', false);

  if (IS_GEN_LOCAL)
  {
    $dir = getenv(ENV_VARIABLE_NAME_LOCAL_PATH);
    chdir($dir) or exit("Cannot change directory to \"$dir\"");
  }

/* ============================================================
   some functions related to IS_GEN_LOCAL */

/* Jako $file_name podaj nazwę pliku. Np. "view3dscene.php",
   "view3dscene.pl.php". Dozwolone jest poprzedzenie tego względnym katalogiem,
   jak np. "miscella/badBlaster.zip".

   Jeżeli IS_GEN_LOCAL i plik nie jest dostępny lokalnie to zwróci false,
   wpp. zwróci true. Ujmując to inaczej zwraca
   (!IS_GEN_LOCAL or plik jest dostępny lokalnie).

   Np. gdy generujemy dokumentację lokalną specjalnie do dołączenia jej do
   programu rayhunter to mamy dostępne pliki "rayhunter.html",
   "rayhunter.pl.html", "common_options.html"
   tzn. dla tych wartości $file_name ta funkcja zwróci true;
   dla pozostałych stron, np. "view3dscene.html", ta funkcja
   zwróci false. Gdy generujemy strony nie-lokalnie (czyli IS_GEN_LOCAL=false)
   to zawsze zwraca true, bo wtedy zakłada że przecież na serwerze CURRENT_URL
   są dostępne wszystkie pliki. */
function is_file_available_locally($file_name)
{
  global $locally_available_files;
  return (!IS_GEN_LOCAL || in_array($file_name, $locally_available_files));
}

/* Jak is_file_available_locally tyle że tej funkcji możesz używać tylko
   dla stron (tzn. HTML/PHP) i nie podajesz tutaj rozszerzenia strony
   (".html" lub ".php"). */
function is_page_available_locally($page_name)
{
  /* Możemy tu zawsze doklejać suffix .html bo strony lokalne zawsze
     mają rozszerzenie .html a dla stron nielokalnych funkcja
     is_file_available_locally działa nie patrząc w ogóle na $file_name. */
  return is_file_available_locally($page_name . '.html');
}

  /* Global consts ====================================================== */

  /* :string = I just wasn't sure how to say "here are binaries" in english */
  define('S_HERE_ARE_BINARIES', 'Here are the binaries. No special installation ' .
    'is required, just unpack these archives and run the program.');

  /* stałe do specyfikowania języka, w zmiennej $page_lang i w parametrach
     wielu funkcji. Kiedyś używałem tutaj stringów "en", "pl" - używanie
     stałych intów ma tą zaletę że trudniej jest się pomylić (w razie czego
     dostaniemy ewidentny błąd - nieznany identyfikator) oraz łatwiej
     przetwarzać (to tylko inty, nie jakieś stringi). */
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

  /* Stala tablica; przetłumacz moje stałe LANG_xxx na lang używany w HTMLu
     (zresztą to nie jest specyficzne dla HTMLa, to jakiś bardziej ogólny
     standard nazewnictwa języków). */
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
    'jamy_i_nory' => array(LANG_PL),
    'kno_status' => array(LANG_PL)
  );

  /* Global variables =======================================================

     Żadna funkcja z tego pliku (poza common_header)
     nie może być wywołana przed zainicjowaniem
     wszystkich poniższych zmiennych (za wyjątkiem kilku wyjątków które będą
     miały to wyraźnie stwierdzone w komentarzu; będzie wtedy wyraźnie określone
     których zmiennych dana funkcja wymaga).

     Jest gwarantowane że wywołanie common_header zapewnia że wszystkie te
     zmienne są zainicjowane, więc po wywołaniu common_header nie musisz się
     już o nic martwić. */

  /* Poniższe zmienne są inicjowane w common_header, wcześniej należy je
     traktować jako NIE zainicjowane (czyli nie nadające się do użycia). */
  $page_title = '';
  /* Język strony, po zainicjowaniu wartość to jedna ze stałych LANG_. */
  $page_lang = -1;
  /* internal, used only in common_header/footer() */
  $s_quick_links = '';

  /* Poniższe zmienne są zainicjowane na domyślne wartości z chwilą włączenia
     tego pliku. Jeśli chcesz zmienić
     ich wartości to musisz to zrobić ręcznie przed wywołaniem jakiejkolwiek
     innej funkcji z tego pliku (także common_header). */
  $main_page = false;

/* functions ======================================================= */

/* Zwraca string = tagi htmla (block-level) ktore generuja ladny heading.
   Na pewno zdefiniuje min jeden element w stylu h1
   (wiec subsections na stronie powinienes tagowac jako h2).

   Kiedyś był tu heading z cieniem przy użyciu CSSa (ten sam napis
   wypisany dwa razy), ale usunąłem to -- to nie chciało wyglądać
   dobrze we wszystkich przeglądarkach przy dowolnych ustawieniach.

   You may supply $version_number, this is intended for pages
   that document functionality of some program.

   You may supply $subheading_text, this will be printed in newline
   and with smaller font below heading text. */
function pretty_heading($heading_text, $version_number = NULL, $subheading_text = '')
{
  $result = "<h1>$heading_text";

  if ($subheading_text != '')
    $result .= '<br/><span style="font-size: medium">' . $subheading_text . '</span>';

  if (!is_null($version_number))
    $result .= " <span class=\"version_number\">(version&nbsp;$version_number)</span>";

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

/* Call this only when global $page_lang variable is set.

   For all "normal" pages, this should be automatically called by common_header.
   You have to explicitly call this only in special circumstances, if you need
   some functions defined by this but really don't want to include default HTML
   header (e.g., this is used by our RSS feed generating code).

   This *defines* some new functions (and constants) that really require
   $page_lang variable to set *before they are even defined*.
   What's the reason ? PAGE_LANG constant is needed as a default parameter
   for a_href_page (and it's a really neat and useful trick that a_href_page
   has this default parameter, I don't want to resign from it). */
function common_set_page_functions()
{
  global $page_lang;
  define('PAGE_LANG', $page_lang);

  /* ============================================================
     Define page_url and a_href_page* family of functions */

  function page_url($page_name, $hash_link, $target_page_lang, &$url_comment)
  /* $page_name: string, nie zawiera extension, nie zawiera takze sufixu
     jezyka (".pl" lub innych, jesli kiedys je dodam).
     Czyli basename strony.

     Eventually, if $page_name already contains a .php extension, then we will
     not add any new language/php/html extension at the end.
     Remember that this disables language switching and local html generation,
     and so generally should not be used, as it means that page_url
     isn't of much use.

     $hash_link, jeżeli różny od '', to będzie dopisany (i poprzedzony hashem)
     do URLa strony.

     $target_page_lang: stała LANG_xxx, język docelowy strony.
     Możesz tutaj podawać zmienną globalną $page_lang.
     Jeżeli dana strona w danym języku nie istnieje (chwilowo sprawdzanie
     istnienia strony nie bierze pod uwage IS_GEN_LOCAL, ale powinno)
     to może wybrać arbitralnie inny język. Chwilowo "zmiana języka na inny"
     jest łatwa do zdefiniowania bo mamy tylko dwa języki (en i pl) więc
     jeśli jeden nie istnieje to bierzemy drugi. Zawsze przy takiej zmianie
     języka (tzn. gdy dostajemy stronę w innym języku niż zażądany
     $target_page_lang)
     dostaniemy dodatkowo komentarz w url_comment (np. " (polish)" gdy
     ze strony w języku LANG_EN zażądaliśmy innej strony po angielsku
     ($target_page_lang = LANG_EN) ale dostaliśmy stronę po polsku
     (np. dla 'glplottera' który nie ma wersji angielskiej)).

     Innymi słowy, podając $target_page_lang NIE myśl o tym w jakim języku
     dana strona jest dostępna. Myśl o tym w jakim języku CHCESZ dostać
     stronę (a więc zazwyczaj w takim samym języku w jakim jest aktualna
     strona). Jeżeli strona w języku jakiego chcesz jest niedostępna to
     zostanie podana strona w języku dostępnym i odpowiednie ostrzeżenie
     dostaniesz w $url_comment. Ma to m.in. tą zaletę że jeżeli kiedyś dodam
     np. stronę glplottera po angielsku to wystarczy że uaktualnię tablicę
     $pages_langs i wszystkie linki do glplottera ze stron angielskich
     (które chwilowo są linkami do strony glplottera po poslku) automatycznie
     zmienią się na linki do strony po angielsku.

     Ta funkcja zwraca url zadanej strony, czyli page_name z doklejonym suffixem
     jezyka i rozszerzeniem i ew. poprzedzone CURRENT_URL (jeśli strona
     generowana z IS_GEN_LOCAL odwołuje się do czegoś niedostępnego lokalnie).

     Pod $url_comment zwraca komentarz do URLa w postaci "" (pusty string) lub
     " <i>(komentarz)</i>" (zwróć uwagę na spację na początku),
     np. " <i>(nie-lokalny link)</i>". Powinieneś gdzieś pokazać ten url_comment
     użytkownikowi bo on zawiera generalnie ważne informacje. */
  {
    global $page_lang, $lang_to_html_lang;

    /* init $url_comment to empty. Everywhere in this function we will
       append things to $url_comment using
         str_append_part_to1st($url_comment, ', ', $next_part) */
    $url_comment = '';

    $already_has_extension = strpos($page_name, '.php') !== FALSE;

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

    $result = $page_name;
    if (!$already_has_extension)
      $result .= $language_suffix;

    if (!is_page_available_locally($result))
    {
      if (!$already_has_extension)
        $result = CURRENT_URL . $result . '.php';
      str_append_part_to1st($url_comment, ', ', 'online docs');
    } else
    if (!$already_has_extension)
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

  /* Simpler version of page_url for websites that are only in English language. */
  function en_page_url($page_name, $hash_link = '')
  {
    $url_comment = '';
    return page_url($page_name, $hash_link, LANG_EN, $url_comment);
  }

  function a_href_page_core($link_title, $page_name, $hash_link,
    $target_page_lang)
  /* Internal, aby zapewnić wspólną implementację dla a_href_page i
     a_href_page_hashlink */
  {
    $page_url = page_url($page_name, $hash_link, $target_page_lang, $url_comment);
    return "<a href=\"$page_url\">$link_title$url_comment</a>";
  }

  function a_href_page($link_title, $page_name, $target_page_lang = PAGE_LANG)
  /* Zwraca href do tej strony, coś w rodzaju <a href=$page_url>$link_title</a>,
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

  /* ============================================================
     Define other constants and things that need a_href_page* */

  switch ($page_lang)
  {
    case LANG_PL:
      define('SOURCES_OF_THIS_PROG_ARE_AVAIL',
        'To jest wolne/otwarte oprogramowanie.  Możesz ' .
        a_href_page('pobrać źródła programu (dla programistów)', 'engine') . '.');
      break;
    case LANG_EN:
      define('SOURCES_OF_THIS_PROG_ARE_AVAIL',
        'This is free/open-source software. Developers can ' .
        a_href_page('download sources of this program', 'engine') .
        '.');
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
    ' is strongly suggested if you want to hear sound
    (under Windows appropriate DLL files are already included
    in program\'s archive, so you don\'t have to do anything)');
  define('SUGGESTS_OPENAL_VORBISFILE',
    a_href_page_hashlink('OpenAL', 'openal_notes', 'section_install') .
    ' and <a href="http://xiph.org/vorbis/">OggVorbis (VorbisFile and dependencies)</a>
    libraries are strongly suggested if you want to hear sound
    (under Windows appropriate DLL files are already included
    in program\'s archive, so you don\'t have to do anything)');
  define('DEPENDS_UNIX_GLWINDOW_GTK_1',
    'Under Unices (Linux, FreeBSD, Mac OS X):
    <a href="http://www.gtk.org/">GTK+</a> 1.x and gtkglarea');
  define('DEPENDS_UNIX_GLWINDOW_GTK_2',
    'Under Unix (Linux, FreeBSD, Mac OS X):
    <a href="http://www.gtk.org/">GTK+</a> >= 2.6 and
    <a href="http://gtkglext.sourceforge.net/">GtkGLExt</a> >= 1.0.6');
    /* I also use some GTK >= 2.8 features, but since Mac OS X fink stable
       includes only GTK 2.6, we work Ok with GTK 2.6 too. */
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
}

function echo_kambi_common_css()
{
?>

<!--
  This is unclean, but it's more comfortable to keep below CSS
  directly inside HTML page. This is in kambi_common.php,
  that should be useful for both michalis.ii and vrmlengine.sf webpages,
  and must be suitable also for offline documentation in vrmlengine programs.
-->
<!-- link type="text/css" rel="stylesheet" media="all"  href="kambi-php-lib/kambi_common.css" -->

<style type="text/css"><!--
body { background: white; font-family: sans-serif; }

dt { font-weight: bold; }
div.page_footer { }
span.page_copyright { font-size: smaller }

span.version_number { font-size: small }

div.quick_links_bottom_line { text-align: <?php
  echo (IS_GEN_LOCAL ? 'left' : 'center' ) ?>; }

.rss_link {
  float: right;
  background: red;
  padding: 0.3em;
  border: thin outset black;
}

.rss_link a {
  color: white;
  font-weight: bold
}
--></style>
<?php
}

/* header ============================================================ */

/* $meta_description :string/NULL = krótki opis strony,
   o ile nie będzie NULL będzie wypisany jako <meta name="Description" ...>

   $meta_keywords :string/NULL = dodatkowe keywords strony rozdzielone ",",
   o ile nie będzie NULL to będzie wypisany jako <meta name="Keywords" ...>.

   $bonus_header_tags = beda wypisane tuz przed </head>.
   Moga zawierac rozne rzeczy specyficzne dla strony,
   np. jej deklaracje <style>, <script> i inne (bede rozszerzal ta liste
   gdy znajde do tego powody).

   $a_page_lang musi byc elementem supported_page_langs($page_basename)

   Sets global $page_basename, if not already set.
*/
function common_header($a_page_title, $a_page_lang,
  $meta_description = NULL, $meta_keywords = NULL, $bonus_header_tags = '')
{
  global $page_title, $page_lang, $s_quick_links, $main_page,
    $lang_to_html_lang, $this_page_name, $page_basename;

  $page_title = $a_page_title;
  $page_lang = $a_page_lang;

  /* calculate $this_page_name */
  /* Poprzez Apache'a (na moim Linuxie, moim Windowsie, i na camelot.homedns.org)
     dostaję dobre $_SERVER['PHP_SELF']. Uruchomiony z linii poleceń (do --gen-local):
     pod Linuxem dostaję $_SERVER['PHP_SELF'], pod Windowsem nie (pod Windowsem
     dostaję $_SERVER['PHP_SELF'] ustawione na '' (ale ustawione, tzn. nie jest NULL).
     Więc pod Windowsem biorę je z $_SERVER['argv'][0]. */
  $this_page_name = $_SERVER['PHP_SELF'];
  if ($this_page_name == '')
    $this_page_name = $_SERVER['argv'][0];
  $this_page_name = basename($this_page_name);

  /* calculate $page_basename (requires $page_lang and $this_page_name) */
  if (!isset($page_basename))
  {
    $page_basename = $this_page_name;
    $page_basename = basename($page_basename, '.php');
    $page_basename = basename($page_basename, '.' . $lang_to_html_lang[$page_lang]);
  }

  /* evaluate $supported_langs (requires $page_basename) */
  $supported_langs = supported_page_langs($page_basename);
  assert('in_array($a_page_lang, $supported_langs)');

  common_set_page_functions();

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
      case LANG_PL: $SBackToMain = 'powrót do strony głównej'; break;
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
<meta http-equiv="Content-Type" content="text/html; charset=UTF-8">
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
      case LANG_PL: $main_page_title = 'Strona główna'; break;
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

<?php echo_kambi_common_css(); ?>

<?php
  echo_header_bonus();
  echo $bonus_header_tags;
?>
</head>
<body>

<?php
  if ( (!defined('KAMBI_NO_HOME_LINK')) &&
       (!$main_page) &&
       ($s_quick_links != '') ) { ?>
    <p align="right"><small> <?php echo $s_quick_links; ?> </small></p>
<?php };
}

/* footer ============================================================ */

function common_footer()
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
  if ( (!defined('KAMBI_NO_HOME_LINK')) &&
      (!$main_page) &&
      ($s_quick_links != '') ) { ?>
    <div class="quick_links_bottom_line"> <?php echo $s_quick_links; ?> </div>
<?php };

  echo_footer();
?>

</div>

</body>
</html>

<?php
}

?>
