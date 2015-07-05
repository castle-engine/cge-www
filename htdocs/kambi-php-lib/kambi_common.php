<?php /* -*- mode: php -*- (This page should be edited in php mode,
                            mmm mode is too slowish for this page). */

/* Copyright 2001-2014 Michalis Kamburelis.

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
   http://castle-engine.sourceforge.net/ and
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
     katalogu domowego jest nieco bardziej złożony (CastleUtils.HomeDir)
     niż tylko GetEnvironmentVariable('HOME'), to jednak zapis $HOME
     uważam za lepszy od ~ bo jest dłuższy.
  */

/* assert_options(ASSERT_ACTIVE, 1); */

require_once 'funcs.php';
require_once 'kambi_toc.php';

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
     with href's to htmls and everything worked as expected;
     now it is still useful
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

// tests: echo "Locally available are "; print_r($locally_available_files);

/* Jeżeli stałe nie zostały ustawione przed odpowiednie parametry
   to ustaw je teraz. */
define_if_needed('IS_GEN_LOCAL', false);
define_if_needed('IS_GEN_PAGE_HREFS_TO_HTML', IS_GEN_LOCAL);
define_if_needed('HTML_VALIDATION', false);

if (IS_GEN_LOCAL)
{
  $dir = getenv(ENV_VARIABLE_NAME_LOCAL_PATH) . 'www/htdocs/';
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

/* Constants for languages, used for $page_lang parameter of common_header.
   They are also suitable for HTML (e.g. as <html lang="..."> value). */
define('LANG_PL', 'pl');
define('LANG_EN', 'en');

/* in normal circumstances this should always be 'index' */
define('MAIN_PAGE_BASENAME', 'index');

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
  if (!is_null($version_number))
    $result .= " <span class=\"label label-default version_number\">$version_number</span>";
  if ($subheading_text != '')
    $result .= '<br><small>' . $subheading_text . '</small>';
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
   czesto musialbym wywolywac pack_binary.sh dwa razy pod rzad tylko po to
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

/* URL of desired page, i.e. page_name z doklejonym
   rozszerzeniem i ew. poprzedzone CURRENT_URL (jeśli strona
   generowana z IS_GEN_LOCAL odwołuje się do czegoś niedostępnego lokalnie).

   $page_name: string, nie zawiera extension, basename strony.

   Eventually, if $page_name already contains a .php extension, then we will
   not add any new php/html extension at the end.
   Remember that this disables local html generation,
   and so generally should not be used, as it means that page_url
   isn't of much use.

   $hash_link, jeżeli różny od '', to będzie dopisany (i poprzedzony hashem)
   do URLa strony.

   Pod $url_comment zwraca komentarz do URLa w postaci "" (pusty string) lub
   " <i>(komentarz)</i>" (zwróć uwagę na spację na początku),
   np. " <i>(nie-lokalny link)</i>". Powinieneś gdzieś pokazać ten url_comment
   użytkownikowi bo on zawiera generalnie ważne informacje. */
function page_url($page_name, $hash_link, &$url_comment)
{
  /* init $url_comment to empty. Everywhere in this function we will
     append things to $url_comment using
       str_append_part_to1st($url_comment, ', ', $next_part) */
  $url_comment = '';

  $result = $page_name;

  $parse_url_result = parse_url($result);

  if (!isset($parse_url_result['scheme'])) {
    $already_has_extension = strpos($page_name, '.php') !== FALSE;
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
  }

  if ($hash_link != '') {
    $result .= '#' . $hash_link;
  }

  if ($url_comment != '') {
    $url_comment = " <i>($url_comment)</i>";
  }

  return $result;
}

/* Simpler version of page_url. */
function en_page_url($page_name, $hash_link = '')
{
  $url_comment = '';
  return page_url($page_name, $hash_link, $url_comment);
}

/* Internal, aby zapewnić wspólną implementację dla a_href_page i
   a_href_page_hashlink. */
function a_href_page_core($link_title, $page_name, $hash_link)
{
  $page_url = page_url($page_name, $hash_link, $url_comment);
  return "<a href=\"$page_url\">$link_title$url_comment</a>";
}

/* Zwraca href do tej strony, coś w rodzaju <a href=$page_url>$link_title</a>,
   gdzie $page_url = page_url($page_name, ''). */
function a_href_page($link_title, $page_name)
{
  return a_href_page_core($link_title, $page_name, '');
}

/* Jak a_href_page ale page_url ma odpowiedni hash_link. */
function a_href_page_hashlink($link_title, $page_name, $hash_link)
{
  return a_href_page_core($link_title, $page_name, $hash_link);
}

/* Sets global $page_basename and $this_page_name, if not already set.
   It is Ok (harmless) to call this more than once during page request
   (useful, since we call it from common_header but "Castle Game Engine"
   also wants to call it earlier). */
function kambi_bootstrap()
{
  global $this_page_name, $page_basename;

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

  /* calculate $page_basename (requires $this_page_name) */
  if (!isset($page_basename))
  {
    $page_basename = $this_page_name;
    $page_basename = basename($page_basename, '.php');
  }
}

/* header ============================================================ */

/* URL relative path from this page to root, where kambi-php-lib/
   is a subdirectory.
   If a particular page is not within root directory,
   then change this before calling common_header. */
global $relative_path_to_root;
if (empty($relative_path_to_root)) {
  $relative_path_to_root = '';
}

/* $meta_description :string/NULL = krótki opis strony,
   o ile nie będzie NULL będzie wypisany jako <meta name="Description" ...>

   $meta_keywords :string/NULL = dodatkowe keywords strony rozdzielone ",",
   o ile nie będzie NULL to będzie wypisany jako <meta name="Keywords" ...>.

   $bonus_header_tags = beda wypisane tuz przed </head>.
   Moga zawierac rozne rzeczy specyficzne dla strony,
   np. jej deklaracje <style>, <script> i inne (bede rozszerzal ta liste
   gdy znajde do tego powody).

   Sets global $page_basename, if not already set.
*/
function common_header($a_page_title, $page_lang,
  $meta_description = NULL, $meta_keywords = NULL, $bonus_header_tags = '')
{
  global $page_title, $s_quick_links, $main_page, $this_page_name, $page_basename,
    $relative_path_to_root;

  $page_title = $a_page_title;

  kambi_bootstrap();

  /* calculate $s_quick_links */
  $s_quick_links = '';

  if (!IS_GEN_LOCAL)
  {
    switch ($page_lang)
    {
      case LANG_PL: $SBackToMain = 'powrót do strony głównej'; break;
      case LANG_EN: $SBackToMain = 'back to main page'; break;
    }
    $s_quick_links = str_append_part($s_quick_links, ' | ', a_href_page(
      $SBackToMain, $relative_path_to_root . MAIN_PAGE_BASENAME));
  }

  if ($s_quick_links != '') $s_quick_links = '[' . $s_quick_links . ']';

?>
<!DOCTYPE html>
<?php echo "<html lang=\"" .$page_lang. "\">\n"; ?>

<head>
<!-- meta suggested by bootstrap, but generally sensible -->
<meta charset="utf-8">
<meta http-equiv="X-UA-Compatible" content="IE=edge">
<meta name="viewport" content="width=device-width, initial-scale=1">

<meta http-equiv="Content-Type" content="text/html; charset=UTF-8">
<meta name="Author" content="Michalis Kamburelis">

<?php
  if (!is_null($meta_keywords))
  { echo "<meta name=\"Keywords\" content=\"$meta_keywords\">\n"; }

  if (!is_null($meta_description))
  { echo "<meta name=\"Description\" content=\"$meta_description\">\n"; }

  if (! ($main_page || IS_GEN_LOCAL))
  {
    switch ($page_lang)
    {
      case LANG_PL: $main_page_title = 'Strona główna'; break;
      case LANG_EN: $main_page_title = 'Main page'; break;
    }
    $page_url = page_url(MAIN_PAGE_BASENAME, '', $url_comment);
    echo "<link rel=\"Start\"
                type=\"text/html\"
                href=\"$page_url\"
                title=\"$main_page_title$url_comment\">\n";
  }
?>

<title><?php echo $page_title ?></title>

<!-- Bootstrap -->
<link href="<?php echo $relative_path_to_root; ?>kambi-php-lib/bootstrap/css/bootstrap.min.css" rel="stylesheet">
<!-- Bootstrap theme -->
<link href="<?php echo $relative_path_to_root; ?>kambi-php-lib/bootstrap/css/bootstrap-theme.min.css" rel="stylesheet">

<!-- Colorbox -->
<link href="<?php echo $relative_path_to_root; ?>kambi-php-lib/colorbox/example3/colorbox.css" type="text/css" rel="stylesheet">

<!-- HTML5 shim and Respond.js for IE8 support of HTML5 elements and media queries -->
<!-- WARNING: Respond.js doesn't work if you view the page via file:// -->
<!--[if lt IE 9]>
  <script src="https://oss.maxcdn.com/html5shiv/3.7.2/html5shiv.min.js"></script>
  <script src="https://oss.maxcdn.com/respond/1.4.2/respond.min.js"></script>
<![endif]-->

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
    <p class="text-right"><small> <?php echo $s_quick_links; ?> </small></p>
<?php };
}

/* footer ============================================================ */

function common_footer()
{
  global $s_quick_links, $main_page, $relative_path_to_root;

  /* This is html text with copyright of these pages. */
  define('PAGE_COPYRIGHT',
  '<span class="page_copyright">Copyright <a href="http://michalis.ii.uni.wroc.pl/~michalis/">Michalis Kamburelis</a>.
This page is considered part of documentation of my programs,
and you are free to modify and further distribute it on terms of
<a href="http://www.gnu.org/licenses/gpl.html">GNU General Public License</a>.
</span>');

?>

<?php
  if ( (!defined('KAMBI_NO_HOME_LINK')) &&
      (!$main_page) &&
      ($s_quick_links != '') ) { ?>
    <div class="quick_links_bottom_line"> <?php echo $s_quick_links; ?> </div>
<?php };

  echo_footer();
?>

<!-- jQuery (necessary for Bootstrap's JavaScript plugins).
     Used also by colorbox. -->
<script src="<?php echo $relative_path_to_root; ?>kambi-php-lib/js/jquery.min.js" type="text/javascript"></script>
<!-- Include colorbox after jQuery is known -->
<script src="<?php echo $relative_path_to_root; ?>kambi-php-lib/colorbox/jquery.colorbox-min.js" type="text/javascript"></script>
<script type="text/javascript">
  jQuery('a.screenshot').colorbox({opacity: 0.9, rel:'screenshot', maxWidth:'90%', maxHeight:'90%'});
</script>
<!-- Include all compiled plugins (below), or include individual files as needed -->
<script src="<?php echo $relative_path_to_root; ?>kambi-php-lib/bootstrap/js/bootstrap.min.js"></script>
</body>
</html>

<?php
}

?>
