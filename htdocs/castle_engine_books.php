<?php /* -*- mode: kambi-php -*- */

/* Global information about books. During bootstrap, every book
   will be extended with 'chapters' list (based on castle_sitemap). */
global $castle_books;
$castle_books = array(
  'manual' => array(
    'title' => 'Manual',
    'path' => array('documentation', 'manual_intro'),
  ),
  'creating_data' => array(
    'title' => 'Creating Game Data',
    'path' => array('documentation', 'creating_data_intro'),
  ),
);

/* Fill $pages list with flat book pages information,
   based on traversing (recursively) everything in $book_list (subset of sitemap). */
function enumerate_book_pages(&$pages, &$previous_basename, $book_list)
{
  foreach ($book_list as $new_basename => $new_info)
  {
    $pages[$new_basename] = $new_info;
    $pages[$new_basename]['previous'] = $previous_basename;
    if ($previous_basename != NULL) { // make a link in reverse direction
      $pages[$previous_basename]['next'] = $new_basename;
    }
    // initialize next to NULL; if this is not the last page, it will be updated later
    $pages[$new_basename]['next'] = NULL;
    $previous_basename = $new_basename;

    if (isset($new_info['sub'])) {
      enumerate_book_pages($pages, $previous_basename, $new_info['sub']);
    }
  }
}

/*
  Calculate information about book pages.
  This fills $castle_books[$book_name]['pages'].

    It will be a flat (with chapters, subchapters etc. on a single level)
    array with book pages, with every item key=>value just like
    in $castle_sitemap, with additional fields:
    - number: HTML-safe string with number of this chapter/subchapter.
    - previous/next: strings naming previous/next page, or NULL if none.

  This should be called from castle_bootstrap, to update
  global $castle_sitemap and $castle_books before calling any book_header
  or rendering any sidebar.
*/
function castle_book_calculate_pages($book_name)
{
  global $castle_books, $castle_sitemap;

  $book_info = $castle_books[$book_name];

  // calculate $book_list, subset of $castle_sitemap for this book
  $book_list = $castle_sitemap;
  foreach ($book_info['path'] as $book_path_component) {
    $book_list = $book_list[$book_path_component]['sub'];
  }

  $pages = array();
  $previous_basename = NULL;
  enumerate_book_pages($pages, $previous_basename, $book_list);

  $castle_books[$book_name]['pages'] = $pages;
}

function book_bar($book_name)
{
  global $castle_books;
  global $page_basename;

  if (array_key_exists($page_basename, $castle_books[$book_name]['pages'])) {
    $this_info = $castle_books[$book_name]['pages'][$page_basename];
  } else {
    /* This happens when the current page is the ToC page of the book,
       not part of the book. */
    /* Get the first chapter of the book,
       http://stackoverflow.com/questions/1921421/get-the-first-element-of-an-array */
    $first_chapter = array_keys(array_slice($castle_books[$book_name]['pages'], 0, 1));
    $this_info = array(
      'number' => NULL,
      'previous' => NULL,
      'next' => $first_chapter[0],
    );
  }

  $result = '<div class="book-header">
    <div class="book-previous">';
  if ($this_info['previous'] !== NULL)
  {
    $previous_info = $castle_books[$book_name]['pages'][$this_info['previous']];
    $result .= a_href_page('Previous: ' .
      // Do not show chapter numbers now.
      //$previous_info['number'] .
      $previous_info['title'], $this_info['previous']);
  } else
    $result .= '&nbsp;';

  $result .= '</div> <div class="book-next">';

  if ($this_info['next'] !== NULL)
  {
    $next_info = $castle_books[$book_name]['pages'][$this_info['next']];
    $result .= a_href_page('Next: ' .
      // Do not show chapter numbers now.
      //$next_info['number'] .
      $next_info['title'], $this_info['next']);
  } else
    $result .= '&nbsp;';

  $result .= '</div>
    <div class="book-title">' . $castle_books[$book_name]['title'] . '</div>
    <div style="clear:both"></div>
  </div>';

  return $result;
}

/* Returns book name (key of $castle_books, like 'manual')
   or NULL (if not part of any book).
   Searches for $page_name inside global $castle_sitemap, using global $castle_books info.
*/
function detect_current_book($page_name)
{
  global $castle_sitemap, $castle_books;

  foreach ($castle_books as $book_name => $book_info) {
    // early exit in case current page is the top (start) page of the book
    if ($page_name == end($book_info['path'])) {
      return $book_name;
    }

    // calculate $book_list, subset of $castle_sitemap for this book
    $book_list = $castle_sitemap;
    foreach ($book_info['path'] as $book_path_component) {
      $book_list = $book_list[$book_path_component]['sub'];
    }

    if (_detect_page_path_core($page_name, $book_list) !== NULL) {
      return $book_name;
    }
  }
  return NULL;
}
