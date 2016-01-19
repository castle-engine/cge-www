<?php /* -*- mode: kambi-php -*- */

/* Global information about books. During bootstrap, every book
   will be extended with 'chapters' list (based on castle_sitemap). */
global $castle_books;
$castle_books = array(
  'tutorial' => array(
    'title' => 'Tutorial',
    'path' => array('engine', 'tutorial_intro'),
  ),
  'creating_data' => array(
    'title' => 'Guide to creating game data',
    'path' => array('engine', 'creating_data_intro'),
  ),
);

/*
  Update information about book chapters.
  This does two things:

  - Updates $sitemap_book_sub (which should be a subarray of
    global $castle_sitemap) to add chapter numbers to book pages.

  - Fills $castle_books[$book_name]['chapters'].

    It will be a flat (with chapters and subchapters on a single level)
    array with book pages, with every item key=>value just like
    in $castle_sitemap, with additional fields:
    - number: HTML-safe string with number of this chapter/subchapter.
    - previous/next: strings naming previous/next page, or NULL if none.

  This should be called from castle_bootstrap, to update
  global $castle_sitemap and $castle_books before calling any book_header
  or rendering any sidebar.
*/
function castle_sitemap_book_correct($book_name, &$sitemap_book_sub)
{
  global $castle_books;

  $castle_books[$book_name]['chapters'] = array();
  $chapter_number = 1;
  $previous_basename = NULL;
  foreach ($sitemap_book_sub as $chapter_basename => &$chapter)
  {
    $castle_books[$book_name]['chapters'][$chapter_basename] = $chapter +
      array('number' => $chapter_number . '. ',
            'previous' => $previous_basename,
            'next' => NULL);
    $chapter['title'] =
      $castle_books[$book_name]['chapters'][$chapter_basename]['number'] .
      $chapter['title'];
    $previous_basename = $chapter_basename;

    if (isset($chapter['sub']))
    {
      $subchapter_number = 1;
      foreach ($chapter['sub'] as
        $subchapter_basename => &$subchapter)
      {
        $castle_books[$book_name]['chapters'][$subchapter_basename] = $subchapter +
          array('number' => $chapter_number . '.' . $subchapter_number . '. ',
                'previous' => $previous_basename,
                'next' => NULL);
        $subchapter['title'] =
          $castle_books[$book_name]['chapters'][$subchapter_basename]['number'] .
          $subchapter['title'];
        $previous_basename = $subchapter_basename;

        $subchapter_number++;
      }
    }

    $chapter_number++;
  }

  /* iterate over $castle_books[$book_name]['chapters'] to calculate 'next' links */
  foreach ($castle_books[$book_name]['chapters'] as $page_basename => &$page)
    if ($page['previous'] !== NULL)
      $castle_books[$book_name]['chapters'][$page['previous']]['next'] = $page_basename;
}

function book_bar($book_name)
{
  global $castle_books;
  global $page_basename;

  $this_info = $castle_books[$book_name]['chapters'][$page_basename];

  $result = '<div class="book-header">
    <div class="book-previous">';
  if ($this_info['previous'] !== NULL)
  {
    $previous_info = $castle_books[$book_name]['chapters'][$this_info['previous']];
    $result .= a_href_page('Previous: ' .
      $previous_info['number'] . $previous_info['title'], $this_info['previous']);
  } else
    $result .= '&nbsp;';

  $result .= '</div> <div class="book-next">';

  if ($this_info['next'] !== NULL)
  {
    $next_info = $castle_books[$book_name]['chapters'][$this_info['next']];
    $result .= a_href_page('Next: ' .
      $next_info['number'] . $next_info['title'], $this_info['next']);
  } else
    $result .= '&nbsp;';

  $result .= '</div>
    <div class="book-title">' . $castle_books[$book_name]['title'] . '</div>
    <div style="clear:both"></div>
  </div>';

  return $result;
}

function book_header($book_name, $a_page_title, $subheading_text)
{
  global $castle_books;
  global $page_basename;
  $number = $castle_books[$book_name]['chapters'][$page_basename]['number'];
  $a_page_title = $number . $a_page_title;

  castle_header($a_page_title .
    ' | ' . $castle_books[$book_name]['title'] .
    ' | Castle Game Engine', NULL, $castle_books[$book_name]['path']);
  echo book_bar($book_name);
  echo pretty_heading($a_page_title, NULL, $subheading_text);
}

function book_footer($book_name)
{
  echo book_bar($book_name);
  castle_footer();
}

function tutorial_header($a_page_title, $subheading_text = '')
{
  book_header('tutorial', $a_page_title, $subheading_text);
}

function tutorial_footer()
{
  book_footer('tutorial');
}

function creating_data_header($a_page_title, $subheading_text = '')
{
  book_header('creating_data', $a_page_title, $subheading_text);
}

function creating_data_footer()
{
  book_footer('creating_data');
}
