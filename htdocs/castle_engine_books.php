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
      // Do not show chapter numbers now.
      //$castle_books[$book_name]['chapters'][$chapter_basename]['number'] .
      $chapter['title'];
    // If the chapter is an external URL link (like creating_data_spine),
    // omit it from next/previous navigation.
    if (empty($chapter['url'])) {
      $previous_basename = $chapter_basename;
    }

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
          // Do not show chapter numbers now.
          //$castle_books[$book_name]['chapters'][$subchapter_basename]['number'] .
          $subchapter['title'];
        // If the chapter is an external URL link (like creating_data_spine),
        // omit it from next/previous navigation.
        if (empty($subchapter['url'])) {
          $previous_basename = $subchapter_basename;
        }

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

  if (array_key_exists($page_basename, $castle_books[$book_name]['chapters'])) {
    $this_info = $castle_books[$book_name]['chapters'][$page_basename];
  } else {
    /* This happens when the current page is the ToC page of the book,
       not part of the book. */
    /* Get the first chapter of the book,
       http://stackoverflow.com/questions/1921421/get-the-first-element-of-an-array */
    $first_chapter = array_keys(array_slice($castle_books[$book_name]['chapters'], 0, 1));
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
    $previous_info = $castle_books[$book_name]['chapters'][$this_info['previous']];
    $result .= a_href_page('Previous: ' .
      // Do not show chapter numbers now.
      //$previous_info['number'] .
      $previous_info['title'], $this_info['previous']);
  } else
    $result .= '&nbsp;';

  $result .= '</div> <div class="book-next">';

  if ($this_info['next'] !== NULL)
  {
    $next_info = $castle_books[$book_name]['chapters'][$this_info['next']];
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

/* Echo a header.

   $parameters allowed fields:
   - 'social_share_image'
   - 'subheading_text' */
function book_header($book_name, $a_page_title, array $parameters = array())
{
  global $castle_books;
  global $page_basename;

  // Do not show chapter numbers now.
  // if (array_key_exists($page_basename, $castle_books[$book_name]['chapters'])) {
  //   $number = $castle_books[$book_name]['chapters'][$page_basename]['number'];
  // } else {
  //   /* This happens when the current page is the ToC page of the book,
  //      not part of the book. */
  //   $number = '';
  // }
  // $a_page_title = $number . $a_page_title;

  /* call castle_header with proper params */
  $castle_header_parameters = array(
    'path' => $castle_books[$book_name]['path']
  );
  if (isset($parameters['social_share_image'])) {
    $castle_header_parameters['social_share_image'] = $parameters['social_share_image'];
  }
  castle_header($a_page_title . ' | ' . $castle_books[$book_name]['title'],
    $castle_header_parameters);

  echo book_bar($book_name);
  $subheading_text = isset($parameters['subheading_text']) ? $parameters['subheading_text'] : '';
  echo pretty_heading($a_page_title, NULL, $subheading_text);
}

function book_footer($book_name)
{
  echo book_bar($book_name);
  castle_footer();
}

/* Echo a header.

   $parameters allowed fields same as for book_header:
   - 'social_share_image'
   - 'subheading_text' */
function manual_header($a_page_title, array $parameters = array())
{
  book_header('manual', $a_page_title, $parameters);
}

function manual_footer()
{
  book_footer('manual');
}

/* Echo a header.

   $parameters allowed fields same as for book_header:
   - 'social_share_image'
   - 'subheading_text' */
function creating_data_header($a_page_title, array $parameters = array())
{
  book_header('creating_data', $a_page_title, $parameters);
}

function creating_data_footer()
{
  book_footer('creating_data');
}
