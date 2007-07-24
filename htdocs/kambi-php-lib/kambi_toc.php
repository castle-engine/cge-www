<?php /* -*- mode: php -*- */

class TocItem {
  var $display_name;
  var $anchor_name;
  var $nesting;

  function TocItem($a_display_name, $an_anchor_name, $a_nesting = 0)
  {
    $this->display_name = $a_display_name;
    $this->anchor_name = $an_anchor_name;
    $this->nesting = $a_nesting;
  }
}

class TableOfContents {
  var $items;
  var $next_section_heading;

  /* Gets $an_items array, must be indexed by integers starting from 0
     (and keys must be sorted by these integers too, so possibly
     you have to use ksort first). */
  function TableOfContents($an_items)
  {
    $this->items = $an_items;
    $this->next_section_heading = 0;
  }

  /* Returns a string that contains HTML <ol> item with our
     table of contents. */
  function html_toc()
  {
    $result = '';
    $old_nesting = -1;
    foreach($this->items as $toc_item)
    {
      $now_nesting = $toc_item->nesting;

      $list_item =
        "<li><a href=\"#section_" . $toc_item->anchor_name .
          "\">" . $toc_item->display_name . "</a>\n";

      if ($now_nesting == $old_nesting + 1)
      {
        $result .= "<ol>\n" . $list_item;
      } else
      if ($now_nesting == $old_nesting)
      {
        $result .= "</li>\n" . $list_item;
      } else
      if ($now_nesting == $old_nesting - 1)
      {
        $result .= "</li>\n</ol>\n</li>\n" . $list_item;
      } else
      {
        exit('Incorrect toc items nesting: ' .
          'nesting of successfull items must differ at most by 1');
      }

      $old_nesting = $now_nesting;
    }

    while ($old_nesting >= 0)
    {
      $result .= "</li>\n</ol>\n";
      $old_nesting--;
    }

    return $result;
  }

  function html_section()
  {
    $toc_item = $this->items[$this->next_section_heading];

    /* Our $toc_item->nesting is always from 0.
       For HTML, <h1> should be taken by page title, so our nesting
       0 corresponds to <h2>. */
    $heading_level = min($toc_item->nesting + 2, 6);

    $result = "<h$heading_level><a name=\"section_" .
      $toc_item->anchor_name . "\">" .
      $toc_item->display_name . "</a></h$heading_level>\n";
    $this->next_section_heading++;
    return $result;
  }
}

?>