<?php
require_once 'castle_engine_functions.php';
tutorial_header('Standard 2D controls: user interface');
?>

TODO: finish this.

Design simple window with an image and two buttons. "Attack the zombie?"

- parents hierarchy, relative coords
- tuicontrolrectangle as parent invisible
- tcastlelabel, TCastleImage control, tcastlebutton, tcastleshape and tcastlerectangle as common ui controls

Note that *Every* control can be a parent for more controls. So you can place children controls e.g. inside the TCastleBytton, to make a button contain any UI you like.

Also note that the parent by default does *not* cut the visibility of children, I.e. it doesn't vmake a "scissor" for children. It is your responsibility to put children inside the parent (otherwise some things will not work intuitively, e.g. mouse clicks may not be passed to children OK). Or you can activate the scissor, to cut children visibility to parent area, see the tscrollviw for example.
castlebutton is very flexible, you can really adjust it
- you can adjust theme, and lots of their properties. Label may be HTML.
- position relative to parent using anchor, usually. Ev. In resize adjust as needed, modify left or bottom or call Align then.
- check size with calculated width/height. Do not just use button width/height, it is left as set when using autosize. Also, for some controls it's not available until the first resize event.
- use ui scaling as needed. ScreeRect gives rect after scaling, Rect gives size after scaling too, and font has size after scaling too, generally everything else is unscaled. Everything you set is unscaled.

- remember that Tabstract castle viewport (tcastl scene manager, t2dscene manager, tcastleviewport) are 2d controls too. You CAN go wild and insert this way animations from x3d / spine on top of your UI, it works as always, and we have no problems with multiple vscene managers.

- Special UIs:
  - you may find it useful to use TCastleuistate to organize your application states. In this case, you will usually create ui in start, and pass Free AtStop as owner (or free manually in stop).
  - you can use tuicontrolrectangle fullscreen to capture keys

<?php
tutorial_footer();
?>
