<?php
require_once 'castle_engine_functions.php';
manual_header('Text and fonts');

$toc = new TableOfContents(
  array(
    new TocItem('TCastleFont class', 'font'),
    new TocItem('Create a new font', 'create'),
    new TocItem('International characters', 'international'),
    new TocItem('Localization (translation)', 'localization'),
  )
);
?>

<?php echo $toc->html_toc(); ?>
<?php echo $toc->html_section(); ?>

<p>To draw some text you need an instance of the
<?php api_link('TCastleFont', 'CastleFonts.TCastleFont.html'); ?> class.
To make it easy, one global instance of this class is already created for you:
<?php api_link('UIFont', 'CastleControls.html#UIFont'); ?>
 (part of <?php api_link('CastleControls', 'CastleControls.html'); ?> unit).
So you can simply draw text like this:

<?php echo pascal_highlight(
'UIFont.Print(10, 10, Yellow, \'Some text to print\');'); ?>

<p>You should place such drawing code inside a render method,
for example inside the
 <?php api_link('OnRender event of TCastleWindowCustom',
 'CastleWindow.TCastleWindowCustom.html#OnRender'); ?> or
 <?php api_link('OnRender event of TCastleControlCustom',
 'CastleControl.TCastleControlCustom.html#OnRender'); ?> or
inside the overridden <?php api_link('TUIControl.Render',
  'CastleUIControls.TUIControl.html#Render'); ?> implementation.
See <?php echo a_href_page('the manual about 2D drawing',
  'manual_2d_ui_custom_drawn'); ?> for a general info about 2D rendering.

<p>Instead of directly drawing the text,
you can also use
 <?php api_link('TCastleLabel', 'CastleControls.TCastleLabel.html'); ?>
 that manages the drawing for you.
 You can customize it's font using
 <?php api_link('CustomFont', 'CastleControls.TUIControlFont.html#CustomFont'); ?>,
 <?php api_link('FontSize', 'CastleControls.TUIControlFont.html#SmallFont'); ?>
 and
 <?php api_link('SmallFont', 'CastleControls.TUIControlFont.html#SmallFont'); ?>
 properties.
 Many other UI controls (see for example unit
 <?php api_link('CastleControls', 'CastleControls.html'); ?>)
 descend from
 <?php api_link('TUIControlFont', 'CastleControls.TUIControlFont.html'); ?>.
 and thus may have their font customized, for example
 <?php api_link('TCastleButton', 'CastleControls.TCastleButton.html'); ?>.

<p><?php api_link('TCastleFont', 'CastleFonts.TCastleFont.html'); ?> class
has a lot of methods and properties.
<ul>
  <li>You can simply print the text
    (<?php api_link('Print', 'CastleFonts.TCastleFont.html#Print'); ?>).
  <li>You can scale the font
    (<?php api_link('Scale', 'CastleFonts.TCastleFont.html#Scale'); ?>,
     <?php api_link('Size', 'CastleFonts.TCastleFont.html#Size'); ?>).
  <li>You can add an outline around it
    (<?php api_link('Outline', 'CastleFonts.TCastleFont.html#Outline'); ?>,
    <?php api_link('OutlineColor', 'CastleFonts.TCastleFont.html#OutlineColor'); ?>).
  <li>You can measure the text
    (<?php api_link('TextWidth', 'CastleFonts.TCastleFont.html#TextWidth'); ?>,
    <?php api_link('TextHeight', 'CastleFonts.TCastleFont.html#TextHeight'); ?>,
    <?php api_link('TextSize', 'CastleFonts.TCastleFont.html#TextSize'); ?>,
    <?php api_link('RowHeight', 'CastleFonts.TCastleFont.html#RowHeight'); ?>...).
  <li>You can print a multi-line text, with optional line wrapping
    (<?php api_link('PrintRect', 'CastleFonts.TCastleFont.html#PrintRect'); ?>,
    <?php api_link('PrintRectMultiline', 'CastleFonts.TCastleFont.html#PrintRectMultiline'); ?>,
    <?php api_link('PrintStrings', 'CastleFonts.TCastleFont.html#PrintStrings'); ?>,
    <?php api_link('PrintBrokenString', 'CastleFonts.TCastleFont.html#PrintBrokenString'); ?>
    and other methods).
</ul>

<?php echo $toc->html_section(); ?>

<p><?php api_link('TCastleFont', 'CastleFonts.TCastleFont.html'); ?> is actually
an abstract class representing some font that can be drawn.
To create a new font, you create an instance of a non-abstract class,
most often the <?php api_link('TTextureFont', 'CastleFonts.TTextureFont.html'); ?>
 class &mdash; it draws font glyphs from a texture, and can be loaded from a TTF font.
There are other possible font implementations, for example
<?php api_link('TSimpleTextureFont', 'CastleFonts.TSimpleTextureFont.html'); ?>
 allows to use a font drawn on an image (so you can make colorful letters,
with fancy custom outline and such).

<p>See <code>castle_game_engine/examples/fonts/font_from_texture.lpr</code>
for a simple example of creating fonts.
In the basic version, you simply use
<?php api_link('TTextureFont', 'CastleFonts.TTextureFont.html'); ?> constructor
to load a font from a TTF file (or any other font format supported by the
FreeType2 library). So you construct a font like this:

<?php echo pascal_highlight(
'MyNewFont := TTextureFont.Create(ApplicationData(\'MyFontFile.ttf\'), 20, true);'); ?>

<p>Remember to install the FreeType2 library for this to work. On Windows,
place appropriate FreeType2 DLL alongside the exe, you can get the DLL
from <code>castle_game_engine/tools/build-tool/data/external_libraries/</code> directory
of the engine.

<p>Note that you can also assign the new font as the global <code>UIFont</code>,
so it will be by default used by all standard UI controls:

<?php echo pascal_highlight(
'UIFont := TTextureFont.Create(ApplicationData(\'MyFontFile.ttf\'), 20, true);'); ?>

<p>Instead of loading the font data from a TTF file, you can also provide
a <?php api_link('TTextureFontData', 'CastleFonts.TTextureFontData.html'); ?>
 instance to the <?php api_link('TTextureFont', 'CastleFonts.TTextureFont.html'); ?>
 constructor. This allows to create the font data at runtime
 or <b>to use the font data embedded in a Pascal source code</b>.
You can use the <code>texture-font-to-pascal</code> program (compile it from
<code>castle_game_engine/tools/texture-font-to-pascal/texture-font-to-pascal.lpr</code>)
to convert a TTF file into a Pascal unit:

<pre>
texture-font-to-pascal --size 20 MyFontFile.ttf
</pre>

<p>In response, it will create a unit called
<code>CastleTextureFont_MyFontFile_20</code> with a public function:

<?php echo pascal_highlight(
'function TextureFont_MyFontFile_20: TTextureFontData;'); ?>

<p>You can use this unit in your program, and create a font instance like this:

<?php echo pascal_highlight(
'MyNewFont := TTextureFont.Create(TextureFont_MyFontFile_20);'); ?>

<p>The advantages of embedding a font inside a Pascal unit are:

<ul>
  <li>You don't need to distribute the FreeType2 library.
    This is especially useful when developing for Android or iOS or web plugin,
    when linking with an additional library can be troublesome.
  <li>Font is loaded slightly faster, since it's already processed to
    a suitable texture data.
</ul>

<p>The disadvantages are of course that you cannot easily edit the TTF anymore,
you will need to rerun the <code>texture-font-to-pascal</code> command
and recompile your program to see the new font.

<?php echo $toc->html_section(); ?>

<?php
echo castle_thumbs(array(
  array('filename' => 'test_local_characters.png', 'titlealt' => 'Testing local (international) characters'),
));
?>

<p>(<b>A complete program using the concepts discussed below is in the engine examples</b>,
in the <code>examples/fonts/test_local_characters/</code>.
The main code is in <code>game.pas</code> unit there.
Check it out!)

<p>All font routines (printing, measuring) expect
the international characters to be encoded using UTF-8.
To draw the international characters (anything beyond basic English ASCII set)
you also need to create a font with these characters.

<p>When constructing <?php api_link('TTextureFont', 'CastleFonts.TTextureFont.html'); ?>,
you need to use the overloaded constructor with parameter
<code>ACharacters (TUnicodeCharList)</code>. Provide there a list
of the characters (including all the possible international characters)
that you want to display. Like this:

<?php echo pascal_highlight(
'uses ..., CasteFonts, CastleStringUtils, CastleUnicode;

function CreateMyFont: TCastleFont;
var
  Characters: TUnicodeCharList;
begin
  Characters := TUnicodeCharList.Create;
  try
    Characters.Add(SimpleAsciiCharacters);
    { below is a string containing all my international chars, in UTF-8 }
    Characters.Add(\'你好世界ΓειασουκόσμεЗдравствуймир\');
    Result := TTextureFont.Create(ApplicationData(\'MyFontFile.ttf\'), 20, true, Characters);
  finally FreeAndNil(Characters) end;
end;'); ?>

<p>Make sure to provide the sample characters encoded in UTF-8.
In the example above, they are simply hardcoded in the Pascal source file,
so make sure that compiler understands it as UTF-8 data.
Make sure your source code is in UTF-8 (edit it using an UTF-8
capable editor, consider adding an UTF-8 BOM,
consider using <code>{$CODEPAGE UTF8}</code>,
see <a href="http://www.freepascal.org/docs-html/prog/progsu88.html">FPC source codepage</a> option).

<p>If you use the <code>texture-font-to-pascal</code> utility to embed fonts in
Pascal sources (see above) then use it's parameter <code>--sample-text</code>
to provide the additional (beyond simple ASCII) chars. Like this:

<pre>
texture-font-to-pascal --size 20 MyFontFile.ttf --sample-text '你好世界ΓειασουκόσμεЗдравствуймир'
</pre>

<p>And make sure that your command-line, and/or your script interpreter, correcly
handles UTF-8 (on Linux, this should be a breeze, since everything works with UTF-8
out of the box; on modern Windows it should also work).

<?php echo $toc->html_section(); ?>

<p>You can use GetText to localize your game.

<p>GetText is a standard unit in FPC, and it's a translation system used in many projects (both in Pascal and other languages). <a href="http://wiki.lazarus.freepascal.org/Step-by-step_instructions_for_creating_multi-language_applications">Lazarus is also using it and advicing for LCL applications.</a>

<p>The example showing how to use the engine with GetText is in <a href="https://github.com/castle-engine/castle-engine/tree/master/examples/localization">examples/localization/</a>.

<?php
manual_footer();
?>
