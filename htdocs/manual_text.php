<?php
require_once 'castle_engine_functions.php';
castle_header('Text and fonts');

$toc = new TableOfContents(
  array(
    new TocItem('Show text using a label UI control (TCastleLabel)', 'label'),
    new TocItem('Explicitly draw text (TCastleFont)', 'font'),
    new TocItem('Create a new font', 'create'),
    new TocItem('International characters', 'international'),
    new TocItem('Localization (translation) using CastleLocalizationGetText', 'localization_gettext'),
    new TocItem('Localization (translation) using CastleLocalization (deprecated)', 'localization_custom'),
  )
);
?>

<?php echo $toc->html_toc(); ?>
<?php echo $toc->html_section(); ?>

<p>The most comfortable way to show text is to use
 <?php echo cgeRef('TCastleLabel'); ?>.
 You can customize it's font using
 <?php echo cgeRef('TCastleUserInterfaceFont.CustomFont'); ?>
 and
 <?php echo cgeRef('TCastleUserInterfaceFont.FontSize'); ?>
 properties.

<p>Many UI controls (see for example unit
 <?php echo cgeRef('CastleControls'); ?>)
 descend from
 <?php echo cgeRef('TCastleUserInterfaceFont'); ?>,
 and thus can render text and have their font customized, for example
 <?php echo cgeRef('TCastleButton'); ?>.

<p>You can add and configure UI controls (like <?php echo cgeRef('TCastleLabel'); ?>,
 <?php echo cgeRef('TCastleButton'); ?>
 and many more)
 by code, or using <a href="manual_editor.php">the CGE editor</a>.

<?php echo $toc->html_section(); ?>

<p>Instead of using
<?php echo cgeRef('TCastleLabel'); ?>,
you can explicitly draw the text.
For this you need an instance of the
<?php echo cgeRef('TCastleAbstractFont'); ?> class.
To make it easy, one global instance of this class is already created for you:
<?php echo cgeRef('UIFont'); ?>
 (part of <?php echo cgeRef('CastleControls'); ?> unit).
So you can simply draw text like this:

<?php echo pascal_highlight(
'UIFont.Print(10, 10, Yellow, \'Some text to print\');'); ?>

<p>You should place such drawing code inside a render method,
for example inside the
 <?php echo cgeRef('TCastleWindow.OnRender'); ?> or
 <?php echo cgeRef('TCastleControl.OnRender'); ?> or
inside the overridden <?php echo cgeRef('TCastleUserInterface.Render'); ?> implementation.
See <?php echo a_href_page('the manual about 2D drawing', 'manual_2d_ui_custom_drawn'); ?> for a general info about 2D rendering.

<p><?php echo cgeRef('TCastleAbstractFont'); ?> class
has a lot of methods and properties.
<ul>
  <li>You can simply print the text
    (<?php echo cgeRef('TCastleAbstractFont.Print'); ?>).
  <li>You can change the font size
    (<?php echo cgeRef('TCastleAbstractFont.Size'); ?>).
  <li>You can add an outline around it
    (<?php echo cgeRef('TCastleAbstractFont.Outline'); ?>,
    <?php echo cgeRef('TCastleAbstractFont.OutlineColor'); ?>).
  <li>You can measure the text
    (<?php echo cgeRef('TCastleAbstractFont.TextWidth'); ?>,
    <?php echo cgeRef('TCastleAbstractFont.TextHeight'); ?>,
    <?php echo cgeRef('TCastleAbstractFont.TextSize'); ?>,
    <?php echo cgeRef('TCastleAbstractFont.RowHeight'); ?>...).
  <li>You can print a multi-line text, with optional line wrapping
    (<?php echo cgeRef('TCastleAbstractFont.PrintRect'); ?>,
    <?php echo cgeRef('TCastleAbstractFont.PrintRectMultiline'); ?>,
    <?php echo cgeRef('TCastleAbstractFont.PrintStrings'); ?>,
    <?php echo cgeRef('TCastleAbstractFont.PrintBrokenString'); ?>
    and other methods).
</ul>

<?php echo $toc->html_section(); ?>

<p><?php echo cgeRef('TCastleAbstractFont'); ?> is
an abstract class representing some font that can be drawn.
To create a new font, you create an instance of a non-abstract class,
most often the <?php echo cgeRef('TCastleFont'); ?>
 class &mdash; it draws font glyphs from a texture,
 and can be loaded from a font file (TTF, OTF).
There are other possible font implementations, for example
<?php echo cgeRef('TCastleBitmapFont'); ?>
 allows to use a font drawn on an image (so you can make colorful letters,
with fancy custom outline and such).

<p>See <code>castle_game_engine/examples/fonts/font_from_texture.lpr</code>
for a simple example of creating fonts.
In the basic version, you simply use
<?php echo cgeRef('TCastleFont.Load'); ?>
 to load a font from a file (TTF, OTF or any other font format supported by the
FreeType2 library). So you construct and load a font like this:

<?php echo pascal_highlight(
'MyNewFont := TCastleFont.Create(Application { any TComponent to act as owner });
MyNewFont.Load(\'castle-data:/MyFontFile.ttf\', 20, true);'); ?>

<p>Remember to install the FreeType2 library for this to work. On Windows,
place appropriate FreeType2 DLL alongside the exe, you can get the DLL
from <code>castle_game_engine/tools/build-tool/data/external_libraries/</code> directory
of the engine.

<p>You can assign the new font as the global <code>UIFont</code>,
so it will be by default used by all standard UI controls:

<?php echo pascal_highlight(
'UIFont := MyNewFont;'); ?>

<p>(Instead of assigning to the <code>UIFont</code>,
you can assign to <code>Container.DefaultFont</code>
or define a <code>default_font</code> inside the
<a href="manual_castle_settings.php">CastleSettings.xml</a> file.
This way CGE editor will also use the new font.)

<p>Instead of loading the font data from a file, you can also provide
a <?php echo cgeRef('TTextureFontData'); ?>
 instance to the <?php echo cgeRef('TCastleFont'); ?>
 constructor. This allows to create the font data at runtime
 or <b>to use the font data embedded in a Pascal source code</b>.
You can use the <code>texture-font-to-pascal</code> program (compile it from
<code>castle_game_engine/tools/texture-font-to-pascal/texture-font-to-pascal.lpr</code>)
to convert a font file into a Pascal unit:

<pre>
texture-font-to-pascal --size 20 MyFontFile.ttf
</pre>

<p>In response, it will create a unit called
<code>CastleTextureFont_MyFontFile_20</code> with a public function:

<?php echo pascal_highlight(
'function TextureFont_MyFontFile_20: TTextureFontData;'); ?>

<p>You can use this unit in your program, and create a font instance like this:

<?php echo pascal_highlight(
'MyNewFont := TCastleFont.Create(Application { any TComponent to act as owner });
MyNewFont.Load(TextureFont_MyFontFile_20);'); ?>

<p>The advantages of embedding a font inside a Pascal unit are:

<ul>
  <li>You don't need to distribute the FreeType2 library. (Although this shouldn't be a big problem,
    CGE can package FreeType2 with your project for all platforms automatically.)

  <li>Font is loaded slightly faster, since it's already processed to
    a suitable texture data.
</ul>

<p>The disadvantages are of course that you cannot simply change the font file anymore,
you need to rerun the <code>texture-font-to-pascal</code> command
and recompile your program to see the new font.

<?php echo $toc->html_section(); ?>

<?php
echo castle_thumbs(array(
  array('filename' => 'test_local_characters.png', 'titlealt' => 'Testing local (international) characters'),
));
?>

<p>(<b>A complete program using the concepts discussed below is in the engine examples</b>,
in the <a href="https://github.com/castle-engine/castle-engine/tree/master/examples/fonts/test_local_characters">examples/fonts/test_local_characters/</a>.
 The main code is in <a href="https://github.com/castle-engine/castle-engine/blob/master/examples/fonts/test_local_characters/gameinitialize.pas">gameinitialize.pas</a> unit there.
Check it out!)

<p>All font routines (printing, measuring) expect
the international characters to be encoded using UTF-8.
To draw the international characters (anything beyond basic English ASCII set)
you also need to create a font with these characters.

<p>To <?php echo cgeRef('TCastleFont'); ?>
 provide a list
of the characters (including all the possible international characters)
that you want to display. Like this:

<?php echo pascal_highlight(
'uses ..., CastleFonts, CastleStringUtils;

function CreateMyFont: TCastleFont;
begin
  Result := TCastleFont.Create(nil);
  { below is a string containing all my international chars, in UTF-8 }
  Result.LoadCharacters := SimpleAsciiCharacters + \'你好世界ΓειασουκόσμεЗдравствуймир\';
  Result.OptimalSize := 20;
  Result.Url := \'castle-data:/MyFontFile.ttf\';
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

<p>You can use the <?php echo cgeRef('CastleLocalizationGetText'); ?> for a localization approach based on GetText.

<p>You use standard GetText formats for translating (PO, MO) and utilizing GetText tools like <a href="https://poedit.net/">PoEdit</a>. You can automatically translate strings in Pascal code, declared as <code>resourcestring</code> (use <?php echo cgeRef('CastleTranslateResourceStrings'); ?>). You can automatically translate user interface (use <?php echo cgeRef('TranslateAllDesigns'); ?>). You can generate starting file to translate user interface (use <?php echo cgeRef('GenerateGetTextPo'); ?>).

<p>A typical workflow for translating an application looks like this:

<ol>
  <li><p>Generate POT (PO Template) files containing everything to translate:

    <ul>
      <li><p>Generate POT file to translate the user interface
        by calling <code>GenerateGetTextPo('castle-data:/gui/*.castle-user-interface');</code>.
        Place the resulting contents in <code>user_interface.pot</code>.

      <li><p>Generate POT file to translate all resourcestrings
        using the <code>rstconv</code> tool from FPC.
        Place the resulting contents in <code>game.pot</code>.
     </ul>

  <li><p>Create and translate the PO files for each language you support.

    <p>For each <code>xxx.pot</code>,
    create a file <code>xxx.&lt;language-code&gt;.po</code>.
    For example for Polish translation you would create files like
    <code>game.pl.po</code> and
    <code>user_interface.pl.po</code>.
    For Japanese translation you would create files
    <code>game.ja.po</code> and
    <code>user_interface.ja.po</code>.

    <p>Note that <code>game.xx.po</code> should contain
    a map from English text -&gt; localized (Polish, Japanese etc.) text.
    In contrast, <code>user_interface.xx.po</code> should contain
    a map from internal identifier (qualified component names) -&gt; localized (Polish, Japanese etc.) text.
    Both approaches are possible with GetText.

    <p>You can create and edit PO files using any GetText PO editor,
    like <a href="https://poedit.net/">PoEdit</a>.
    The PO is a text file format, so you can use any regular text editor
    (like Atom or Emacs) as well.

  <li><p>Generate MO files from PO using the GetText <code>msgfmt</code> tool.
    Some editors like <a href="https://poedit.net/">PoEdit</a> may also do this automatically.

    <p>In effect you will get
    <code>game.pl.mo</code>,
    <code>user_interface.pl.mo</code> (Polish translation) and
    <code>game.ja.mo</code>,
    <code>user_interface.ja.mo</code> (Japanese translation).

    <p>Place these MO files inside the <a href="manual_data_directory.php">data directory</a>
    of your application.

  <li><p>In game, determine user preferred language, e.g. using
    <?php echo cgeRef('CastleSystemLanguage'); ?>.

    <p>Then translate things by loading appropriate MO file.

    <ul>
      <li><p>To translate all user interface that will be loaded,
        call <code>TranslateAllDesigns('castle-data:/translations/user_interface.ja.mo');</code>
        (where "ja" stands for Japanese localization, just an example).

      <li><p>To translate resourcestrings, call
        <code>CastleTranslateResourceStrings('castle-data:/translations/game.ja.mo');</code>.
    </ul>
</ol>

<p>For more details and example how to do it all see the README and source code of our
<a href="https://github.com/castle-engine/castle-engine/tree/master/examples/localization/gettext">example application using CastleLocalizationGetText (examples/localization/gettext/)</a>.

<p>You can tweak this workflow to your needs by using various other routines from
<?php echo cgeRef('CastleLocalizationGetText'); ?>
 unit and overriding <?php echo cgeRef('TCastleComponent.TranslateProperties'); ?>.
You can use more POT / PO files for your own needs.
You can translate strings explicitly at any moment, using
<a href="https://www.freepascal.org/docs-html/fcl/gettext/tmofile.translate.html">TMOFile.Translate('my_id')</a>.

<p>The engine uses resourcestrings for some internally-generated messages, so these can be translated too.

<?php echo $toc->html_section(); ?>

<p>You can use our own localization approach from the
<a href="https://github.com/castle-engine/castle-engine/blob/master/src/deprecated_units/castlelocalization.pas">CastleLocalization</a>
<?php /* api_link('CastleLocalization', 'CastleLocalization.html'); */ ?>
 unit. It can read from a number of translation formats (XML, JSON, CSV, GetText MO). It can translate user-interface controls, like <?php echo cgeRef('TCastleLabel'); ?>. The demo is inside <a href="https://github.com/castle-engine/castle-engine/tree/master/examples/localization/custom">examples/localization/custom/</a>.

<p>For advanced users, the system allows to aid in localizing your custom classes too (see
<?php /* api_link('OnUpdateLocalization', 'CastleLocalization.TCastleLocalization.html#OnUpdateLocalization'); */ ?>
<code>OnUpdateLocalization</code>)
and to add your own translation formats (see
<?php /* api_link('FileLoader', 'CastleLocalization.TCastleLocalization.html#FileLoader'); */ ?>
<code>FileLoader</code>).

<p>As with GetText approach, you can use a cross-platform <?php echo cgeRef('CastleSystemLanguage'); ?> unit that tells you the preferred user language. You can also translate strings "explicitly" using the
<?php /* api_link('Localization.Items[\'my_id\']', 'CastleLocalization.TCastleLocalization.html#Items'); */ ?>
 <code>Localization.Items['my_id']</code>
 in CastleLocalization.

<p><i>Thousand thanks go to Benedikt Magnus for developing this approach!</i>

<p>It is deprecated, as it has (for now) less features than the GetText approach.
<?php echo cgeRef('CastleLocalizationGetText'); ?> offers these additional features:

<ul>
  <li>Translating <code>resourcestrings</code> (so the constant strings in code are "magically" translated),
  <li><?php echo cgeRef('GenerateGetTextPo'); ?> (generating translation template),
  <li><?php echo cgeRef('TranslateAllDesigns'); ?> (automatic translation of all deserialized components),
  <li>uses <?php echo cgeRef('TCastleComponent.TranslateProperties'); ?> (mutiple translatable properties on a component are possible).
</ul>

<?php
castle_footer();
?>
