<?php
require_once 'castle_engine_functions.php';
castle_header('Text and fonts');

$toc = new TableOfContents(
  array(
    new TocItem('Show text using a label in user interface: TCastleLabel', 'label'),
    new TocItem('Show (potentially 3D) text in a viewport: TCastleText', 'text_in_viewport'),
    new TocItem('Customizing font', 'custom_font'),
    new TocItem('Change default font', 'default_font'),
    new TocItem('International characters', 'international'),
    new TocItem('Localization (translation) using CastleLocalizationGetText', 'localization_gettext'),
    new TocItem('Deprecated', 'deprecated'),
    new TocItem('Explicitly drawing text', 'draw_font', 1),
    new TocItem('Embed font data in compiled application using texture-font-to-pascal', 'embed_font', 1),
    new TocItem('Localization (translation) using CastleLocalization', 'localization_custom', 1),
  )
);
?>

<?php echo $toc->html_toc(); ?>

<?php
echo cgeImg('block', array(
  array('filename' => 'fonts_editor.png', 'titlealt' => 'Various fonts in CGE editor'),
  array('filename' => 'fonts_runtime.png', 'titlealt' => 'Various fonts'),
));
?>

<?php echo $toc->html_section(); ?>

<p>The most comfortable way to show text is to use <?php echo cgeRef('TCastleLabel'); ?>. You can customize it's font using <?php echo cgeRef('TCastleUserInterfaceFont.CustomFont'); ?> and <?php echo cgeRef('TCastleUserInterfaceFont.FontSize'); ?> properties.

<p>Many UI controls (see for example unit <?php echo cgeRef('CastleControls'); ?>) descend from <?php echo cgeRef('TCastleUserInterfaceFont'); ?>, and thus can render text and have their font customized, for example <?php echo cgeRef('TCastleButton'); ?>.

<p>You can add and configure UI controls (like <?php echo cgeRef('TCastleLabel'); ?>, <?php echo cgeRef('TCastleButton'); ?> and many more) by code, or using <a href="manual_editor.php">the CGE editor</a>.

<?php echo $toc->html_section(); ?>

<p>Another way to render text is to use <?php echo cgeRef('TCastleText'); ?>. You can place such component in a <a href="viewport_and_scenes">viewport</a> and it can be transformed just like any other 3D object.

<p>This is a great way to display text in 3D and/or attach the text to some 3D or 2D game object, e.g. as a debug text over some character.

<p>The properties of <?php echo cgeRef('TCastleText'); ?> and <?php echo cgeRef('TCastleLabel'); ?> are deliberately very similar. They also use the same font classes underneath.

<?php echo $toc->html_section(); ?>

<p>You can add and configure fonts. To customize font in the <a href="https://castle-engine.io/manual_editor.php">CGE editor</a>:

<ol>
  <li>
    <p>Use the "Design -&gt; Add Non-Visual Component" menu item, or the context menu (when you right-click in the hierarchy on the left). Font is added as <i>"Non-Visual Component"</i> to whatever parent you selected.

     <p>The parent that keeps reference to the font node can be anything -- <code>TCastleComponent</code>, <code>TCastleTransform</code>, <code>TCastleUserInterace</code> etc. It is similar to VCL/LCL non-visual components: it doesn't really matter where you drop them on the form.

  <li>
    <p>Then assign this font to <?php echo cgeRef('TCastleUserInterfaceFont.CustomFont', 'TCastleLabel.CustomFont'); ?>. Or any other UI control descending from <?php echo cgeRef('TCastleUserInterfaceFont', 'TCastleUserInterfaceFont'); ?>, like <?php echo cgeRef('TCastleButton', 'TCastleButton'); ?> or <?php echo cgeRef('TCastleEdit', 'TCastleEdit'); ?>.

    <p>You can also assign font to <?php echo cgeRef('TCastleText.CustomFont', 'TCastleText.CustomFont'); ?> to customize font used by the (potentially 3D) <?php echo cgeRef('TCastleText'); ?> in the viewport.
</ol>

<p>Our most important font classes:

<ul>
  <li><p><?php echo cgeRef('TCastleAbstractFont', 'TCastleAbstractFont'); ?>
  <li><p><?php echo cgeRef('TCastleFont', 'TCastleFont'); ?> - the most often used font class, loads font from TTF, OTF and other file formats.
    <p>Most important properties:
    <ul>
      <li><p><?php echo cgeRef('TCastleFont.URL', 'URL'); ?> (font file URL, e.g. TTF or OTF),
      <li><p><?php echo cgeRef('TCastleFont.OptimalSize', 'OptimalSize'); ?>,
      <li><p><?php echo cgeRef('TCastleFont.AntiAliased', 'AntiAliased'); ?>,
      <li><p><?php echo cgeRef('TCastleFont.LoadCharacters', 'LoadCharacters'); ?>,
      <li><p><?php echo cgeRef('TCastleFont.LoadBasicCharacters', 'LoadBasicCharacters'); ?> to control the loaded font.
    </ul>
  <li><p><?php echo cgeRef('TCastleBitmapFont', 'TCastleBitmapFont'); ?> - define letters using an image.
    <p>Most important properties:
    <ul>
      <li><p><?php echo cgeRef('TCastleBitmapFont.ImageUrl', 'ImageUrl'); ?>,
      <li><p><?php echo cgeRef('TCastleBitmapFont.ImageColumns', 'ImageColumns'); ?>,
      <li><p><?php echo cgeRef('TCastleBitmapFont.ImageRows', 'ImageRows'); ?>,
      <li><p><?php echo cgeRef('TCastleBitmapFont.ImageMargin', 'ImageMargin'); ?>,
      <li><p><?php echo cgeRef('TCastleBitmapFont.DisplayMargin', 'DisplayMargin'); ?>.
    </ul>
  <li><p><?php echo cgeRef('TCastleFontFamily', 'TCastleFontFamily'); ?> - a collection of other fonts to provide bold/italic variants, useful with rich text in <?php echo cgeRef('TCastleLabel.Html'); ?>.
</ul>

<p>See examples, e.g. <a href="https://github.com/castle-engine/castle-engine/tree/master/examples/fonts/text_tests">examples/fonts/text_tests</a> for demos of it.

<?php echo $toc->html_section(); ?>

<p>You can define a <code>default_font</code> inside the <a href="manual_castle_settings.php">CastleSettings.xml</a> file to change the default font. This way CGE editor will also use the new font as default.

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

<?php
echo cgeImg('block', array(
  array('filename' => 'escape_universe_japanese.jpg', 'titlealt' => 'Escape from the Universe (Switch) - Japanese edition'),
));
?>

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

<?php echo $toc->html_section(); ?>

<p><b>NOTE: While this functionality is still available, we advise to rather render all text using  <?php echo cgeRef('TCastleLabel'); ?> (in UI) or <?php echo cgeRef('TCastleText'); ?> (in viewport, maybe in 3D).</b>

<p>Instead of using <?php echo cgeRef('TCastleLabel'); ?>, you can explicitly draw the text. For this you need an instance of the <?php echo cgeRef('TCastleAbstractFont'); ?> class. To make it easy, one global instance of this class is already created for you: <?php echo cgeRef('UIFont'); ?> (part of <?php echo cgeRef('CastleControls'); ?> unit). So you can simply draw text like this:

<?php echo pascal_highlight(
'UIFont.Print(10, 10, Yellow, \'Some text to print\');'); ?>

<p>You should place such drawing code inside a render method, for example inside the  <?php echo cgeRef('TCastleWindow.OnRender'); ?> or  <?php echo cgeRef('TCastleControl.OnRender'); ?> or inside the overridden <?php echo cgeRef('TCastleUserInterface.Render'); ?> implementation. See <?php echo a_href_page('the manual about 2D drawing', 'manual_2d_ui_custom_drawn'); ?> for a general info about 2D rendering.

<p><?php echo cgeRef('TCastleAbstractFont'); ?> class has a lot of methods and properties.
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

<p><b>NOTE: While this functionality is still available, we advise to rather load fonts from TTF / OTF. It is more flexible.</b>

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

<p><b>NOTE: This localization approach is deprecated, as it has less features than the GetText approach</b>.

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

<p>This localization approach is deprecated.
<?php echo cgeRef('CastleLocalizationGetText'); ?> offers more features:

<ul>
  <li>Translating <code>resourcestrings</code> (so the constant strings in code are "magically" translated),
  <li><?php echo cgeRef('GenerateGetTextPo'); ?> (generating translation template),
  <li><?php echo cgeRef('TranslateAllDesigns'); ?> (automatic translation of all deserialized components),
  <li>uses <?php echo cgeRef('TCastleComponent.TranslateProperties'); ?> (mutiple translatable properties on a component are possible).
</ul>

<?php
castle_footer();
?>
