<?php
require_once 'castle_engine_functions.php';
manual_header('Customize look by CastleSettings.xml');

$toc = new TableOfContents(
  array(
    new TocItem('Description', 'description'),
    new TocItem('Example', 'example'),
  )
);
?>

<?php echo $toc->html_toc(); ?>

<?php echo $toc->html_section(); ?>

<p>You can place a file named <code>CastleSettings.xml</code> in your
<a href="manual_data_directory.php">data directory</a>
to influence the look of your application.
From the Pascal code (usually, early in the
<code>Application.OnInitialize</code> callback) load it by calling <code>Window.Container.LoadSettings('castle-data:/CastleSettings.xml');</code>.

<p>The <a href="manual_editor.php">Castle Game Engine Editor</a> will also
automatically load this file. This way the editor can show (at design-time)
your application in the same way as it will appear to the player.

<p>TODO:
In the future, the <code>CastleSettings.xml</code> file should be editable by
the CGE editor GUI, for now you just have to edit it directly.
I expect that this file will also get more features in the future.

<p>The <a href="https://castle-engine.io/apidoc-unstable/html/CastleUIControls.TUIContainer.html#LoadSettings">LoadSettings</a> call sets

<ul>
  <li><p><a href="https://castle-engine.io/apidoc-unstable/html/CastleUIControls.TUIContainer.html#UIScaling">Container.UIScaling</a>,
     <a href="https://castle-engine.io/apidoc-unstable/html/CastleUIControls.TUIContainer.html#UIReferenceWidth">Container.UIReferenceWidth</a>,
     <a href="https://castle-engine.io/apidoc-unstable/html/CastleUIControls.TUIContainer.html#UIReferenceHeight">Container.UIReferenceHeight</a>.

    <p>These settings control <i>user interface scaling</i>.
    See the <a href="https://castle-engine.io/apidoc-unstable/html/CastleUIControls.TUIContainer.html#UIScaling">UIScaling</a>
    documentation and <a href="manual_2d_user_interface.php">manual page about 2D user interface</a>
    for an explanation what is this.

    <p>The default value of <code>UIScaling</code> is <code>usNone</code>,
    so no scaling is done.
    If you don't specify <code>&lt;ui_scaling&gt;</code> element in <code>CastleSettings.xml</code>,
    we keep using this default.

    <p>However, we advise all new cross-platform projects to use some <code>UIScaling</code>.
    This is the best practical way to achieve a consistent look
    on various screen sizes and devices.
    It is useful even for desktop-only applications (since people have
    various monitor resolutions), and it is crucial for mobile applications
    (where devices have wildly different resolutions).

    <p>The <i>"New Project"</i> templates provided by the CGE editor
    all set up by default UI scaling to the reference sizes of 1600x900
    (the most popular aspect ratio in 2018).

  <li><p><a href="https://castle-engine.io/apidoc-unstable/html/CastleUIControls.TUIContainer.html#DefaultFont">Container.DefaultFont</a>.

    <p>This controls the default font look (font file, font size)
    for all user-interface controls.
    Note that each control can customize it
    (using <a href="https://castle-engine.io/apidoc-unstable/html/CastleControls.TCastleUserInterfaceFont.html#CustomFont">TCastleUserInterfaceFont.CustomFont</a>,
    <a href="https://castle-engine.io/apidoc-unstable/html/CastleControls.TCastleUserInterfaceFont.html#FontSize">TCastleUserInterfaceFont.FontSize</a>).

</ul>

<?php echo $toc->html_section(); ?>

<?php echo xml_highlight(
'<?xml version="1.0" encoding="utf-8"?>
<castle_settings>
  <!--
    UI scaling settings.
    The allowed mode values are
    - None,
    - EncloseReferenceSize,
    - FitReferenceSize,
    - ExplicitScale,
    - DpiScale.
  -->
  <ui_scaling
    mode="EncloseReferenceSize"
    reference_width="1920"
    reference_height="1080"
  />

  <!--
    Default font.

    Properties size, size_at_load, anti_aliased are optional.
    Their default values are shown below.

    The difference between size and size_at_load:

    - `size` determines the `TCastleFont.Size`,
      it determines how large the font is on the screen.

    - `sizes_at_load`, if defined, should be a list of font sizes
      (integers separated by whitespace).
      The font sizes should represent a spectrum of sizes you plan
      to use in your application, e.g. "10 15 20 25".

      Internally, the font is loaded into a number of textures,
      one for each size, using TCustomizedFont.Load that creates
      a number of TTextureFont instances.
      When rendering using this font, the closest size will be used,
      and scaled to the requested size, to be rendered exactly at
      requested size.

      This gives greatest font quality.

    - `size_at_load` is the font size used to create an internal texture
      with letters. By default it is equal to `size`,
      but it can be set to something larger to improve the quality of the font.
      This is useful if in your game you will often use this font
      with other sizes.
      (E.g. your controls leave `TCastleUserInterfaceFont.CustomFont = nil`,
      but often use large `TCastleUserInterfaceFont.FontSize` or
      `TCastleUserInterfaceFont.FontScale`).

      This is used only when the list `sizes_at_load` is empty.

      Internally, one TTextureFont instance will be created,
      for given size.
      When rendering it will be scaled to the requested size.
      If you try to render font much smaller or much larger than
      the loaded size, results may look aliased or blurry.
  -->
  <default_font
    url="castle-data:/MyFontFile.ttf"
    size="20"
    sizes_at_load="10 20 30"
    anti_aliased="true"
  />
</castle_settings>'); ?>

<?php
manual_footer();
?>
