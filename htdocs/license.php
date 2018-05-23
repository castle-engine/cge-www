<?php
require_once 'castle_engine_functions.php';
castle_header("License", array(
  'path' => array('documentation'),
  'social_share_image' => page_url('images/castle_game_engine_icon.png'),
));

echo '<img src="images/castle_game_engine_icon.png"
  alt="Castle Game Engine icon"
  class="engine-icon" />';

echo pretty_heading($page_title);
?>

<p><i>Castle Game Engine</i> is available on terms of GPL or LGPL (your choice):

<ul>
  <li><p>You can use <a href="http://www.gnu.org/licenses/gpl.html">GNU General Public License (version 2 or above)</a>. See <a href="http://www.gnu.org/">www.gnu.org</a> for more information about this license (including translations of it to various languages) and philosophy of free software.</p>

  <li><p>Or you can use more permissive <a href="http://www.gnu.org/copyleft/lesser.html">GNU Lesser General Public License (again version 2 or above)</a> with the <i>"static linking exception"</i>. The idea of this exception is to allow statically linking with the engine on the same terms as dynamically linking. <i>Static linking</i> is what usually happens when you compile a program using the engine units (without wrapping the engine in a dynamic library (dll, so, dylib) or Delphi runtime package).</p>

    <p>All this basically means that <b>you can use the engine in closed-source programs, you only have to share your modifications <i>to the engine units</i></b>.</p>

    <p>The same license is used by <a href="http://www.freepascal.org/faq.var#general-license">FPC (Free Pascal Compiler) Runtime Library</a>, <a href="http://www.lazarus-ide.org/">Lazarus Component Library (LCL)</a> and many other projects.</p>

    <p>The precise legal text of the "static linking exception" is this:</p>

    <p style="margin-left: 2em; font-style: italic">
    As a special exception, the copyright holders of this library give you permission to link this library with independent modules to produce an executable, regardless of the license terms of these independent modules, and to copy and distribute the resulting executable under terms of your choice, provided that you also meet, for each linked independent module, the terms and conditions of the license of that module. An independent module is a module which is not derived from or based on this library. If you modify this library, you may extend this exception to your version of the library, but you are not obligated to do so. If you do not wish to do so, delete this exception statement from your version.
    </p>

    <p>Note that if you want to use the engine on LGPL terms (as opposed to more strict GPL) you <i>must</i> compile the engine with <code>CASTLE_ENGINE_LGPL</code> symbol defined:
      <ul>
        <li>You can define it e.g. in file <code>castle_game_engine/base/castleconf.inc</code>. Just put there <code>{$define CASTLE_ENGINE_LGPL}</code> line (or simply remove the beginning space in already prepared comment <code>{&nbsp;$define CASTLE_ENGINE_LGPL}</code>).
        <li>Or you can define <code>CASTLE_ENGINE_LGPL</code> in the compilation options of CGE Lazarus packages.</li>
        <li>Or you can define <code>CASTLE_ENGINE_LGPL</code> in the <a href="https://github.com/castle-engine/castle-engine/wiki/CastleEngineManifest.xml-examples#compiler-options-and-paths">compilation options in CastleEngineManifest.xml</a>
      </ul>

    <p>This is necessary to avoid pulling in GPL-only dependencies. For now, this is only the NURBS unit (uses GPL-only code from <a href="http://wdune.ourproject.org/">White_dune</a>). This missing NURBS implementation is the only difference between LGPL and "strict GPL" engine version.</p></li>
</ul>

<p>The tools and demos related to the <i>Castle Game Engine</i>, but available in a different repository than <a href="https://github.com/castle-engine/castle-engine/">https://github.com/castle-engine/castle-engine/</a>, and available only on terms of strict GPL license. This concerns e.g.

<ul>
  <li><a href="view3dscene.php">view3dscene</a>
  <li><a href="darkest_before_dawn.php">"Darkest Before the Dawn" demo</a>
  <li><a href="https://www.patreon.com/posts/wyrd-forest-demo-15811244">"Wyrd Forest" demo</a>
</ul>

<?php
  castle_footer();
?>
