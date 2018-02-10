<?php
require_once 'castle_engine_functions.php';
castle_header("License", array(
  'path' => array('documentation'),
  'social_share_image' => CURRENT_URL . 'images/castle_game_engine_icon.png',
));

echo '<img src="images/castle_game_engine_icon.png"
  alt="Castle Game Engine icon"
  class="engine-icon" />';

echo pretty_heading($page_title);
?>

<p>The whole engine and all related programs' sources are licensed on terms of <a href="http://www.gnu.org/licenses/gpl.html">GNU General Public License (version 2 or above)</a>. See <a href="http://www.gnu.org/">www.gnu.org</a> for more information about this license (including translations of it to various languages) and philosophy of free software.</p>

<p>Moreover, the core of the engine is also alternatively available under the more permissive <a href="http://www.gnu.org/copyleft/lesser.html">GNU Lesser General Public License (again version 2 or above)</a> with the so-called "static linking exception". The idea of this exception is to allow statically linking with the engine on the same terms as dynamically linking. (<i>Static linking</i> is what normally happens when you compile a program using my units, without wrapping them in a DLL / Delphi runtime package.)</p>

<p>All this basically means that you have to share your modifications <i>to the engine</i>, and you can use the engine in closed-source programs.</p>

<p>The precise legal text of the "static linking exception" follows (it's the same as used by <a href="http://www.freepascal.org/faq.var#general-license">Free Pascal Runtime Library</a> and many other projects):</p>

<p style="margin-left: 2em; background: #EEE;">
As a special exception, the copyright holders of this library give you permission to link this library with independent modules to produce an executable, regardless of the license terms of these independent modules, and to copy and distribute the resulting executable under terms of your choice, provided that you also meet, for each linked independent module, the terms and conditions of the license of that module. An independent module is a module which is not derived from or based on this library. If you modify this library, you may extend this exception to your version of the library, but you are not obligated to do so. If you do not wish to do so, delete this exception statement from your version.
</p>

<p>Important notes:</p>
<ul>
  <li><p>If you want to use the engine on LGPL terms (as opposed to more strict GPL) you <i>must</i> compile the engine with <code>CASTLE_ENGINE_LGPL</code> symbol defined in file <code>castle_game_engine/base/castleconf.inc</code>. Just put there <code>{$define CASTLE_ENGINE_LGPL}</code> line (or simply remove the beginning space in already prepared comment <code>{&nbsp;$define CASTLE_ENGINE_LGPL}</code>).</p>

    <p>This is necessary to avoid pulling in GPL-only dependencies. For now, this is only the NURBS unit (uses GPL-only code from <a href="http://wdune.ourproject.org/">White_dune</a>). This missing NURBS implementation is the only difference between LGPL and "strict GPL" engine version.</p></li>

  <li><p>Note that LGPL stuff concerns only the engine, i.e. things inside <code>castle_game_engine</code> archive. The rest of the programs (<code>view3dscene</code>, <code>castle</code> etc.) are still strict GPL.</p></li>
</ul>

<?php
  castle_footer();
?>
