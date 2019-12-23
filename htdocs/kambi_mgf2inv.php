<?php
require_once "castle_engine_functions.php";
castle_header("kambi_mgf2inv", array(
  'meta_description' => 'Modified version of mgf2inv converter, this converts mgf files to VRML / Inventor and adds non-standard fields describing physical properties of materials.',
));
?>

<?php echo pretty_heading("kambi_mgf2inv"); ?>

<p><code>kambi_mgf2inv</code> is just a slightly modified version
of the <code>mgf2inv</code> program by Greg Ward. <code>mgf2inv</code> converts
files in <a href="http://floyd.lbl.gov/mgf/">MGF (Materials and Geometry Format,
also developed by Greg Ward)</a> format into VRML 1.0 models (or Inventor
1.0 or 2.0).

<p>My <code>kambi_mgf2inv</code> writes
<?php echo a_href_page_hashlink("six additional fields of node <code>Material</code>
that describe physical material properties for Phong BRDF",
"x3d_extensions", "ext_material_phong_brdf_fields"); ?>.
These properties are already available in MGF (with the exception of two
exponents, <code>reflSpecularExp</code> and <code>transSpecularExp</code>,
they have to be calculated from MGF's <code>roughness</code>)
so writing these values into VRML was not a problem. It allowed me to test
<?php echo a_href_page("rayhunter", "rayhunter"); ?> &mdash; raytracer based on
VRML models &mdash; on existing MGF models.
All I had to do was to convert MGF models with this program.

<p><code>kambi_mgf2inv</code> fixes also a small bug in <code>mgf2inv</code>,
non-convex faces
were sometimes marked as convex with <code>mgf2inv</code>. (This is already fixed
in the current Radiance version (current, i.e. experimental, not official;
downloadable from <a href="http://www.radiance-online.org/">
http://www.radiance-online.org/</a>) so soon it will be fixed in official
mgf2inv version too)

<p>Program:
<ul>
  <li><?php echo current_www_a_href_size("Sorce code in ANSI C", "kambi_mgf2inv.c.gz"); ?>.
    If you want to compile this you need the rest of mgflib's sources :
    <a href="http://floyd.lbl.gov/mgf/">here is a page with MGF documentation
    and sources and documentation for MGF parser and a few MGF converters,
    in particular mgf2inv</a>
  <li><?php echo current_www_a_href_size("kambi_mgf2inv compiled for Linux",
    "kambi_mgf2inv.gz"); ?>
  <!--li> < ?php echo current_www_a_href_size("kambi_mgf2inv skompilowany pod Windowsa",
    "kambi_mgf2inv.zip"); ?-->
</ul>

<!-- p>If you are curious what "kambi" means - it is just an abbreviation of my
name... -->

<?php
  castle_footer();
?>
