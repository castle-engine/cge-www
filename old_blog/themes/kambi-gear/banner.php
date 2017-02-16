<?php
	global $options;
	foreach ($options as $value) {
	    if (get_settings( $value['id'] ) === FALSE) { $$value['id'] = $value['std']; } else { $$value['id'] = get_settings( $value['id'] ); } }
?>
<?php if ($gear_banner_visibility == "on") { ?>
					<div class="banner">
						<div class="paddings">
<?php
/* Kambi-
  <img src="< ?php bloginfo('stylesheet_directory'); ? >/images/ico.big/< ?php echo $gear_banner_image; ? >" alt="Gear"></img>
*/ 
?>
							<?php echo $gear_banner_text; ?>
						</div>
					</div>
<?php } ?>
