<?php

if ( function_exists('register_sidebar') ) {
    register_sidebar(array(
		'1' => 'Wide',
        'before_widget' => '<div id="%1$s" class="%2$s">',
        'after_widget' => '</div>',
        'before_title' => '<h3>',
        'after_title' => '</h3>',
    ));
    
}

	/* -------------------------------------------------
				  SEARCH WIDGET
	-------------------------------------------------- */
	// define the widget
	function widget_search() {
		global $tag_widget_title;
?>
			<div class="widget_search">
				<h3><?php _e('Search', 'default'); ?></h3>
				<form action="<?php bloginfo('home'); ?>/" method="post">
					<div class="search-wrapper">
						<input type="text" value="" class="textfield" name="s" />
						<input type="submit" value="Find" class="button" />
					</div>
				</form>
			</div>


<?php
	}
	
	// if wp supports widgets, register it (make it available)
	if (function_exists('register_sidebar_widget'))
		register_sidebar_widget('Search', 'widget_search');

$themename = "Gear";
$shortname = "gear";
$dir = get_bloginfo ( 'template_directory' );
$options = array (
    array(  "name" => "Background",
            "id" => $shortname."_bg",
            "type" => "select",
            "std" => "#1E2022 url(" . $dir . "/images/bg/two-gears.gif) repeat scroll 0 0",
            "options" => array("Two gears (default)" => "#1E2022 url(" . $dir . "/images/bg/two-gears.gif) repeat scroll 0 0",
    		"Two gears (yellow)" => "#1E2022 url(" . $dir . "/images/bg/two-gears-yellow.gif) repeat scroll 0 0",
    		"Two gears (grayscale)" => "#e7e8e8 url(" . $dir . "/images/bg/two-gears-gray.gif) repeat scroll 0 0",
    		"One gear" => "#1E2022 url(" . $dir . "/images/bg/gear.gif) repeat scroll 0 0",
    		"Lines" => "#1E2022 url(" . $dir . "/images/bg/lines.gif) repeat scroll 0 0",
    		"Lines (grayscale)" => "#e7e8e8 url(" . $dir . "/images/bg/lines-gray.gif) repeat scroll 0 0",
    		"Lines (intensive)" => "#1E2022 url(" . $dir . "/images/bg/lines-intensive.gif) repeat scroll 0 0",
    		"Clover" => "#1E2022 url(" . $dir . "/images/bg/clover.gif) repeat scroll 0 0",
    		"Solid (light black)" => "#1E2022 none no-repeat scroll 0 0",
    		"Solid (grey)" => "#e7e8e8 none no-repeat scroll 0 0",
    		"Gradient (dark)" => "#1e2022 url(" . $dir . "/images/bg/gradient-dark.gif) repeat-x scroll 0 0",
    		"Gradient (grey)" => "#e3e4e4 url(" . $dir . "/images/bg/gradient-grey.gif) repeat-x scroll 0 0",
    		"Gradient (grey reversed)" => "#b9baba url(" . $dir . "/images/bg/gradient-grey-rev.gif) repeat-x scroll 0 0"
    		),
        ),
    array(  "name" => "Email's icon visibility",
            "id" => $shortname."_email_visibility",
            "type" => "select",
            "std" => "on",
            "options" => array("Show" => "on",
    		"Hide" => "off"
    		),
        ),
    array(  "name" => "Banner's visibility",
            "id" => $shortname."_banner_visibility",
            "type" => "select",
            "std" => "on",
            "options" => array("Show" => "on",
    		"Hide" => "off"
    		),
        ),
    array(  "name" => "Banner's image",
            "id" => $shortname."_banner_image",
            "type" => "select",
            "std" => "gear.png",
            "options" => array("Gear" => "gear.png",
    		"Bird" => "bird.png",
    		"Bulb" => "bulb.png",
    		"Earth" => "earth.png",
    		"User" => "user.png",
    		"User 2" => "user2.png",
    		"Binoculars" => "bino.png"
    		),
        ),
        array(  "name" => "Banner's text",
            "id" => $shortname."_banner_text",
            "type" => "textarea",
            "std" => "<h2>Here you could put some info!</h2>
    
<p>In order to edit this block, please go to the option page and edit the text. If you want to get rid of this banner, you may also do this through an options page.</p>
"
        )

);


foreach ($options as $value) {
    if (get_settings( $value['id'] ) === FALSE) { $$value['id'] = $value['std']; } else { $$value['id'] = get_settings( $value['id'] ); } }
    
    
function mytheme_add_admin() {

    global $themename, $shortname, $options;

    if ( $_GET['page'] == basename(__FILE__) ) {
    
        if ( 'save' == $_REQUEST['action'] ) {

                foreach ($options as $value) {
                    update_option( $value['id'], $_REQUEST[ $value['id'] ] ); }
                    

                foreach ($options as $value) {
                    if( isset( $_REQUEST[ $value['id'] ] ) ) { update_option( $value['id'], $_REQUEST[ $value['id'] ]  ); } else { delete_option( $value['id'] ); } }

                wp_redirect("themes.php?page=functions.php&saved=true");
                die;

        } else if( 'reset' == $_REQUEST['action'] ) {

            foreach ($options as $value) {
                delete_option( $value['id'] ); }

            header("Location: themes.php?page=functions.php&reset=true");
            die;

        }
    }
    add_theme_page($themename." Options", "Theme Options", 'edit_themes', basename(__FILE__), 'mytheme_admin');
}

function mytheme_admin() {

    global $themename, $shortname, $options;

    if ( $_REQUEST['saved'] ) echo '<div id="message" class="updated fade"><p><strong>'.$themename.' settings saved.</strong></p></div>';
    if ( $_REQUEST['reset'] ) echo '<div id="message" class="updated fade"><p><strong>'.$themename.' settings reset.</strong></p></div>';
    
?>
<div class="wrap">
<h2><?php echo $themename; ?> <?php _e('settings', 'default'); ?></h2>

<form method="post">

<table class="optiontable">

<?php foreach ($options as $value) { 
    
if ($value['type'] == "text") { ?>
        
<tr valign="top"> 
    <th scope="row"><?php echo $value['name']; ?>:</th>
    <td>
        <input name="<?php echo $value['id']; ?>" id="<?php echo $value['id']; ?>" type="<?php echo $value['type']; ?>" value="<?php if ( get_option( $value['id'] ) != "") { echo get_option( $value['id'] ); } else { echo $value['std']; } ?>" />
    </td>
</tr>

<?php } elseif ($value['type'] == "select") { ?>

    <tr valign="top"> 
        <th scope="row"><?php echo $value['name']; ?>:</th>
        <td>
            <select name="<?php echo $value['id']; ?>" id="<?php echo $value['id']; ?>">
                <?php foreach ($value['options'] as $key => $val) { ?>
                <option value="<?php echo $val; ?>"<?php if (get_option ( $value['id'] )) {if ( get_option( $value['id'] ) == $val) { echo ' selected="selected"'; }} elseif ($val == $value['std']) { echo ' selected="selected"'; } ?>><?php echo $key; ?></option>
                <?php } ?>
            </select>
        </td>
    </tr>

<?php } elseif ($value['type'] == "textarea") { ?>

	<tr valign="top"> 
	    <th scope="row"><?php echo $value['name']; ?>:</th>
	    <td>
	        <textarea cols="100" rows="10" name="<?php echo $value['id']; ?>" id="<?php echo $value['id']; ?>"><?php if ( get_option( $value['id'] ) != "") { echo get_option( $value['id'] ); } else { echo $value['std']; } ?></textarea>
	    </td>
	</tr>


<?php 
} 
}
?>

</table>

<p class="submit">
<input name="save" type="submit" value="Save changes" />
<input type="hidden" name="action" value="save" />
</p>
</form>
<form method="post">
<p class="submit">
<input name="reset" type="submit" value="Reset" />
<input type="hidden" name="action" value="reset" />
</p>
</form>

<?php
}

add_action('admin_menu', 'mytheme_add_admin'); 


function theme_init(){
	load_theme_textdomain('default', get_template_directory() . '/languages');
}
add_action ('init', 'theme_init');

?>