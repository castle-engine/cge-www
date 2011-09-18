			<div class="header span-24">
				<div class="intro span-18">
					<div class="intro-wrapper paddings">
						<?php if (is_home()) { ?>
							<h1 class="logo"><a href="<?php echo get_option('home'); ?>/"><?php bloginfo('name'); ?></a></h1>
						<?php } else { ?>
							<span class="logo"><a href="<?php echo get_option('home'); ?>/"><?php bloginfo('name'); ?></a></span>
						<?php } ?>
						<span class="slogan kambi-vrmlengine-para"><?php bloginfo('description'); ?>
                                                  <!-- Kambi+ insert slogan directly with HTML: -->
                                                  News about the <a href="http://castle-engine.sourceforge.net/">Castle Game Engine</a> development.
                                                  <!--By Kambi, aka Michalis Kamburelis.--!>
                                                </span>
					</div>
				</div>

				<div class="icons span-6 last">
					<div class="paddings">
						<div class="icons-wrapper">
							<a href="<?php bloginfo('rss2_url'); ?>" title="RSS link"><img src="<?php bloginfo('stylesheet_directory'); ?>/images/ico/rss.gif" alt="<?php _e('RSS icon', 'default'); ?>" /></a>
							<?php if ($gear_email_visibility == "on") { ?>
							<a href="mailto:<?php echo antispambot(get_option('admin_email')); ?>"><img src="<?php bloginfo('stylesheet_directory'); ?>/images/ico/mail.gif" alt="<?php _e('Email icon', 'default'); ?>" /></a>
							<?php } ?>
							<a href="<?php echo get_option('home'); ?>/"><img src="<?php bloginfo('stylesheet_directory'); ?>/images/ico/home.gif" alt="<?php _e('Home icon', 'default'); ?>" /></a>
						</div>

						<div class="search fr">
							<?php include (TEMPLATEPATH . "/searchform.php"); ?>
						</div>
					</div>
				</div>
			</div>

<?php
/*
Kambi-, don't want this (for now)

<div class="menu span-24">
  <ul class="menu-wrapper">
          <li class="first < ?php if ( is_home() ) { ? >current_page_item< ?php } ? >"><a href="< ?php echo get_option('home'); ? >/" title="< ?php _e('A link to home page', 'default'); ? >">< ?php _e('Home', 'default'); ? ></a></li>
          < ?php wp_list_pages('sort_column=menu_order&depth=1&title_li=');? >
  </ul>
</div>
*/
?>
