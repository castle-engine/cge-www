<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="EN" lang="EN">
<head>
	<?php get_header(); ?>
</head>
<body>
	<div class="main">
		<div class="container">
			<?php include (TEMPLATEPATH . "/head.php"); ?>

			<div class="content span-24">
				<div class="posts span-18 last">
					<?php include (TEMPLATEPATH . "/banner.php"); ?>

					<div class="paddings">
						<ul class="items">
							<?php if (have_posts()) : ?>
							<?php while (have_posts()) : the_post(); ?>
							<li>
								<?php include (TEMPLATEPATH . "/item.php"); ?>
								<?php comments_template(); ?>
							</li>
							<?php endwhile; ?>
							<?php else : ?>
							<li>
								<?php include (TEMPLATEPATH . "/missing.php"); ?>
							</li>
							<?php endif; ?>
							<li>
								<div class="navigation">
									<div class="fl"><?php next_posts_link(__('&laquo; Older Entries', 'default')) ?></div>
			                        <div class="fr"><?php previous_posts_link(__('Newer Entries &raquo;', 'default')) ?></div>
			                    	<div class="clear"></div>
		                        </div>
		                    </li>
						</ul>
					</div>
				</div>

				<?php get_sidebar(); ?>
			</div>
			<div class="clear"></div>

			<?php include (TEMPLATEPATH . "/footer.php"); ?>
		</div>
	</div>
</body>
</html>
