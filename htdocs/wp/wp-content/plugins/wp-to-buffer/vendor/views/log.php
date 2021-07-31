<div class="wrap">
    <h1 class="wp-heading-inline">
        <?php echo $this->base->plugin->displayName; ?>

        <span>
            <?php _e( 'Logs', 'wp-to-social-pro' ); ?>
        </span>
    </h1>

    <?php
    // Search Subtitle
    if ( isset( $_REQUEST['s'] ) && ! empty( $_REQUEST['s'] ) ) {
        ?>
        <span class="subtitle left"><?php _e( 'Search results for', 'page-generator-pro' ); ?> &#8220;<?php echo urldecode( $_REQUEST['s'] ); ?>&#8221;</span>
        <?php
    }
    ?>

    <form action="admin.php?page=<?php echo sanitize_text_field( $_REQUEST['page'] ); ?>" method="post" id="posts-filter">
        <?php   
        // Output Search Box
        $table->search_box( __( 'Search' ), 'wp-to-social-log' );

        // Output Table
        $table->display(); 
        ?>  
    </form>
</div><!-- /.wrap -->