<?php
/**
* Settings screen sidebar for free plugins with a pro version. Display the reasons to upgrade
* and the mailing list.
*/
?>
<!-- Keep Updated -->
<div class="postbox">
    <h3 class="hndle">
        <?php _e( 'Keep Updated', $this->base->plugin->name ); ?>
    </h3>
    
    <div class="wpzinc-option">
        <p class="description">
            <?php _e( 'Subscribe to the newsletter and receive updates on our WordPress Plugins.', $this->base->plugin->name ); ?>
        </p>
    </div>
    
    <form action="https://n7studios.createsend.com/t/r/s/jdutdyj/" method="post">
        <div class="wpzinc-option">
            <div class="full">                        
                <input id="fieldEmail" name="cm-jdutdyj-jdutdyj" type="email" class="widefat" placeholder="<?php _e( 'Your Email Address', $this->base->plugin->name ); ?>" class="widefat" required />
            </div>
        </div>
        <div class="wpzinc-option">
            <div class="full"> 
                <input type="submit" name="submit" value="<?php _e( 'Subscribe', $this->base->plugin->name ); ?>" class="button button-primary" />
            </div>
        </div>
    </form> 
</div>