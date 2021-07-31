<select size="1" class="left tags" data-textarea="<?php echo $textarea; ?>">
    <option value=""><?php _e( '--- Insert Tag ---', 'wp-to-social-pro' ); ?></option>
    <?php
    foreach ( $this->base->get_class( 'common' )->get_tags( $post_type ) as $tag_group => $tag_group_tags ) {
        ?>
        <optgroup label="<?php echo $tag_group; ?>">
            <?php
            foreach ( $tag_group_tags as $tag => $tag_attributes ) {
                // If the tag attributes is an array, this is a more complex tag
                // that requires user input
                if ( is_array( $tag_attributes ) ) {
                    ?>
                    <option value="<?php echo $tag; ?>" data-question="<?php echo $tag_attributes['question']; ?>" data-default-value="<?php echo $tag_attributes['default_value']; ?>" data-replace="<?php echo $tag_attributes['replace']; ?>"><?php echo $tag_attributes['label']; ?></option>
                    <?php
                } else {
                    ?>
                    <option value="<?php echo $tag; ?>"><?php echo $tag_attributes; ?></option>
                    <?php
                }
            }
            ?>
        </optgroup>
        <?php
    }
    ?>
</select>