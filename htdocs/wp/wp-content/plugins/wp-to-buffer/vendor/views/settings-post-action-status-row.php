<tr class="status sortable<?php echo ( $key == 0 ? ' first' : '' ); ?>" data-status-index="<?php echo $key; ?>" data-status='<?php echo json_encode( $status, JSON_HEX_APOS ); ?>' data-labels='<?php echo json_encode( $labels, JSON_HEX_APOS ); ?>'>
    <td class="count">#<?php echo ( $key + 1 ); ?></td>
    <td class="actions">
        <a href="#" class="dashicons dashicons-edit edit-status" title="<?php _e( 'Edit Status', 'wp-to-social-pro' ); ?>"></a>
    </td>
    <td class="message"><?php echo $row['message']; ?></td>
    <td class="image"><?php echo $row['image']; ?></td>
    <td class="schedule"><?php echo $row['schedule']; ?></td>
</tr>