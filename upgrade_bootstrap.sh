#!/bin/bash
set -euo pipefail
IFS=$'\n\t'

# A short script to upgrade from Bootstrap 3.0 through 4.0 to 5.0
# As the upgrade instructions are very vague (because teapot can't brew coffee or something)
# the table below hypothetically should only try to make some meaningful replacement
# keeping the rule "better replace changed/dropped item with something than nothing"

# Replacement table from https://getbootstrap.com/docs/4.0/migration/

sed -i 's/\b$thumbnail-transition\b/$ERROR/g' $1
sed -i 's/\b.col-form-legend\b/.col-form-label/g' $1
sed -i 's/\b.input-group-addon\b/.input-group-prepend/g' $1
sed -i 's/\b.input-group-btn\b/.input-group-append/g' $1
sed -i 's/\b$badge-color\b/$ERROR/g' $1
sed -i 's/\bgrayscale()\b/gray()/g' $1
sed -i 's/\b.table-inverse\b/.table-dark/g' $1
sed -i 's/\b.thead-inverse\b/.thead-dark/g' $1
sed -i 's/\b.thead-default\b/.thead-light/g' $1
sed -i 's/\b.img-responsive\b/.img-fluid/g' $1
sed -i 's/\b.img-rounded\b/.rounded/g' $1
sed -i 's/\b.img-circle\b/.rounded-circle/g' $1
sed -i 's/\b.table-condensed\b/.table-sm/g' $1
sed -i 's/\b.active\b/.table-active/g' $1
sed -i 's/\b.success\b/.table-success/g' $1
sed -i 's/\b.warning\b/.table-warning/g' $1
sed -i 's/\b.danger\b/.table-danger/g' $1
sed -i 's/\b.info\b/.table-info/g' $1
sed -i 's/\b.control-label\b/.col-form-label/g' $1
sed -i 's/\b.input-lg\b/.form-control-lg/g' $1
sed -i 's/\b.input-sm\b/.form-control-sm/g' $1
sed -i 's/\b.form-group-\b/.form-control-/g' $1
sed -i 's/\b.help-block\b/.form-text/g' $1
sed -i 's/\b.radio-inline\b/.ERROR/g' $1
sed -i 's/\b.checkbox-inline\b/.ERROR/g' $1
sed -i 's/\b.checkbox\b/.form-check/g' $1
sed -i 's/\b.radio\b/.form-check/g' $1
sed -i 's/\b.has-error\b/:invalid/g' $1
sed -i 's/\b.has-warning\b/:invalid/g' $1
sed -i 's/\b.has-success\b/:valid/g' $1
sed -i 's/\b.form-control-static\b/.form-control-plaintext/g' $1
sed -i 's/\b.btn-default\b/.btn-secondary/g' $1
sed -i 's/\b.btn-xs\b/.btn-sm/g' $1
sed -i 's/\b[disabled]\b/:disabled/g' $1
sed -i 's/\b.btn-group-justified\b/.ERROR/g' $1
sed -i 's/\b.btn-group-xs\b/.ERROR/g' $1
sed -i 's/\b.divider\b/.dropdown-divider/g' $1
sed -i 's/\b.col-8.push-4\b/.col-8.order-2/g' $1
sed -i 's/\b.col-4.pull-8\b/.col-4.order-1/g' $1
sed -i 's/\b.list-group-item\b/.list-group-item-action/g' $1
sed -i 's/\bloaded.bs.modal\b/ERROR/g' $1
sed -i 's/\b.navbar-default\b/.navbar-light/g' $1
sed -i 's/\b.navbar-toggle\b/.navbar-toggler/g' $1
sed -i 's/\b.navbar-form\b/.form-inline/g' $1
sed -i 's/\b.pager\b/.ERROR/g' $1
sed -i 's/\b.label\b/.badge/g' $1
sed -i 's/\b.badge-default\b/.ERROR/g' $1
sed -i 's/\b.panel\b/.card/g' $1
sed -i 's/\b.panel-default\b/.ERROR/g' $1
sed -i 's/\b.panel-group\b/.ERROR/g' $1
sed -i 's/\b.panel-heading\b/.card-header/g' $1
sed -i 's/\b.panel-title\b/.card-title/g' $1
sed -i 's/\b.panel-body\b/.card-body/g' $1
sed -i 's/\b.panel-footer\b/.card-footer/g' $1
sed -i 's/\b.panel-primary\b/.text-primary/g' $1
sed -i 's/\b.panel-success\b/.text-success/g' $1
sed -i 's/\b.panel-info\b/.text-info/g' $1
sed -i 's/\b.panel-warning\b/.text-warning/g' $1
sed -i 's/\b.panel-danger\b/.text-danger/g' $1
sed -i 's/\b.progress-bar-\b/.bg-/g' $1
sed -i 's/\b.next\b/.carousel-item-next/g' $1
sed -i 's/\b.prev\b/.carousel-item-prev/g' $1
sed -i 's/\b.left\b/.carousel-item-left/g' $1
sed -i 's/\b.right\b/.carousel-item-right/g' $1
sed -i 's/\b.item\b/.carousel-item/g' $1
sed -i 's/\b.carousel-control.right\b/.carousel-control-next/g' $1
sed -i 's/\b.carousel-control.left\b/.carousel-control-prev/g' $1
sed -i 's/\b.hidden-sm-up\b/.d-sm-none/g' $1
sed -i 's/\b.hidden\b/.ERROR/g' $1
sed -i 's/\b.pull-left\b/.float-left/g' $1
sed -i 's/\b.pull-right\b/.float-right/g' $1
sed -i 's/\b.center-block\b/.mx-auto/g' $1
sed -i 's/\banimation-delay\b/REMOVED/g' $1
sed -i 's/\banimation-direction\b/REMOVED/g' $1
sed -i 's/\banimation-duration\b/REMOVED/g' $1
sed -i 's/\banimation-fill-mode\b/REMOVED/g' $1
sed -i 's/\banimation-iteration-count\b/REMOVED/g' $1
sed -i 's/\banimation-name\b/REMOVED/g' $1
sed -i 's/\banimation-timing-function\b/REMOVED/g' $1
sed -i 's/\banimation-visibility\b/REMOVED/g' $1
sed -i 's/\banimation\b/REMOVED/g' $1
sed -i 's/\bbox-sizing\b/REMOVED/g' $1
sed -i 's/\bcontent-columns\b/REMOVED/g' $1
sed -i 's/\bhyphens\b/REMOVED/g' $1
sed -i 's/\bopacity\b/REMOVED/g' $1
sed -i 's/\bperspective-origin\b/REMOVED/g' $1
sed -i 's/\bperspective\b/REMOVED/g' $1
sed -i 's/\brotate\b/REMOVED/g' $1
sed -i 's/\brotateX\b/REMOVED/g' $1
sed -i 's/\brotateY\b/REMOVED/g' $1
sed -i 's/\bscale\b/REMOVED/g' $1
sed -i 's/\bscaleX\b/REMOVED/g' $1
sed -i 's/\bscaleY\b/REMOVED/g' $1
sed -i 's/\bskew\b/REMOVED/g' $1
sed -i 's/\btransform-origin\b/REMOVED/g' $1
sed -i 's/\btransition-delay\b/REMOVED/g' $1
sed -i 's/\btransition-duration\b/REMOVED/g' $1
sed -i 's/\btransition-property\b/REMOVED/g' $1
sed -i 's/\btransition-timing-function\b/REMOVED/g' $1
sed -i 's/\btransition-transform\b/REMOVED/g' $1
sed -i 's/\btranslate\b/REMOVED/g' $1
sed -i 's/\btranslate3d\b/REMOVED/g' $1
sed -i 's/\buser-select\b/REMOVED/g' $1
sed -i 's/\b@screen-\b/media-breakpoint-ERROR()/g' $1
sed -i 's/\b.hidden\b/.ERROR/g' $1
sed -i 's/\b.show\b/.ERROR/g' $1
sed -i 's/\b.visible-print-block\b/.d-print-block/g' $1
sed -i 's/\b.visible-print-inline\b/.d-print-inline/g' $1
sed -i 's/\b.visible-print-inline-block\b/.d-print-inline-block/g' $1
sed -i 's/\b.hidden-print\b/.d-print-none/g' $1

# Replacement table from https://getbootstrap.com/docs/5.0/migration/

sed -i 's/\bxxxxx\b/ERROR/g' $1
sed -i 's/\bxxxxx\b/ERROR/g' $1
sed -i 's/\bxxxxx\b/ERROR/g' $1
sed -i 's/\bxxxxx\b/ERROR/g' $1
sed -i 's/\bxxxxx\b/ERROR/g' $1
sed -i 's/\bxxxxx\b/ERROR/g' $1
sed -i 's/\bxxxxx\b/ERROR/g' $1
sed -i 's/\bxxxxx\b/ERROR/g' $1
sed -i 's/\bxxxxx\b/ERROR/g' $1
sed -i 's/\bxxxxx\b/ERROR/g' $1

sed -i 's/\bxxxxx\b/ERROR/g' $1
