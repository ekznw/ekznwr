inventory_apply_rules <- function(dt) {
  dt <- inventory_rules_gis(dt)
  dt
}

inventory_rules_gis <- function(dt) {
  dt <- apply_format_overrides(dt)
  dt
}