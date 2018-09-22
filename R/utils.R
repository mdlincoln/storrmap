# Assert that all keys exist in the target storr
all_keys_exist <- function(storr, keys, namespace) {
  extant_keys <- which_keys(storr, keys, namespace)
  assertthat::assert_that(all(extant_keys),
              msg = sprintf("The keys %s are not present in the namespace '%s'.",
                            paste0(keys[!extant_keys], collapse = ", "), namespace))
}

# Assert that none of the keys exist in the target storr
no_keys_exist <- function(storr, keys, namespace) {
  extant_keys <- which_keys(storr, keys, namespace)
  assertthat::assert_that(!any(extant_keys),
             msg = sprintf("The keys %s already exist in the namespace '%s'.
                           Please delete them from the storr before proceeding.",
                           paste0(keys[extant_keys], collapse = ", "), namespace))
}

which_keys <- function(storr, keys, namespace) {
  storr$exists(keys, namespace)
}
