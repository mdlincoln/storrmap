#' Map a function over a list of storr objects
#'
#' This variant saves the [purrr::map] results within the current environment.
#'
#' @import storr
#' @importFrom purrr map
#'
#' @param keys List of keys present in `storr`
#' @param storr Storr object
#' @param namespace Namespace of keys (optional)
#' @param .f Function to run on each object retrieved from `keys`
#' @param ... Other arguments passed to `.f`
#' @param .flush_cache Flush cache of objects at each step? Defaults to `TRUE`
#'
#' @return A list resulting from map.
#'
#' @export
map_from_storr <- function(keys, storr, namespace = storr$default_namespace, .f,
                      ..., .flush_cache = FALSE) {
  assertthat::assert_that(all(storr$exists(keys, namespace = namespace)))

  map(keys, function(x) {
    rx <- storr$get(x, namespace = namespace)
    res <- .f(rx, ...)

    # Flush the storr cache once the object has been passed through the function.
    if (.flush_cache) storr$flush_cache()

    res
  })
}

#' Store the results of each mapping step within storr
map_to_storr <- function(storr, keys, namespace = storr$default_namespace, x, .f, ..., .flush_cache = TRUE) {
  extant_keys <- storr$exists(keys, namespace)

  if (any(extant_keys))
    stop(sprintf("The keys %s already exist in the namespace '%s'. Please delete them from the storr before proceeding.", paste0(keys[extant_keys], collapse = ", "), namespace))

  res <- map2_chr(x, keys, function(a, i) {
    hash <- storr$set(key = i,
              value = .f(a, ...),
              namespace = namespace,
              use_cache = !.flush_cache)
    return(hash)
  })

  invisible(res)
}

#' Map inputs from storr into outputs also saved in storr
map_within_storr <- function() {

}
