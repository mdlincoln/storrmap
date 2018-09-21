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
#' @param .keep_cache Keep the retrieved objects cached? Defaults to `FALSE`
#'
#' @return A list resulting from map.
#'
#' @export
map_from_storr <- function(keys, storr, namespace = storr$default_namespace, .f,
                      ..., .keep_cache = FALSE) {
  assertthat::assert_that(all(storr$exists(keys, namespace = namespace)))

  map(keys, function(x) {
    rx <- storr$get(x, namespace = namespace)
    res <- .f(rx, ...)

    # Flush the storr cache once the object has been passed through the function.
    if (!.keep_cache) storr$flush_cache()

    res
  })
}

#' Store the results of each mapping step within storr
map_to_storr <- function() {

}

#' Map inputs from storr into outputs also saved in storr
map_within_storr <- function() {

}
