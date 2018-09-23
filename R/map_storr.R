#' Map a function over a list of storr objects
#'
#' This variant saves the [purrr::map] results within the current environment.
#'
#' @inheritParams map_within_storr
#'
#' @param input_keys List of keys present in `storr`
#' @param storr Storr object
#' @param .flush_cache Flush cache of objects at each step? Defaults to `TRUE`
#'
#' @importFrom assertthat assert_that
#' @importFrom purrr map
#'
#' @return A list.
#'
#' @export
map_from_storr <- function(input_keys, storr, .f, ..., .input_namespace = storr$default_namespace, .flush_cache = FALSE) {

  assert_that(inherits(storr, what = "storr"))
  assert_that(is.function(.f))
  all_keys_exist(storr, input_keys, .input_namespace)

  map(input_keys, function(x) {
    rx <- storr$get(x, namespace = .input_namespace)
    res <- .f(rx, ...)

    # Flush the storr cache once the object has been passed through the function.
    if (.flush_cache) storr$flush_cache()

    res
  })
}

#' Store the results of each mapping step within storr
#'
#' @inheritParams map_within_storr
#' @param x A list or vector of inputs
#'
#' @importFrom assertthat assert_that
#' @importFrom purrr map2_chr
#'
#' @return Character vector containing the hashes of each saved object.
#'
#' @export
map_to_storr <- function(x, storr, output_keys, .f, ..., .output_namespace = storr$default_namespace,
                         .flush_cache = TRUE) {

  assert_that(inherits(storr, what = "storr"))
  assert_that(is.function(.f))
  assert_that(length(output_keys) == length(x))
  no_keys_exist(storr, output_keys, .output_namespace)

  res <- map2_chr(x, output_keys, function(a, i) {
    hash <- storr$set(key = i,
              value = .f(a, ...),
              namespace = .output_namespace,
              use_cache = !.flush_cache)
    return(hash)
  })

  invisible(res)
}

#' Map inputs from storr into outputs also saved in storr
#'
#' @param input_keys Keys already present in `storr`
#' @param output_keys Keys for objects to be written to `storr`
#' @param .f Function to run on each object
#' @param ... Other arguments passed to `.f`
#' @param .input_namespace Namespace of `input_keys` (optional)
#' @param .output_namespace Namescpae of `output_keys` (optional)
#' @param .flush_cache Flush cache of objects at each step? Defaults to `TRUE`
#'
#' @importFrom assertthat assert_that
#' @importFrom purrr map2_chr
#'
#' @return Character vector containing the hashes of each saved object.
#'
#' @export
map_within_storr <- function(input_keys, output_keys, storr,
                             .f, ...,
                             input_namespace = storr$default_namespace,
                             output_namespace = storr$default_namespace,
                             .flush_cache = TRUE) {

  assert_that(inherits(storr, what = "storr"))
  assert_that(length(input_keys) == length(output_keys))
  all_keys_exist(storr, input_keys, input_namespace)
  no_keys_exist(storr, output_keys, output_namespace)

  res <- map2_chr(input_keys, output_keys, function(i, o) {
    input_val <- storr$get(i, namespace = input_namespace)
    output_val <- .f(input_val, ...)
    hash <- storr$set(o, output_val, namespace = output_namespace)

    if (.flush_cache) storr$flush_cache()

    hash
  })

  invisible(res)
}
