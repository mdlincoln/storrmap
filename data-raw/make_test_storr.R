# Create the storr DB in inst/storr used for testing

library(storr)

test_storr_path <- "inst/storr"
test_storr <- storr_rds(test_storr_path, compress = TRUE, mangle_key = TRUE)

lapply(letters, function(x) {
  res <- list(val = x)
  test_storr$set(key = as.character(which(letters == x)), x)
})
