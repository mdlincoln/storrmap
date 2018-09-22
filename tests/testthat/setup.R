# Create temporary storr to test on

library(storr)

temp_storr_parent <- tempdir(check = TRUE)
temp_storr_path <- paste(temp_storr_parent, "storr", sep = "/")
stopifnot(file.copy(system.file("storr", package = "storrmap", mustWork = TRUE), to = temp_storr_parent, recursive = TRUE))
test_storr <- storr_rds(temp_storr_path)
