context("map_from_storr")

test_that("Trying to call non-existing key causes error", {
  expect_error(map_from_storr(input_keys = "foo",
                              storr = test_storr,
                              .f = toupper), regexp = "are not present")
})

test_that("Returns stored values", {
  expect_equivalent(
    map_from_storr(input_keys = c("1", "2", "3"),
                   storr = test_storr,
                   .f = toupper),
    list("A", "B", "C")
  )
})
