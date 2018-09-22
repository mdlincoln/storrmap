context("map_from_storr")

test_that("Trying to call non-existing key causes error", {
  expect_error(map_from_storr("foo", test_storr, toupper), regexp = "are not present")
})

test_that("Returns stored values", {
  expect_equivalent(
    map_from_storr(c("1", "2", "3"), test_storr, toupper),
    list("A", "B", "C")
  )
})
