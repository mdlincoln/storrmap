context("map_to_storr")

test_that("input and keys of different length throws error", {
  expect_error(
    map_to_storr(letters, keys = c("a", "b"), test_storr, toupper), regexp = "length")
})

test_that("adding exisitng key in same namespace causes error", {
  expect_error(map_to_storr("foo", keys = "2", test_storr, toupper), regexp = "already exist")
})

test_that("mapping writes new keys", {
  newkeys <- paste0("uppercase", letters)
  expect_equivalent(map_to_storr(letters, test_storr, keys = newkeys, toupper), c("75f1160e72554f4270c809f041c7a776", "3a5505c06543876fe45598b5e5e5195d",
                                                                                                       "475bf9280aab63a82af60791302736f6", "c1f86f7430df7ddb256980ea6a3b57a4",
                                                                                                       "01a75cb73d67b0f895ff0e61449c7bf8", "f76b651ab8fcb8d470f79550bf2af53a",
                                                                                                       "0749ff40a91e92e8826bfab6725da1fd", "1cd9eab1de80419dbedb231bf3b1704d",
                                                                                                       "d5e8fabeb94e101d52896d3ca23fc0b5", "91c1363aaeea4d37562a38a6b9c7cee6",
                                                                                                       "3ba3544fdd7d346a2f7482206a38c04e", "a16f493a486d58777ad16c4576d46c8b",
                                                                                                       "5f0922939c45ef1054f852e83f91c660", "a44f24026794d35d95331fa25508eb78",
                                                                                                       "b508e4f76211aa87de643d05ca0e1d56", "cb0a9b3af7d9c22a19c9a66388ec9d40",
                                                                                                       "247c455e7736ad9055bc61f9f9ae9a09", "45489e8db881f9e7cb0a1e13cef35adf",
                                                                                                       "f9350dd216ad39deddfd9f6ccfb3abb4", "2ac2e9e75f4a6fa7a91cb7ae495a087f",
                                                                                                       "b72e42364e59f4139ed7c4ba554fb5e4", "700c5fa9e15c929a6131149588e48dfe",
                                                                                                       "9ec5a5e3b79a8d3273f0054c5b8a7380", "617cfe25b1050e3caf27b44e2c0bb219",
                                                                                                       "0184a9fecb62b8cdbd87e237220e5de7", "2388473d9b6ffd1c75aa6c2aaa9689b0"
  ))
  expect_true(all(test_storr$exists(newkeys)))

  # Remove keys set by this test
  expect_true(all(test_storr$del(newkeys)))
})
