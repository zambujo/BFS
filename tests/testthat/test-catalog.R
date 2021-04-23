
test_that("bfs_get_catalog() arguments in range", {
  expect_error(bfs_get_catalog())
  expect_error(bfs_get_catalog("zzz"))
})

test_that("bfs_get_catalog() works", {
  vcr::use_cassette("bfs_get_catalog-de", {
    de <- expect_type(bfs_get_catalog("de"), "list")
  })
  expect_type(de, "list")
})
