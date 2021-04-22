
test_that("bfs_get_catalog() arguments in range", {
  expect_error(bfs_get_catalog())
  expect_error(bfs_get_catalog("zzz"))
})

test_that("bfs_get_catalog() works", {
  expect_type(bfs_get_catalog("de"), "list")
  # expect_type(bfs_get_catalog("fr"), "list")
  # expect_type(bfs_get_catalog("it"), "list")
  # expect_type(bfs_get_catalog("en"), "list")
})
