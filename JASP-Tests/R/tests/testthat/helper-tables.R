expect_equal_tables <- function(test, ref, ...) {
  testTable <- JASPTools:::collapseTable(test)
  expect_equal(testTable, ref, tolerance=1e-5, ...)
}
