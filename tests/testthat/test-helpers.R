
test_that("cn function works properly",{
  x <- "test"
  y <- ""
  expect_equal(cn(x,y),"")
  x <- "test"
  y <- "it"
  expect_equal(cn(x,y),"testit")
})
