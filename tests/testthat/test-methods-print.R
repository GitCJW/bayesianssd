
### Test 'format'

test_that("test format.bayesianssd", {
  ssd1 <- example1_bayesianssd()
  ssd2 <- example2_bayesianssd()
  expect_true(is.character(format(ssd1)))
  expect_true(is.character(format(ssd2)))
})



### Test 'print'

test_that("test print.bayesianssd", {
  ssd <- example1_bayesianssd()
  expect_no_error(print(ssd))
})

