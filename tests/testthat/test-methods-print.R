
### Test 'format'

test_that("test format.bayesianssd", {
  ssd <- example1_bayesianssd()
  expect_true(is.character(format(ssd)))
})



### Test 'print'

test_that("test print.bayesianssd", {
  ssd <- example1_bayesianssd()
  expect_no_error(print(ssd))
})

