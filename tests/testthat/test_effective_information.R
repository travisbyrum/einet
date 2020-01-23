context("Testing Effective Information")

### Testing Effective Information ---------------------------------------------
test_that("is_truthy", {
  copy_copy <- matrix(
    cbind(
      c(1.0, 0.0, 0.0, 0.0),
      c(0.0, 0.0, 1.0, 0.0),
      c(0.0, 1.0, 0.0, 0.0),
      c(0.0, 0.0, 0.0, 1.0)
    ),
    nrow = 4
  )

  expect_equal(effective_information(copy_copy), 2.0)
})
