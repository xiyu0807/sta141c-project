test_that("basic works", {
  
  fit <- blblm(mpg ~ wt  +   drat , data = mtcars, m = 3, B = 100)
  expect_s3_class(fit, "blblm")
  expect_equal(length(coef(fit)), 3)
  
  
  fit <- blblm(mpg ~ wt + drat , data = mtcars, m = 2, B = 200)
  expect_s3_class(fit, "blblm")
  expect_equal(length(coef(fit)), 3)
  
  fit <- blblm(mpg ~ wt  +  drat , data = mtcars, m = 3, B = 200)
  expect_s3_class(fit, "blblm")
  expect_equal(dim(confint(fit, c("wt","drat"))), c(2,2))
  
  
})
