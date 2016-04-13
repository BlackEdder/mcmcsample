library(broom)
context( "Geometry helper functions")

test_that("normalize returns values between 0 and 1", {
  df <- data.frame( list("x"=runif(100,0,1),"y"=runif(100,-3,-1)) )
  summ <- tidy(normalize.samples(df))
  expect_equal( summ$max, c(1,1) )
  expect_equal( summ$min, c(0,0) )
})
