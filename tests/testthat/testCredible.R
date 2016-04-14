library(broom)
context( "Credible helper functions")

test_that("normalize returns values between 0 and 1", {
  df <- data.frame( list("x"=runif(100,0,1),"y"=runif(100,-3,-1)) )
  summ <- tidy(normalize.samples(df))
  expect_equal( summ$max, c(1,1) )
  expect_equal( summ$min, c(0,0) )
})

test_that("hpdi.discard.id discards the correct id", {
  df <- data.frame(matrix(
    c( 0, 0, 0,
       1, 100, 1,
       1.9,110, 1, # should be removed when normalizing
       1.1,150, 1,
       0.1, 2, 0.1,
       0.97, 90, 0.95
       ), ncol=3, byrow=T))
  expect_equal( hpdi.discard.id( df ), 3)
})

test_that("hpdi.discard.id works for multiple values", {
  full.df <- data.frame(list(
    "x"=c(runif(96,-1,1), -10, -10, 10, 10),
    "y"=c(runif(96,-1,1), -10, 10, 10, -10)
  ))
  expect_equal( sort(hpdi.discard.id( full.df, 4 )), c(97,98,99,100) )
})

test_that("ci.chull returns the correct number of samples", {
  full.df <- data.frame(list(
    "x"=c(runif(96,-1,1), -10, -10, 10, 10),
    "y"=c(runif(96,-1,1), -10, 10, 10, -10)
  ))
  expect_equal( sum(ci.chull( full.df, 0.9 )), 90 )
  expect_equal( sum(ci.chull( full.df, 0.8 )), 80 )
  expect_equal( length(ci.chull( full.df, 0.8 )), 100 )
  expect_equal( ci.chull( full.df, 0.9 )[97:100], c(F,F,F,F) )
})

test_that("minmax.discard.id works for multiple values", {
  full.df <- data.frame(list(
    "x"=c(runif(96,-1,1), -10, 0, 10, 0),
    "y"=c(runif(96,-1,1), 0, 10, 0, -10)
  ))
  expect_equal( length(minmax.discard.id( full.df, 3 )), 0 )
  expect_equal( sort(minmax.discard.id( full.df, 4 )), c(97,98,99,100) )
  expect_equal( sort(minmax.discard.id( full.df, 6 )), c(97,98,99,100) )
})

test_that("minmax.discard.id only returns unique values", {
    full.df <- data.frame(list(
    "x"=c(runif(96,-1,1), -5, 0, 5, 0, -10, 10),
    "y"=c(runif(96,-1,1), 0, 5, 0, -5, -10, 10)
  ))
  expect_equal( sort(minmax.discard.id( full.df, 4 )), c(101,102) )
  expect_equal( length(minmax.discard.id( full.df, 7 )), 6 )
})
