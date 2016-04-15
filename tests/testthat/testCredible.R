library(broom)
context( "Credible helper functions")

test_that("normalize returns values between 0 and 1", {
  df <- data.frame( list("x"=runif(100,0,1),"y"=runif(100,-3,-1)) )
  summ <- tidy(normalize.samples(df))
  expect_equal( summ$max, c(1,1) )
  expect_equal( summ$min, c(0,0) )
})

test_that("ci.chull discards the correct id", {
  df <- data.frame(matrix(
    c( 0, 0, 0,
       1, 100, 1,
       1.9,110, 1, # should be removed when normalizing
       1.1,150, 1,
       0.1, 2, 0.1,
       0.97, 90, 0.95
       ), ncol=3, byrow=T))
  expect_equal( ci.chull( df ), 3)
})

test_that("ci.chull works for multiple values", {
  full.df <- data.frame(list(
    "x"=c(runif(96,-1,1), -10, -10, 10, 10),
    "y"=c(runif(96,-1,1), -10, 10, 10, -10)
  ))
  expect_equal( sort(ci.chull( full.df, 4 )), c(97,98,99,100) )
})

test_that("ci.minmax works for multiple values", {
  full.df <- data.frame(list(
    "x"=c(runif(96,-1,1), -10, 0, 10, 0),
    "y"=c(runif(96,-1,1), 0, 10, 0, -10)
  ))
  expect_equal( length(ci.minmax( full.df, 3 )), 0 )
  expect_equal( sort(ci.minmax( full.df, 4 )), c(97,98,99,100) )
  expect_equal( sort(ci.minmax( full.df, 6 )), c(97,98,99,100) )
})

test_that("ci.minmax only returns unique values", {
    full.df <- data.frame(list(
    "x"=c(runif(96,-1,1), -5, 0, 5, 0, -10, 10),
    "y"=c(runif(96,-1,1), 0, 5, 0, -5, -10, 10)
  ))
  expect_equal( sort(ci.minmax( full.df, 4 )), c(101,102) )
  expect_equal( length(ci.minmax( full.df, 7 )), 6 )
})

test_that("inside.ci returns the correct number of samples", {
  full.df <- data.frame(list(
    "x"=c(runif(96,-1,1), -10, -10, 10, 10),
    "y"=c(runif(96,-1,1), -10, 10, 10, -10)
  ))
  expect_equal( sum(inside.ci( full.df, 0.9 )), 90 )
  expect_equal( sum(inside.ci( full.df, 0.8 )), 80 )
  expect_equal( length(inside.ci( full.df, 0.8 )), 100 )
  expect_equal( inside.ci( full.df, 0.9 )[97:100], c(F,F,F,F) )
})

test_that("inside.ci takes minmax as method", {
  full.df <- data.frame(list(
    "x"=c(runif(96,-1,1), -10, -10, 10, 10),
    "y"=c(runif(96,-1,1), -10, 10, 10, -10)
  ))
  expect_gte( sum(inside.ci( full.df, 0.9, method="minmax" )), 90 )
  expect_gte( sum(inside.ci( full.df, 0.8, method="minmax" )), 80 )
  expect_equal( length(inside.ci( full.df, 0.8, method="minmax" )), 100 )
  expect_equal( inside.ci( full.df, 0.9, method="minmax" )[97:100], c(F,F,F,F) )
})

test_that("inside.ci works on one dimension", {
  full.df <- data.frame(list(
    "x"=c(runif(96,-1,1), -10.1, -10, 10, 10.1)
  ))
  expect_gte( sum(inside.ci( full.df, 0.9, method="chull" )), 90 )
  expect_gte( sum(inside.ci( full.df, 0.8, method="minmax" )), 80 )
  expect_equal( length(inside.ci( full.df, 0.8, method="minmax" )), 100 )
  expect_equal( inside.ci( full.df, 0.9, method="minmax" )[97:100], c(F,F,F,F) )
})

test_that("bin.id works on data frames", {
  full.df <- data.frame(list(
    "x"=runif(100,-1,1),
    "y"=runif(100,100,1000)
  ))
  bins <- apply(full.df,2,bin.id)
  expect_equal( range(bins[,1]), c(1,10) )
  expect_equal( range(bins[,2]), c(1,10) )

  bins <- apply(full.df,2,function(v) bin.id(v,51))
  expect_equal( range(bins[,1]), c(1,51) )
  expect_equal( range(bins[,2]), c(1,51) )
})
