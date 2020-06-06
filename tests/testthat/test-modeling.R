set.seed(2020)

X <- rnorm(100)
Y <- ifelse(X < 0.5, 0, 1 - sample(0:1, prob = c(0.9, .1)))
test.data <- data.frame(X, Y)
test.mod <- glm(Y ~ X, family='binomial', data = test.data)

test_that("classification metrics works with model", {
  metrics.mod <- classification_metrics(mod = test.mod)
  expect_s3_class(metrics.mod$y.hat, 'factor')
  expect_s3_class(metrics.mod$confmatrix, 'table')
  expect_equal(
    all( sapply( X = c(
      'tpos',
      'tneg',
      'fpos',
      'fneg'
    ), FUN = (function(x){
      return( class( metrics.mod[[x]] ))
    })
    ) == 'integer'), TRUE
  )
  expect_equal(
    all( sapply( X = c(
        'acc',
        'recall',
        'precision',
        'tpr',
        'fpr',
        'fdcr',
        'fnr',
        'fomr',
        'tnr',
        'npv',
        'ppv',
        'F1'
      ), FUN = (function(x){
        return( class( metrics.mod[[x]] ))
      })
    ) == 'numeric'), TRUE
  )
  expect_equal(metrics.mod$tpos, 6)
  expect_equal(metrics.mod$tneg, 78)
  expect_equal(metrics.mod$fneg, 11)
  expect_equal(metrics.mod$fpos, 5)
  expect_equal(metrics.mod$F1, 0.429, tolerance = 0.01)
})

test_that("classification metrics works with y and y.hat", {
  metrics.y <- classification_metrics(y = test.mod$y, y.hat = test.mod$fitted.values)
  expect_s3_class(metrics.y$y.hat, 'factor')
  expect_s3_class(metrics.y$confmatrix, 'table')
  expect_equal(
    all( sapply( X = c(
      'tpos',
      'tneg',
      'fpos',
      'fneg'
    ), FUN = (function(x){
      return( class( metrics.y[[x]] ))
    })
    ) == 'integer'), TRUE
  )
  expect_equal(
    all( sapply( X = c(
      'acc',
      'recall',
      'precision',
      'tpr',
      'fpr',
      'fdcr',
      'fnr',
      'fomr',
      'tnr',
      'npv',
      'ppv',
      'F1'
    ), FUN = (function(x){
      return( class( metrics.y[[x]] ))
    })
    ) == 'numeric'), TRUE
  )
  expect_equal(metrics.y$tpos, 6)
  expect_equal(metrics.y$tneg, 78)
  expect_equal(metrics.y$fneg, 11)
  expect_equal(metrics.y$fpos, 5)
  expect_equal(metrics.y$F1, 0.429, tolerance = 0.01)
})

test_that("classification metrics works with differing pi_0", {
  metrics.pi_0 <- classification_metrics(y = test.mod$y, y.hat = test.mod$fitted.values, pi_0 = 0.7)
  expect_equal(metrics.pi_0$tpos, 2)
  expect_equal(metrics.pi_0$tneg, 80)
  expect_equal(metrics.pi_0$fneg, 15)
  expect_equal(metrics.pi_0$fpos, 3)
  expect_equal(metrics.pi_0$F1, 0.182, tolerance = 0.01)
})

test_that("classification metrics error conditions work", {
  expect_error(classification_metrics())
  expect_error(classification_metrics(y = test.mod$y))
  expect_error(classification_metrics(y = test.mod$y, mod = test.mod))
  expect_error(classification_metrics(mod = lm(Y ~ X, data = test.data)))
  expect_error(classification_metrics(y = test.mod$y, y.hat = rep('a',length(test.mod$y))))
  expect_error(classification_metrics(y = test.mod$y, y.hat = c(0.4,0.5)))
  expect_error(classification_metrics(y = test.mod$y, y.hat = test.mod$fitted.values, pi_0 = NULL))
  expect_error(classification_metrics(y = test.mod$y, y.hat = test.mod$fitted.values, pi_0 = 0))
  expect_error(classification_metrics(y = test.mod$y, y.hat = test.mod$fitted.values, pi_0 = 1.01))
})

test_that("train test validation works", {
  zeallot::`%<-%`(c(train.1, test.1), train_test_val(test.data))
  expect_equal(dim(train.1), c(80, 2))
  expect_equal(dim(test.1), c(20, 2))

  zeallot::`%<-%`(c(train.2, test.2), train_test_val(test.data, 0.6))
  expect_equal(dim(train.2), c(60, 2))
  expect_equal(dim(test.2), c(40, 2))

  zeallot::`%<-%`(c(train.2.1, test.2.1), train_test_val(test.data, 0.6, 0.4, 0))
  expect_equal(dim(train.2.1), c(60, 2))
  expect_equal(dim(test.2.1), c(40, 2))

  zeallot::`%<-%`(c(train.3, test.3, validation.3), train_test_val(test.data, 0.6, 0.2, 0.2))
  expect_equal(dim(train.3), c(60, 2))
  expect_equal(dim(test.3), c(20, 2))
  expect_equal(dim(validation.3), c(20, 2))

  zeallot::`%<-%`(c(train.4, test.4, validation.4), train_test_val(test.data, 0.6, 0, 0.4))
  expect_equal(dim(train.4), c(60, 2))
  expect_equal(dim(test.4), c(0, 2))
  expect_equal(dim(validation.4), c(40, 2))
})

test_that("train test validation catches errors", {
  expect_error(train_test_val(1), 'data is not a data frame')
  expect_error(train_test_val(test.data, 1, 2, 3))
  expect_error(train_test_val(test.data, -1, 0, 2))
})
