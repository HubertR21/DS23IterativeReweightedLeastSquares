test_that('test-irls', {
  X1 <- data.frame(x1 = rnorm(100), x2 = rnorm(100, 2, 5), x3 = rnorm(100, 4))
  y1 <- as.matrix(rep(0, 100))
  X2 <- data.frame(x1 = rnorm(100, 1, 2), x2 = rnorm(100, 12, 5), x3 = rnorm(100, 14))
  y2 <- as.matrix(rep(1, 100))
  X  <- rbind(X1, X2)
  y  <- rbind(y1, y2)
  X_train <- X

  m <- irls(X, y)

  expect_true(length(m) == 6)
  expect_true(class(m) == 'irls')
  expect_true(nrow(m$beta) == 4)
  expect_true(m$max_iter >= m$n_iter)
  expect_true(is.null(m$interactions))

  X1 <- data.frame(x1=sqrt(rnorm(100, 20)), x1a=-sqrt(rnorm(100, 200)), x2=rnorm(100, 2, 5), x3=rnorm(100, 4))
  y1 <- as.matrix(rep(0, 100))
  X2 <- data.frame(x1=sqrt(rnorm(100, 15, 2)), x1a=sqrt(rnorm(100, 15, 2)), x2=rnorm(100, 12, 5), x3=rnorm(100, 14))
  y2 <- as.matrix(rep(1, 100))
  X <- rbind(X1, X2)
  y <- rbind(y1, y2)
  X_train <- X

  m <- irls(X, y)

  expect_true(length(m) == 6)
  expect_true(class(m) == 'irls')
  expect_true(nrow(m$beta) == 5)
  expect_true(m$max_iter >= m$n_iter)
  expect_true(is.null(m$interactions))

  m <- irls(X, y, interactions = data.frame(a=c('x1'), b=c('x1a')))

  expect_true(length(m) == 6)
  expect_true(class(m) == 'irls')
  expect_true(nrow(m$beta) == 6)
  expect_true(m$max_iter >= m$n_iter)
  expect_true(nrow(m$interactions) == 1)

  m <- irls(X, y, interactions = data.frame(a=c('x1', 'x2'), b=c('x1a', 'x3')))

  expect_true(length(m) == 6)
  expect_true(class(m) == 'irls')
  expect_true(nrow(m$beta) == 7)
  expect_true(m$max_iter >= m$n_iter)
  expect_true(nrow(m$interactions) == 2)
})
