test_that('test-summary', {
  X1 <- data.frame(x1 = rnorm(100), x2 = rnorm(100, 2, 5), x3 = rnorm(100, 4))
  y1 <- as.matrix(rep(0, 100))
  X2 <- data.frame(x1 = rnorm(100, 1, 2), x2 = rnorm(100, 12, 5), x3 = rnorm(100, 14))
  y2 <- as.matrix(rep(1, 100))
  X  <- rbind(X1, X2)
  y  <- rbind(y1, y2)
  X_train <- X

  m <- irls(X, y)
  expect_output(summary(m))

  t <- na.omit(titanic::titanic_train)
  y <- t$Survived
  X <- t[, c('Pclass', 'Age', 'Sex', 'Fare')]
  X$Sex <- as.numeric(as.factor(X$Sex))

  m1    <- irls(X, y)
  m2    <- irls(X, y, interactions = data.frame(a = c('Pclass', 'Age'), b = c('Fare', 'Sex')))
  expect_output(summary(m1))
  expect_output(summary(m2))

})
