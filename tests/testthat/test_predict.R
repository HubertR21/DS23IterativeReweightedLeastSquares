test_that('test-predict', {
  X1 <- data.frame(x1 = rnorm(100), x2 = rnorm(100, 2, 5), x3 = rnorm(100, 4))
  y1 <- as.matrix(rep(0, 100))
  X2 <- data.frame(x1 = rnorm(100, 1, 2), x2 = rnorm(100, 12, 5), x3 = rnorm(100, 14))
  y2 <- as.matrix(rep(1, 100))
  X  <- rbind(X1, X2)
  y  <- rbind(y1, y2)
  X_train <- X

  m <- irls(X, y)

  preds1 <- predict(m, X, prob = FALSE)

  expect_equal(as.matrix(preds1$y_pred), y)

  preds2 <- predict(m, X, prob = TRUE)

  expect_true(99 < sum(preds2))
  expect_true(sum(preds2) < 101)



  X1 <- data.frame(x1=sqrt(rnorm(100, 20)), x1a=-sqrt(rnorm(100, 200)), x2=rnorm(100, 2, 5), x3=rnorm(100, 4))
  y1 <- as.matrix(rep(0, 100))
  X2 <- data.frame(x1=sqrt(rnorm(100, 15, 2)), x1a=sqrt(rnorm(100, 15, 2)), x2=rnorm(100, 12, 5), x3=rnorm(100, 14))
  y2 <- as.matrix(rep(1, 100))
  X <- rbind(X1, X2)
  y <- rbind(y1, y2)
  X_train <- X

  m <- irls(X, y)

  preds1 <- predict(m, X, prob = FALSE)

  expect_equal(as.matrix(preds1$y_pred), y)

  preds2 <- predict(m, X, prob = TRUE)

  expect_true(99 < sum(preds2))
  expect_true(sum(preds2) < 101)

  m <- irls(X, y, interactions = data.frame(a=c('x1'), b=c('x1a')))

  preds1 <- predict(m, X, prob = FALSE)

  expect_equal(as.matrix(preds1$y_pred), y)

  preds2 <- predict(m, X, prob = TRUE)

  expect_true(99 < sum(preds2))
  expect_true(sum(preds2) < 101)

  m <- irls(X, y, interactions = data.frame(a=c('x1', 'x2'), b=c('x1a', 'x3')))

  preds1 <- predict(m, X, prob = FALSE)

  expect_equal(as.matrix(preds1$y_pred), y)

  preds2 <- predict(m, X, prob = TRUE)

  expect_true(99 < sum(preds2))
  expect_true(sum(preds2) < 101)


  ## Titanic interaction are better

  t <- na.omit(titanic::titanic_train)
  y <- t$Survived
  X <- t[, c('Pclass', 'Age', 'Sex', 'Fare')]
  X$Sex <- as.numeric(as.factor(X$Sex))

  m1    <- irls(X, y)
  mean1 <- mean(y == predict(m1, X, prob = FALSE))

  m2    <- irls(X, y, interactions = data.frame(a = c('Pclass', 'Age'), b = c('Fare', 'Sex')))
  mean2 <- mean(y == predict(m2, X, prob = FALSE))
  expect_true(mean1 < mean2)
})
