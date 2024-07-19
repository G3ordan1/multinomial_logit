# From https://library.virginia.edu/data/articles/simulating-multinomial-logistic-regression-data
# Simulate multinomial data

set.seed(1)
# randomly sample 300 values from uniform dist'n ranging from 0.5 - 3
x <- runif(n = 300, min = 0.5, max = 3)
# randomly sample "a" and "b" 300 times with replacement
g <- sample(c("a", "b"), size = 300, replace = TRUE)

lp2 <- 3 + -2 * x + -0.7 * (g == "b")
lp3 <- -2 + 1.2 * x + -0.3 * (g == "b")

# denominator, ensures probabilities sum to 1
den <- (1 + exp(lp2) + exp(lp3))
p1 <- 1 / den
p2 <- exp(lp2) / den
p3 <- exp(lp3) / den
P <- cbind(p1, p2, p3)
head(P)

# use round() to suppress precision errors
all(round(apply(P, 1, sum)) == 1)

set.seed(2)
sample(1:3, size = 1, prob = c(0.1, 0.3, 0.6))

set.seed(3)
y <- apply(P, MARGIN = 1, function(x) sample(x = 1:3, size = 1, prob = x))
table(y)

d <- data.frame(y = factor(y), x, g)
str(d)

library(nnet)
m <- multinom(y ~ x + g, data = d, trace = FALSE)
coef(m)

car::Anova(m)

rout <- replicate(n = 500, expr = {
  y <- apply(P, MARGIN = 1, function(x) sample(x = 1:3, size = 1, prob = x))
  d <- data.frame(y = factor(y), x, g)
  m <- multinom(y ~ x + g, data = d, trace = FALSE)
  aod <- car::Anova(m)
  aod["g", "Pr(>Chisq)"] < 0.05
})

mean(rout)

sim_mod <- function(n) {
  # generate predictors
  x <- runif(n = n, min = 0.5, max = 3)
  g <- sample(c("a", "b"), size = n, replace = TRUE)
  # linear predictors
  lp2 <- 3 + -2 * x + -0.7 * (g == "b")
  lp3 <- -2 + 1.2 * x + -0.3 * (g == "b")
  # probabilities
  den <- (1 + exp(lp2) + exp(lp3))
  p1 <- 1 / den
  p2 <- exp(lp2) / den
  p3 <- exp(lp3) / den
  P <- cbind(p1, p2, p3)
  rout <- replicate(n = 500, expr = {
    y <- apply(P, MARGIN = 1, function(x) sample(x = 1:3, size = 1, prob = x))
    d <- data.frame(y = factor(y), x, g)
    m <- multinom(y ~ x + g, data = d, trace = FALSE)
    aod <- car::Anova(m)
    aod["g", "Pr(>Chisq)"] < 0.05
  })
  mean(rout)
}

sizes <- sapply(c(400, 500, 600, 700), sim_mod)
sizes

# group = b with x = 0.5
lp2_b <- 3 + 1.2 * 0.5 + -0.7 * 1
# group = a with x = 0.5
lp2_a <- 3 + 1.2 * 0.5 + -0.7 * 0
# odds of being in category 2 when group = b
odds_b <- plogis(lp2_b) / (1 - plogis(lp2_b))
# odds of being in category 2 when group = a
odds_a <- plogis(lp2_a) / (1 - plogis(lp2_a))
# odds ratio
odds_b / odds_a

# x = 1 with group = a
lp3_x1 <- -2 + 1.2 * 1 + -0.3 * 0
# x = 2 with group = a
lp3_x2 <- -2 + 1.2 * 2 + -0.3 * 0
lp3_x3 <- -2 + 1.2 * 3 + -0.3 * 0
# odds of being in category 3 when x = 1
odds_x1 <- plogis(lp3_x1) / (1 - plogis(lp3_x1))
# odds of being in category 3 when x = 2
odds_x2 <- plogis(lp3_x2) / (1 - plogis(lp3_x2))
odds_x3 <- plogis(lp3_x3) / (1 - plogis(lp3_x3))
# odds ratio
odds_x2 / odds_x1
odds_x3 / odds_x2

linpred1 <- 0.2 + log(2.26) * x
linpred2 <- 0.1 + log(6) * x
