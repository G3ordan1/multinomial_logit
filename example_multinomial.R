# From https://stats.oarc.ucla.edu/r/dae/multinomial-logistic-regression/

# install.packages(c("foreign", "nnet", "ggplot2", "reshape2"))
require(foreign)
require(nnet)
require(ggplot2)
require(reshape2)
# The data set contains variables on 200 students. The outcome variable is prog,
# program type. The predictor variables are social economic status, ses, a
# three-level categorical variable and writing score, write, a continuous
# variable.

# Read a stata file from that website; assign to ml
ml <- read.dta("https://stats.idre.ucla.edu/stat/data/hsbdemo.dta")

names(ml)
# [1] "id"      "female"  "ses"     "schtyp"  "prog"    "read"    "write"   
# "math"    "science" "socst"   "honors"  "awards"  "cid"
str(ml)

with(ml, table(ses, prog)) # Attach the data and makes a table with it

# calculate mean and sd of write by categories found in prog then bind them
# together by row
with(
  ml,
  do.call(
    rbind, tapply(write,prog,function(x) c(M = mean(x), SD = sd(x)))
    )
)

# Change the reference category from general to academic
ml$prog2 <- relevel(ml$prog, ref = "academic")

# multinomial logistic regression for prog2 based on ses and write
test <- multinom(prog2 ~ ses + write, data = ml) 

summary(test) # Get coefficients

# Value of test statistic Z
z <- summary(test)$coefficients / summary(test)$standard.errors 
z

# 2-tailed z test
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p

## extract the coefficients from the model and exponentiate
exp(coef(test)) # Gives the probability I think. Article says relative risk.

head(pp <- fitted(test)) # So THIS one gives probabilities? Will have to check

# dses is a data frame with a column ses for low, middle and high and another
# column with the mean of write on each row.
dses <- data.frame(ses = c("low", "middle", "high"), write = mean(ml$write))
# Predict probabilities using test model and dses as the test data.
predict(test, newdata = dses, "probs")

# data frame with same ses as before but 41 times each and write as range 
# 30 to 70 3 times
dwrite <- data.frame(ses = rep(c("low", "middle", "high"), each = 41),
                     write = rep(c(30:70), 3)
                     )

## store the predicted probabilities for each value of ses and write
pp.write <- cbind(dwrite, predict(test,
                                  newdata = dwrite,
                                  type = "probs",
                                  se = TRUE)
                  )

## calculate the mean probabilities within each level of ses
# colMeans is a function that returns column means.by is a bit like tapply.
# Personal note: Damn never knew about all these functions.
by(pp.write[, 3:5], pp.write$ses, colMeans)

## melt data set to long for ggplot2
# For this one I know you can also use pivot longer.
# It converts wide data (many cols) to long data (many rows).
lpp <- melt(pp.write, id.vars = c("ses", "write"), value.name = "probability")
head(lpp) # view first few rows

## plot predicted probabilities across write values for each level of ses
## facetted by program type
ggplot(lpp, aes(x = write, y = probability, colour = ses)) +
  geom_line() +
  facet_grid(variable ~ ., scales = "free")
