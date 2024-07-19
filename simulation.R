# Enrollment data for a university detailing the program type of students 
# and some information about them.
# Dependent variable program type. General Academy Vocational
# Predictors: ses (socioeconomic status), write (writing score),
# schtyp (school type they attended)

# loading required libraries
library(dplyr)
library(nnet)
set.seed(7) # set seed for reproducibility
# Generate a random sample of socioeconomic statuses (ses)
ses <- sample( 
  c("low", "middle", "high"),  # Categories to sample from
  size = 10000L,               # Sample size of 10,000
  replace = TRUE,              # Allow repetition in sampling
  prob = c(0.235, 0.475, 0.29) # Probability weights for each category
)


# Convert the 'ses' vector to a factor with specified levels
ses <- factor(ses, levels = c("low", "middle", "high"))

# Define parameters for skew-normal distribution for write with "high" SES
high_params <- sn::cp2dp(c(56, 9.44, -0.5), "SN")

# Define mean and standard deviation for normal distribution for write with "low" SES
low_params <- list("mean" = 50.6, "sd" = 9.49)

# Define parameters for skew-normal distribution for write with "middle" SES
middle_params <- sn::cp2dp(c(51.9, 9.11, -0.25), "SN")


# Create a numeric vector of length 10,000 to store data
write <- vector("numeric", 10000L)

# Fill the vector with normally distributed data for "low" SES
write[ses == "low"] <- rnorm(sum(ses == "low"), low_params$mean, low_params$sd)

# Fill the vector with skew-normal distributed data for "middle" SES
write[ses == "middle"] <- sn::rsn(sum(ses == "middle"), dp = middle_params)

# Fill the vector with skew-normal distributed data for "high" SES
write[ses == "high"] <- sn::rsn(sum(ses == "high"), dp = high_params)

# Create a character vector of length 10,000 to store school types
schtyp <- vector("character", 10000L)

# Assign school types to subjects with "low" SES with specified probabilities
schtyp[ses == "low"] <- sample(c("private", "public"), sum(ses == "low"), replace = TRUE, prob = c(0.04, 0.96))

# Assign school types to subjects with "middle" SES with specified probabilities
schtyp[ses == "middle"] <- sample(c("private", "public"), sum(ses == "middle"), replace = TRUE, prob = c(0.2, 0.8))

# Assign school types to subjects with "high" SES with specified probabilities
schtyp[ses == "high"] <- sample(c("private", "public"), sum(ses == "high"), replace = TRUE, prob = c(0.2, 0.8))

# Calculate the linear predictor for general education
lp_general <- 2.39 + -0.43 * (ses == "middle") + -1.10 * (ses == "high") + -0.06 * write + 0.48 * (schtyp == "public")

# Calculate the linear predictor for vocational education
lp_vocation <- 3.23 + 0.51 * (ses == "middle") + -0.83 * (ses == "high") + -0.11 * write + 1.84 * (schtyp == "public")

# Calculate the denominator for the multinomial logistic model probabilities
denominator <- (1 + exp(lp_general) + exp(lp_vocation))

# Calculate the probability of the first outcome
p1 <- 1 / denominator

# Calculate the probability of the second outcome (general education)
p2 <- exp(lp_general) / denominator

# Calculate the probability of the third outcome (vocational education)
p3 <- exp(lp_vocation) / denominator

# Combine the probabilities into a matrix
P <- cbind(p1, p2, p3)

# Display the first few rows of the probability matrix
head(P)

# Check that the sum of probabilities for each row is 1
all(round(apply(P, 1, sum)) == 1)

# Simulate enrollment outcomes based on the probabilities
y <- apply(P, MARGIN = 1, function(x) sample(x = c("academy", "general", "vocation"), size = 1, prob = x))

# Convert the outcome variable to a factor with specified levels
y <- factor(y, levels = c("academy", "general", "vocation"))

# Display the frequency table of the outcome variable
table(y)

# Create a data frame with the simulated data
enrollment <- data.frame(prog = y, write, ses, schtyp)

# Display the structure of the data frame
str(enrollment)

# Fit a multinomial logistic regression model
m <- multinom(prog ~ ses + write + schtyp, data = enrollment, trace = FALSE)

# Display the summary of the model
summary(m)

# Perform an ANOVA on the model
car::Anova(m)

# Odds ratio
exp(coef(m))
# Plot how the variables affect the probability of being in a category
library(effects)
plot(allEffects(m))

newdata <- data.frame(ses = "low", write = 30, schtyp = "public") # Expect vocation
predict(m, newdata = newdata, "probs")
predict(m, newdata)
