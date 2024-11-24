library(tidyverse)
library(broom)
library(kableExtra)

# Simulation
n <- 100                   # set number of obs to simulate
set.seed(1234)             # set seed for reproducibility
sex <-  rbinom(n, 1, 0.5)  # generate sex variable ~ 50% females (0) and 50% males (1)
# Generate bodyweight from regression model based on additive effect of sex - on average, males are 5 kg heavier than females
bodyweight <-  5 + 5 * sex + rnorm(n, 0, 1)
# Now, let's create some missingness in bodyweight at random (where the missingness in bodyweight depends on sex)
# To do this we will leave the actual bodyweight values intact (as we will need them later) and create a separate missing variable to indicate which values would be absent in a real dataset.
# The model intercept and coefficient have been selected so that the probability of missingness will be ~ 12% for males and ~ 73% for females
logodds_missing <-  1 - 3 * sex                      
odds_missing <-  exp(logodds_missing)               # calculate odds of missingness
prob_missing  <-  odds_missing/(1 + odds_missing)   # calculate probability of missingness
# Assemble dataframe
dat <-  data.frame(sex = factor(sex, levels = c("0", "1"), labels = c("females", "males")),
                   bodyweight = bodyweight,
                   odds_missing = odds_missing,
                   prob_missing = prob_missing,
                   missing = factor(rbinom(n, 1, prob_missing), levels = c("0", "1"), labels = c("no", "yes")))
head(dat, 20) |> 
  kable(align = "c", digits = 2)

# Check that we recover the original coefficients:
summary(lm(bodyweight ~ sex , data = dat))

# Check simulated missing proportions with tabulation
table(dat$prob, dat$missing)
# This is about right
0.119*45 # gives 5/45 males with missingness
0.731*55 # gives 40/55 females with missingness

#++++++++++++++++++++++++++++++

# Test for an association between missingness and sex (MAR)
# Step 1 - test whether missingness depends on sex
mod_sex <- glm(missing ~ sex, data = dat, family = "binomial")
tidy(mod_sex, exp = T)  # i.e. there's an ~ 87% reduction in the odds of having missingness in bodyweight for males relative to females and this is significant

# Step 2 - now test whether missingness depends on bodyweight WITHIN each category of sex - i.e. missingness is random (MCAR) within categories
# An important point here is that we need the missing bodyweight values to test this: see p15. of the "Multiple Imputation and its Application" text -"The statement ‘income is MAR dependent on job type’ is an untestable assumption. The data we would need to test it (represented by the triangles in Figure 1.2) are missing!"
# Females
summary(mod_females <- lm(bodyweight ~ missing, data = subset(dat, sex == "females"))) # i.e. missingness does not depend on bodyweight for females
# Males
summary(mod_males <- lm(bodyweight ~ missing, data = subset(dat, sex == "males"))) # i.e. missingness does not depend on bodyweight for males

# More succint approach
summary(lm(bodyweight ~ missing * sex, data = dat))
# IF MAR, then we expect the coefficients of missing and the interaction of missing with sex to be N.S. In other words, there is no difference in bodyweight based on missingness, once you adjust for sex.
# IF there was a difference in bodyweight based on missingness, once adjusted for sex, this may imply MNAR.

# Boxplots of distributions by sex
ggplot(dat, aes(x = sex, y = bodyweight, fill = missing)) +
  geom_boxplot() +
  geom_dotplot(binaxis = 'y', stackdir = 'center', dotsize = 0.5, position = position_dodge(0.75)) +
  theme_bw(base_size = 20)
