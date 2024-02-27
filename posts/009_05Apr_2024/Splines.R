# I think the plan for this post would be to show how fitting a straight line is not a good option. Then illustrate alternatives - quadratic/cubic, piecewise linear and finally splines
# 


# From: https://bookdown.org/tpinto_home/Beyond-Linearity/#introduction

library(MultiKink)
library(tidyverse)
library(here)

set.seed(1974)
data(triceps)
heart <- read.csv(here("data", "SA_heart.csv"), header = T)

# 1. Polynomial Regression section ----
tri.age.plot <- ggplot(triceps, aes(x = age, y = triceps)) +
  geom_point(alpha = 0.55, color = "black") + 
  theme_minimal() 
tri.age.plot

model.cubic <- lm(triceps~age + I(age^2) + I(age^3),
                  data=triceps)
summary(model.cubic)

model.cubic.poly <- lm(triceps~poly(age,3),
                       data=triceps)
summary(model.cubic.poly)

plot(predict(model.cubic.poly), predict(model.cubic))

# Plots - use stat_smooth
tri.age.plot + 
  stat_smooth(method = "lm", 
              formula = y~poly(x,3,raw=T), size = 1)
# Compare to other visualisation methods
library(jtools)
effect_plot(model.cubic, pred = age, plot.points = T) 
library(sjPlot)
plot_model(model.cubic, type = "pred", terms = c("age [all]"), show.data = T)

# Logistic model - exercise
mod.logistic <- glm(chd ~ tobacco, family = 'binomial', data = heart)
summary(mod.logistic)
ggplot(heart, aes(x=tobacco, y=chd)) +
  geom_point(alpha=0.55, color="black") + 
  theme_minimal() +
  stat_smooth(method = "glm", 
              formula = y~x, method.args=list(family="binomial"), size = 1)
effect_plot(mod.logistic, pred = tobacco, plot.points = T) 

# Visualise what the observed trend (not predicted) actually looks like by binning tobacco and plotting
heart <- heart %>% 
  mutate(tobacco_cat = cut(tobacco, breaks = c(seq(-2,32, by = 2))))

heart |> 
  group_by(tobacco_cat) |> 
  summarise(mean = mean(chd)) |> 
  ggplot(aes(x = tobacco_cat, y = mean, group = 1)) +
  geom_point() +
  geom_line() +
  stat_smooth(method = "loess",size = 1)


# 2. Piecewise Regression and Splines section ----
# Create the new age variables at predefined knots to see what is happening
triceps <- triceps |> 
  mutate(age5 = (age - 5) * (age >= 5),     # will be 0 if age < 5
         age10 = (age - 10) * (age >= 10),  # will be 0 if age < 10, etc
         age20 = (age - 20) * (age >= 20),
         age30 = (age - 30) * (age >= 30),
         age40 = (age - 40) * (age >= 40))
head(triceps, 40)
# This is equivalent to 'change-in-slope' coding as in Michael Mitchell's Stata Visualisation text.

# Predict
triceps$pred7 <- predict(lm(triceps ~ age + 
                              I((age - 5) * (age >= 5)) +
                              I((age - 10) * (age >= 10)) +
                              I((age - 20) * (age >= 20)) +
                              I((age - 30) * (age >= 30)) +
                              I((age - 40) * (age >= 40)),
                            data = triceps))

# Should give the same result
triceps$pred8 <- predict(lm(triceps ~ age + age5 + age10 + age20 + age30 + age40, data = triceps))

# Plot
ggplot(triceps, aes(x = age, y = triceps)) +
  geom_point(alpha = 0.55, color = "black") + 
  theme_minimal()  +
  geom_line(data = triceps, 
            aes(y = pred7, x=age), size = 1, col="blue") 

# Piecewise Linear Model
mod_linear_piece <- lm(triceps ~ age + age5 + age10 + age20 + age30 + age40, data = triceps)
summary(mod_linear_piece)

# Get the slopes for each segment
library(multcomp)
names(coef(mod_linear_piece))
summary(lin_com <- glht(mod_linear_piece, linfct = c("age = 0",
                                          "age + age5 = 0",
                                          "age + age5 + age10 = 0",
                                          "age + age5 + age10 + age20 = 0",
                                          "age + age5 + age10 + age20 + age30= 0",
                                          "age + age5 + age10 + age20 + age30 + age40= 0")))
confint(lin_com)

# This also works
library(survey)
svycontrast(mod_linear_piece, c(0, 1, 0, 0, 0, 0, 0)) # age < 5 slope
svycontrast(mod_linear_piece, c(0, 1, 1, 0, 0, 0, 0)) # age 5 - 10 slope
svycontrast(mod_linear_piece, c(0, 1, 1, 1, 0, 0, 0)) # age 10 - 20 slope
svycontrast(mod_linear_piece, c(0, 1, 1, 1, 1, 0, 0)) # age 20 - 30 slope
svycontrast(mod_linear_piece, c(0, 1, 1, 1, 1, 1, 0)) # age 30 - 40 slope
svycontrast(mod_linear_piece, c(0, 1, 1, 1, 1, 1, 1)) # age > 40 slope

# B splines
library(splines)
cub.splines.bs <- lm(triceps ~ bs(age, knots = c(5,10,20,30,40)), data=triceps)
summary(cub.splines.bs)

# Natural splines (restricted cubic splines)
cub.splines.ns <- lm(triceps ~ ns(age, knots = c(5,10,20,30,40)), data=triceps)
summary(cub.splines.ns)

# Plot
tri.age.plot <- ggplot(triceps, aes(x=age, y=triceps)) +
geom_point(alpha=0.55, color="black") + 
  theme_minimal() 

tri.age.plot +
  stat_smooth(method = "lm", 
              formula = y~bs(x,knots = c(5,10,20,30,40)), 
              lty = 1, col = "blue") + 
  stat_smooth(method = "lm", 
              formula = y~ns(x,knots = c(5,10,20,30,40)), 
              lty = 1, col = "red") +
  stat_smooth(method = "loess", col = "green")




tri.age.plot +
  stat_smooth(method = "loess", aes(colour = "Loess"), se=F) +
  stat_smooth(method = "lm", 
              formula = y~ns(x,knots = c(5,10,20,30,40)), 
              lty = 1, aes(colour = "RCS"), se=F) + 
  scale_colour_manual(name="Model", values=c("blue", "red")) +
  theme_classic(base_size = 20)
