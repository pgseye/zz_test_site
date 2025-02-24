library(survival)
library(tidyverse)

# Load veteran data from survival package
vdata1 <-  veteran
# Make treatment variable equal to 0,1 instead of 1,2 (not necessary, but I just like to work with the former)
vdata1$trt <-  as.numeric(vdata1$trt) - 1
# Create id variable and organise columns
vdata1$id <-  seq(1:dim(vdata1)[1])
vdata1 <-  vdata1 |> 
  select(id, everything())

# Fit basic model with treatment and karno as the only predictors
vfit1 <- coxph(Surv(time, status) ~ trt + age, vdata1)
summary(vfit1)



# Model
vfit2 <- coxph(Surv(tstart, time, status) ~ trt + trt:nsk(time, df = 3), vdata2)
summary(vfit2)
# Create newdat df
tdata <- expand.grid(trt = 1, time = seq(1, 1000, length = 100))
# Predict HR and plot
yhat <- predict(vfit2, newdata = tdata, se.fit = TRUE, type = "risk", reference = "zero")
tdata$fit <-  yhat$fit
ggplot(tdata, aes(x = time, y = fit)) + 
  geom_line(col = 'blue') +
  #scale_y_continuous(trans = scales::log_trans()) +
  scale_y_continuous(trans="log", breaks = c(0, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2)) +
  #scale_x_continuous(limits = c(0, 1000), breaks = seq(0, 1000, by = 50)) +
  #scale_y_continuous(limits = c(0, 2), breaks = seq(0, 2, by = 0.25)) +
  xlab("Observation Time") + ylab("HR") + ggtitle("Cox Model - Spline trt:time interaction") +
  theme_bw(base_size = 15) 

