ggplot(preds_rp_cox, aes(x = time, y = Estimate, color = Model)) + 
  geom_line() + 
  scale_x_continuous(breaks = seq(0, 400, by = 50), limits = c(0, 400)) +
  scale_y_continuous(trans = "log", breaks = c(0, 0.5, 1, 1.5, 2)) +
  xlab("Observation Time") + ylab("HR for Treatment") + ggtitle("Time-varying HR for Treatment") +
  theme_bw(base_size = 20) +
  theme(legend.position="bottom")
