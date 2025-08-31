# This script runs bootstraps on a sample mean and correlation coefficient and then plots the sampling distributions and CI's from the different methods as well as comparing to theoretical results.

library(tidyverse)
library(ggExtra)
library(boot)

data("swiss")
swiss

# Function to quickly extract all CI's from a boot.ci object
extract_boot_ci <-  function(boot_ci_obj){
    ci_df <- names(boot_ci_obj) |> 
        # We only want the CI types, which are stored as matrices
        keep(~ is.matrix(boot_ci_obj[[.x]])) |> 
        map_df(~ {
            ci_matrix <- boot_ci_obj[[.x]]
            # Extract the lower/upper bounds
            lower_bound <- ci_matrix[1, ncol(ci_matrix) - 1]
            upper_bound <- ci_matrix[1, ncol(ci_matrix)]
            # Return a data frame for this CI type
            tibble(
                type = .x,
                lower_ci = lower_bound,
                upper_ci = upper_bound
            )
        }) |> 
        # Optionally, arrange the data frame for cleaner viewing
        mutate(type = factor(type, levels = c("normal", "basic", "percent", "bca", 'Theoretical'), labels = c("Bootstrap - Normal", "Bootstrap - Basic", "Bootstrap - Percentile", "Bootstrap - Bias Corrected", 'Theoretical'))) |> 
        arrange(type)
    ci_df
}

#++++++++++++++++++++++++++++++

# SAMPLE MEAN ----

# Plot raw data
ggplot(swiss, aes(x = Fertility)) +
    geom_histogram(aes(y = ..density..), binwidth = 0.5, fill = "lightblue", color = "black") +
    stat_density(aes(y = ..density..), geom = "point", position = "identity", 
                 color = "darkblue", size = 2) +
    scale_y_continuous(labels = abs) +
    labs(title = "Raw Data Distribution",
         x = "Data Value", 
         y = "Density") +
    theme_minimal()

# boot function
boot_calc_mean <- function(data, indices) {
    # Use the indices to resample the data
    sample_data <- data[indices]
    # Return the statistic (mean in this case)
    return(mean(sample_data))
}

# boot
set.seed(20250111)
boot_mean <- boot(swiss$Fertility, boot_calc_mean, R = 1000)
boot_mean_CIs <- boot.ci(boot_mean, type = "all")
boot_mean_CIs

# df of bootstrapped estimates
bootstrap_df <- data.frame(
    bootstrap_means = as.vector(boot_mean$t),
    original_mean = boot_mean$t0
)

# Calculate percentiles for confidence interval
ci_lower <- quantile(boot_mean$t, 0.025)
ci_upper <- quantile(boot_mean$t, 0.975)

# Plot
ggplot(bootstrap_df, aes(x = bootstrap_means)) +
    geom_histogram(aes(y = after_stat(density)), bins = 30, 
                   fill = "lightblue", color = "black", alpha = 0.7) +
    geom_density(color = "blue", size = 1) +
    geom_vline(aes(xintercept = original_mean, color = "Mean of Original Sample"), linetype = "dashed", size = 1) +
    geom_vline(aes(xintercept = mean(boot_mean$t), color = "Mean of Bootstrap Sample Means"), linetype = "dashed", size = 1) +
    # Add percentile lines for 95% CI
    geom_vline(xintercept = ci_lower, color = "darkorange3", linetype = "dotted", size = 1) +
    geom_vline(xintercept = ci_upper, color = "darkorange3", linetype = "dotted", size = 1) +
    # Add labels for percentile values
    annotate("text", x = ci_lower, y = max(density(boot_mean$t)$y) * 0.8, 
             label = paste("2.5%\n", round(ci_lower, 2)), 
             color = "darkorange3", hjust = 1.1, size = 3.5) +
    annotate("text", x = ci_upper, y = max(density(boot_mean$t)$y) * 0.8, 
             label = paste("97.5%\n", round(ci_upper, 2)), 
             color = "darkorange3", hjust = -0.1, size = 3.5) +
    # Manual color scale for legend
    scale_color_manual(name = "",
                       values = c("Mean of Original Sample" = "red", "Mean of Bootstrap Sample Means" = "darkmagenta")) +
    labs(title = "Bootstrap Distribution with 95% Percentile Confidence Interval",
         x = "Bootstrap Sample Means",
         y = "Density",
         caption = paste("95% Percentile CI: [", round(ci_lower, 2), ", ", round(ci_upper, 2), "]")) +
    theme_minimal() +
    theme(legend.position = "top")

# Compare to  (t.test gives exact CI's - not technically large sample)
t.test_sample_ci <-  t.test(swiss$Fertility)

# Extract CI's into df
boot_mean_CIs_df <-  extract_boot_ci(boot_mean_CIs)
# Add in t.test CI's
boot_mean_CIs_df <-  rbind(boot_mean_CIs_df, data.frame(type = "Theoretical", lower_ci = t.test_sample_ci$conf.int[1], upper_ci = t.test_sample_ci$conf.int[2]))

# Plot all CI's for visualisation
ggplot(boot_mean_CIs_df) +
    aes(xmin = lower_ci, xmax = upper_ci, y = type, color = type) + 
    geom_vline(xintercept = boot_mean$t0, color = "red", linetype = "dashed", linewidth = 1.2) + 
    geom_vline(xintercept = mean(boot_mean$t), color = "darkmagenta", linetype = "dashed", linewidth = 1.2) + 
    geom_errorbar() + 
    theme_minimal() + 
    theme(legend.position = "none") + 
    labs(y = "")
    

#++++++++++++++++++++++++++++++

# SAMPLE CORRELATION COEFFICIENT ----

# Plot bivariate raw data
scatterplot <-  ggplot(swiss, aes(x = Fertility, y = Education)) +
    geom_point(color = "darkblue", alpha = 0.7) +
    theme_minimal() +
    labs(title = "Bivariate Raw Data Distribution",
         x = "Data Value 1",
         y = "Data Value 2")
# Add the marginal density plots using ggMarginal().
# The 'type' argument specifies the type of marginal plot (e.g., "density", "histogram", "boxplot").
# The 'fill' argument sets the fill color of the marginal plots.
ggMarginal(scatterplot,
           type = "density",
           fill = "#D55E00",
           alpha = 0.7)

# boot function (note: data should be a data frame or matrix with 2 columns)
boot_calc_cor <- function(data, indices) {
    # Resample the data using indices (this resamples paired observations)
    resampled_data <- data[indices, ]
    # Calculate correlation between the two variables
    cor(resampled_data[, 1], resampled_data[, 2])
}

# boot
set.seed(20250111)
boot_cor <- boot(cbind(swiss$Fertility, swiss$Education), boot_calc_cor, R = 1000)
boot_cor_CIs <- boot.ci(boot_cor, type = "all")
boot_cor_CIs

# df of bootstrapped estimates
bootstrap_df <- data.frame(
    bootstrap_cors = as.vector(boot_cor$t),
    original_cor = boot_cor$t0
)

# Calculate percentiles for confidence interval
ci_lower <- quantile(boot_cor$t, 0.025)
ci_upper <- quantile(boot_cor$t, 0.975)

# Plot
ggplot(bootstrap_df, aes(x = bootstrap_cors)) +
    geom_histogram(aes(y = after_stat(density)), bins = 30, 
                   fill = "lightblue", color = "black", alpha = 0.7) +
    geom_density(color = "blue", size = 1) +
    geom_vline(aes(xintercept = original_cor, color = "Correlation of Original Sample"), linetype = "dashed", size = 1) +
    geom_vline(aes(xintercept = mean(boot_cor$t), color = "Mean of Bootstrap Sample Correlations"), linetype = "dashed", size = 1) +
    # Add percentile lines for 95% CI
    geom_vline(xintercept = ci_lower, color = "darkorange3", linetype = "dotted", size = 1) +
    geom_vline(xintercept = ci_upper, color = "darkorange3", linetype = "dotted", size = 1) +
    # Add labels for percentile values
    annotate("text", x = ci_lower, y = max(density(boot_mean$t)$y) * 0.8, 
             label = paste("2.5%\n", round(ci_lower, 2)), 
             color = "darkorange3", hjust = 1.1, vjust = -7, size = 3.5) +
    annotate("text", x = ci_upper, y = max(density(boot_mean$t)$y) * 0.8, 
             label = paste("97.5%\n", round(ci_upper, 2)), 
             color = "darkorange3", hjust = -0.1, vjust = -7, size = 3.5) +
    # Manual color scale for legend
    scale_color_manual(name = "",
                       values = c("Correlation of Original Sample" = "red", "Mean of Bootstrap Sample Correlations" = "darkmagenta")) +
    labs(title = "Bootstrap Distribution with 95% Percentile Confidence Interval",
         x = "Bootstrap Sample Correlations",
         y = "Density",
         caption = paste("95% Percentile CI: [", round(ci_lower, 2), ", ", round(ci_upper, 2), "]")) +
    theme_minimal() +
    theme(legend.position = "top")

# Compare to (cor.test gives large sample approximation)
cor.test_sample_ci <-  cor.test(swiss$Fertility, swiss$Education)

# Extract CI's into df
boot_cor_CIs_df <-  extract_boot_ci(boot_cor_CIs)
# Add in t.test CI's
boot_cor_CIs_df <-  rbind(boot_cor_CIs_df, data.frame(type = "Theoretical", lower_ci = cor.test_sample_ci$conf.int[1], upper_ci = cor.test_sample_ci$conf.int[2]))

# Plot all CI's for visualisation
ggplot(boot_cor_CIs_df) +
    aes(xmin = lower_ci, xmax = upper_ci, y = type, color = type) + 
    geom_vline(xintercept = boot_cor$t0, color = "red", linetype = "dashed", linewidth = 1.2) + 
    geom_vline(xintercept = mean(boot_cor$t), color = "darkmagenta", linetype = "dashed", linewidth = 1.2) + 
    geom_errorbar() + 
    theme_minimal() + 
    theme(legend.position = "none") + 
    labs(y = "")
