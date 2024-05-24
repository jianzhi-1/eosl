library(ggplot2)

# Generate some sample data
set.seed(123)
x = seq(0, 10, length.out = 100)
y = sin(x) + rnorm(100, sd = 0.2)  # True function sin(x) with added noise
data = data.frame(x, y)

# Fit a smoothing spline
smoothed = smooth.spline(x, y)
smoothed_values = smoothed$y # fitted values

# Convert to data frame
smoothed_df = data.frame(x = x, y = smoothed_values$y)

# Plotting
ggplot(data) +
  geom_point(aes(x = x, y = y), color = "blue") +
  geom_line(data = smoothed_df, aes(x = x, y = y), color = "red", linewidth = 1) +
  labs(title = "Smoothed Spline vs. Original Data", x = "x", y = "y") +
  theme_minimal()
