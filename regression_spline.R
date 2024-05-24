library(splines)
library(ggplot2)

x = seq(0, 10, length.out = 100)  # Define the range of x values
degree = 3  # Degree of the B-spline
knots = c(2, 4, 6, 8)  # Knots at which the B-spline basis functions are defined
bspline_basis = bs(x, degree = degree, knots = knots, Boundary.knots = c(min(x), max(x)))

# Plotting
bspline_df = as.data.frame(bspline_basis)
bspline_df$x = x
bspline_df <- tidyr::pivot_longer(bspline_df, -x, names_to = "Basis", values_to = "Value")

ggplot(bspline_df, aes(x = x, y = Value, color = Basis)) +
  geom_line() +
  labs(x = "x", y = "B-spline basis functions") +
  theme_minimal()

### Fitting example
set.seed(123)
x = seq(0, 10, length.out = 100)
y = sin(x) + rnorm(100, sd = 0.2)  # observed with normal noise
y.true = sin(x)
data = data.frame(x, y)

degree = 3  # Degree of the B-spline
knots = c(2, 4, 6, 8)  # Knots at which the B-spline basis functions are defined
bspline_basis = bs(x, degree = degree, knots = knots, Boundary.knots = c(min(x), max(x)))

# Fit linear regression model using B-spline basis functions
model <- lm(y ~ bspline_basis)
summary(model)

# Plotting
ggplot() + geom_line(aes(x=x,y=model$fitted.values),color="blue") +
  geom_line(aes(x=x,y=y)) +
  geom_line(aes(x=x,y=y.true),color="green")
