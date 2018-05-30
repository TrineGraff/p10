# Mean Absolute Error
mae <- function(error)
{
  mean(abs(error))
}

# Root Mean Squared Error
mse <- function(error)
{
  (mean(error^2))
}