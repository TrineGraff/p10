# Mean Absolute Error
mae <- function(error)
{
  mean(abs(error))
}

# Root Mean Squared Error
rmse <- function(error)
{
  sqrt(mean(error^2))
}

