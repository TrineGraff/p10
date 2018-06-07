getmin_l = function (lambda, cvm, cvsd) 
{
  cvmin = min(cvm, na.rm = TRUE)
  idmin = cvm <= cvmin
  lambda.min = max(lambda[idmin], na.rm = TRUE)
  idmin = match(lambda.min, lambda)
  semin = (cvm + cvsd)[idmin]
  idmin = cvm <= semin
  lambda.1se = min(lambda[idmin], na.rm = TRUE) 
  list(lambda.min = lambda.min, lambda.1se = lambda.1se,
       idx_1se = match(lambda.1se, lambda), idx_min = match(lambda.min, lambda))
}