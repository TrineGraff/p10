eta = larinf$vmat
teta = t(eta)

mu = larinf$y
tmu = t(mu)
s = larinf$sign

round(s * ( eta %*% mu ), digits = 3)


