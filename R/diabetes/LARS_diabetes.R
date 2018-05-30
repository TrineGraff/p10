library(lars)
library(reshape)
library(gridExtra)

data(diabetes)
diabetes$x

diabetes <- read.csv("/Users/louisenygaardchristensen/Documents/AAU/10. semester/r codes/diabetes/diabetes.csv")
head(diabetes)
tail(diabetes)
# p = 11 (y = prog) og n = 442

diabetes_design <- model.matrix(prog ~ . - 1, data = diabetes)
# y = prog, og vi har fjernet intercept
diabetes_design_std <- scale(diabetes_design, center = TRUE, scale = TRUE)

diabetes_response <- diabetes$prog - mean(diabetes$prog)

# LARS --------------------------------------------------------------------

fit_lars <- lars(diabetes_design_std, diabetes_response, type = "lar", trace = TRUE, 
                   normalize = FALSE, intercept = FALSE) 
# 10 steps
plot(fit_lars)

beta = coef(fit_lars)
s1 <- apply(abs(beta), 1, sum)
f = s1/max(s1)
path <- data.frame(melt(beta),f)
names(path)[1:3] <- c("step","variable","standardized.coef")
gg1 <- ggplot(path,aes(f,standardized.coef,colour=variable))+
  geom_line(aes(group=variable)) +
  labs( x = expression(abs(beta) / max(abs(beta))), y = "Koefficienter") +
  geom_vline(xintercept = f[1], colour = "grey") +
  geom_vline(xintercept = f[2], colour = "grey") +
  geom_vline(xintercept = f[3], colour = "grey") +
  geom_vline(xintercept = f[4], colour = "grey") +
  geom_vline(xintercept = f[5], colour = "grey") +
  geom_vline(xintercept = f[6], colour = "grey") +
  geom_vline(xintercept = f[7], colour = "grey") +
  geom_vline(xintercept = f[8], colour = "grey") +
  geom_vline(xintercept = f[9], colour = "grey") +
  geom_vline(xintercept = f[10], colour = "grey") +
  geom_vline(xintercept = f[11], colour = "grey") +
  annotate(geom = "text", x=f[1] - 0.03, y=40, label="lambda", size=4, parse = TRUE) +
  annotate(geom = "text", x=f[2] +0.02, y=40, label="lambda", size=4, parse = TRUE) +
  annotate(geom = "text", x=f[3] + 0.02, y=40, label="lambda", size=4, parse = TRUE) +
  annotate(geom = "text", x=f[4] + 0.02, y=40, label="lambda", size=4, parse = TRUE) +
  annotate(geom = "text", x=f[5] + 0.02, y= 40, label="lambda", size=4, parse = TRUE) +
  annotate(geom = "text", x=f[6] + 0.02, y= 40, label="lambda", size=4, parse = TRUE) +
  annotate(geom = "text", x=f[7] + 0.02, y= 40, label="lambda", size=4, parse = TRUE) +
  annotate(geom = "text", x=f[8] + 0.02, y= 40, label="lambda", size=4, parse = TRUE) +
  annotate(geom = "text", x=f[9] + 0.02, y= 40, label="lambda", size=4, parse = TRUE) +
  annotate(geom = "text", x=f[10] + 0.02, y= 40, label="lambda", size=4, parse = TRUE) +
  annotate(geom = "text", x=f[11] + 0.02, y= 40, label="lambda", size=4, parse = TRUE) +
  annotate(geom = "text", x=f[1] - 0.015, y=38, label="0", size=2, parse = TRUE) +
  annotate(geom = "text", x=f[2] + 0.035, y=38, label="1", size=2, parse = TRUE) +
  annotate(geom = "text", x=f[3] + 0.035, y=38, label="2", size=2, parse = TRUE) +
  annotate(geom = "text", x=f[4] + 0.035, y=38, label="3", size=2, parse = TRUE) +
  annotate(geom = "text", x=f[5] + 0.035, y=38, label="4", size=2, parse = TRUE) +
  annotate(geom = "text", x=f[6] + 0.035, y=38, label="5", size=2, parse = TRUE) +
  annotate(geom = "text", x=f[7] + 0.035, y=38, label="6", size=2, parse = TRUE) +
  annotate(geom = "text", x=f[8] + 0.035, y=38, label="7", size=2, parse = TRUE) +
  annotate(geom = "text", x=f[9] + 0.035, y=38, label="8", size=2, parse = TRUE) +
  annotate(geom = "text", x=f[10] + 0.035, y=38, label="9", size=2, parse = TRUE) +
  annotate(geom = "text", x=f[11] + 0.035, y=38, label="10", size=2, parse = TRUE) +
  theme(legend.title=element_blank()) + 
  ggtitle("LARS")
gg1


# LARS med lasso ----------------------------------------------------------

fit_lars.lasso <- lars(diabetes_design_std, diabetes_response, type = "lasso", trace = TRUE, 
                   normalize = FALSE, intercept = FALSE) 
# 12 steps (variablerne tilføjes og nogle fjernes igen)
plot(fit_lars.lasso)

beta = coef(fit_lars.lasso)
s1 <- apply(abs(beta), 1, sum)
f = s1/max(s1)
path <- data.frame(melt(beta),f)
names(path)[1:3] <- c("step","variable","standardized.coef")
gg2 <- ggplot(path,aes(f,standardized.coef,colour=variable))+
  geom_line(aes(group=variable)) +
  labs( x = expression(abs(beta) / max(abs(beta))), y = "Koefficienter") +
  geom_vline(xintercept = f[1], colour = "grey") +
  geom_vline(xintercept = f[2], colour = "grey") +
  geom_vline(xintercept = f[3], colour = "grey") +
  geom_vline(xintercept = f[4], colour = "grey") +
  geom_vline(xintercept = f[5], colour = "grey") +
  geom_vline(xintercept = f[6], colour = "grey") +
  geom_vline(xintercept = f[7], colour = "grey") +
  geom_vline(xintercept = f[8], colour = "grey") +
  geom_vline(xintercept = f[9], colour = "grey") +
  geom_vline(xintercept = f[10], colour = "grey") +
  geom_vline(xintercept = f[11], colour = "grey") +
  geom_vline(xintercept = f[12], colour = "grey") +
  geom_vline(xintercept = f[13], colour = "grey") +
  annotate(geom = "text", x=f[1] - 0.03, y=40, label="lambda", size=4, parse = TRUE) +
  annotate(geom = "text", x=f[2] +0.02, y=40, label="lambda", size=4, parse = TRUE) +
  annotate(geom = "text", x=f[3] + 0.02, y=40, label="lambda", size=4, parse = TRUE) +
  annotate(geom = "text", x=f[4] + 0.02, y=40, label="lambda", size=4, parse = TRUE) +
  annotate(geom = "text", x=f[5] + 0.02, y= 40, label="lambda", size=4, parse = TRUE) +
  annotate(geom = "text", x=f[6] + 0.02, y= 40, label="lambda", size=4, parse = TRUE) +
  annotate(geom = "text", x=f[7] + 0.02, y= 40, label="lambda", size=4, parse = TRUE) +
  annotate(geom = "text", x=f[8] + 0.02, y= 40, label="lambda", size=4, parse = TRUE) +
  annotate(geom = "text", x=f[9] + 0.02, y= 40, label="lambda", size=4, parse = TRUE) +
  annotate(geom = "text", x=f[10] + 0.02, y= 40, label="lambda", size=4, parse = TRUE) +
  annotate(geom = "text", x=f[11] - 0.03, y= 40, label="lambda", size=4, parse = TRUE) +
  annotate(geom = "text", x=f[12] + 0.02, y= 40, label="lambda", size=4, parse = TRUE) +
  annotate(geom = "text", x=f[13] + 0.02, y= 40, label="lambda", size=4, parse = TRUE) +
  annotate(geom = "text", x=f[1] - 0.015, y=38, label="0", size=2, parse = TRUE) +
  annotate(geom = "text", x=f[2] + 0.035, y=38, label="1", size=2, parse = TRUE) +
  annotate(geom = "text", x=f[3] + 0.035, y=38, label="2", size=2, parse = TRUE) +
  annotate(geom = "text", x=f[4] + 0.035, y=38, label="3", size=2, parse = TRUE) +
  annotate(geom = "text", x=f[5] + 0.035, y=38, label="4", size=2, parse = TRUE) +
  annotate(geom = "text", x=f[6] + 0.035, y=38, label="5", size=2, parse = TRUE) +
  annotate(geom = "text", x=f[7] + 0.035, y=38, label="6", size=2, parse = TRUE) +
  annotate(geom = "text", x=f[8] + 0.035, y=38, label="7", size=2, parse = TRUE) +
  annotate(geom = "text", x=f[9] + 0.035, y=38, label="8", size=2, parse = TRUE) +
  annotate(geom = "text", x=f[10] + 0.035, y=38, label="9", size=2, parse = TRUE) +
  annotate(geom = "text", x=f[11] - 0.015, y=38, label="10", size=2, parse = TRUE) +
  annotate(geom = "text", x=f[12] + 0.035, y=38, label="11", size=2, parse = TRUE) +
  annotate(geom = "text", x=f[13] + 0.035, y=38, label="12", size=2, parse = TRUE) +
  theme(legend.title=element_blank()) + 
  ggtitle("LARS med lasso modifikation")
gg2

grid.arrange(gg1, gg2)
