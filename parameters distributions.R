library(gstat)
library(dplyr)
library(sp)
library(tidyr)
library(MASS)
library(fitdistrplus)
library(R6)
library(evd)
library(rlist)
library(ggplot2)
library(gridExtra)

trees = read.csv('inventario.csv')
dim(trees)
trees = trees %>% 
  filter(type == 'O') %>%
  mutate(incR = (DBH.2014 - DBH.2012)/DBH.2012) %>%
  filter(incR >= 0) %>%
  drop_na(canopy.2012) %>%
  drop_na(family.name) %>%
  filter(canopy.2012 != 'C ')
dim(trees)

unique(trees$canopy.2012)
unique(trees$family.name)
unique(trees$Light.2012)

# Verificar se incremento diamétrico tem dependência espacial"""

coordinates(trees) <- ~UTM.Easting+UTM.Northing
trees.var <- variogram(incR~1,data=trees, cutoff=100) 
plot(trees.var)

# Verificar se incremento diamétrico tem relação com posição sociológica família ou luz"""

glm.growth <- glm(incR ~ family.name + canopy.2012 + Light.2012, data = trees, family = binomial(link = "logit"))
summary(glm.growth)

# Ajusta distribuição

## Parâmetros distribuição incR


hist(trees$incR, breaks=100, xlim=c(0,0.7))

incR.exp <- fitdist(trees$incR, "exp")
summary(incR.exp)

#sim.incR <- rbeta(2000, shape1 = 0.35, shape2 = 11)        # Draw N beta distributed values
sim.incR = rexp(2000, 34.26)
hist(sim.incR, breaks = 100, xlim = c(0, 0.7))

# Parâmetros distribuição DAP"""

centers = seq(10, max(trees$DBH.2012), by = 10)

hist(trees$DBH.2012, breaks = centers, xlim = c(0, 200))

dap.weibull <- fitdist(trees$DBH.2012, "weibull")
summary(dap.weibull)

sim.dap <- rweibull(300, scale=40.07, shape=1.82)
hist(sim.dap, xlim = c(0, 200))

dap.exp <- fitdist(trees$DBH.2012, "exp")
summary(dap.exp)

sim.dap = rexp(300, 0.02819)
hist(sim.dap, breaks = 10, xlim = c(0, 200))

# Parâmetros distribuição mortalidade por centro de classe"""

cc = c(15, 20, 25, 30, 35, 100, 110, 150, 200, 300)
mort = c(0.06, 0.04, 0.015, 0.012, 0.011, 0.01, 0.02, 0.011, 0.025, 0.03)
mort_nls <- nls(mort ~ a*exp(r*cc), 
               start = list(a = 0.04, r = -0.02))
coef(mort_nls)

cc = seq(15, 200, 10)
mort = 0.04 * exp(-0.004051 * cc)
probMortalidade = data.frame(cc = cc, probMort = mort)

ggplot(probMortalidade, aes(cc, probMort)) + geom_bar(stat="identity")

cc = seq(15, 200, 10)
mort = 0.02 * exp(0.004051 * cc)
probMortalidade = data.frame(cc = cc, probMort = mort)

ggplot(probMortalidade, aes(cc, probMort)) + geom_bar(stat="identity")

cc = seq(15, 200, 10)
#mort = 2 + 0.0005 * cc                                                         # mortalidade constante
#mort = 2 + 0.003 * cc                                                          # mortalidade cresce com o dap de forma menos intensa
mort = 2 + 0.008 * cc
probMortalidade = data.frame(cc = cc, probMort = mort)

ggplot(probMortalidade, aes(cc, probMort)) + geom_bar(stat="identity")]