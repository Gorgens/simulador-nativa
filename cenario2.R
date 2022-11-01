# Prepara ambiente R

# Carrega pacotes
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

## Mortalidade exponencial positivo: + dap + mortalidade

source('arvore.R')

ingressoProb = 0.06
anoSim = 0
tempo.sim = 300
floresta = list()								                                                # inicializa lista de árvore da parcela

i = 1											                                                      # inicializa contador para dap incluídos
while (i <= 240){								                                                # looping para criação de 580 árvores
  new = tree$new(dap = rexp(1, 0.02819),
                 risk = 0.024)
  new$risk_update(1)
  floresta = c(floresta, new)                                # cria um objeto árvore viva
  i = i + 1                                                   									# incrementa contador de árvores incluídas
}

# Calcula indicadores da floresta inicial
df.floresta = c()
for(t in floresta){
  df.floresta = rbind(df.floresta, t$get_data())
}

g1 = ggplot(df.floresta, aes(self.dap)) + geom_histogram(binwidth = 10)         # Distribuição diamétrica inicial

narv = c(length(floresta))
volF = c(sum(df.floresta$self.volume))

while(anoSim <= tempo.sim){                                                     # inicia simulação onde cada ciclo é um ano.
  ingresso = sum(rbinom(100, 1, ingressoProb))
  i = 1
  while(i <= ingresso){
    new = tree$new(dap = 10)
    new$risk_update(1)
    floresta = c(floresta, new)
    i = i + 1
  }
  ntemp = 0
  vtemp = 0
  for(t in floresta){
    t$evolve(1)
    if(t$live == 1){
      ntemp = ntemp + 1
      vtemp = vtemp + t$volume
    }
  }
  narv = c(narv, ntemp)
  volF = c(volF, vtemp)
  anoSim = anoSim + 1
}

df.floresta.futuro = c()
for(t in floresta){
  df.floresta.futuro = rbind(df.floresta.futuro, t$get_data())
}

g2 = df.floresta.futuro %>% 
  filter(self.live == 1) %>% 
  ggplot(aes(self.dap)) + geom_histogram(binwidth = 10)                         # Distribuição diamétrica final

g3 = df.floresta.futuro %>%
  filter(self.live == 0) %>%
  group_by(self.trackLife) %>%
  summarise(narv = n()) %>%
  ggplot(aes(self.trackLife, narv)) + geom_line(colour = "red")                 # Número de árvores mortas por hectare

g4 = ggplot(data.frame(tempo = seq(0, anoSim, 1), narv = narv), 
            aes(tempo, narv)) + geom_line()                                     # Número de árvores ao longo do tempo

g5 = ggplot(data.frame(tempo = seq(0, anoSim, 1), volume = volF), 
            aes(tempo, volume)) + geom_line()                                   # Volume de madeira ao longo do tempo

grid.arrange(g1,g2,g3,g4,g5, layout_matrix = rbind(c(1,1,1,2,2,2),c(3,3,4,4,5,5)))