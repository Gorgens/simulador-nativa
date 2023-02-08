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

## b1 = (mortalidade - 0.01)/300

# Prepara ambiente R
tree = R6Class("tree",											                                    # cria objeto árvore
               public = list(												                            # inicia lista de parâmetros e métodos
                 dap = NULL, 											                              # cria atributo dap
                 risk = NULL,                                                   # cria probabilidade de morrer
                 live = NULL, 											                            # cria flag para identificar vivas
                 trackLife = NULL, 									                            # cria atributo para idade das árvores
                 volume = NULL,                                                 # cria atributo  volume da árvore
                 dead = NULL,											                              # cria flag para identificar ano em que a árvore morreu 
                 initialize = function(
    dap = rexp(1, 0.02819),																				             	# inicia DAP recebendo valor vindo de distribuição exponencial
    risk = 0.024,																										            # inicia probabilidade de morrer fixa
    live = 1, 						                                                      # inicia flag como viva
    trackLife = 0, 					                                                    # inicia contagem de tempo como 1
    volume = 0,                                                                 # inicia volume
    dead = NA){						                                                      # inicia identificador da morte como NA
                   self$dap = dap                                               # atribui o valor dap ao atributo dap
                   self$risk = risk									                            # atribui o valor risk ao atributo risk
                   self$live = live									                            # atribui o valor live à flag live
                   self$dead = dead									                            # atribui o valor dead à flag dead
                   self$trackLife = trackLife							                      # atribui o valor tracklife ao atributo tracklife
                   self$volume = -0.068854 + 0.000841 * dap^2                   # atribui o valor volume
                 },
    evolve = function(tag){				                                              # cria método para avançar um ano na simulação
      if(runif(1, 0, 1) <= self$risk){				                                  # verifica se morre ou não com base na probabilidade 
        self$live = 0									                                          # se morrer, flag live assume valor 0
        self$dead = self$trackLife						                                  # se morrer, flag dead assumo valor armazenado em trackLife
      } 
      if(self$live == 1){									                                      # condicional para árvore que permenecem vivas
        self$dap = self$dap + 
          abs(rexp(1, 34.26)) * self$dap 	                                      # aplica incremento baseado numa probabilidade com distribuição exponencial negativa
        self$risk = self$risk_update(tag)                                       # atualiza a mortalidade em função do novo diâmetro
        self$trackLife = self$trackLife + 1				                              # adiciona um ano no atributo trackLife
        self$volume = -0.068854 + 0.000841 * self$dap^2                         # calcula volume a partir do novo diametro
      }
    },
    risk_update = function(tag){                                                # funcao para atualziar a mortalidade
      self$risk = ifelse(tag == 0, self$risk,                                   # 0 se mortalidade constante
                         ifelse(tag == 1, abs(0.01 + 0.00037 * self$dap),                         # 1 se mortalidade crescente
                                ifelse(tag == 2, abs(0.04 * exp(-0.004051 * self$dap)), self$risk)))        # 2 se mortalidade decrescente 
    },
    get_data = function(){
      return(data.frame(self$dap, self$live, self$dead, self$trackLife, self$volume))
    }
               )
)

ingressoProb = 0.06
anoSim = 0
tempo.sim = 300
floresta = list()								                                                # inicializa lista de árvore da parcela

i = 1											                                                      # inicializa contador para dap incluídos
while (i <= 240){								                                                # looping para criação de 580 árvores
  d = rexp(1, 0.02819)
  new = tree$new(dap = d,
                 risk = abs(0.01 + 0.00037 * d))
  new$risk_update(1)
  floresta = c(floresta, new)                                                   # cria um objeto árvore viva
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