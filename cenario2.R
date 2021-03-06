# Prepara ambiente R

install.packages("gstat")
install.packages('fitdistrplus')
install.packages('R6')
install.packages('evd')
install.packages('rlist')
install.packages('gridExtra')

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

tree = R6Class("tree",											                                    # cria objeto árvore
	public = list(												                                        # inicia lista de parâmetros e métodos
		dap = NULL, 											                                          # cria atributo DAP
		risk = NULL,
    live = NULL, 											                                          # cria flag para identificar vivas
		trackLife = NULL, 									                                        # cria atributo para contar de vida das árvores
		dead = NULL,											                                          # cria flag para identificar ano em que a árvore morreu 
		initialize = function(
							  dap = rexp(1, 0.02819),																					# inicia DAP recebendo valor vindo de distribuição exponencial
								#risk = abs(0.02 * exp(0.004051 * dap)),											  # inicia probabilidade de morrer em função do dap exp positiva
                risk = abs(2 + 0.008 * dap)/100,
                live = 1, 						                                          # inicia flag como viva
							  trackLife = 0, 					                                        # inicia contagem de tempo como 1
							  dead = NA){						                                          # inicia identificador da morte como NA
			self$dap = dap                                                            # atribui o valor dap ao atributo dap
      self$risk = risk									                                        # atribui o valor risk ao atributo risk
			self$live = live									                                        # atribui o valor live à flag live
			self$dead = dead									                                        # atribui o valor dead à flag dead
			self$trackLife = trackLife							                                  # atribui o valor tracklife ao atributo tracklife
		},
		evolve = function(){				                                                # cria método para avançar um ano na simulação
			if(runif(1, 0, 1) <= self$risk){				                                  # verifica se morre ou não com base na probabilidade 
				self$live = 0									                                          # se morrer, flag live assume valor 0
				self$dead = self$trackLife						                                  # se morrer, flag dead assumo valor armazenado em trackLife
			} 
			if(self$live == 1){									                                      # condicional para árvore que permenecem vivas
				self$dap = self$dap + 
					abs(rexp(1, 34.26)) * self$dap 	                                      # aplica incremento baseado numa probabilidade com distribuição exponencial negativa
				#self$risk = abs(0.02 * exp(0.004051 * self$dap))												# atualiza risco de morrer em função do diâmetro exp positiva
        self$risk = abs(2 + 0.008 * self$dap)/100
				self$trackLife = self$trackLife + 1				                              # adiciona um ano no atributo trackLife
			}
		},		
		volume = function(){			                                                  # cria método para obter o volume da árvore
			return(-0.068854 + 0.000841 * self$dap^2)						                      # volume baseado numa formula V = f(DAp^2)
		}
	)
)

floresta = list()								                                                # inicializa lista de árvore da parcela

i = 1											                                                      # inicializa contador para dap incluídos
while (i <= 240){								                                                # looping para criação de 580 árvores
  floresta = c(floresta, tree$new())                                            # cria um objeto árvore viva
  i = i + 1                                                   									# incrementa contador de árvores incluídas
}

volume.simulacao = c()
mortalidade.simulacao = c()
arvores.simulacao = c()

anoSim = 0                                                                      # Calcula indicadores da floresta inicial

volList = c()
mortalidadeList = c()
dapList = c()
for(t in floresta){
  if(t$live == 1){
    volList = c(volList, t$volume())
    dapList = c(dapList, t$dap)
  } else {
    if(t$dead == anoSim){
      mortalidadeList = c(mortalidadeList, 1)
    }
  }
}
volume.simulacao = c(volume.simulacao, sum(volList))
mortalidade.simulacao = c(mortalidade.simulacao, sum(mortalidadeList))
arvores.simulacao = c(arvores.simulacao, length(dapList))

#paste(arvores.simulacao, 'arvores com', volume.simulacao, 'm³')
#paste('morreram', mortalidade.simulacao, 'árvores')

diametros = data.frame(dap = dapList)
g1 = ggplot(diametros, aes(dap)) + geom_histogram(binwidth = 10)                # Distribuição diamétrica inicial

ingressoProb = 0.024

while(anoSim <= 300){                                                           # inicia simulação onde cada ciclo é um ano.
  ingresso = sum(rbinom(length(dapList), 1, ingressoProb))
  i = 1
  while(i <= ingresso){
    floresta = c(floresta, tree$new(dap = 10))
    i = i + 1
  }
  volList = c()
  mortalidadeList = c()
  dapList = c()
  for(t in floresta){
    t$evolve()
    if(t$live == 1){
      volList = c(volList, t$volume())
      dapList = c(dapList, t$dap)
    } else {
      if(t$dead == anoSim){
        mortalidadeList = c(mortalidadeList, 1)
      }
    }
  }
  volume.simulacao = c(volume.simulacao, sum(volList))
  mortalidade.simulacao = c(mortalidade.simulacao, sum(mortalidadeList))
  arvores.simulacao = c(arvores.simulacao, length(dapList))
  anoSim = anoSim + 1
}

diametros = data.frame(dap = dapList)
g2 = ggplot(diametros, aes(dap)) + geom_histogram(binwidth = 10)                # Distribuição diamétrica final


mortalidadeSim = data.frame(tempo = seq(0, anoSim, 1), arvMortas = mortalidade.simulacao)
g3 = ggplot(mortalidadeSim, aes(tempo, arvMortas)) + geom_line(colour = "red")

arvoresSim = data.frame(tempo = seq(0, anoSim, 1), narv = arvores.simulacao)
g4 = ggplot(arvoresSim, aes(tempo, narv)) + geom_line()

estoqueSim = data.frame(tempo = seq(0, anoSim, 1), volume = volume.simulacao)
g5 = ggplot(estoqueSim, aes(tempo, volume)) + geom_line()

grid.arrange(g1,g2,g3,g4,g5, layout_matrix = rbind(c(1,1,1,2,2,2),c(3,3,4,4,5,5)))
