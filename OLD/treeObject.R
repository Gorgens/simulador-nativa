# Prepara ambiente do R

install.packages("gstat")
install.packages('fitdistrplus')
install.packages('R6')
install.packages('evd')
install.packages('rlist')
install.packages('gridExtra')

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

## Modelo do agente árvore

tree = R6Class("tree",									                        # cria objeto árvore
	public = list(							                                        # inicia lista de parâmetros e métodos
		dap = NULL,				                           			        # cria atributo DAP
		risk = NULL,   		
		live = NULL,						                                        # cria flag para identificar vivas
		trackLife = NULL, 									        # cria atributo para contar de vida das árvores
		dead = NULL,											# cria flag para identificar ano em que a árvore morreu 
		initialize = function(
			#dap = abs(rweibull(1, scale=40.07, shape=1.82)), 			        	# inicia DAP recebendo valor vindo de distribuição weibull
			dap = rexp(1, 0.02819),																					# inicia DAP recebendo valor vindo de distribuição exponencial
			risk = 0.024,																										# inicia probabilidade de morrer fixa
			#risk = abs(0.04 * exp(-0.004051 * dap)),                       			# inicia probabilidade de morrer em função do dap exp negativa
			#risk = abs(0.02 * exp(0.004051 * dap)),						# inicia probabilidade de morrer em função do dap exp positiva
                	live = 1,					                                        # inicia flag como viva
			trackLife = 0, 					                                        # inicia contagem de tempo como 1
			dead = NA){						                                # inicia identificador da morte como NA
		self$dap = dap                                                            			# atribui o valor dap ao atributo dap
		self$risk = risk									        # atribui o valor risk ao atributo risk
		self$live = live									        # atribui o valor live à flag live
		self$dead = dead									        # atribui o valor dead à flag dead
		self$trackLife = trackLife							                # atribui o valor tracklife ao atributo tracklife
		},
	evolve = function(){				                                                	# cria método para avançar um ano na simulação
		if(runif(1, 0, 1) <= self$risk){				                                # verifica se morre ou não com base na probabilidade 
			self$live = 0									        # se morrer, flag live assume valor 0
			self$dead = self$trackLife						                # se morrer, flag dead assumo valor armazenado em trackLife
		} 
		if(self$live == 1){									        # condicional para árvore que permenecem vivas
			self$dap = self$dap + 
				abs(rexp(1, 34.26)) * self$dap 	                                      		# aplica incremento baseado numa probabilidade com distribuição exponencial negativa
				#self$risk = abs(0.04 * exp(-0.004051 * self$dap))											# atualiza risco de morrer em função do diâmetro exp negativa
				#self$risk = abs(0.02 * exp(0.004051 * self$dap))												# atualiza risco de morrer em função do diâmetro exp positiva
				self$trackLife = self$trackLife + 1				                # adiciona um ano no atributo trackLife
		}
	},		
	volume = function(){			                                                  		# cria método para obter o volume da árvore
		return(-0.068854 + 0.000841 * self$dap^2)						        # volume baseado numa formula V = f(DAp^2)
	})
)

## Sandbox----------------------------------------------------
tree$new()
