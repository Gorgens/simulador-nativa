library(dplyr)
library(tidyr)
library(R6)
library(ggplot2)
library(gridExtra)

#cria objeto
arvore = R6Class("arvore",
	# os parâmetros do objeto são diametro (DAP em cm), chance de morrer (%), identificador de viva ou morta (binário),
	# anos sob monitoramento (anos), volume da árvore
	public = list(dap = NULL, chanceMorrer = NULL, viva = NULL, anosMonitoramento = NULL, volume = NULL,
		# função de inicialização
		initialize = function(dap = rexp(1, 0.02819), chanceMorrer = 0.02, viva = 1, anosMonitoramento = 0, volume = 0){
			self$dap = dap
			self$chanceMorrer = chanceMorrer
			self$viva = viva
			self$anosMonitoramento = anosMonitoramento
			self$volume = -0.068854 + 0.000841 * dap^2},
		# Função para crescer uma árvore
		cresce = function(tag){
			# Antes de crescer, verifica se árvore permanece viva
			if(runif(1, 0, 1) <= self$chanceMorrer){
				self$viva = 0}
			# Função de crescimento só é aplicada se a árvore estiver viva
			if(self$viva == 1){
				self$dap = self$dap + abs(rexp(1, 34.26)) * self$dap
				self$chanceMorrer = self$chanceMorrer_update(tag)
				self$anosMonitoramento = self$anosMonitoramento + 1
				self$volume = ifelse(-0.068854 + 0.000841 * self$dap^2 < 0, 0.001, -0.068854 + 0.000841 * self$dap^2)}},
		# Atualiza chance de morrer por três regras diferentes
		chanceMorrer_update = function(tag){
			# Se tag == 0, mantem valor fixo para a chance
			# Se tag == 1, atualiza valor em que a chance de morrer aumenta com o aumento do DAP
			# Se tag == 2, atualiza valor em que a chance de morrer diminui com o aumento do DAP 
			self$chanceMorrer = ifelse(tag == 0, self$chanceMorrer,
								# tendencia linear de aumento da probabilidade
								# com intercepto em 1% e probabilidade a 300 cm de DAP é de 8%
								ifelse(tag == 1, 0.01 + 0.00023 * self$dap,
								# tendencia exponencial de redução da probabilidade com o aumento do DAP
								ifelse(tag == 2, abs(0.04 * exp(-0.004051 * self$dap)), 1)))},
		# Função para exibir atributos da árvore
		mostraAtributos = function(){
			return(data.frame(self$dap, self$viva, self$anosMonitoramento, self$volume))}))

percentualRecrutamento = 0.06
anoSimulacao = 0
tempoSimulacao = 300
# Inicializa a floresta onde as árvores serão armazenadas
floresta = list()

i = 1
# cria uma floresta com o número de árvores difinido dentro do looping
while (i <= 240){
	new = arvore$new(dap = rexp(1, 0.02819))
	new$chanceMorrer_update(1)
	floresta = c(floresta, new)
	i = i + 1
}

# Abre dataframe com a floresta inicial
dataFrameFloresta = c()
for(t in floresta){
  dataFrameFloresta = rbind(dataFrameFloresta, t$mostraAtributos())
}

# Obtem a distribuição diamétrica inicial
g1 = ggplot(dataFrameFloresta, aes(self.dap)) + geom_histogram(binwidth = 10)
# Calcula o número de árvore presente na floresta inicial
numeroArvoresNaSimulacao = c(length(floresta))
# Calcula volume total presente na floresta inicial
evolucaoVolumeNaSimulacao = c(sum(dataFrameFloresta$self.volume))

# inicia simulação da floresta
while(anoSimulacao <= tempoSimulacao){
	ingresso = sum(rbinom(100, 1, percentualRecrutamento))
	i = 1
	# Adiciona árvores que estão ingressando
	while(i <= ingresso){
		# Toda árvore que ingressa chega na base com 10 cm de diâmetro
		new = arvore$new(dap = 10)
		new$chanceMorrer_update(1)
		floresta = c(floresta, new)
		i = i + 1
	}
	# Acompanha o número de árvores ao longo da simulação
	numeroArvoresTemporario = 0
	# Acompanha o volume de árvores ao longo da simulação
	volumeTemporario = 0
	for(t in floresta){
		t$cresce(1)
		if(t$viva == 1){
			numeroArvoresTemporario = numeroArvoresTemporario + 1
			volumeTemporario = volumeTemporario + t$volume
		}
	}
	numeroArvoresNaSimulacao = c(numeroArvoresNaSimulacao, numeroArvoresTemporario)
	evolucaoVolumeNaSimulacao = c(evolucaoVolumeNaSimulacao, volumeTemporario)
	anoSimulacao = anoSimulacao + 1
}

dataFrameFloresta.futuro = c()
for(t in floresta){
	dataFrameFloresta.futuro = rbind(dataFrameFloresta.futuro, t$mostraAtributos())
}

# Calcula distribuição diamétrica final
g2 = dataFrameFloresta.futuro %>% 
	filter(self.viva == 1) %>% 
	ggplot(aes(self.dap)) + geom_histogram(binwidth = 10)

# Número de árvores mortas por hectare
g3 = dataFrameFloresta.futuro %>%
	filter(self.viva == 0) %>%
	group_by(self.anosMonitoramento) %>%
	summarise(mortalidadeNaSimulacao = n()) %>%
	ggplot(aes(self.anosMonitoramento, mortalidadeNaSimulacao)) + geom_line(colour = "red")                 

# Número de árvores ao longo do tempo
g4 = ggplot(data.frame(tempo = seq(0, anoSimulacao, 1), ArvoresNaSimulacao = numeroArvoresNaSimulacao), 
	aes(tempo, ArvoresNaSimulacao)) + geom_line()                                     

# Volume de madeira ao longo do tempo
g5 = ggplot(data.frame(tempo = seq(0, anoSimulacao, 1), volumeNaSimulacao = evolucaoVolumeNaSimulacao), 
            aes(tempo, volumeNaSimulacao)) + geom_line()                                   

grid.arrange(g1,g2,g3,g4,g5, layout_matrix = rbind(c(1,1,1,2,2,2),c(3,3,4,4,5,5)))