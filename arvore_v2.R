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
				self$volume = -0.068854 + 0.000841 * self$dap^2}},
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