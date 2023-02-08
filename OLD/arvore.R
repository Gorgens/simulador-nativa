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
        ifelse(tag == 1, abs(2 + 0.008 * self$dap)/100,                         # 1 se mortalidade crescente
          ifelse(tag == 2, abs(0.04 * exp(-0.004051 * self$dap)), 1)))          # 2 se mortalidade decrescente 
    },
    get_data = function(){
      return(data.frame(self$dap, self$live, self$dead, self$trackLife, self$volume))
    }
               )
)