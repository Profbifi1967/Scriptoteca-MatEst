#instalar
install.packages("gganimate")
install.packages(c("gifski", "transformr"))
# Carga das bibliotecas necessárias
library(ggplot2)
library(gganimate)
library(dplyr)

# Simulação
simulacao_moeda <- function(n) {
  # Verificação se n é um número inteiro positivo
  if (!is.numeric(n) | n < 1 | n %% 1 != 0) {
    stop("n deve ser um número inteiro positivo.")
  }
  
  # Simula o lançamento de moeda
  resultados <- sample(c("Cara", "Coroa"), n, replace = TRUE)
  
  # Cálculo da proporção cumulativa de caras
  dados <- data.frame(
    Lancamento = 1:n,
    Resultado = resultados,
    Proporcao_Cara = cumsum(resultados == "Cara") / (1:n)
  )
  
  return(dados)
}

# Processamento dos Dados
processamento_dados <- function(dados) {
  # Não há necessidade de processamento adicional
  return(dados)
}

# Geração do Gráfico e Animação
geracao_grafico <- function(dados) {
  # Geração do gráfico
  grafico <- ggplot(dados, aes(x = Lancamento, y = Proporcao_Cara)) +
    geom_line() +
    geom_hline(yintercept = 0.5, color = "red", linetype = "dashed") +
    labs(title = "Proporção Cumulativa de Caras", x = "Número de Lançamentos", y = "Proporção de Caras") +
    theme_minimal() +
    transition_reveal(Lancamento) +
    ease_aes('linear') +
    shadow_mark() 
  
  return(grafico)
}

# Orientações de Uso
# Para executar a simulação e gerar a animação, siga os passos abaixo:
n <- 1000 # Altere aqui o número de lançamentos desejado
dados <- simulacao_moeda(n)
dados_processados <- processamento_dados(dados)
grafico <- geracao_grafico(dados_processados)

# Exibir a animação
animate(grafico, nframes = n, fps = 30, width = 800, height = 600)

# Salvar a animação
anim_save("animacao_moeda.gif", grafico, nframes = n, fps = 30, width = 800, height = 600)