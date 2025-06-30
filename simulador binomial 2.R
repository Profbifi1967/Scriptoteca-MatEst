# =========================================
# Instalação de pacotes (descomente se necessário)
# install.packages("ggplot2")
# install.packages("gganimate")
# install.packages("dplyr")
# =========================================

library(ggplot2)
library(gganimate)
library(dplyr)

# =========================================
# Funções para simulação e visualização
# =========================================

# Função para solicitar entrada do usuário com validação
entrada_inteira <- function(prompt, min_val = 0) {
  repeat {
    inp <- readline(prompt)
    val <- suppressWarnings(as.integer(inp))
    if (!is.na(val) && val >= min_val) {
      return(val)
    }
    cat("Por favor, insira um número inteiro maior ou igual a", min_val, "\n")
  }
}

# Criação do DataFrame para a Distribuição Poisson
cria_df_poisson <- function(lambda) {
  k_max <- max(15, ceiling(lambda + 4 * sqrt(lambda)))  # Faixa de valores razoável
  k_vals <- 0:k_max
  prob <- dpois(k_vals, lambda = lambda)
  tibble(k = k_vals, Probabilidade = prob)
}

# Função para gerar gráfico animado com curva fixa
gera_grafico_poisson <- function(lambda, k_user) {
  df <- cria_df_poisson(lambda) %>%
    mutate(Destaque = ifelse(k == k_user, TRUE, FALSE))
  
  # Adiciona a curva fixa (todos os valores de probabilidade de k)
  curva_fixa <- df
  
  g <- ggplot() +
    # Curva de densidade discreta fixa (Linha Laranja)
    geom_line(data = curva_fixa, aes(x = k, y = Probabilidade, group = 1),
              color = "darkorange", size = 1.2) +
    geom_point(data = curva_fixa, aes(x = k, y = Probabilidade),
               color = "darkorange", size = 2) +
    # Barras animadas
    geom_col(data = df, aes(x = k, y = Probabilidade, fill = Destaque),
             show.legend = FALSE, width = 0.8) +
    scale_fill_manual(values = c("TRUE" = "red", "FALSE" = "royalblue")) +
    labs(
      title = sprintf("Distribuição Poisson: λ = %.2f — destaque para k = %d", lambda, k_user),
      subtitle = "Linha laranja: probabilidade P(X = k) fixa | Barras: animação por k",
      x = "Número de eventos (k)",
      y = "Probabilidade P(X = k)"
    ) +
    theme_minimal(base_size = 15) +
    theme(plot.title = element_text(face = "bold", hjust = 0.5),
          plot.subtitle = element_text(size = 12, hjust = 0.5)) +
    geom_text(data = df, aes(x = k, y = Probabilidade,
                             label = ifelse(Destaque, sprintf("%.4f", Probabilidade), "")),
              vjust = -0.5, size = 5, color = "black") +
    transition_states(k, transition_length = 1, state_length = 1, wrap = FALSE) +
    enter_fade() +
    exit_fade()
  
  # Gera e exibe a animação
  animate(g, nframes = length(curva_fixa$k) + 5, fps = 15, width = 900, height = 500, renderer = gifski_renderer())
}

# =========================================
# Função principal do programa
# =========================================

main_poisson <- function() {
  cat("===== Simulação da Distribuição Poisson =====\n")
  cat("\nEste simulador modela distribuições Poisson.\n")
  cat("Você deve informar:\n  1. O valor de λ (média esperada de eventos).\n  2. O valor de eventos observados (k).\n")
  
  repeat