# =========================================
# Instalação de pacotes (descomente se necessário)
# install.packages("ggplot2")
# install.packages("gganimate")
# install.packages("dplyr")
# =========================================

library(ggplot2)
library(gganimate)
library(dplyr)

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

cria_df_poisson <- function(lambda) {
  k_max <- max(15, ceiling(lambda + 4 * sqrt(lambda)))
  k_vals <- 0:k_max
  prob <- dpois(k_vals, lambda = lambda)
  tibble(k = k_vals, Probabilidade = prob)
}

gera_grafico_poisson <- function(lambda, k_user) {
  # Dados completos para curva e destaque
  curva <- cria_df_poisson(lambda) %>%
    mutate(Destaque = ifelse(k == k_user, TRUE, FALSE))
  
  # Dados para animação das barras
  frames <- 0:max(curva$k)
  df_barra <- lapply(frames, function(f) {
    curva %>%
      mutate(Probabilidade = ifelse(k <= f, Probabilidade, 0),
             frame = f)
  }) %>%
    bind_rows()
  
  g <- ggplot() +
    # Curva e pontos laranja FIXOS
    geom_line(data = curva, aes(x = k, y = Probabilidade), color = "darkorange", size = 1.2) +
    geom_point(data = curva, aes(x = k, y = Probabilidade), color = "darkorange", size = 2) +
    # Barras ANIMADAS (crescem por frame)
    geom_col(data = df_barra, aes(x = k, y = Probabilidade, fill = Destaque, group = k), 
             show.legend = FALSE, width = 0.8) +
    scale_fill_manual(values = c("TRUE" = "red", "FALSE" = "royalblue")) +
    geom_text(data = filter(curva, Destaque), 
              aes(x = k, y = Probabilidade, label = sprintf("%.4f", Probabilidade)),
              vjust = -0.5, size = 6, color = "black") +
    labs(
      title = sprintf("Distribuição Poisson (λ = %.2f) – destaque para k = %d", lambda, k_user),
      subtitle = "Linha laranja: massa de probabilidade Poisson fixa; Barras: animação discreta",
      x = "Número de eventos (k)",
      y = "Probabilidade P(X = k)"
    ) +
    theme_minimal(base_size = 15) +
    theme(plot.title = element_text(face = "bold", hjust = 0.5),
          plot.subtitle = element_text(size = 12, hjust = 0.5)) +
    transition_manual(frames = frame)
  
  animate(g, nframes = length(frames), fps = 15, width = 900, height = 500, renderer = gifski_renderer())
}

main_poisson <- function() {
  cat("===== Simulação da Distribuição Poisson =====\n")
  cat("\nEste simulador modela distribuições Poisson.\n")
  cat("Você deve informar:\n  1. O valor de λ (média esperada de eventos).\n  2. O valor de eventos observados (k).\n")
  
  repeat {
    lambda <- as.numeric(readline("Digite o valor de λ (média esperada de eventos, λ > 0): "))
    while (is.na(lambda) || lambda <= 0) {
      cat("Por favor, insira um valor válido para λ (λ > 0).\n")
      lambda <- as.numeric(readline("Digite o valor de λ: "))
    }
    k <- entrada_inteira(sprintf("Digite o número de eventos observados (k, k >= 0): "), min_val = 0)
    
    prob <- dpois(k, lambda = lambda)
    cat(sprintf("\nA probabilidade Poisson de observar k = %d eventos é P(X = %d) = %.6f\n", k, k, prob))
    cat("A curva laranja mostra o comportamento fixo da probabilidade para diferentes valores de k.\n")
    
    g <- gera_grafico_poisson(lambda, k)
    print(g)
    
    continuar <- tolower(readline("Deseja realizar outra simulação? (s/n): "))
    if (continuar == "n" || continuar == "não") {
      cat("Simulação encerrada. Bons estudos!\n")
      break
    }
  }
}

main_poisson()