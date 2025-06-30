# =========================================
# Instalação de pacotes (descomente se necessário)
# install.packages("ggplot2")
# install.packages("dplyr")
# install.packages("gganimate")
# =========================================

library(ggplot2)
library(gganimate)
library(dplyr)

# =========================================
# Funções para simulação e geração de gráfico
# =========================================

# Função para entrada de valores validados
entrada_inteira <- function(prompt, min_val = 0, max_val = Inf) {
  # Lê e valida se uma entrada é um número inteiro entre o intervalo
  repeat {
    inp <- readline(prompt)
    val <- suppressWarnings(as.integer(inp))
    if (!is.na(val) && val >= min_val && val <= max_val) {
      return(val)
    }
    cat("Por favor, insira um número inteiro entre", min_val, "e", max_val, "\n")
  }
}

# Função para criar o DataFrame da Distribuição Binomial
cria_df_binomial <- function(n, p) {
  k_vals <- 0:n  # Possíveis sucessos
  prob <- dbinom(k_vals, size = n, prob = p)  # Probabilidade
  tibble(k = k_vals, Probabilidade = prob)  # Cria um dataframe
}

# Função para criar gráfico animado da Distribuição Binomial
gera_grafico_binomial <- function(n, p, k_user) {
  df <- cria_df_binomial(n, p) %>%
    mutate(Destaque = ifelse(k == k_user, TRUE, FALSE))
  
  g <- ggplot(df, aes(x = k, y = Probabilidade)) +
    geom_col(aes(fill = Destaque), show.legend = FALSE, width = 0.8) +
    scale_fill_manual(values = c("TRUE" = "red", "FALSE" = "blue")) +
    labs(
      title = sprintf("Distribuição Binomial: n = %d, p = %.2f\nDestaque: k = %d", n, p, k_user),
      x = "Número de sucessos (k)",
      y = "Probabilidade P(X = k)"
    ) +
    theme_minimal(base_size = 15) +
    theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
    geom_text(aes(label = ifelse(Destaque, sprintf("%.4f", Probabilidade), "")),
              vjust = -0.5, size = 5, color = "black") +
    transition_states(k, transition_length = 1, state_length = 1, wrap = FALSE) +
    enter_grow() + 
    exit_fade()
  
  # Gera e exibe a animação
  animate(g, nframes = n + 2, fps = 10, width = 800, height = 500, renderer = gifski_renderer())
}

# =========================================
# Função principal para interatividade
# =========================================

main_binomial <- function() {
  cat("===== Simulação da Distribuição Binomial =====\n")
  cat("\nEste simulador modela experimentos com distribuições binomiais.\n")
  cat("Você deve informar:\n  1. O número de tentativas (n).\n  2. A probabilidade de sucesso (p).\n  3. O valor de sucessos (k).\n\n")
  
  repeat {
    n <- entrada_inteira("Digite o número total de lançamentos (n): ", min_val = 1)
    p <- as.numeric(readline("Digite a probabilidade de sucesso (p entre 0 e 1): "))
    while (is.na(p) || p < 0 || p > 1) {
      cat("Por favor, insira uma probabilidade válida entre 0 e 1.\n")
      p <- as.numeric(readline("Digite a probabilidade de sucesso (p entre 0 e 1): "))
    }
    k <- entrada_inteira(sprintf("Digite o número de sucessos desejados (k, de 0 a %d): ", n), min_val = 0, max_val = n)
    
    prob <- dbinom(k, size = n, prob = p)
    cat(sprintf("\nA probabilidade Binomial de obter k = %d sucessos é P(X = %d) = %.6f\n", k, k, prob))
    
    g <- gera_grafico_binomial(n, p, k)
    print(g)
    
    # Pergunta se o usuário deseja continuar
    continuar <- tolower(readline("Deseja realizar outra simulação? (s/n): "))
    if (continuar == "n" || continuar == "não") {
      cat("Simulação encerrada. Bons estudos!\n")
      break
    }
  }
}

# =========================================
# Executa o programa
# =========================================

main_binomial()