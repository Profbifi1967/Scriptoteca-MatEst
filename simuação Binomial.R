# =========================================
# Instalação de pacotes (descomente se necessário)
# install.packages("ggplot2")
# install.packages("gganimate")
# install.packages("dplyr")
# =========================================

library(ggplot2)
library(gganimate)
library(dplyr)

# Função de entrada segura para inteiros
entrada_inteira <- function(prompt, min_val = 0, max_val = Inf) {
  repeat {
    inp <- readline(prompt)
    val <- suppressWarnings(as.integer(inp))
    if (!is.na(val) && val >= min_val && val <= max_val) {
      return(val)
    }
    cat("Por favor, insira um número inteiro entre", min_val, "e", max_val, "\n")
  }
}

# DataFrame da distribuição Binomial
cria_df_binomial <- function(n, p) {
  k_vals <- 0:n
  prob <- dbinom(k_vals, size = n, prob = p)
  tibble(k = k_vals, Probabilidade = prob)
}

# Gráfico animado e linha massa de probabilidade discreta
gera_grafico_binomial <- function(n, p, k_user) {
  df <- cria_df_binomial(n, p) %>%
    mutate(Destaque = ifelse(k == k_user, TRUE, FALSE))
  
  frames <- 0:n
  df_anim <- lapply(frames, function(f) {
    df %>%
      mutate(Probabilidade = ifelse(k <= f, Probabilidade, 0),
             frame = f)
  }) %>% bind_rows()
  
  g <- ggplot() +
    # Linha laranja da "curva" discreta de massa de probabilidade (sempre visível)
    geom_line(data = df, aes(x = k, y = Probabilidade), color = "darkorange", size = 1.2) +
    geom_point(data = df, aes(x = k, y = Probabilidade), color = "darkorange", size = 2) +
    # Barras animadas
    geom_col(data = df_anim, aes(x = k, y = Probabilidade, fill = Destaque, group = k),
             show.legend = FALSE, width = 0.8) +
    scale_fill_manual(values = c("TRUE" = "red", "FALSE" = "royalblue")) +
    # Valor da barra vermelha
    geom_text(data = filter(df, Destaque),
              aes(x = k, y = Probabilidade, label = sprintf("%.4f", Probabilidade)),
              vjust = -0.5, size = 6, color = "black") +
    labs(
      title = sprintf("Distribuição Binomial: n = %d, p = %.2f — destaque para k = %d", n, p, k_user),
      subtitle = "Linha laranja: massa de probabilidade binomial; Barras: animação da contagem de sucessos",
      x = "Número de sucessos (k)",
      y = "Probabilidade P(X = k)"
    ) +
    theme_minimal(base_size = 15) +
    theme(plot.title = element_text(face = "bold", hjust = 0.5),
          plot.subtitle = element_text(size = 12, hjust = 0.5)) +
    transition_manual(frames = frame)
  
  animate(g, nframes = length(frames), fps = 12, width = 900, height = 500, renderer = gifski_renderer())
}

# =========================================
# Função principal
# =========================================

main_binomial <- function() {
  cat("===== Simulação da Distribuição Binomial =====\n")
  cat("\nVocê deve informar:\n  1. O número de tentativas (n).\n  2. A probabilidade de sucesso (p).\n  3. O número de sucessos (k).\n\n")
  
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
    
    continuar <- tolower(readline("Deseja realizar outra simulação? (s/n): "))
    if (continuar == "n" || continuar == "não") {
      cat("Simulação encerrada. Bons estudos!\n")
      break
    }
  }
}

main_binomial()