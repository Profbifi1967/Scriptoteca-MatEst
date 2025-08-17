#Prof Bifi - todos os direitos de divulgação são necessários
# Pacotes necessários
library(ggplot2)
library(gganimate)
library(dplyr)

# Limpar ambiente
rm(list = ls())

# Configurar seed para reprodutibilidade
set.seed(42)

# Gerar dados sintéticos com padrões diferentes
n <- 100
x <- seq(0, 10, length.out = n)

# Dados com múltiplos padrões locais
y_linear <- 2 + 1.5*x + rnorm(n, 0, 1.5)
y_exponential <- 2 * exp(0.3*x) + rnorm(n, 0, 5)
y_logistic <- 10 / (1 + exp(-(x-5))) + rnorm(n, 0, 0.5)

# Criar dataframe base
data_base <- data.frame(
  x = rep(x, 3),
  y = c(y_linear, y_exponential, y_logistic),
  tipo = rep(c("Linear", "Exponencial", "Logística"), each = n)
)

# Função para criar segmentos locais
create_segments <- function(x, y, n_segments = 4) {
  segments <- cut(x, breaks = n_segments, labels = FALSE)
  return(segments)
}

# Criar dados para animação
animation_data <- data.frame()

# Fase 1: Regressões Segmentadas (frames 1-20)
for(frame in 1:20) {
  temp_data <- data_base
  temp_data$frame <- frame
  temp_data$fase <- "Segmentada"
  temp_data$segment <- rep(create_segments(x, y_linear), 3)
  animation_data <- rbind(animation_data, temp_data)
}

# Fase 2: Regressão Global (frames 21-40)
for(frame in 21:40) {
  temp_data <- data_base
  temp_data$frame <- frame
  temp_data$fase <- "Global"
  temp_data$segment <- 1
  animation_data <- rbind(animation_data, temp_data)
}

# Criar gráfico animado
p <- ggplot(animation_data, aes(x = x, y = y)) +
  # Pontos de dados
  geom_point(color = "#00D4AA", alpha = 0.7, size = 2) +
  
  # Linhas de regressão condicionais
  geom_smooth(
    aes(group = interaction(tipo, segment, fase)),
    method = "lm", 
    se = FALSE, 
    color = "#FF6B6B", 
    size = 1.2,
    alpha = 0.8
  ) +
  
  # Facetas por tipo de regressão
  facet_wrap(~tipo, scales = "free_y", ncol = 3) +
  
  # Tema escuro personalizado
  theme_void() +
  theme(
    plot.background = element_rect(fill = "#000000", color = NA),
    panel.background = element_rect(fill = "#000000", color = NA),
    strip.background = element_rect(fill = "#1a1a1a", color = NA),
    strip.text = element_text(color = "#FFFFFF", size = 14, face = "bold"),
    plot.title = element_text(color = "#FFFFFF", size = 18, hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(color = "#CCCCCC", size = 12, hjust = 0.5),
    panel.grid = element_blank(),
    axis.text = element_text(color = "#FFFFFF"),
    axis.title = element_text(color = "#FFFFFF")
  ) +
  
  # Labels dinâmicos
  labs(
    title = "Regressões Segmentadas vs. Globais",
    subtitle = "Fase: {closest_state}",
    x = "Variável X",
    y = "Variável Y"
  ) +
  
  # Animação
  transition_states(
    fase,
    transition_length = 2,
    state_length = 3
  ) +
  ease_aes('sine-in-out')

# Renderizar animação
anim <- animate(
  p,
  width = 1200,
  height = 600,
  fps = 10,
  duration = 8,
  renderer = gifski_renderer("regressoes_animadas.gif")
)

# Exibir animação
anim

# Versão estática para teste
static_plot <- ggplot(data_base, aes(x = x, y = y)) +
  geom_point(color = "#00D4AA", alpha = 0.7, size = 2) +
  geom_smooth(method = "lm", se = FALSE, color = "#FF6B6B", size = 1.2) +
  facet_wrap(~tipo, scales = "free_y", ncol = 3) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "#000000", color = NA),
    panel.background = element_rect(fill = "#000000", color = NA),
    strip.background = element_rect(fill = "#1a1a1a", color = NA),
    strip.text = element_text(color = "#FFFFFF", size = 14, face = "bold"),
    plot.title = element_text(color = "#FFFFFF", size = 18, hjust = 0.5, face = "bold"),
    panel.grid = element_blank()
  ) +
  labs(
    title = "Padrões de Regressão: Linear, Exponencial e Logística",
    x = "Variável X",
    y = "Variável Y"
  )

# Mostrar versão estática primeiro
print(static_plot)

# Informações sobre o conceito
cat("\n🔬 CONCEITO DEMONSTRADO:\n")
cat("📊 Regressão Segmentada (Piecewise Regression)\n")
cat("📈 Múltiplas regressões locais vs. regressão global\n")
cat("🎯 Aplicação em Machine Learning e Estatística\n")