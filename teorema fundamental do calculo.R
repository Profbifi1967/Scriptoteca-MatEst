# Carregando os pacotes necessários
# Instalação dos pacotes necessários (descomente se precisar instalar)
install.packages(c("ggplot2", "gganimate", "dplyr", "av"))

library(ggplot2)
library(gganimate)
library(dplyr)
library(av)  # Para codificação de vídeo MP4

# Definindo a função que vamos integrar
f <- function(x) {
  return(x^2 + 2*x + 1)
}

# Definindo os limites de integração
a <- 0
b <- 2

# Valor exato da integral (calculado analiticamente)
# ∫(x^2 + 2x + 1)dx de 0 a 2 = [x^3/3 + x^2 + x]_0^2 = 8/3 + 4 + 2 = 14/3 + 6 = 20/3 ≈ 6.67
valor_exato <- (b^3/3 + b^2 + b) - (a^3/3 + a^2 + a)

# Criando pontos para plotar a função
curve_points <- data.frame(
  x = seq(a, b, length.out = 500),
  y = f(seq(a, b, length.out = 500))
)

# PARTE 1: ANIMAÇÃO DA SOMA DE RIEMANN
# Gerando dados para diferentes números de retângulos
n_values <- c(4, 8, 16, 32, 64, 128)
all_rectangles <- data.frame()
results <- data.frame(n = n_values, soma = numeric(length(n_values)))

for (i in 1:length(n_values)) {
  n <- n_values[i]
  dx <- (b - a) / n
  x_points <- seq(a, b - dx, by = dx)
  heights <- f(x_points)
  areas <- heights * dx
  soma_riemann <- sum(areas)
  results$soma[i] <- soma_riemann
  
  # Criando um dataframe para os retângulos
  for (j in 1:length(x_points)) {
    rect <- data.frame(
      x = c(x_points[j], x_points[j] + dx, x_points[j] + dx, x_points[j]),
      y = c(0, 0, heights[j], heights[j]),
      n = n,
      id = j,
      frame = i
    )
    all_rectangles <- rbind(all_rectangles, rect)
  }
}

# Criando a primeira parte da animação (Soma de Riemann)
p1 <- ggplot() +
  # Plotando os retângulos
  geom_polygon(data = all_rectangles, aes(x = x, y = y, group = interaction(frame, id)), 
               fill = "skyblue", alpha = 0.8, color = "blue", size = 0.2) +
  # Plotando a curva
  geom_line(data = curve_points, aes(x = x, y = y), color = "red", size = 2) +
  # Eixos
  geom_segment(aes(x = a, y = 0, xend = b + 0.2, yend = 0), 
               arrow = arrow(length = unit(0.5, "cm")), size = 1) +
  geom_segment(aes(x = a, y = 0, xend = a, yend = max(curve_points$y) * 1.2), 
               arrow = arrow(length = unit(0.5, "cm")), size = 1) +
  # Título e rótulos
  labs(
    title = "Teorema Fundamental do Cálculo: Soma de Riemann",
    x = "x",
    y = "f(x)"
  ) +
  # Anotações
  annotate("text", x = 1, y = max(curve_points$y) * 1.3, 
           label = "f(x) = x² + 2x + 1", color = "red", size = 8, fontface = "bold") +
  annotate("text", x = b + 0.1, y = 0.3, label = "x", size = 8, fontface = "bold") +
  annotate("text", x = a + 0.2, y = max(curve_points$y) * 1.1, label = "f(x)", size = 8, fontface = "bold") +
  # Informações sobre a soma de Riemann em posição elevada
  annotate("text", x = 1, y = max(curve_points$y) * 0.8, 
           label = "Número de retângulos: {closest_state}", 
           size = 8, hjust = 0.5, color = "darkblue", fontface = "bold") +
  annotate("text", x = 1, y = max(curve_points$y) * 0.65, 
           label = "Soma de Riemann: {sprintf('%.4f', results$soma[which(n_values == closest_state)[1]])}", 
           size = 8, hjust = 0.5, color = "darkblue", fontface = "bold") +
  annotate("text", x = 1, y = max(curve_points$y) * 0.5, 
           label = paste("Valor exato da integral: ", sprintf("%.4f", valor_exato)), 
           size = 8, hjust = 0.5, color = "darkgreen", fontface = "bold") +
  # Explicação
  annotate("text", x = 1, y = -1.5, 
           label = paste("Quando o número de retângulos aumenta,", 
                         "\na soma de Riemann se aproxima da área exata sob a curva."), 
           size = 7, hjust = 0.5, fontface = "bold") +
  # Tema e estilo
  theme_minimal(base_size = 18) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 24, face = "bold", margin = margin(b = 30)),
    axis.title = element_text(size = 20, face = "bold"),
    axis.text = element_text(size = 18),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray90"),
    plot.margin = margin(t = 40, r = 40, b = 60, l = 40, unit = "pt")
  ) +
  # Limites do gráfico
  coord_cartesian(xlim = c(a - 0.3, b + 0.3), ylim = c(-2, max(curve_points$y) * 1.5)) +
  # Animação com pausa no final
  transition_states(
    n,
    transition_length = 2,
    state_length = ifelse(n == max(n_values), 42, 3)  # 7 segundos (42 frames) para o último estado
  ) +
  ease_aes('linear')

# PARTE 2: ANIMAÇÃO DA ÁREA SOB A CURVA
# Criando dados para a área sob a curva com retângulos de largura decrescente
area_data <- data.frame()
n_frames <- 6  # Número de frames para a animação

for (frame in 1:n_frames) {
  # Aumentando o número de retângulos a cada frame
  n_rects <- 4 * 2^(frame-1)  # 4, 8, 16, 32, 64, 128
  dx <- (b - a) / n_rects
  x_points <- seq(a, b - dx, by = dx)
  
  for (j in 1:length(x_points)) {
    rect <- data.frame(
      x = c(x_points[j], x_points[j] + dx, x_points[j] + dx, x_points[j]),
      y = c(0, 0, f(x_points[j]), f(x_points[j])),
      frame = frame,
      id = j
    )
    area_data <- rbind(area_data, rect)
  }
}

# Criando a segunda parte da animação (Área sob a curva)
p2 <- ggplot() +
  # Plotando os retângulos
  geom_polygon(data = area_data, aes(x = x, y = y, group = interaction(frame, id)), 
               fill = "skyblue", alpha = 0.8, color = "blue", size = 0.2) +
  # Plotando a curva
  geom_line(data = curve_points, aes(x = x, y = y), color = "red", size = 2) +
  # Eixos
  geom_segment(aes(x = a, y = 0, xend = b + 0.2, yend = 0), 
               arrow = arrow(length = unit(0.5, "cm")), size = 1) +
  geom_segment(aes(x = a, y = 0, xend = a, yend = max(curve_points$y) * 1.2), 
               arrow = arrow(length = unit(0.5, "cm")), size = 1) +
  # Título e rótulos
  labs(
    title = "Interpretação Geométrica do Teorema Fundamental do Cálculo",
    x = "x",
    y = "f(x)"
  ) +
  # Anotações
  annotate("text", x = 1, y = max(curve_points$y) * 1.3, 
           label = "f(x) = x² + 2x + 1", color = "red", size = 8, fontface = "bold") +
  # Informações sobre a interpretação geométrica
  annotate("text", x = 1, y = max(curve_points$y) * 0.8, 
           label = "Número de retângulos: {4 * 2^(closest_state-1)}", 
           size = 8, hjust = 0.5, color = "darkblue", fontface = "bold") +
  annotate("text", x = 1, y = max(curve_points$y) * 0.65, 
           label = paste("Área exata sob a curva = ", sprintf("%.4f", valor_exato)), 
           size = 8, hjust = 0.5, color = "darkgreen", fontface = "bold") +
  # Explicação
  annotate("text", x = 1, y = -1.5, 
           label = paste("O Teorema Fundamental do Cálculo estabelece que:",
                         "\nF(b) - F(a) = ∫f(x)dx de a até b = área sob a curva f(x)"), 
           size = 7, hjust = 0.5, fontface = "bold") +
  # Tema e estilo
  theme_minimal(base_size = 18) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 24, face = "bold", margin = margin(b = 30)),
    axis.title = element_text(size = 20, face = "bold"),
    axis.text = element_text(size = 18),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray90"),
    plot.margin = margin(t = 40, r = 40, b = 60, l = 40, unit = "pt")
  ) +
  # Limites do gráfico
  coord_cartesian(xlim = c(a - 0.3, b + 0.3), ylim = c(-2, max(curve_points$y) * 1.5)) +
  # Animação com pausa no final
  transition_states(
    frame,
    transition_length = 2,
    state_length = ifelse(frame == max(frame), 42, 3)  # 7 segundos (42 frames) para o último estado
  ) +
  ease_aes('linear')

# Criando o diretório se não existir
dir.create("E:/Fatec Araraquara/2025-1/Calculo ADS", showWarnings = FALSE, recursive = TRUE)

# Definindo a taxa de frames por segundo
fps <- 6

# Gerando as animações como vídeos MP4
# Para a primeira animação (Soma de Riemann)
anim_save(
  "E:/Fatec Araraquara/2025-1/Calculo ADS/soma_riemann.mp4",
  animate(
    p1,
    nframes = 100,  # Número suficiente de frames para mostrar toda a animação
    fps = fps,
    width = 1000,
    height = 800,
    renderer = av_renderer()  # Usando o renderer av para MP4
  )
)

# Para a segunda animação (Área sob a curva)
anim_save(
  "E:/Fatec Araraquara/2025-1/Calculo ADS/area_sob_curva.mp4",
  animate(
    p2,
    nframes = 100,  # Número suficiente de frames para mostrar toda a animação
    fps = fps,
    width = 1000,
    height = 800,
    renderer = av_renderer()  # Usando o renderer av para MP4
  )
)

# Criando um script explicativo
cat("
# Script para demonstração do Teorema Fundamental do Cálculo
# Criado para Fatec Araraquara - Cálculo ADS 2025-1

# Este script gera dois vídeos MP4:
# 1. Soma de Riemann aproximando a integral (soma_riemann.mp4) - com pausa de 7 segundos no final
# 2. Área sob a curva com retângulos diminuindo de largura (area_sob_curva.mp4) - com pausa de 7 segundos no final

# Os vídeos foram salvos em:
# E:/Fatec Araraquara/2025-1/Calculo ADS/soma_riemann.mp4
# E:/Fatec Araraquara/2025-1/Calculo ADS/area_sob_curva.mp4

# Para executar este script novamente, certifique-se de ter os pacotes necessários:
# install.packages(c('ggplot2', 'gganimate', 'dplyr', 'av'))

", file = "E:/Fatec Araraquara/2025-1/Calculo ADS/script_teorema_fundamental_calculo.R")

print("Vídeos MP4 salvos com sucesso em: E:/Fatec Araraquara/2025-1/Calculo ADS/")