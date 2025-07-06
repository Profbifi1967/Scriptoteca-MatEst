# Instale os pacotes se ainda não os tiver
# install.packages("ggplot2")
# install.packages("gganimate")
# install.packages("transformr")
# install.packages("sn")
# install.packages("dplyr")

library(ggplot2)
library(gganimate)
library(transformr)
library(sn)
library(dplyr)

# Definir o caminho para salvar o GIF
output_path <- "E:/codigo/animacao_distribuicao_aprimorada.gif"

# Verificar se o diretório existe, caso contrário, criá-lo
if (!dir.exists(dirname(output_path))) {
  dir.create(dirname(output_path), recursive = TRUE)
}

# Número de frames na animação
n_frames_transition <- 60 # Frames para transição
pause_frames <- 100 # Frames para pausa na simetria (5 segundos a 20fps)

# Criar sequência de alpha values
alpha_first_part <- seq(-12, -0.5, length.out = n_frames_transition/2)
alpha_symmetry <- rep(0, pause_frames) # Pausa na simetria
alpha_second_part <- seq(0.5, 12, length.out = n_frames_transition/2)

alpha_values <- c(alpha_first_part, alpha_symmetry, alpha_second_part)
n_frames <- length(alpha_values)

# Função para criar dados de um frame
create_frame_data <- function(frame_index, alpha_val) {
  # Determinar se estamos na fase de pausa (simetria)
  is_symmetry_pause <- abs(alpha_val) < 0.1
  
  # Gerar dados
  set.seed(if(is_symmetry_pause) 42 else 42 + frame_index)
  sim_data <- rsn(2000, xi = 0, omega = 1, alpha = alpha_val)
  
  # Densidade
  density_data <- density(sim_data, n = 512)
  df_density <- data.frame(
    x = density_data$x,
    y = density_data$y,
    frame = frame_index,
    alpha_value = alpha_val,
    alpha_label = round(alpha_val, 2),
    is_symmetry = is_symmetry_pause,
    data_type = "density"
  )
  
  # Boxplot stats
  quantiles <- quantile(sim_data, probs = c(0.25, 0.5, 0.75))
  q1 <- quantiles[1]
  median_val <- quantiles[2]
  q3 <- quantiles[3]
  iqr <- q3 - q1
  
  lower_bound <- q1 - 1.5 * iqr
  upper_bound <- q3 + 1.5 * iqr
  
  lower_whisker <- min(sim_data[sim_data >= lower_bound])
  upper_whisker <- max(sim_data[sim_data <= upper_bound])
  
  # Outliers
  outliers <- sim_data[sim_data < lower_whisker | sim_data > upper_whisker]
  
  # Boxplot data
  df_boxplot <- data.frame(
    frame = frame_index,
    alpha_value = alpha_val,
    alpha_label = round(alpha_val, 2),
    q1 = q1,
    median = median_val,
    q3 = q3,
    lower_whisker = lower_whisker,
    upper_whisker = upper_whisker,
    is_symmetry = is_symmetry_pause,
    data_type = "boxplot"
  )
  
  # Outliers data (sempre criar, mesmo se vazio)
  if (length(outliers) > 0) {
    df_outliers <- data.frame(
      x = outliers,
      frame = frame_index,
      alpha_value = alpha_val,
      alpha_label = round(alpha_val, 2),
      is_symmetry = is_symmetry_pause,
      data_type = "outliers"
    )
  } else {
    df_outliers <- data.frame(
      x = numeric(0),
      frame = integer(0),
      alpha_value = numeric(0),
      alpha_label = numeric(0),
      is_symmetry = logical(0),
      data_type = character(0)
    )
  }
  
  # Text data
  df_text <- data.frame(
    frame = frame_index,
    alpha_value = alpha_val,
    alpha_label = round(alpha_val, 2),
    is_symmetry = is_symmetry_pause,
    text_label = if(is_symmetry_pause) "Distribuição Simétrica" else "",
    data_type = "text"
  )
  
  return(list(
    density = df_density,
    boxplot = df_boxplot,
    outliers = df_outliers,
    text = df_text
  ))
}

# Gerar todos os dados
cat("Gerando dados para", n_frames, "frames...\n")

all_data <- list()
for (i in 1:n_frames) {
  all_data[[i]] <- create_frame_data(i, alpha_values[i])
  if (i %% 50 == 0) cat("Processado frame", i, "de", n_frames, "\n")
}

# Combinar dados
final_density_data <- do.call(rbind, lapply(all_data, function(x) x$density))
final_boxplot_data <- do.call(rbind, lapply(all_data, function(x) x$boxplot))

# Combinar outliers apenas se houver dados
outlier_list <- lapply(all_data, function(x) x$outliers)
non_empty_outliers <- outlier_list[sapply(outlier_list, nrow) > 0]
if (length(non_empty_outliers) > 0) {
  final_outlier_data <- do.call(rbind, non_empty_outliers)
} else {
  final_outlier_data <- data.frame(
    x = numeric(0),
    frame = integer(0),
    alpha_value = numeric(0),
    alpha_label = numeric(0),
    is_symmetry = logical(0),
    data_type = character(0)
  )
}

final_text_data <- do.call(rbind, lapply(all_data, function(x) x$text))

# Calcular parâmetros visuais
x_min_global <- min(final_density_data$x, final_boxplot_data$lower_whisker, na.rm = TRUE)
x_max_global <- max(final_density_data$x, final_boxplot_data$upper_whisker, na.rm = TRUE)
x_range_padding <- (x_max_global - x_min_global) * 0.1

max_density_y_global <- max(final_density_data$y, na.rm = TRUE)
y_boxplot_pos <- max_density_y_global * 1.3
boxplot_box_height <- max_density_y_global * 0.08

# Adicionar posições aos dados
final_boxplot_data$y_pos <- y_boxplot_pos
final_boxplot_data$box_height <- boxplot_box_height

final_text_data$x_pos <- 0
final_text_data$y_pos <- max_density_y_global * 0.3

# Criar o gráfico
cat("Criando animação...\n")

p <- ggplot() +
  # Área sob a curva
  geom_area(data = final_density_data, 
            aes(x = x, y = y),
            fill = "lightblue", alpha = 0.3) +
  
  # Curva de densidade
  geom_line(data = final_density_data, 
            aes(x = x, y = y),
            color = "darkblue", size = 1.5, alpha = 0.9) +
  
  # Caixa do boxplot
  geom_rect(data = final_boxplot_data,
            aes(xmin = q1, xmax = q3,
                ymin = y_pos - box_height/2, 
                ymax = y_pos + box_height/2),
            fill = "lightsteelblue", alpha = 0.8, 
            color = "black", size = 0.8) +
  
  # Mediana
  geom_segment(data = final_boxplot_data,
               aes(x = median, xend = median,
                   y = y_pos - box_height/2, 
                   yend = y_pos + box_height/2),
               color = "darkred", size = 1.5) +
  
  # Bigodes
  geom_segment(data = final_boxplot_data,
               aes(x = lower_whisker, xend = q1, 
                   y = y_pos, yend = y_pos),
               color = "black", size = 0.8) +
  geom_segment(data = final_boxplot_data,
               aes(x = q3, xend = upper_whisker, 
                   y = y_pos, yend = y_pos),
               color = "black", size = 0.8) +
  
  # Barras dos bigodes
  geom_segment(data = final_boxplot_data,
               aes(x = lower_whisker, xend = lower_whisker,
                   y = y_pos - box_height/3, 
                   yend = y_pos + box_height/3),
               color = "black", size = 0.8) +
  geom_segment(data = final_boxplot_data,
               aes(x = upper_whisker, xend = upper_whisker,
                   y = y_pos - box_height/3, 
                   yend = y_pos + box_height/3),
               color = "black", size = 0.8) +
  
  # Texto de simetria
  geom_text(data = subset(final_text_data, text_label != ""),
            aes(x = x_pos, y = y_pos, label = text_label),
            color = "blue", size = 6, fontface = "bold",
            hjust = 0.5, vjust = 0.5) +
  
  # Escalas
  scale_x_continuous(
    limits = c(x_min_global - x_range_padding, x_max_global + x_range_padding),
    breaks = seq(-8, 8, 2),
    minor_breaks = seq(-8, 8, 1)
  ) +
  scale_y_continuous(
    limits = c(0, y_boxplot_pos + boxplot_box_height * 0.8),
    breaks = seq(0, 0.5, 0.1),
    minor_breaks = seq(0, 0.5, 0.05)
  ) +
  
  # Rótulos
  labs(
    title = "Distribuição e Boxplot: Transição de Assimétrico para Simétrico",
    subtitle = "Parâmetro de Assimetria (α): {closest_state}",
    x = "Valor",
    y = "Densidade de Probabilidade"
  ) +
  
  # Tema
  theme_minimal() +
  theme(
    plot.title = element_text(
      hjust = 0.5, face = "bold", size = 18, 
      color = "black", margin = margin(b = 10)
    ),
    plot.subtitle = element_text(
      hjust = 0.5, size = 14, color = "black",
      face = "bold", margin = margin(b = 15)
    ),
    axis.title = element_text(size = 14, color = "black", face = "bold"),
    axis.text = element_text(size = 12, color = "black", face = "bold"),
    panel.grid.major = element_line(color = "gray30", size = 0.8),
    panel.grid.minor = element_line(color = "gray50", size = 0.5, linetype = "dotted"),
    panel.border = element_rect(color = "black", fill = NA, size = 1.2),
    axis.line = element_line(color = "black", size = 1),
    axis.ticks = element_line(color = "black", size = 0.8),
    axis.ticks.length = unit(0.3, "cm"),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  ) +
  
  # Animação
  transition_states(
    alpha_label,
    transition_length = 1,
    state_length = 1,
    wrap = FALSE
  ) +
  ease_aes('cubic-in-out')

# Adicionar outliers apenas se existirem
if (nrow(final_outlier_data) > 0) {
  p <- p + geom_point(data = final_outlier_data, 
                      aes(x = x, y = y_boxplot_pos),
                      color = "red", shape = 8, size = 2.5, stroke = 1.2)
}

# Animar
cat("Renderizando animação...\n")
anim <- animate(
  p, 
  nframes = n_frames, 
  fps = 20, 
  width = 1000, 
  height = 700,
  renderer = gifski_renderer(file = output_path),
  end_pause = 60
)

cat(paste("Animação salva em:", output_path, "\n"))
cat(paste("Total de frames:", n_frames, "\n"))
cat(paste("Frames de pausa na simetria:", pause_frames, "\n"))
cat(paste("Duração aproximada:", round(n_frames/20, 1), "segundos\n"))