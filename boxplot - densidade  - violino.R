# Define o caminho para o diretório onde o script será salvo e a animação gerada.
# Certifique-se de que este caminho exista ou possa ser criado.
script_dir <- "E:/codigo"

# Cria o diretório se ele não existir
if (!dir.exists(script_dir)) {
  dir.create(script_dir, recursive = TRUE)
}

# Define o caminho completo para o arquivo do script
script_file_path <- file.path(script_dir, "animacao_didatica_representacoes_dados.R")

# Este é um placeholder. No contexto de um ambiente de execução real, você salvaria
# este código em `script_file_path`. Para demonstração, ele será executado diretamente.

#-------------------------------------------------------------------------------
# Início do Script R Principal
#-------------------------------------------------------------------------------

# 1. Instalação e Carregamento de Pacotes Necessários
# Verifica se os pacotes estão instalados e os instala se não estiverem.
# Em seguida, carrega os pacotes.
packages <- c("ggplot2", "gganimate", "dplyr", "tidyr", "gifski", "magrittr")

for (p in packages) {
  if (!require(p, character.only = TRUE)) {
    install.packages(p, dependencies = TRUE)
    library(p, character.only = TRUE)
  }
}

# 2. Geração de Dados Sintéticos Animados
# Define o número de frames (estados) na animação e o número de pontos por frame.
n_frames <- 50
n_points_per_frame <- 1000
set.seed(123) # Para reprodutibilidade dos dados

# Listas para armazenar os parâmetros de média e desvio padrão para cada frame
mean_param <- numeric(n_frames)
sd_param <- numeric(n_frames)

df_simulated_data_list <- list() # Lista para armazenar dataframes de cada frame

# Loop para gerar dados para cada frame, variando média e desvio padrão
for (i in 1:n_frames) {
  # Varia a média usando uma função seno (oscila entre 40 e 60)
  mean_val <- 50 + 10 * sin(i * pi / (n_frames / 2))
  # Varia o desvio padrão usando uma função cosseno (oscila entre 5 e 15)
  sd_val <- 10 + 5 * cos(i * pi / n_frames)
  
  # Armazena os parâmetros para uso nos subtítulos da animação
  mean_param[i] <- mean_val
  sd_param[i] <- sd_val
  
  # Gera dados aleatórios de uma distribuição normal com os parâmetros atuais
  data_frame <- data.frame(
    value = rnorm(n_points_per_frame, mean = mean_val, sd = sd_val),
    frame_id = i # Identificador único para cada frame
  )
  df_simulated_data_list[[i]] <- data_frame
}

# Combina todos os dataframes em um único dataframe grande
df_simulated_data <- bind_rows(df_simulated_data_list)

# 3. Preparação dos Dados para Plotagem Unificada (facet_wrap)
# Duplica os dados e adiciona uma coluna 'plot_type_label' para cada tipo de gráfico.
# Isso permite que todos os dados sejam passados para um único ggplot e faceted.
df_plot_combined <- bind_rows(
  df_simulated_data %>% mutate(plot_type_label = "Boxplot"),
  df_simulated_data %>% mutate(plot_type_label = "Gráfico de Violino"),
  df_simulated_data %>% mutate(plot_type_label = "Gráfico de Densidade")
)

# Converte plot_type_label para um fator para garantir a ordem desejada das facetas
df_plot_combined$plot_type_label <- factor(
  df_plot_combined$plot_type_label,
  levels = c("Boxplot", "Gráfico de Violino", "Gráfico de Densidade")
)

# Determina os limites consistentes do eixo X (valor da variável) para toda a animação.
# Isso garante que a escala não mude, facilitando a comparação visual.
global_min_val <- min(df_simulated_data$value)
global_max_val <- max(df_simulated_data$value)
# Adiciona uma margem para garantir que os dados não sejam cortados durante as flutuações
x_axis_range <- c(floor((global_min_val - 2 * max(sd_param)) / 10) * 10,
                  ceiling((global_max_val + 2 * max(sd_param)) / 10) * 10)

# 4. Construção do Gráfico Base com ggplot2
p <- ggplot(df_plot_combined, aes(x = value)) +
  # Camada para o Boxplot (aplicada apenas à faceta "Boxplot")
  # O Boxplot é horizontal, por isso `y` é uma categoria dummy
  geom_boxplot(
    data = subset(df_plot_combined, plot_type_label == "Boxplot"),
    aes(y = factor(plot_type_label)),
    fill = "#8B0000", color = "darkred", alpha = 0.7, width = 0.5
  ) +
  # Camada para o Gráfico de Violino (aplicada apenas à faceta "Gráfico de Violino")
  # O Gráfico de Violino é horizontal, por isso `y` é uma categoria dummy
  geom_violin(
    data = subset(df_plot_combined, plot_type_label == "Gráfico de Violino"),
    aes(y = factor(plot_type_label)),
    fill = "#006400", color = "darkgreen", alpha = 0.7, scale = "width"
  ) +
  # Camada para o Gráfico de Densidade (aplicada apenas à faceta "Gráfico de Densidade")
  # Aqui, `y` é mapeado para `after_stat(density)` que calcula a densidade.
  geom_density(
    data = subset(df_plot_combined, plot_type_label == "Gráfico de Densidade"),
    aes(y = after_stat(density)),
    fill = "#00008B", color = "darkblue", alpha = 0.7
  ) +
  # Faceting para exibir os três tipos de gráficos lado a lado
  # `scales = "free_y"` é necessário porque os eixos Y têm significados diferentes
  # (categoria vs. densidade), mas o eixo X (valor da variável) é consistente.
  facet_wrap(~ plot_type_label, ncol = 1, scales = "free_y") +
  # Define os limites fixos do eixo X para toda a animação
  coord_cartesian(xlim = x_axis_range) +
  # Rótulos e Títulos
  labs(
    title = "Animação: Análise de Distribuição de Dados - Frame {as.integer(closest_state)}",
    subtitle = paste0("Média: {round(mean_param[as.integer(closest_state)], 2)} | Desvio Padrão: {round(sd_param[as.integer(closest_state)], 2)}"),
    x = "Valor da Variável",
    y = "Densidade / Representação" # Rótulo geral para o eixo Y
  ) +
  # Tema visual do gráfico
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    plot.subtitle = element_text(hjust = 0.5, face = "italic", size = 12),
    strip.text = element_text(face = "bold", size = 12, margin = margin(b = 5, t = 5)),
    axis.title.y = element_text(margin = margin(r = 10)),
    # Ajusta o tamanho do texto do eixo Y para as facetas de boxplot/violino
    axis.text.y = element_text(size = 10)
  )

# 5. Configuração da Animação com gganimate
anim <- p +
  transition_states(
    frame_id,
    transition_length = 2, # Duração da transição entre estados
    state_length = 1       # Duração de cada estado antes da transição
  ) +
  ease_aes('sine-in-out') + # Easing function para transições suaves
  enter_fade() +           # Efeito de entrada suave
  exit_fade()              # Efeito de saída suave

# 6. Renderização e Salvamento da Animação
output_file_name <- "animacao_didatica_representacoes_dados.gif"
output_file_path <- file.path(script_dir, output_file_name)

message(paste("Gerando animação. Isso pode levar alguns minutos...", Sys.time()))

# Renderiza a animação
# `fps`: frames por segundo
# `width`, `height`: dimensões da imagem em pixels
# `res`: resolução em pixels por polegada
animate(anim,
        fps = 15,
        width = 900, height = 700,
        renderer = gifski_renderer(),
        res = 100 # Qualidade da imagem
)

# Salva a animação gerada
anim_save(output_file_path, animation = last_animation())

message(paste("Animação salva em:", output_file_path, Sys.time()))

#-------------------------------------------------------------------------------
# Fim do Script R Principal
#-------------------------------------------------------------------------------