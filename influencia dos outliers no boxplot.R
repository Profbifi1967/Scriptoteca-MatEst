#---------------------------------------------------------------------------------
# Script corrigido para animação de boxplot com outliers
# Correções finais: formatação de textos e fontes
#---------------------------------------------------------------------------------

# 1. Carregar os pacotes necessários
library(ggplot2)
library(gganimate)
library(dplyr)
library(gifski)

# Define um 'seed' para reprodutibilidade
set.seed(42)

# 2. Definir o caminho para salvar o arquivo
output_path <- "E:/codigo"
if (!dir.exists(output_path)) {
  dir.create(output_path, recursive = TRUE)
}

# 3. Geração dos Dados BASE (sem outliers)
dados_base <- rnorm(100, mean = 50, sd = 8)  # Aumentei o desvio padrão para mais variação

# 4. Criar sequência de dados com adição GRADUAL de outliers
# Vamos criar uma sequência mais lenta e controlada
outliers_para_adicionar <- c(
  15,   # Outlier inferior leve
  10,   # Outlier inferior moderado  
  5,    # Outlier inferior extremo
  85,   # Outlier superior leve
  95,   # Outlier superior moderado
  110   # Outlier superior extremo
)

# Criar frames da animação
total_frames <- 60  # Reduzido para animação mais lenta
frames_por_outlier <- 10  # Cada outlier fica visível por 10 frames
lista_dados <- list()

frame_atual <- 1

# Frame inicial - apenas dados base
for(i in 1:frames_por_outlier) {
  dados_frame <- data.frame(
    valor = dados_base,
    frame = frame_atual,
    fase = "Distribuição Normal"
  )
  lista_dados[[frame_atual]] <- dados_frame
  frame_atual <- frame_atual + 1
}

# Adicionar outliers progressivamente
for(outlier_idx in 1:length(outliers_para_adicionar)) {
  # Dados atuais = dados base + todos os outliers até agora
  outliers_atuais <- outliers_para_adicionar[1:outlier_idx]
  dados_atuais <- c(dados_base, outliers_atuais)
  
  # Criar vários frames com os mesmos dados para desacelerar
  for(i in 1:frames_por_outlier) {
    dados_frame <- data.frame(
      valor = dados_atuais,
      frame = frame_atual,
      fase = paste("Outlier", outlier_idx, "adicionado")
    )
    lista_dados[[frame_atual]] <- dados_frame
    frame_atual <- frame_atual + 1
  }
}

# Combinar todos os dados
dados_completos <- do.call(rbind, lista_dados)

# 5. Calcular estatísticas para cada frame
stats_por_frame <- dados_completos %>%
  group_by(frame, fase) %>%
  summarise(
    Min = min(valor),
    Q1 = quantile(valor, 0.25),
    Mediana = median(valor),
    Media = mean(valor),
    Q3 = quantile(valor, 0.75),
    Max = max(valor),
    n_pontos = n(),
    .groups = 'drop'
  ) %>%
  mutate(
    # Criar labels das estatísticas
    label_max = sprintf("Máx: %.1f", Max),
    label_q3 = sprintf("Q3: %.1f", Q3),
    label_media = sprintf("Média: %.1f", Media),
    label_mediana = sprintf("Mediana: %.1f", Mediana),
    label_q1 = sprintf("Q1: %.1f", Q1),
    label_min = sprintf("Mín: %.1f", Min),
    label_n = sprintf("n = %d", n_pontos)
  )

# 6. Identificar outliers para coloração
dados_completos <- dados_completos %>%
  group_by(frame) %>%
  mutate(
    Q1 = quantile(valor, 0.25),
    Q3 = quantile(valor, 0.75),
    IQR = Q3 - Q1,
    limite_inf = Q1 - 1.5 * IQR,
    limite_sup = Q3 + 1.5 * IQR,
    eh_outlier = valor < limite_inf | valor > limite_sup,
    tipo_ponto = ifelse(eh_outlier, "Outlier", "Normal")
  ) %>%
  ungroup()

# 7. Criar o gráfico animado
p <- ggplot(dados_completos, aes(x = factor(1), y = valor)) +
  
  # Boxplot - CHAVE: será recalculado para cada frame automaticamente
  geom_boxplot(
    width = 0.6, 
    fill = "lightblue", 
    color = "darkblue", 
    alpha = 0.7,
    outlier.size = 3,
    outlier.color = "red",
    outlier.alpha = 0.8
  ) +
  
  # Pontos individuais com jitter
  geom_jitter(
    aes(color = tipo_ponto),
    width = 0.2, 
    alpha = 0.6, 
    size = 2
  ) +
  
  # Escala de cores para os pontos
  scale_color_manual(
    values = c("Normal" = "gray40", "Outlier" = "red"),
    name = ""
  ) +
  
  # ESCALA Y FIXA - soluciona o problema do "achatamento"
  scale_y_continuous(
    limits = c(-5, 120),  # Limites fixos que cobrem todos os dados
    breaks = seq(0, 120, 20),
    expand = c(0.02, 0)
  ) +
  
  # Estatísticas dinâmicas - posicionadas à direita (FONTE AUMENTADA +1)
  geom_text(data = stats_por_frame, aes(x = 1.8, y = 110, label = label_max), 
            hjust = 0, size = 5, color = "black", fontface = "bold") +
  geom_text(data = stats_por_frame, aes(x = 1.8, y = 105, label = label_q3), 
            hjust = 0, size = 5, color = "black", fontface = "bold") +
  geom_text(data = stats_por_frame, aes(x = 1.8, y = 100, label = label_media), 
            hjust = 0, size = 5, color = "darkgreen", fontface = "bold") +
  geom_text(data = stats_por_frame, aes(x = 1.8, y = 95, label = label_mediana), 
            hjust = 0, size = 5, color = "darkred", fontface = "bold") +
  geom_text(data = stats_por_frame, aes(x = 1.8, y = 90, label = label_q1), 
            hjust = 0, size = 5, color = "black", fontface = "bold") +
  geom_text(data = stats_por_frame, aes(x = 1.8, y = 85, label = label_min), 
            hjust = 0, size = 5, color = "black", fontface = "bold") +
  # N = alterado para mesmo tamanho e negrito dos demais
  geom_text(data = stats_por_frame, aes(x = 1.8, y = 75, label = label_n), 
            hjust = 0, size = 5, color = "black", fontface = "bold") +
  
  # Texto explicativo na parte inferior (COR PRETA, NEGRITO, FONTE +1)
  geom_text(data = stats_por_frame, aes(x = 1, y = -2, label = fase), 
            hjust = 0.5, size = 5.5, color = "black", fontface = "bold") +
  
  # Títulos e tema
  labs(
    title = "Impacto dos Outliers na Estrutura do Boxplot",
    subtitle = "Observe como a caixa, mediana e bigodes se alteram",
    y = "Valores",
    x = ""
  ) +
  
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 16, face = "bold", color = "darkblue", hjust = 0.5),
    plot.subtitle = element_text(size = 12, color = "gray50", hjust = 0.5),
    plot.margin = margin(20, 20, 20, 20),
    legend.text = element_text(size = 10)
  ) +
  
  # Ajustar limites do plot para acomodar as estatísticas
  coord_cartesian(xlim = c(0.5, 2.5), ylim = c(-5, 120)) +
  
  # ANIMAÇÃO - mais lenta para melhor visualização
  transition_time(frame) +
  ease_aes('linear')  # Transição linear para mudanças mais suaves

# 8. Renderizar com configurações otimizadas
message("Iniciando renderização da animação corrigida...")

# Configurações da animação
anim <- animate(
  p,
  nframes = max(dados_completos$frame),
  fps = 3,  # MUITO mais lento - 3 frames por segundo
  width = 900,
  height = 600,
  renderer = gifski_renderer(loop = TRUE)
)

# Salvar
anim_save(
  filename = "boxplot_outliers_final.gif",
  animation = anim,
  path = output_path
)

message(paste("Animação salva em:", file.path(output_path, "boxplot_outliers_final.gif")))

# Mostrar resumo dos dados
cat("\n=== RESUMO DA ANIMAÇÃO ===\n")
cat("Total de frames:", max(dados_completos$frame), "\n")
cat("FPS:", 3, "\n")
cat("Duração aproximada:", round(max(dados_completos$frame)/3, 1), "segundos\n")
cat("Outliers adicionados:", paste(outliers_para_adicionar, collapse = ", "), "\n")
