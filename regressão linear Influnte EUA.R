# --- ANIMAÇÃO: TRANSIÇÃO FLUIDA DE REGRESSÃO COM PONTO INFLUENTE ---
# Baseado na relação Cigarros (1930) vs Mortes por Câncer (1950)
# Movimento: Linha B (SEM EUA) → Linha A (COM EUA) + Surgimento suave do ponto EUA
# Salvo em: E:\codigo

# Carregar bibliotecas
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("dplyr")) install.packages("dplyr")
if (!require("tibble")) install.packages("tibble")
if (!require("gifski")) install.packages("gifski")

library(ggplot2)
library(dplyr)
library(tibble)
library(gifski)

# --- CONFIGURAÇÃO DO DIRETÓRIO DE SAÍDA ---
output_dir <- "E:/codigo"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
message(paste("Diretório de saída configurado:", output_dir))

# --- 1. DADOS BASEADOS NA IMAGEM ---
# Aproximação dos valores vistos no gráfico original
paises_data <- tibble(
  pais = c("Islândia", "Noruega", "Suécia", "Dinamarca", "Holanda", 
           "Finlândia", "Suíça", "Canadá", "Austrália", "Grã-Bretanha", "EUA"),
  cigarros_1930 = c(230, 250, 300, 380, 460, 1100, 510, 500, 480, 1150, 1300),
  mortes_1950 = c(60, 95, 120, 170, 240, 350, 250, 150, 170, 460, 200)
)

# Identificar o ponto influente (EUA)
ponto_influente <- "EUA"

# --- 2. CALCULAR MODELOS DE REGRESSÃO ---
# Modelo A: COM o ponto influente (linha contínua)
modelo_A <- lm(mortes_1950 ~ cigarros_1930, data = paises_data)
coef_A <- coef(modelo_A)

# Modelo B: SEM o ponto influente (linha pontilhada)
dados_sem_influente <- paises_data %>% filter(pais != ponto_influente)
modelo_B <- lm(mortes_1950 ~ cigarros_1930, data = dados_sem_influente)
coef_B <- coef(modelo_B)

# Mostrar informações dos modelos
message("\n📊 ANÁLISE DOS MODELOS:")
message(paste("🔵 Modelo A (COM EUA) - R²:", round(summary(modelo_A)$r.squared, 3)))
message(paste("🔴 Modelo B (SEM EUA) - R²:", round(summary(modelo_B)$r.squared, 3)))
message(paste("📈 Intercepto A:", round(coef_A[1], 2), "| Inclinação A:", round(coef_A[2], 4)))
message(paste("📈 Intercepto B:", round(coef_B[1], 2), "| Inclinação B:", round(coef_B[2], 4)))

# --- 3. FUNÇÃO PARA INTERPOLAÇÃO SUAVE ---
interpolar_coeficientes <- function(t, coef_inicio, coef_fim) {
  # t vai de 0 (início) a 1 (fim)
  # Interpolação linear suave
  intercepto <- (1 - t) * coef_inicio[1] + t * coef_fim[1]
  inclinacao <- (1 - t) * coef_inicio[2] + t * coef_fim[2]
  return(c(intercepto, inclinacao))
}

# --- 4. FUNÇÃO PARA CRIAR CADA FRAME ---
criar_frame_animacao <- function(t_valor, frame_num, total_frames) {
  
  # Interpola de B para A
  coef_atual <- interpolar_coeficientes(t_valor, coef_B, coef_A)
  
  # Criar dados para a linha de regressão
  x_range <- seq(200, 1400, length.out = 100)
  linha_regressao <- tibble(
    cigarros_1930 = x_range,
    mortes_1950 = coef_atual[1] + coef_atual[2] * x_range
  )
  
  # Título dinâmico baseado no progresso
  if (t_valor == 0) {
    titulo <- "Ilustração de Ponto Influente"
    subtitulo <- "LINHA B: Regressão SEM ponto influente (EUA excluído)"
    cor_linha <- "red"
    tipo_linha <- "dashed"
    alpha_linha <- 0.9
  } else if (t_valor == 1) {
    titulo <- "Ilustração de Ponto Influente"
    subtitulo <- "LINHA A: Regressão COM todos os pontos (EUA incluído)"
    cor_linha <- "blue"
    tipo_linha <- "solid"
    alpha_linha <- 0.9
  } else {
    progresso <- round(t_valor * 100)
    titulo <- "Ilustração de Ponto Influente"
    subtitulo <- paste0("TRANSIÇÃO EM ANDAMENTO (", progresso, "%) - EUA surgindo e reta se ajustando")
    # Transição suave de cor: vermelho → roxo → azul
    cor_linha <- rgb(1-t_valor, 0, t_valor)
    tipo_linha <- "solid"
    alpha_linha <- 0.8
  }
  
  # ALTERAÇÃO PRINCIPAL: Preparar dados com surgimento gradual do EUA
  paises_plot <- paises_data %>%
    mutate(
      eh_influente = ifelse(pais == ponto_influente, "EUA (Ponto Influente)", "Outros Países"),
      # EUA surge gradualmente conforme t_valor (de 0 a 1)
      alpha_ponto = case_when(
        pais == ponto_influente ~ t_valor,  # EUA: de 0 (invisível) a 1 (totalmente visível)
        TRUE ~ 0.8  # Outros países: sempre visíveis
      ),
      # Tamanho do EUA também cresce gradualmente
      tamanho_ponto = case_when(
        pais == ponto_influente ~ 2 + (3 * t_valor),  # EUA: de tamanho 2 a 5
        TRUE ~ 3  # Outros países: tamanho fixo 3
      )
    )
  
  # Preparar dados do texto (label) do EUA com surgimento gradual
  paises_text <- paises_data %>%
    mutate(
      # Texto do EUA surge junto com o ponto
      alpha_texto = case_when(
        pais == ponto_influente ~ t_valor,  # EUA: texto surge gradualmente
        TRUE ~ 1  # Outros países: texto sempre visível
      )
    )
  
  # Criar o gráfico
  p <- ggplot() +
    # Linha de regressão animada
    geom_line(data = linha_regressao, 
              aes(x = cigarros_1930, y = mortes_1950),
              color = cor_linha, size = 2.5, linetype = tipo_linha, alpha = alpha_linha) +
    
    # Pontos dos países (com surgimento gradual do EUA)
    geom_point(data = paises_plot, 
               aes(x = cigarros_1930, y = mortes_1950, 
                   color = eh_influente, size = tamanho_ponto,
                   alpha = alpha_ponto)) +
    
    # Labels dos países (com surgimento gradual do texto EUA)
    geom_text(data = paises_text,
              aes(x = cigarros_1930, y = mortes_1950, label = pais,
                  alpha = alpha_texto),
              vjust = -1.3, hjust = 0.5, size = 3.2, fontface = "bold", color = "black") +
    
    # Escalas e cores
    scale_color_manual(values = c("EUA (Ponto Influente)" = "red", "Outros Países" = "navy")) +
    scale_size_identity() +
    scale_alpha_identity() +
    
    # Limites e labels
    coord_cartesian(xlim = c(180, 1420), ylim = c(40, 520)) +
    labs(
      title = titulo,
      subtitle = subtitulo,
      caption = paste0("Frame ", frame_num, " de ", total_frames, 
                       " | Cigarros per capita (1930) vs Mortes por câncer de pulmão (1950)"),
      x = "Cigarros per capita em 1930",
      y = "Mortes por câncer de pulmão em 1950 (por 100.000 habitantes)",
      color = "Classificação"
    ) +
    
    # Tema otimizado
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(size = 20, face = "bold", hjust = 0.5, color = "black", margin = margin(b = 10)),
      plot.subtitle = element_text(size = 14, hjust = 0.5, color = "darkblue", margin = margin(b = 15)),
      plot.caption = element_text(size = 10, hjust = 0.5, color = "gray50"),
      axis.title = element_text(size = 12, face = "bold"),
      axis.text = element_text(size = 11),
      legend.position = "bottom",
      legend.title = element_text(size = 12, face = "bold"),
      legend.text = element_text(size = 11),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      panel.grid.major = element_line(color = "gray90", size = 0.5),
      panel.grid.minor = element_line(color = "gray95", size = 0.3),
      plot.margin = margin(t = 25, r = 25, b = 25, l = 25, unit = "pt")
    ) +
    guides(size = "none", alpha = "none")
  
  return(p)
}

# --- 5. GERAR A ANIMAÇÃO ---
temp_frames_dir <- file.path(output_dir, "temp_frames")
dir.create(temp_frames_dir, recursive = TRUE, showWarnings = FALSE)

# Configurações da animação
n_frames_total <- 150        # 15 segundos a 10fps
n_frames_pausa_inicio <- 40  # 4s no início (Linha B - SEM EUA)
n_frames_transicao <- 70     # 7s de transição fluida
n_frames_pausa_fim <- 40     # 4s no final (Linha A - COM EUA)

png_files <- c()
frame_counter <- 1

message("\n🎬 INICIANDO GERAÇÃO DA ANIMAÇÃO...")
message("🔄 MOVIMENTO: Linha B (SEM EUA) → Linha A (COM EUA)")
message("✨ EFEITO ESPECIAL: EUA surge suavemente junto com o deslocamento da reta")
message(paste("📁 Frames temporários salvos em:", temp_frames_dir))

# Barra de progresso simples
total_operacoes <- n_frames_total
progresso_atual <- 0

# Pausa inicial (Linha B - SEM EUA, EUA invisível)
message("🔴 Gerando frames iniciais (Linha B - EUA invisível)...")
for (i in 1:n_frames_pausa_inicio) {
  plot <- criar_frame_animacao(0, frame_counter, n_frames_total)
  file_path <- file.path(temp_frames_dir, paste0("frame_", sprintf("%05d", frame_counter), ".png"))
  ggsave(filename = file_path, plot = plot, width = 14, height = 10, dpi = 100)
  png_files <- c(png_files, file_path)
  frame_counter <- frame_counter + 1
  
  # Progresso
  progresso_atual <- progresso_atual + 1
  if (progresso_atual %% 10 == 0) {
    message(paste("   Progresso:", round(100 * progresso_atual / total_operacoes), "%"))
  }
}

# Transição suave (B para A + EUA surgindo)
message("🟣 Gerando frames de transição (B → A + EUA surgindo)...")
for (i in 1:n_frames_transicao) {
  # Função de suavização (ease-in-out) para movimento natural
  t_raw <- (i - 1) / (n_frames_transicao - 1)
  t_suave <- 0.5 * (1 - cos(pi * t_raw))  # Transição suave tipo senoidal
  
  plot <- criar_frame_animacao(t_suave, frame_counter, n_frames_total)
  file_path <- file.path(temp_frames_dir, paste0("frame_", sprintf("%05d", frame_counter), ".png"))
  ggsave(filename = file_path, plot = plot, width = 14, height = 10, dpi = 100)
  png_files <- c(png_files, file_path)
  frame_counter <- frame_counter + 1
  
  # Progresso
  progresso_atual <- progresso_atual + 1
  if (progresso_atual %% 10 == 0) {
    message(paste("   Progresso:", round(100 * progresso_atual / total_operacoes), "%"))
  }
}

# Pausa final (Linha A - COM EUA, EUA totalmente visível)
message("🔵 Gerando frames finais (Linha A - EUA totalmente visível)...")
for (i in 1:n_frames_pausa_fim) {
  plot <- criar_frame_animacao(1, frame_counter, n_frames_total)
  file_path <- file.path(temp_frames_dir, paste0("frame_", sprintf("%05d", frame_counter), ".png"))
  ggsave(filename = file_path, plot = plot, width = 14, height = 10, dpi = 100)
  png_files <- c(png_files, file_path)
  frame_counter <- frame_counter + 1
  
  # Progresso
  progresso_atual <- progresso_atual + 1
  if (progresso_atual %% 10 == 0) {
    message(paste("   Progresso:", round(100 * progresso_atual / total_operacoes), "%"))
  }
}

# --- 6. CRIAR GIF FINAL ---
output_gif_path <- file.path(output_dir, "ponto_influente_surgimento_suave.gif")
message("\n🎯 CRIANDO GIF FINAL...")
message("   Isso pode levar alguns minutos...")

gifski(
  png_files = png_files,
  gif_file = output_gif_path,
  width = 1400,
  height = 1000,
  delay = 0.1  # 10 fps
)

# --- 7. LIMPEZA E RELATÓRIO FINAL ---
# Limpar arquivos temporários
message("🧹 Limpando arquivos temporários...")
unlink(temp_frames_dir, recursive = TRUE)

# Verificar se o arquivo foi criado
if (file.exists(output_gif_path)) {
  file_size <- round(file.info(output_gif_path)$size / (1024^2), 2)
  
  message("\n🎉 ANIMAÇÃO CRIADA COM SUCESSO!")
  message("=" * 50)
  message(paste("📁 Localização:", output_gif_path))
  message(paste("📊 Tamanho do arquivo:", file_size, "MB"))
  message(paste("🎬 Duração:", n_frames_total / 10, "segundos"))
  message(paste("🖼️  Resolução: 1400x1000 pixels"))
  message(paste("⚡ Taxa de quadros: 10 FPS"))
  message(paste("🎯 Total de frames:", n_frames_total))
  message("=" * 50)
  
  message("\n📈 CARACTERÍSTICAS DA ANIMAÇÃO:")
  message("   ✅ Movimento sincronizado: Reta B→A + EUA surgindo")
  message("   ✅ Transição fluida de 7 segundos")
  message("   ✅ Pausas de 4s no início e fim para análise")
  message("   ✅ EUA surge gradualmente (alpha: 0 → 1)")
  message("   ✅ Tamanho do EUA cresce junto (2 → 5)")
  message("   ✅ Texto 'EUA' aparece suavemente")
  message("   ✅ Cor da linha: vermelho → roxo → azul")
  message("   ✅ Linha: pontilhada → sólida")
  
  message("\n✨ EFEITOS VISUAIS ESPECIAIS:")
  message("   🎭 EUA completamente invisível no início")
  message("   🌟 Surgimento gradual sincronizado com a reta")
  message("   🎯 Crescimento do tamanho do ponto")
  message("   📝 Texto 'EUA' aparece junto com o ponto")
  message("   🎨 Transição suave de todas as propriedades")
  
  message("\n💡 NARRATIVA DA ANIMAÇÃO:")
  message("   1. Inicia: Correlação forte SEM o ponto influente")
  message("   2. Transição: EUA surge enquanto reta se move")
  message("   3. Finaliza: Correlação fraca COM o ponto influente")
  message("   4. Impacto visual: Como UM ponto muda TUDO")
  
} else {
  message("\n❌ ERRO: Falha na criação do arquivo GIF!")
  message("   Verifique se o diretório E:/codigo é acessível")
  message("   Verifique se há espaço suficiente em disco")
}

message("\n🔚 SCRIPT FINALIZADO!")