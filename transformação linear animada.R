# ===============================================================================
# SCRIPT PARA ANIMAÇÃO GIF DIDÁTICA DE TRANSFORMAÇÕES LINEARES - ROTAÇÃO 360°
# ===============================================================================
# Autor: Script Educacional R
# Objetivo: Criar animação didática de rotação linear COMPLETA (360°) com pausas estratégicas
# Saída: GIF animado em E:/codigo/rotacao_linear_didatica_360.gif
# ===============================================================================

# ===============================================================================
# INSTALAÇÃO E CARREGAMENTO AUTOMÁTICO DE DEPENDÊNCIAS
# ===============================================================================

instalar_e_carregar <- function(pacotes) {
  for (pacote in pacotes) {
    if (!require(pacote, character.only = TRUE, quietly = TRUE)) {
      cat(sprintf("📦 Instalando pacote '%s'...\n", pacote))
      install.packages(pacote, dependencies = TRUE, repos = "https://cran.r-project.org/")
      
      if (!require(pacote, character.only = TRUE, quietly = TRUE)) {
        stop(sprintf("❌ Falha ao instalar/carregar o pacote '%s'", pacote))
      } else {
        cat(sprintf("✅ Pacote '%s' instalado e carregado com sucesso\n", pacote))
      }
    }
  }
}

# Lista de pacotes necessários
pacotes_necessarios <- c("ggplot2", "gganimate", "transformr", "dplyr", "magick", "gifski")

cat("🔧 Verificando e instalando dependências...\n")
instalar_e_carregar(pacotes_necessarios)
cat("✅ Todas as dependências estão prontas!\n\n")

# ===============================================================================
# CLASSE PRINCIPAL PARA ANIMAÇÃO DE ROTAÇÃO LINEAR 360°
# ===============================================================================

RotacaoLinear360 <- function() {
  
  # Configurações principais
  config <- list(
    angulo_total = 360,          # ROTAÇÃO COMPLETA DE 360°
    fps = 25,                    # Frames por segundo
    largura = 900,               # Largura da imagem
    altura = 700,                # Altura da imagem
    
    # Timing estendido para rotação completa (em frames)
    pause_inicial = 50,          # ~2 segundos
    pause_matriz = 40,           # ~1.6 segundos
    frames_rotacao = 180,        # ~7.2 segundos (ROTAÇÃO COMPLETA)
    pause_90_graus = 25,         # ~1 segundo (pausa aos 90°)
    pause_180_graus = 25,        # ~1 segundo (pausa aos 180°)
    pause_270_graus = 25,        # ~1 segundo (pausa aos 270°)
    pause_final = 60,            # ~2.4 segundos
    
    # Cores aprimoradas para rotação completa
    cores = list(
      objeto_inicial = "#2E86AB",
      objeto_final = "#A23B72",
      grid = "#E0E0E0",
      eixos = "#333333",
      vetor_i = "#FF6B35",
      vetor_j = "#F7931E",
      texto_fundo = "#FFFFFF",
      texto_borda = "#333333",
      marcador_angulo = "#FF0080",
      trajetoria = "#FFD700"
    )
  )
  
  # ===============================================================================
  # FUNÇÕES AUXILIARES
  # ===============================================================================
  
  # Função de matriz de rotação
  matriz_rotacao <- function(angulo_graus) {
    angulo_rad <- angulo_graus * pi / 180
    matrix(c(cos(angulo_rad), sin(angulo_rad), 
             -sin(angulo_rad), cos(angulo_rad)), 
           nrow = 2, byrow = TRUE)
  }
  
  # Função de suavização aprimorada para rotação completa
  easing_function <- function(t) {
    # Suavização mais suave para rotação longa
    ifelse(t <= 0, 0, ifelse(t >= 1, 1, 
                             0.5 * (1 - cos(pi * t))))  # Função cosseno suavizada
  }
  
  # Aplica transformação aos pontos
  aplicar_transformacao <- function(pontos_df, matriz) {
    pontos_matrix <- as.matrix(pontos_df[, c("x", "y")])
    resultado <- t(matriz %*% t(pontos_matrix))
    data.frame(x = resultado[, 1], y = resultado[, 2])
  }
  
  # Função para determinar o quadrante atual
  determinar_quadrante <- function(angulo) {
    angulo_norm <- angulo %% 360
    if (angulo_norm >= 0 && angulo_norm < 90) return("I")
    if (angulo_norm >= 90 && angulo_norm < 180) return("II")
    if (angulo_norm >= 180 && angulo_norm < 270) return("III")
    if (angulo_norm >= 270 && angulo_norm < 360) return("IV")
    return("I")
  }
  
  # ===============================================================================
  # CRIAÇÃO DOS OBJETOS GEOMÉTRICOS
  # ===============================================================================
  
  criar_objetos_base <- function() {
    
    # Quadrado unitário
    quadrado <- data.frame(
      x = c(0, 1, 1, 0, 0),
      y = c(0, 0, 1, 1, 0),
      id = 1:5
    )
    
    # Vetores base (apenas pontos de origem e destino)
    vetores_base <- data.frame(
      x_inicio = c(0, 0),
      y_inicio = c(0, 0),
      x_fim = c(1, 0),
      y_fim = c(0, 1),
      vetor = c("i", "j")
    )
    
    # Círculo de referência para mostrar a rotação completa
    angulos_circulo <- seq(0, 2*pi, length.out = 100)
    circulo_referencia <- data.frame(
      x = 1.5 * cos(angulos_circulo),
      y = 1.5 * sin(angulos_circulo)
    )
    
    return(list(
      quadrado = quadrado,
      vetores_base = vetores_base,
      circulo_referencia = circulo_referencia
    ))
  }
  
  # ===============================================================================
  # GERAÇÃO DOS DADOS PARA ANIMAÇÃO
  # ===============================================================================
  
  gerar_dados_animacao <- function() {
    
    cat("🔄 Gerando dados da animação de 360°...\n")
    
    objetos <- criar_objetos_base()
    
    # Calcula total de frames
    total_frames <- config$pause_inicial + config$pause_matriz + 
      config$frames_rotacao + config$pause_90_graus +
      config$pause_180_graus + config$pause_270_graus +
      config$pause_final
    
    cat(sprintf("📊 Total de frames a gerar: %d (%.1f segundos)\n", 
                total_frames, total_frames/config$fps))
    
    # Inicializa listas para armazenar dados
    dados_quadrado <- list()
    dados_vetores <- list()
    dados_texto <- list()
    dados_circulo <- list()
    dados_marcador <- list()
    
    frame_atual <- 1
    
    # Função auxiliar para criar dados do quadrado
    criar_dados_quadrado <- function(frame, tipo, angulo = 0, alpha_orig = 1) {
      
      # Quadrado original
      quad_orig <- objetos$quadrado
      quad_orig$frame <- frame
      quad_orig$tipo <- "original"
      quad_orig$alpha <- alpha_orig
      
      dados <- quad_orig
      
      # Quadrado transformado (sempre mostrar durante rotação)
      if (angulo >= 0) {
        matriz <- matriz_rotacao(angulo)
        quad_transform <- aplicar_transformacao(objetos$quadrado, matriz)
        quad_transform$frame <- frame
        quad_transform$tipo <- "transformado"
        quad_transform$alpha <- ifelse(angulo == 0, 0, 1.0)
        quad_transform$id <- objetos$quadrado$id
        
        dados <- rbind(dados, quad_transform)
      }
      
      return(dados)
    }
    
    # Função auxiliar para criar dados dos vetores
    criar_dados_vetores <- function(frame, angulo = 0, alpha_orig = 1) {
      
      vetores <- objetos$vetores_base
      vetores$frame <- frame
      vetores$tipo <- "original"
      vetores$alpha <- alpha_orig
      
      dados <- vetores
      
      # Vetores transformados (sempre mostrar durante rotação)
      if (angulo >= 0) {
        matriz <- matriz_rotacao(angulo)
        
        # Transforma os pontos finais dos vetores
        pontos_fim <- data.frame(x = vetores$x_fim, y = vetores$y_fim)
        pontos_transform <- aplicar_transformacao(pontos_fim, matriz)
        
        vetores_transform <- vetores
        vetores_transform$x_fim <- pontos_transform$x
        vetores_transform$y_fim <- pontos_transform$y
        vetores_transform$tipo <- "transformado"
        vetores_transform$alpha <- ifelse(angulo == 0, 0, 1.0)
        
        dados <- rbind(dados, vetores_transform)
      }
      
      return(dados)
    }
    
    # Função auxiliar para criar círculo de referência
    criar_dados_circulo <- function(frame) {
      circulo <- objetos$circulo_referencia
      circulo$frame <- frame
      return(circulo)
    }
    
    # Função auxiliar para criar marcador de ângulo
    criar_dados_marcador <- function(frame, angulo) {
      if (angulo == 0) {
        return(data.frame(x = numeric(0), y = numeric(0), frame = integer(0)))
      }
      
      # Ponto no círculo indicando o ângulo atual
      angulo_rad <- angulo * pi / 180
      data.frame(
        x = 1.5 * cos(angulo_rad),
        y = 1.5 * sin(angulo_rad),
        frame = frame
      )
    }
    
    # Função auxiliar para criar texto
    criar_dados_texto <- function(frame, tipo, angulo = 0) {
      
      quadrante <- determinar_quadrante(angulo)
      
      texto <- switch(tipo,
                      "inicial" = "Estado Inicial\nQuadrado unitário e vetores base (i, j)\nRotação completa de 360°",
                      "matriz" = "Matriz de Rotação (360°):\n[cos θ  -sin θ]\n[sin θ   cos θ]\nθ varia de 0° a 360°",
                      "rotacao" = sprintf("Rotacionando... %.1f°\nQuadrante: %s", angulo, quadrante),
                      "pausa_90" = "Pausa aos 90°\nQuadrante II\nVetor i aponta para cima",
                      "pausa_180" = "Pausa aos 180°\nQuadrante III\nRotação de meia volta",
                      "pausa_270" = "Pausa aos 270°\nQuadrante IV\nTrês quartos da rotação",
                      "final" = {
                        cos_val <- cos(config$angulo_total * pi / 180)
                        sin_val <- sin(config$angulo_total * pi / 180)
                        sprintf("Rotação Completa (360°)\nVolta ao estado inicial\nMatriz identidade: [%.0f  %.0f]\n                    [%.0f  %.0f]",
                                cos_val, -sin_val, sin_val, cos_val)
                      })
      
      data.frame(
        x = 0,
        y = -2.3,
        label = texto,
        frame = frame
      )
    }
    
    # 1. FRAMES INICIAIS
    cat("📝 Processando frames iniciais...\n")
    for (i in 1:config$pause_inicial) {
      dados_quadrado[[frame_atual]] <- criar_dados_quadrado(frame_atual, "inicial")
      dados_vetores[[frame_atual]] <- criar_dados_vetores(frame_atual)
      dados_circulo[[frame_atual]] <- criar_dados_circulo(frame_atual)
      dados_marcador[[frame_atual]] <- criar_dados_marcador(frame_atual, 0)
      dados_texto[[frame_atual]] <- criar_dados_texto(frame_atual, "inicial")
      frame_atual <- frame_atual + 1
    }
    
    # 2. FRAMES DA MATRIZ
    cat("🔢 Processando frames da matriz...\n")
    for (i in 1:config$pause_matriz) {
      dados_quadrado[[frame_atual]] <- criar_dados_quadrado(frame_atual, "matriz")
      dados_vetores[[frame_atual]] <- criar_dados_vetores(frame_atual)
      dados_circulo[[frame_atual]] <- criar_dados_circulo(frame_atual)
      dados_marcador[[frame_atual]] <- criar_dados_marcador(frame_atual, 0)
      dados_texto[[frame_atual]] <- criar_dados_texto(frame_atual, "matriz")
      frame_atual <- frame_atual + 1
    }
    
    # 3. FRAMES DE ROTAÇÃO COMPLETA (0° a 360°)
    cat("🔄 Processando frames de rotação completa (360°)...\n")
    
    frame_contador <- 0
    for (i in 1:config$frames_rotacao) {
      progresso_bruto <- (i - 1) / max(1, (config$frames_rotacao - 1))
      progresso_suave <- easing_function(progresso_bruto)
      angulo_atual <- config$angulo_total * progresso_suave
      
      # Alpha do objeto original diminui gradualmente e depois retorna
      ciclo_alpha <- abs(sin(progresso_suave * pi))
      alpha_original <- max(0.2, 1 - ciclo_alpha * 0.7)
      
      dados_quadrado[[frame_atual]] <- criar_dados_quadrado(frame_atual, "rotacao", angulo_atual, alpha_original)
      dados_vetores[[frame_atual]] <- criar_dados_vetores(frame_atual, angulo_atual, alpha_original)
      dados_circulo[[frame_atual]] <- criar_dados_circulo(frame_atual)
      dados_marcador[[frame_atual]] <- criar_dados_marcador(frame_atual, angulo_atual)
      dados_texto[[frame_atual]] <- criar_dados_texto(frame_atual, "rotacao", angulo_atual)
      
      frame_atual <- frame_atual + 1
      frame_contador <- frame_contador + 1
      
      # Pausas em ângulos específicos
      if (abs(angulo_atual - 90) < 2 && frame_contador > 20) {
        cat("⏸️ Pausa aos 90°...\n")
        for (j in 1:config$pause_90_graus) {
          dados_quadrado[[frame_atual]] <- criar_dados_quadrado(frame_atual, "pausa", 90, 0.5)
          dados_vetores[[frame_atual]] <- criar_dados_vetores(frame_atual, 90, 0.5)
          dados_circulo[[frame_atual]] <- criar_dados_circulo(frame_atual)
          dados_marcador[[frame_atual]] <- criar_dados_marcador(frame_atual, 90)
          dados_texto[[frame_atual]] <- criar_dados_texto(frame_atual, "pausa_90")
          frame_atual <- frame_atual + 1
        }
      }
      
      if (abs(angulo_atual - 180) < 2 && frame_contador > 60) {
        cat("⏸️ Pausa aos 180°...\n")
        for (j in 1:config$pause_180_graus) {
          dados_quadrado[[frame_atual]] <- criar_dados_quadrado(frame_atual, "pausa", 180, 0.5)
          dados_vetores[[frame_atual]] <- criar_dados_vetores(frame_atual, 180, 0.5)
          dados_circulo[[frame_atual]] <- criar_dados_circulo(frame_atual)
          dados_marcador[[frame_atual]] <- criar_dados_marcador(frame_atual, 180)
          dados_texto[[frame_atual]] <- criar_dados_texto(frame_atual, "pausa_180")
          frame_atual <- frame_atual + 1
        }
      }
      
      if (abs(angulo_atual - 270) < 2 && frame_contador > 100) {
        cat("⏸️ Pausa aos 270°...\n")
        for (j in 1:config$pause_270_graus) {
          dados_quadrado[[frame_atual]] <- criar_dados_quadrado(frame_atual, "pausa", 270, 0.5)
          dados_vetores[[frame_atual]] <- criar_dados_vetores(frame_atual, 270, 0.5)
          dados_circulo[[frame_atual]] <- criar_dados_circulo(frame_atual)
          dados_marcador[[frame_atual]] <- criar_dados_marcador(frame_atual, 270)
          dados_texto[[frame_atual]] <- criar_dados_texto(frame_atual, "pausa_270")
          frame_atual <- frame_atual + 1
        }
      }
    }
    
    # 4. FRAMES FINAIS (volta ao estado inicial)
    cat("🏁 Processando frames finais...\n")
    for (i in 1:config$pause_final) {
      dados_quadrado[[frame_atual]] <- criar_dados_quadrado(frame_atual, "final", 360, 0.8)
      dados_vetores[[frame_atual]] <- criar_dados_vetores(frame_atual, 360, 0.8)
      dados_circulo[[frame_atual]] <- criar_dados_circulo(frame_atual)
      dados_marcador[[frame_atual]] <- criar_dados_marcador(frame_atual, 360)
      dados_texto[[frame_atual]] <- criar_dados_texto(frame_atual, "final")
      frame_atual <- frame_atual + 1
    }
    
    # Combina todos os dados
    cat("🔗 Combinando dados...\n")
    
    dados_quadrado_final <- do.call(rbind, dados_quadrado)
    dados_vetores_final <- do.call(rbind, dados_vetores)
    dados_circulo_final <- do.call(rbind, dados_circulo)
    dados_marcador_final <- do.call(rbind, dados_marcador[sapply(dados_marcador, nrow) > 0])
    dados_texto_final <- do.call(rbind, dados_texto)
    
    cat(sprintf("✅ Dados gerados: %d frames totais\n", frame_atual - 1))
    
    return(list(
      quadrado = dados_quadrado_final,
      vetores = dados_vetores_final,
      circulo = dados_circulo_final,
      marcador = dados_marcador_final,
      texto = dados_texto_final
    ))
  }
  
  # ===============================================================================
  # CRIAÇÃO DO PLOT GGPLOT APRIMORADO PARA 360°
  # ===============================================================================
  
  criar_plot_animado <- function(dados) {
    
    cat("🎨 Criando visualização ggplot para rotação 360°...\n")
    
    p <- ggplot() +
      
      # Grid de fundo
      geom_hline(yintercept = seq(-2, 2, 0.5), color = config$cores$grid, 
                 alpha = 0.3, size = 0.3) +
      geom_vline(xintercept = seq(-2, 2, 0.5), color = config$cores$grid, 
                 alpha = 0.3, size = 0.3) +
      
      # Eixos principais
      geom_hline(yintercept = 0, color = config$cores$eixos, size = 1, alpha = 0.7) +
      geom_vline(xintercept = 0, color = config$cores$eixos, size = 1, alpha = 0.7) +
      
      # Círculo de referência para mostrar rotação completa
      geom_path(data = dados$circulo,
                aes(x = x, y = y, group = frame),
                color = config$cores$trajetoria, size = 1, alpha = 0.4, linetype = "dotted") +
      
      # Marcador de ângulo atual no círculo
      geom_point(data = dados$marcador,
                 aes(x = x, y = y),
                 color = config$cores$marcador, size = 4, alpha = 0.8) +
      
      # Quadrado original
      geom_path(data = dados$quadrado[dados$quadrado$tipo == "original", ],
                aes(x = x, y = y, alpha = alpha, group = frame),
                color = config$cores$objeto_inicial, size = 2) +
      
      # Quadrado transformado
      geom_path(data = dados$quadrado[dados$quadrado$tipo == "transformado", ],
                aes(x = x, y = y, alpha = alpha, group = frame),
                color = config$cores$objeto_final, size = 2) +
      
      # Preenchimento quadrado original
      geom_polygon(data = dados$quadrado[dados$quadrado$tipo == "original", ],
                   aes(x = x, y = y, alpha = alpha * 0.3, group = frame),
                   fill = config$cores$objeto_inicial) +
      
      # Preenchimento quadrado transformado
      geom_polygon(data = dados$quadrado[dados$quadrado$tipo == "transformado", ],
                   aes(x = x, y = y, alpha = alpha * 0.3, group = frame),
                   fill = config$cores$objeto_final) +
      
      # Vetores base originais - i
      geom_segment(data = dados$vetores[dados$vetores$tipo == "original" & dados$vetores$vetor == "i", ],
                   aes(x = x_inicio, y = y_inicio, xend = x_fim, yend = y_fim, alpha = alpha),
                   color = config$cores$vetor_i, size = 2,
                   arrow = arrow(length = unit(0.3, "cm"), type = "closed")) +
      
      # Vetores base originais - j
      geom_segment(data = dados$vetores[dados$vetores$tipo == "original" & dados$vetores$vetor == "j", ],
                   aes(x = x_inicio, y = y_inicio, xend = x_fim, yend = y_fim, alpha = alpha),
                   color = config$cores$vetor_j, size = 2,
                   arrow = arrow(length = unit(0.3, "cm"), type = "closed")) +
      
      # Vetores base transformados - i
      geom_segment(data = dados$vetores[dados$vetores$tipo == "transformado" & dados$vetores$vetor == "i", ],
                   aes(x = x_inicio, y = y_inicio, xend = x_fim, yend = y_fim, alpha = alpha),
                   color = config$cores$vetor_i, size = 2, linetype = "dashed",
                   arrow = arrow(length = unit(0.3, "cm"), type = "closed")) +
      
      # Vetores base transformados - j
      geom_segment(data = dados$vetores[dados$vetores$tipo == "transformado" & dados$vetores$vetor == "j", ],
                   aes(x = x_inicio, y = y_inicio, xend = x_fim, yend = y_fim, alpha = alpha),
                   color = config$cores$vetor_j, size = 2, linetype = "dashed",
                   arrow = arrow(length = unit(0.3, "cm"), type = "closed")) +
      
      # Texto explicativo
      geom_label(data = dados$texto,
                 aes(x = x, y = y, label = label),
                 fill = config$cores$texto_fundo, color = config$cores$texto_borda,
                 alpha = 0.9, size = 3.2, fontface = "bold",
                 label.padding = unit(0.4, "lines")) +
      
      # Configurações do plot expandidas para 360°
      coord_fixed(ratio = 1, xlim = c(-2.5, 2.5), ylim = c(-2.8, 2.5)) +
      labs(
        title = "Transformação Linear: Rotação Completa 360°",
        subtitle = "Visualização da rotação completa com pausas didáticas",
        x = "X",
        y = "Y"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray40"),
        axis.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 11),
        panel.grid = element_blank(),
        legend.position = "none",
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA)
      ) +
      scale_alpha_identity()
    
    return(p)
  }
  
  # ===============================================================================
  # FUNÇÃO PRINCIPAL DE GERAÇÃO
  # ===============================================================================
  
  gerar_animacao <- function(caminho_saida = "E:/codigo/rotacao_linear_didatica_360.gif") {
    
    cat("🔬 GERADOR DE ANIMAÇÃO DIDÁTICA - ROTAÇÃO 360°\n")
    cat(paste(rep("=", 60), collapse = ""), "\n")
    cat(sprintf("⚙️ Configurações:\n"))
    cat(sprintf("   • Ângulo de rotação: %d° (ROTAÇÃO COMPLETA)\n", config$angulo_total))
    cat(sprintf("   • FPS: %d\n", config$fps))
    cat(sprintf("   • Resolução: %dx%d\n", config$largura, config$altura))
    cat(sprintf("   • Pausas didáticas: 90°, 180°, 270°\n"))
    cat("\n")
    
    # Cria diretório se necessário
    dir.create(dirname(caminho_saida), recursive = TRUE, showWarnings = FALSE)
    
    # Gera dados da animação
    dados <- gerar_dados_animacao()
    
    # Cria plot animado
    plot_animado <- criar_plot_animado(dados)
    
    # Adiciona animação
    cat("🎬 Renderizando animação de 360°...\n")
    
    animacao <- plot_animado +
      transition_manual(frame) +
      ease_aes('linear')
    
    # Calcula duração total
    total_frames <- config$pause_inicial + config$pause_matriz + 
      config$frames_rotacao + config$pause_90_graus +
      config$pause_180_graus + config$pause_270_graus +
      config$pause_final
    duracao <- total_frames / config$fps
    
    # Renderiza e salva
    cat("💾 Salvando GIF de rotação completa...\n")
    
    tryCatch({
      anim <- animate(
        animacao,
        width = config$largura,
        height = config$altura,
        fps = config$fps,
        duration = duracao,
        renderer = gifski_renderer(caminho_saida, loop = TRUE)
      )
      
      cat(sprintf("✅ Animação de 360° salva com sucesso em: %s\n", caminho_saida))
      cat("📊 Estatísticas da Rotação Completa:\n")
      cat(sprintf("   • Total de frames: %d\n", total_frames))
      cat(sprintf("   • Duração: %.1f segundos\n", duracao))
      cat(sprintf("   • FPS: %d\n", config$fps))
      cat(sprintf("   • Resolução: %dx%d\n", config$largura, config$altura))
      cat(sprintf("   • Ângulo total: %d° (rotação completa)\n", config$angulo_total))
      cat(sprintf("   • Pausas didáticas: 4 pontos (90°, 180°, 270°, 360°)\n"))
      cat("\n🎉 Processo de rotação 360° concluído com sucesso!\n")
      cat("📖 A animação mostra uma rotação completa com retorno ao estado inicial.\n")
      
      return(anim)
      
    }, error = function(e) {
      cat(sprintf("❌ Erro ao renderizar: %s\n", e$message))
      cat("🔧 Tentando método alternativo...\n")
      
      # Método alternativo
      anim_alt <- animate(
        animacao,
        width = config$largura,
        height = config$altura,
        fps = config$fps,
        nframes = total_frames,
        renderer = gifski_renderer(caminho_saida)
      )
      
      cat("✅ Animação de 360° salva com método alternativo!\n")
      return(anim_alt)
    })
  }
  
  # Retorna a função principal
  return(list(gerar_animacao = gerar_animacao))
}

# ===============================================================================
# EXECUÇÃO AUTOMÁTICA DA ROTAÇÃO 360°
# ===============================================================================

cat("🚀 INICIANDO GERAÇÃO AUTOMÁTICA DA ANIMAÇÃO 360°...\n\n")

# Cria e executa a animação automaticamente
tryCatch({
  
  # Cria o diretório se não existir
  dir.create("E:/codigo", recursive = TRUE, showWarnings = FALSE)
  
  # Gera a animação de rotação completa
  animador <- RotacaoLinear360()
  animacao <- animador$gerar_animacao()
  
  cat("\n🎯 ANIMAÇÃO DE ROTAÇÃO 360° CONCLUÍDA E SALVA EM E:/codigo/\n")
  cat("🔄 A animação mostra uma rotação completa de 360° com pausas didáticas!\n")
  
}, error = function(e) {
  cat(sprintf("❌ Erro durante a geração: %s\n", e$message))
  cat("🔧 Possíveis soluções:\n")
  cat("   1. Verifique se o caminho E:/codigo/ é acessível\n")
  cat("   2. Execute: dir.create('E:/codigo/', recursive = TRUE)\n")
  cat("   3. Tente um caminho diferente\n")
  cat("   4. Reinstale as dependências se necessário\n")
})