# ===============================================================================
# SCRIPT ANIMADO PARA ANÁLISE DE FUNÇÕES POLINOMIAIS
# Versão Aprimorada com Detecção Precisa de Pontos Críticos e Loop Contínuo
# ===============================================================================

# ===============================================================================
# MÓDULO 1: VALIDAÇÃO E PARSING DA FUNÇÃO POLINOMIAL
# ===============================================================================

validar_funcao_polinomial <- function(entrada) {
  tryCatch({
    # Remove espaços
    entrada_limpa <- gsub(" ", "", tolower(entrada))
    
    # Verifica se começa com f(x)= de forma simples
    if (!startsWith(entrada_limpa, "f(x)=")) {
      return(list(valida = FALSE, erro = "Formato deve ser f(x) = ..."))
    }
    
    # Extrai a expressão após f(x)=
    expressao <- substring(entrada_limpa, 6)  # Remove "f(x)="
    
    # Verifica se a expressão não está vazia
    if (nchar(expressao) == 0) {
      return(list(valida = FALSE, erro = "Expressão vazia"))
    }
    
    # Tenta parsear a expressão
    expr_parsed <- parse(text = expressao)
    
    # Testa se a expressão é avaliável
    teste_x <- c(-2, -1, 0, 1, 2)
    resultado_teste <- sapply(teste_x, function(x) eval(expr_parsed))
    
    # Verifica se todos os resultados são numéricos finitos
    if (!all(is.finite(resultado_teste))) {
      return(list(valida = FALSE, erro = "Expressão produz valores não finitos"))
    }
    
    return(list(valida = TRUE, expressao = expr_parsed, texto = expressao))
    
  }, error = function(e) {
    return(list(valida = FALSE, erro = paste("Erro de parsing:", e$message)))
  })
}

obter_funcao_usuario <- function() {
  repeat {
    cat("\n")
    cat("============================================================\n")
    cat("ENTRADA DA FUNÇÃO POLINOMIAL\n")
    cat("============================================================\n")
    cat("Digite uma função polinomial no formato: f(x) = ...\n")
    cat("Exemplo: f(x) = 2*x^3 - 3*x^2 + 5*x - 1\n")
    cat("Digite 'sair' para encerrar o programa\n")
    cat("------------------------------------------------------------\n")
    
    entrada <- readline(prompt = "f(x) = ")
    
    # Verifica se o usuário quer sair
    if (tolower(entrada) == "sair") {
      return(NULL)
    }
    
    entrada_completa <- paste("f(x) =", entrada)
    
    validacao <- validar_funcao_polinomial(entrada_completa)
    
    if (validacao$valida) {
      cat("\n✓ Função válida aceita!\n")
      return(validacao)
    } else {
      cat("\n✗ Erro na função - Digite novamente\n")
      cat("Erro:", validacao$erro, "\n")
    }
  }
}

# ===============================================================================
# MÓDULO 2: DETECÇÃO APRIMORADA DE PONTOS CRÍTICOS
# ===============================================================================

# Função para calcular derivada numérica com maior precisão
calcular_derivada <- function(expr, x, h = 1e-8) {
  tryCatch({
    (eval(expr, list(x = x + h)) - eval(expr, list(x = x - h))) / (2 * h)
  }, error = function(e) NA)
}

# Função para calcular segunda derivada numérica
calcular_segunda_derivada <- function(expr, x, h = 1e-6) {
  tryCatch({
    (eval(expr, list(x = x + h)) - 2 * eval(expr, list(x = x)) + eval(expr, list(x = x - h))) / (h^2)
  }, error = function(e) NA)
}

# Função aprimorada para encontrar pontos críticos
encontrar_pontos_criticos_aprimorado <- function(expr, intervalo = c(-20, 20)) {
  
  pontos_criticos <- list()
  tipos <- character()
  
  # Grid mais denso para busca inicial
  x_grid <- seq(intervalo[1], intervalo[2], length.out = 1000)
  
  # Calcula derivadas em todo o grid
  derivadas <- sapply(x_grid, function(x) calcular_derivada(expr, x))
  
  # Remove valores NA
  indices_validos <- !is.na(derivadas)
  x_grid <- x_grid[indices_validos]
  derivadas <- derivadas[indices_validos]
  
  # Encontra mudanças de sinal na derivada (pontos críticos)
  mudancas_sinal <- which(diff(sign(derivadas)) != 0)
  
  for (i in mudancas_sinal) {
    # Refina a busca do zero da derivada usando método de bisseção
    x_esq <- x_grid[i]
    x_dir <- x_grid[i + 1]
    
    # Método de bisseção para encontrar o zero da derivada
    for (iter in 1:50) {
      x_meio <- (x_esq + x_dir) / 2
      deriv_meio <- calcular_derivada(expr, x_meio)
      deriv_esq <- calcular_derivada(expr, x_esq)
      
      if (abs(deriv_meio) < 1e-10 || (x_dir - x_esq) < 1e-10) break
      
      if (sign(deriv_meio) == sign(deriv_esq)) {
        x_esq <- x_meio
      } else {
        x_dir <- x_meio
      }
    }
    
    x_critico <- (x_esq + x_dir) / 2
    
    # Verifica se já não foi encontrado (evita duplicatas)
    if (length(pontos_criticos) == 0 || 
        all(abs(unlist(pontos_criticos) - x_critico) > 0.01)) {
      
      # Classifica o tipo do ponto crítico
      segunda_deriv <- calcular_segunda_derivada(expr, x_critico)
      
      if (!is.na(segunda_deriv)) {
        if (segunda_deriv > 1e-6) {
          tipo <- "Mínimo Local"
        } else if (segunda_deriv < -1e-6) {
          tipo <- "Máximo Local"
        } else {
          tipo <- "Ponto de Sela"
        }
        
        pontos_criticos <- append(pontos_criticos, x_critico)
        tipos <- c(tipos, tipo)
      }
    }
  }
  
  # Busca adicional por pontos de inflexão (zeros da segunda derivada)
  segundas_derivadas <- sapply(x_grid, function(x) calcular_segunda_derivada(expr, x))
  indices_validos_2 <- !is.na(segundas_derivadas)
  x_grid_2 <- x_grid[indices_validos_2]
  segundas_derivadas <- segundas_derivadas[indices_validos_2]
  
  mudancas_sinal_2 <- which(diff(sign(segundas_derivadas)) != 0)
  
  for (i in mudancas_sinal_2) {
    x_esq <- x_grid_2[i]
    x_dir <- x_grid_2[i + 1]
    
    # Método de bisseção para encontrar o zero da segunda derivada
    for (iter in 1:50) {
      x_meio <- (x_esq + x_dir) / 2
      segunda_deriv_meio <- calcular_segunda_derivada(expr, x_meio)
      segunda_deriv_esq <- calcular_segunda_derivada(expr, x_esq)
      
      if (abs(segunda_deriv_meio) < 1e-10 || (x_dir - x_esq) < 1e-10) break
      
      if (sign(segunda_deriv_meio) == sign(segunda_deriv_esq)) {
        x_esq <- x_meio
      } else {
        x_dir <- x_meio
      }
    }
    
    x_inflexao <- (x_esq + x_dir) / 2
    
    # Verifica se não é um ponto crítico já encontrado
    todos_pontos <- unlist(pontos_criticos)
    if (length(todos_pontos) == 0 || all(abs(todos_pontos - x_inflexao) > 0.1)) {
      # Verifica se já não foi encontrado
      if (length(pontos_criticos) == 0 || 
          all(abs(unlist(pontos_criticos) - x_inflexao) > 0.01)) {
        
        pontos_criticos <- append(pontos_criticos, x_inflexao)
        tipos <- c(tipos, "Ponto de Inflexão")
      }
    }
  }
  
  # Ordena os pontos por posição x
  if (length(pontos_criticos) > 0) {
    pontos_x <- unlist(pontos_criticos)
    ordem <- order(pontos_x)
    pontos_x <- pontos_x[ordem]
    tipos <- tipos[ordem]
  } else {
    pontos_x <- numeric(0)
  }
  
  return(list(x = pontos_x, tipos = tipos))
}

# ===============================================================================
# MÓDULO 3: DEFINIÇÃO DINÂMICA DO INTERVALO DE PLOTAGEM
# ===============================================================================

definir_intervalo_plotagem <- function(pontos_criticos) {
  if (length(pontos_criticos$x) >= 2) {
    margem <- abs(max(pontos_criticos$x) - min(pontos_criticos$x)) * 0.3
    x_min <- min(pontos_criticos$x) - margem
    x_max <- max(pontos_criticos$x) + margem
  } else if (length(pontos_criticos$x) > 0) {
    x_min <- min(pontos_criticos$x) - 3
    x_max <- max(pontos_criticos$x) + 3
  } else {
    x_min <- -10
    x_max <- 10
  }
  
  return(c(x_min, x_max))
}

# ===============================================================================
# MÓDULO 4: ANIMAÇÃO GRÁFICA APRIMORADA
# ===============================================================================

criar_animacao_grafico <- function(funcao_info) {
  expr <- funcao_info$expressao
  
  cat("\n🔍 Analisando pontos críticos da função...\n")
  
  # Encontra pontos críticos
  pontos_criticos <- encontrar_pontos_criticos_aprimorado(expr)
  
  if (length(pontos_criticos$x) > 0) {
    cat(sprintf("✓ Encontrados %d pontos críticos!\n", length(pontos_criticos$x)))
  } else {
    cat("⚠ Nenhum ponto crítico encontrado no intervalo.\n")
  }
  
  # Define intervalo de plotagem
  intervalo_x <- definir_intervalo_plotagem(pontos_criticos)
  
  # Calcula valores da função
  x_vals <- seq(intervalo_x[1], intervalo_x[2], length.out = 800)
  y_vals <- sapply(x_vals, function(x) eval(expr, list(x = x)))
  
  # Define limites do gráfico
  y_range <- max(y_vals) - min(y_vals)
  y_min <- min(y_vals) - y_range * 0.2
  y_max <- max(y_vals) + y_range * 0.2
  
  cat("\n🎬 Iniciando animação do gráfico...\n")
  
  # Configuração inicial do gráfico (vazio)
  par(family = "sans", cex.main = 1.4, cex.lab = 1.2, bg = "white")
  plot(NULL, xlim = intervalo_x, ylim = c(y_min, y_max),
       xlab = "x", ylab = "f(x)", 
       main = paste("Função:", funcao_info$texto),
       las = 1, col.axis = "darkblue", col.lab = "darkblue")
  
  # Adiciona grade sutil
  grid(col = "lightgray", lty = "dotted", lwd = 0.5)
  
  # Adiciona eixos destacados
  abline(h = 0, col = "gray30", lwd = 1)
  abline(v = 0, col = "gray30", lwd = 1)
  
  # Pausa inicial para mostrar o gráfico vazio
  Sys.sleep(1)
  
  # Animação suave do traçado da função
  n_segmentos <- 120
  indices_segmentos <- seq(1, length(x_vals), length.out = n_segmentos)
  
  # Variáveis para controle dos pontos críticos
  if (length(pontos_criticos$x) > 0) {
    pontos_marcados <- rep(FALSE, length(pontos_criticos$x))
    letras <- LETTERS[1:length(pontos_criticos$x)]
    cores_pontos <- c("red", "blue", "darkgreen", "purple", "orange")[1:length(pontos_criticos$x)]
  } else {
    pontos_marcados <- logical(0)
    letras <- character(0)
    cores_pontos <- character(0)
  }
  
  cat("✏️  Traçando a função suavemente...\n")
  
  for (i in 2:length(indices_segmentos)) {
    # Desenha segmento da curva com efeito de "caneta"
    idx_inicio <- round(indices_segmentos[i-1])
    idx_fim <- round(indices_segmentos[i])
    
    if (idx_fim <= length(x_vals)) {
      # Traça o segmento com linha preta (como caneta)
      lines(x_vals[idx_inicio:idx_fim], y_vals[idx_inicio:idx_fim], 
            col = "black", lwd = 2.5)
      
      # Verifica se passou por algum ponto crítico
      if (length(pontos_criticos$x) > 0) {
        x_atual <- x_vals[idx_fim]
        
        for (j in seq_along(pontos_criticos$x)) {
          if (!pontos_marcados[j] && abs(x_atual - pontos_criticos$x[j]) < 0.8) {
            # Marca o ponto crítico com animação
            x_critico <- pontos_criticos$x[j]
            y_critico <- eval(expr, list(x = x_critico))
            
            # Efeito de "descoberta" do ponto
            for (tamanho in seq(0.5, 2.0, by = 0.3)) {
              points(x_critico, y_critico, pch = 19, col = cores_pontos[j], cex = tamanho)
              Sys.sleep(0.05)
            }
            
            # Adiciona círculo destacado
            points(x_critico, y_critico, pch = 21, col = cores_pontos[j], 
                   bg = "white", cex = 2.2, lwd = 2)
            points(x_critico, y_critico, pch = 19, col = cores_pontos[j], cex = 1.5)
            
            # Adiciona rótulo com fundo
            text(x_critico, y_critico + y_range * 0.12, 
                 letras[j], col = cores_pontos[j], font = 2, cex = 1.5,
                 adj = 0.5)
            
            # Adiciona descrição do tipo com caixa
            tipo_texto <- pontos_criticos$tipos[j]
            text(x_critico, y_critico - y_range * 0.15, 
                 tipo_texto, col = cores_pontos[j], font = 2, cex = 1.0,
                 adj = 0.5)
            
            # Adiciona linha vertical pontilhada
            segments(x_critico, y_min, x_critico, y_critico, 
                     col = cores_pontos[j], lty = "dotted", lwd = 1)
            
            pontos_marcados[j] <- TRUE
            
            cat(sprintf("🎯 Ponto %s identificado: %s em (%.4f, %.4f)\n", 
                        letras[j], tipo_texto, x_critico, y_critico))
          }
        }
      }
      
      # Pausa suave para efeito de animação
      Sys.sleep(0.04)
    }
  }
  
  # Finalização do gráfico com efeitos
  cat("\n🎨 Finalizando gráfico...\n")
  Sys.sleep(0.5)
  
  # Adiciona título final com destaque
  mtext("ANÁLISE COMPLETA", side = 1, line = 4, 
        cex = 1.6, font = 2, col = "darkgreen")
  
  # Adiciona legenda se houver pontos críticos
  if (length(pontos_criticos$x) > 0) {
    legend("topright", 
           legend = paste(letras, "-", pontos_criticos$tipos),
           col = cores_pontos, pch = 19, cex = 0.9,
           bg = "white", box.col = "gray")
  }
  
  # Resumo final
  if (length(pontos_criticos$x) > 0) {
    cat("\n📊 RESUMO DOS PONTOS CRÍTICOS ENCONTRADOS:\n")
    cat("═══════════════════════════════════════════\n")
    for (i in seq_along(pontos_criticos$x)) {
      x_val <- round(pontos_criticos$x[i], 4)
      y_val <- round(eval(expr, list(x = pontos_criticos$x[i])), 4)
      cat(sprintf("🔸 %s: %s\n", letras[i], pontos_criticos$tipos[i]))
      cat(sprintf("   Coordenadas: (x = %.4f, f(x) = %.4f)\n\n", x_val, y_val))
    }
  } else {
    cat("\n⚠️  Nenhum ponto crítico encontrado no intervalo analisado.\n")
  }
  
  cat("✅ Análise gráfica concluída com sucesso!\n")
}

# ===============================================================================
# MÓDULO 5: FUNÇÃO PRINCIPAL COM LOOP CONTÍNUO
# ===============================================================================

executar_analise_polinomial <- function() {
  cat("\n")
  cat("═══════════════════════════════════════════════════════════\n")
  cat("🔬 ANALISADOR ANIMADO DE FUNÇÕES POLINOMIAIS\n")
  cat("   Versão Aprimorada com Detecção Precisa de Pontos Críticos\n")
  cat("═══════════════════════════════════════════════════════════\n")
  
  repeat {
    # Captura e validação da função
    funcao_info <- obter_funcao_usuario()
    
    # Verifica se o usuário quer sair
    if (is.null(funcao_info)) {
      cat("\n👋 Encerrando o programa. Até logo!\n")
      break
    }
    
    # Análise e visualização
    cat("\n🚀 Iniciando análise da função...\n")
    criar_animacao_grafico(funcao_info)
    
    # Pergunta se quer analisar outra função
    cat("\n" , rep("─", 50), "\n")
    cat("💡 Pronto para analisar outra função!\n")
    cat("   (Digite 'sair' quando quiser encerrar)\n")
    
    # Pausa antes de voltar ao início
    Sys.sleep(2)
  }
}

# ===============================================================================
# EXECUÇÃO PRINCIPAL
# ===============================================================================

# Limpa o console e executa o programa principal
cat("\014")  # Limpa console
executar_analise_polinomial()