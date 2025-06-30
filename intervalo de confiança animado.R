# Instalação dos pacotes necessários (executar se ainda não tiver instalado)
# install.packages(c("tidyverse", "gganimate", "gifski", "ggthemes"))
#install
install.packages("ggthemes")
# Carregar bibliotecas
library(tidyverse)
library(gganimate)
library(ggthemes)
library(gifski)

# Definir uma semente para reprodutibilidade
set.seed(123)

# Criar dataset simulado
# 15 semanas de monitoramento, 30 incidentes por semana
criar_dataset_seguranca <- function() {
  # O tempo médio de detecção vai melhorando gradualmente
  semanas <- 1:15
  media_tempo_por_semana <- 7 - (semanas * 0.2)  # Começa em 6.8 min e vai melhorando
  
  # Criar dataframe vazio para preencher
  dados_seg <- data.frame()
  
  for (semana in semanas) {
    # Simular 30 incidentes por semana
    tempo_deteccao <- rnorm(30, mean = media_tempo_por_semana[semana], sd = 1.5)
    
    # Arredondar para duas casas decimais
    tempo_deteccao <- round(pmax(0.1, tempo_deteccao), 2)  # Evitar valores negativos
    
    # Adicionar à base de dados
    dados_semana <- data.frame(
      semana = semana,
      incidente = 1:30,
      tempo_deteccao = tempo_deteccao
    )
    
    dados_seg <- rbind(dados_seg, dados_semana)
  }
  
  return(dados_seg)
}

# Gerar o dataset
dados_seguranca <- criar_dataset_seguranca()

# Calcular estatísticas por semana
estatisticas_semana <- dados_seguranca %>%
  group_by(semana) %>%
  summarise(
    media = mean(tempo_deteccao),
    desvio_padrao = sd(tempo_deteccao),
    erro_padrao = desvio_padrao / sqrt(n()),
    # Intervalo de confiança de 95%
    limite_inferior = media - qt(0.975, n() - 1) * erro_padrao,
    limite_superior = media + qt(0.975, n() - 1) * erro_padrao,
    .groups = "drop"
  )

# Criar a animação
grafico_animado <- ggplot(estatisticas_semana, aes(x = semana, y = media)) +
  # Adicionar uma linha de referência para o limite aceitável
  geom_hline(yintercept = 5, color = "#FF5555", linetype = "dashed", size = 1) +
  annotate("text", x = 1, y = 5.2, label = "Limite Aceitável (5 min)", hjust = 0, color = "#FF5555") +
  
  # Adicionar pontos e linha para as médias
  geom_point(size = 3, color = "#2980b9") +
  geom_line(color = "#2980b9", size = 1) +
  
  # Adicionar os intervalos de confiança
  geom_errorbar(
    aes(ymin = limite_inferior, ymax = limite_superior),
    width = 0.2, color = "#2980b9", alpha = 0.7, size = 1
  ) +
  
  # Adicionar rótulos e títulos
  labs(
    title = "Tempo de Detecção de Intrusões ao Longo do Tempo",
    subtitle = "Semana {closest_state}: Média = {format(round(estatisticas_semana$media[estatisticas_semana$semana == closest_state], 2), nsmall = 2)} min (IC 95%)",
    x = "Semana de Monitoramento",
    y = "Tempo Médio de Detecção (minutos)",
    caption = "Dados simulados de detecção de intrusões na rede corporativa"
  ) +
  
  # Colorir os intervalos de confiança com base em estarem acima ou abaixo do limite
  geom_rect(
    aes(
      xmin = semana - 0.4, 
      xmax = semana + 0.4, 
      ymin = limite_inferior, 
      ymax = limite_superior,
      fill = media < 5  # Verde se estiver abaixo do limite
    ),
    alpha = 0.2
  ) +
  scale_fill_manual(values = c("#e74c3c", "#27ae60"), guide = "none") +
  
  # Personalizar o tema
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 13),
    axis.title = element_text(size = 12),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray90")
  ) +
  
  # Configurar a animação
  transition_states(
    semana,
    transition_length = 2,
    state_length = 3
  ) +
  shadow_mark(alpha = 0.3, size = 1) +
  
  # Adicionar efeito de movimento suave
  ease_aes("cubic-in-out") +
  
  # Adicionar anotação dinâmica
  geom_text(
    aes(
      x = semana, 
      y = media + 0.5,
      label = sprintf("%.2f min", media)
    ), 
    vjust = -0.5
  )

# Renderizar a animação
animacao <- animate(
  grafico_animado, 
  fps = 10, 
  duration = 15,
  width = 800, 
  height = 500,
  renderer = gifski_renderer()
)

# Para salvar a animação
# anim_save("deteccao_intrusoes.gif", animacao)

# Mostrar a animação
print(animacao)