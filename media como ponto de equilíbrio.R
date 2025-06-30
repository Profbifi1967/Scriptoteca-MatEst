# 1. Carregar os pacotes necessários
library(ggplot2)
library(gganimate)
library(dplyr)
library(gifski)

# Definir uma semente para garantir que os resultados sejam reprodutíveis
set.seed(42)

# --- ETAPA DE CRIAÇÃO DOS DADOS ---

# Estágio 1: Distribuição Simétrica (Equilíbrio Inicial)
dados_simetricos <- data.frame(valor = rnorm(200, mean = 50, sd = 10)) %>%
  mutate(
    stage = "1. Equilíbrio Inicial: Dados Simétricos",
    label_text = "Em uma distribuição simétrica,\na média (triângulo) se localiza\nno centro exato dos dados,\natuando como um ponto de equilíbrio perfeito."
  )

# ALTERAÇÃO 1: Aumentar o número de outliers para 25.
dados_outlier_direita <- data.frame(valor = c(dados_simetricos$valor, rep(150, 25))) %>%
  mutate(
    stage = "2. Desequilíbrio: Adicionando Valores Altos",
    label_text = "Ao inserir valores extremos (muito altos),\na 'balança' pende para a direita.\nA média é 'puxada' nessa direção para\nencontrar um novo ponto de equilíbrio."
  )

# ALTERAÇÃO 1: Aumentar o número de outliers para 25.
dados_outlier_esquerda <- data.frame(valor = c(dados_simetricos$valor, rep(-20, 25))) %>%
  mutate(
    stage = "3. Desequilíbrio: Adicionando Valores Baixos",
    label_text = "Da mesma forma, valores extremos (muito baixos)\n'puxam' a média para a esquerda,\nmostrando sua sensibilidade a todos\nos pontos da amostra."
  )

# Combinar todos os dados em um único dataframe
dados_completos <- bind_rows(dados_simetricos, dados_outlier_direita, dados_outlier_esquerda)

# Calcular a média para cada estágio.
dados_animacao <- dados_completos %>%
  group_by(stage, label_text) %>%
  summarise(
    mean_val = mean(valor),
    data = list(valor), 
    .groups = 'drop'
  ) %>%
  tidyr::unnest(data)

names(dados_animacao)[names(dados_animacao) == 'data'] <- 'valor'

# --- ETAPA DE CRIAÇÃO DO GRÁFICO E ANIMAÇÃO ---

x_limits <- range(dados_animacao$valor)

p <- ggplot(dados_animacao, aes(x = valor)) +
  geom_histogram(aes(y = after_stat(density)), fill = "#0072B2", color = "white", binwidth = 5, alpha = 0.8) +
  
  # ALTERAÇÃO 4: Descer o triângulo para que o vértice tangencie o eixo x.
  # Para isso, posicionamos o centro do triângulo ligeiramente abaixo de y=0.
  geom_point(
    aes(x = mean_val, y = -0.001), 
    shape = 24, 
    size = 7, 
    fill = "#D55E00", 
    color = "#D55E00"
  ) +
  
  # ALTERAÇÃO 5: Adicionar o texto "Média" abaixo do triângulo.
  # O texto também usa aes(x = mean_val) para se mover junto com o triângulo.
  geom_text(
    aes(x = mean_val, y = -0.0025), # Posição y ainda mais baixa
    label = "Média",
    size = 4,
    color = "#D55E00",
    fontface = "bold",
    vjust = 1 # Alinha o texto pela parte de cima
  ) +
  
  geom_hline(yintercept = 0, color = "black", linewidth = 1) +
  
  # ALTERAÇÃO 2: Diminuir a fonte do texto explicativo em 1 unidade (de 7 para 6).
  geom_text(
    aes(x = -Inf, y = Inf, label = label_text),
    hjust = -0.05,
    vjust = 1.1,
    size = 6, 
    lineheight = 0.9,
    family = "sans",
    fontface = "bold"
  ) +
  
  labs(
    title = 'A Média como Ponto de Equilíbrio',
    subtitle = 'Estágio Atual: {closest_state}',
    x = 'Valor da Variável',
    y = 'Densidade'
  ) +
  # ALTERAÇÃO 3: Título e subtítulo em negrito (já estava correto, apenas confirmado).
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(size = 20, face = "bold", color = "black"),
    plot.subtitle = element_text(size = 16, face = "bold", color = "#D55E00"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_line(linetype = "dashed", color="gray80"),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  coord_cartesian(xlim = x_limits, clip = "off") + # 'clip = "off"' ajuda a garantir que nada seja cortado
  
  transition_states(
    stage,
    transition_length = 2,
    state_length = 5
  ) +
  ease_aes('cubic-in-out')

# Renderizar a animação
anim <- animate(p, 
                nframes = 270, 
                duration = 18, 
                fps = 15,      
                width = 800,   
                height = 600,  
                renderer = gifski_renderer())

# Salvar o arquivo no caminho especificado 'E:\codigo'
caminho_salvar <- "E:/codigo/media_ponto_de_equilibrio_v3.gif"
anim_save(caminho_salvar, animation = anim)

# Mensagem de confirmação
print(paste("Animação salva com sucesso em:", caminho_salvar))
