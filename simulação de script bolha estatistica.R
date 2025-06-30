# Certifique-se de ter os pacotes instalados:
# install.packages("ggplot2")
# install.packages("dplyr")
# install.packages("gganimate")
# install.packages("gifski")
# install.packages("transformr")

library(ggplot2)
library(dplyr)
library(gganimate)
library(gifski)
library(transformr)

# 1. Dados simulados (mantidos como antes)
set.seed(123)
dados_vendas_animado <- data.frame(
  Trimestre = rep(1:4, each = 25),
  Regiao = sample(c("Norte", "Sul", "Leste", "Oeste"), 100, replace = TRUE),
  Categoria = sample(c("Eletrônicos", "Roupas", "Alimentos", "Livros", "Móveis"), 100, replace = TRUE),
  Quantidade_Vendida = round(runif(100, 50, 500)),
  Lucro_Total = round(runif(100, 1000, 15000))
)

dados_agrupados_animado <- dados_vendas_animado %>%
  group_by(Trimestre, Regiao, Categoria) %>%
  summarise(
    Total_Quantidade_Vendida = sum(Quantidade_Vendida),
    Total_Lucro = sum(Lucro_Total)
  ) %>%
  ungroup()

# 2. Gráfico de bolhas base com ajustes de tema
p <- ggplot(dados_agrupados_animado, aes(x = Total_Quantidade_Vendida, y = Total_Lucro, size = Total_Lucro, color = Regiao)) +
  geom_point(alpha = 0.7) +
  scale_size_continuous(range = c(8, 30)) +
  labs(
    title = "Desempenho de Vendas por Região e Categoria de Produto",
    x = "Quantidade Total de Produtos Vendidos",
    y = "Lucro Total (R$)",
    size = "Lucro Total (R$)",
    color = "Região"
  ) +
  facet_wrap(~ Categoria, scales = "free", ncol = 3) +
  theme_minimal() +
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
    axis.title = element_text(size = 12),
    # Aumenta o tamanho da fonte em 1 unidade e coloca em negrito para os rótulos dos eixos [[1](https://translate.google.com/translate?u=https://stackoverflow.com/questions/14942681/change-size-of-axes-title-and-labels-in-ggplot2&hl=pt&sl=en&tl=pt&client=srp), [3](https://pt.stackoverflow.com/questions/518586/valores-do-eixo-x-em-negrito)]
    axis.text = element_text(size = 11, face = "bold"),
    strip.text = element_text(size = 12, face = "bold"),
    panel.spacing = unit(1.5, "lines"),
    # Adiciona borda preta aos painéis para criar linhas divisórias claras
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 1)
  )

# 3. Camada de animação
animacao <- p +
  transition_time(Trimestre) +
  ease_aes('linear') +
  labs(subtitle = "Trimestre: {frame_time}")

# 4. Salvar o GIF com dimensões maiores no caminho especificado
anim_save(
  "E:/codigo/grafico_bolhas_animado_melhorado_v2.gif",
  animacao,
  nframes = 100,
  fps = 10,
  width = 1200,
  height = 800,
  renderer = gifski_renderer()
)