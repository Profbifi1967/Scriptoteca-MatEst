# ===============================
# Lei dos Grandes Números — Animação SEM TEXTOS
# ===============================

# Instale os pacotes, se necessário:
# install.packages(c("ggplot2", "gganimate", "dplyr", "transformr"))

library(ggplot2)
library(gganimate)
library(dplyr)
library(transformr)

set.seed(456)

n <- 300            # Total de observações
pop <- rexp(100000, rate = 1)   # População assimétrica
media_pop <- mean(pop)
amostra <- sample(pop, n, replace = TRUE)
media_acumulada <- cumsum(amostra) / (1:n)

dados <- tibble(
  x = 1:n,
  media = media_acumulada
)

p <- ggplot(dados, aes(x = x, y = media)) +
  geom_line(color = "#37aacc", size = 1.3) +
  geom_hline(yintercept = media_pop, linetype = "dashed", color = "red", size = 1.1) +
  labs(
    title = "Lei dos Grandes Números em Ação",
    subtitle = "A média amostral (linha azul) se aproxima da média populacional (linha vermelha)",
    x = "Quantidade de observações", y = "Média acumulada"
  ) +
  theme_minimal(base_size = 16) +
  transition_reveal(along = x)

## Exportar como GIF
dir_salvar <- "E:/codigo"
if (!dir.exists(dir_salvar)) dir.create(dir_salvar, recursive = TRUE)
gif_path <- file.path(dir_salvar, "LGN_animacao.gif")
anim <- animate(
  p,
  nframes = n,
  fps = 18,
  width = 950, height = 450,
  renderer = gifski_renderer(gif_path)
)
cat("GIF salvo em:", gif_path, "\n")