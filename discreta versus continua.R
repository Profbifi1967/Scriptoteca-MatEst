# Defina o conteúdo do script
codigo <- '
library(ggplot2)
library(gganimate)
library(dplyr)

set.seed(42)
x_discreta <- 1:5
y_discreta <- c(2, 5, 7, 4, 8)

x_continua <- seq(1, 5, length.out = 100)
dens_continua <- dnorm(x_continua, mean = 3, sd = 0.7)
dens_continua <- dens_continua / max(dens_continua) * max(y_discreta)

# Dados acumulados para as barras (discreta)
barras <- do.call(rbind, lapply(1:5, function(i) {
  data.frame(
    x = x_discreta[1:i],
    y = y_discreta[1:i],
    step = i
  )
}))

# Dados acumulados para a curva (contínua)
curvas <- do.call(rbind, lapply(1:5, function(i) {
  data.frame(
    x = x_continua[x_continua <= x_discreta[i]],
    y = dens_continua[x_continua <= x_discreta[i]],
    step = i
  )
}))

ylim <- max(max(y_discreta), max(dens_continua)) * 1.3

p <- ggplot() +
  geom_col(
    data = barras,
    aes(x = x, y = y),
    fill = "orange",
    width = 0.6,
    alpha = 0.7
  ) +
  geom_line(
    data = curvas,
    aes(x = x, y = y),
    color = "black",
    size = 2
  ) +
  theme_minimal(base_size = 18) +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(50, 50, 50, 50),
    panel.grid = element_blank()
  ) +
  labs(
    title = "Visualização: Variáveis Discretas vs. Contínuas",
    subtitle = "Barras laranja: variável discreta. Curva preta espessa: variável contínua.",
    x = "Valores",
    y = "Frequência / Densidade",
    caption = "Legenda: Laranja = Discreta | Preto = Contínua"
  ) +
  scale_x_continuous(breaks = 1:5, limits = c(0.7, 5.3)) +
  scale_y_continuous(limits = c(0, ylim))

anim <- p +
  transition_states(step, transition_length = 2, state_length = 1, wrap = FALSE) +
  enter_fade() +
  ease_aes("cubic-in-out")

animate(
  anim,
  nframes = 60, fps = 10,
  width = 850, height = 600,
  renderer = gifski_renderer()
)
'
# Salve o script no caminho desejado
writeLines(codigo, "e:/codigo/animacao_variaveis.R")