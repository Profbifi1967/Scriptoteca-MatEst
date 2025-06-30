# Diretório de saída
setwd("E:/codigo")

library(ggplot2)
library(gganimate)
library(gifski)

# Parâmetros
x_vals <- seq(-6, 6, length.out = 600)
sd <- 1.5
altura <- 0.6

posicoes_pico <- c(-2.1, 0, 2.1)
labels_curva  <- c("Assimétrico à esquerda", "Simétrico", "Assimétrico à direita")
textos_curva  <- c(
  "Moda < Mediana < Média\nCauda longa à esquerda",
  "Moda = Mediana = Média\nSimetria perfeita",
  "Média < Mediana < Moda\nCauda longa à direita"
)
cor_curva  <- c("firebrick", "forestgreen", "royalblue")
col_texto  <- c(2.8, 2.8, -5) # posição do texto fora da curva

frames_fixo <- 210  # 7 segundos a 30 fps por curva
final_dados <- data.frame()
final_medidas <- data.frame()
final_textos <- data.frame()

for (i in 1:3) {
  mu <- posicoes_pico[i]
  yvals <- dnorm(x_vals, mean = mu, sd = sd)
  yvals <- yvals / max(yvals) * altura
  for (j in 1:frames_fixo) {
    frameid <- (i-1)*frames_fixo + j
    final_dados <- rbind(final_dados, data.frame(
      x = x_vals, y = yvals, frame = frameid, tipo = i
    ))
    # Medidas centrais ilustrativas
    media   <- mu
    mediana <- mu + ifelse(i==2,0, ifelse(i==1, 0.6,-0.6))
    moda    <- mu + ifelse(i==2,0, ifelse(i==1,-0.6, 0.6))
    final_medidas <- rbind(final_medidas, data.frame(
      frame = frameid,
      x = c(moda, mediana, media),
      y = altura + c(0.03, 0, 0.03),
      cor = c("purple","seagreen","red"),
      label = c("Moda","Mediana","Média"),
      tipo = i
    ))
    final_textos <- rbind(final_textos, data.frame(
      frame = frameid,
      x = col_texto[i],
      y = 0.46,
      texto = paste(labels_curva[i], textos_curva[i], sep = "\n"),
      cor = cor_curva[i]
    ))
  }
}

p <- ggplot() +
  geom_line(data = final_dados, aes(x = x, y = y), size = 1.7, color = "steelblue") +
  geom_hline(yintercept = 0, col = "grey50") +
  geom_segment(
    data = final_medidas,
    aes(x = x, xend = x, y = 0, yend = altura), linetype = "dashed",
    color = "black", size = .8
  ) +
  geom_label(
    data = final_medidas,
    aes(x = x, y = altura + 0.07, label = label, fill = cor),
    color = "white", fontface = "bold", size = 5, label.size = 0,
    show.legend = FALSE
  ) +
  scale_fill_manual(values = c("red", "seagreen", "purple")) +
  geom_label(
    data = final_textos,
    aes(x = x, y = y, label = texto), 
    fill = "#FFFA", color = "black", size = 6, fontface = "bold", label.size = .6, hjust = 0
  ) +
  scale_x_continuous(breaks = seq(-6,6,2), limits = c(-6,6)) +
  scale_y_continuous(limits = c(-0.06, .78), expand = c(0,0)) +
  theme_minimal(base_size = 19) +
  theme(
    plot.title = element_text(size = 22, hjust = 0.5, face = "bold", margin = margin(t = 10, b = 20)),
    plot.margin = margin(t = 40, r = 30, b = 16, l = 30, unit = "pt"),
    axis.title.y = element_blank(),
    legend.position = "none",
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  labs(
    title = "Distribuições: Simétrica e Assimétricas\nTendências Centrais e Caudas Fixas",
    x = "Valor"
  ) +
  transition_manual(frame)

anim_save("distribuicoes_simetria_assimetria.gif",
          animate(p, width = 1100, height = 540, fps = 30,
                  renderer = gifski_renderer(loop = TRUE)))
cat("GIF salvo em:", file.path(getwd(), "distribuicoes_simetria_assimetria.gif"), "\n")