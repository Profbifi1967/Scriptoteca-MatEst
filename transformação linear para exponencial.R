# Instale os pacotes uma única vez, se necessário:
# install.packages(c("shiny", "ggplot2", "gganimate", "plotly", "transformr"))
#instalação
install.packages("plotly")
#pacotes
library(shiny)
library(ggplot2)
library(gganimate)
library(plotly)

# Função para simular dados com transição gradual do linear para o exponencial
simular_dados <- function(n = 100, transicao = 0.3, intensidade = 2) {
  x <- seq(1, 10000, length.out = n)
  # alpha controla a mistura linear-exponencial
  alpha <- seq(0, 1, length.out = n)
  alpha <- ifelse(alpha < transicao, 0, (alpha - transicao)/(1-transicao))
  
  y_linear <- 0.08 * x + rnorm(n, 0, 200)
  y_exp <- 0.000015 * (x ^ intensidade)
  y <- (1 - alpha) * y_linear + alpha * y_exp
  data.frame(
    Investimento = x,
    Vendas = pmax(y, 0),
    Progresso = round(alpha * 100),
    Fase = ifelse(alpha == 0, "Linear", ifelse(alpha == 1, "Exponencial", "Transição"))
  )
}

# UI do Shiny
ui <- fluidPage(
  titlePanel("Evolução da Relação entre Investimento e Vendas: Linear para Exponencial"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("transicao", "Momento de início da transição (percentual):", min = 0.05, max = 0.8, value = 0.3, step = 0.05),
      sliderInput("intensidade", "Intensidade do crescimento exponencial:", min = 1.2, max = 3, value = 2, step = 0.1),
      actionButton("animar", "Animar evolução"),
      helpText("Ajuste o tempo de transição e a intensidade do crescimento. Ao clicar em 'Animar evolução', observe a dispersão mudar de linear para exponencial.")
    ),
    mainPanel(
      plotlyOutput("plot_interativo"),
      br(),
      conditionalPanel("output.show_anim",
                       imageOutput("animacao", height = "300px")
      ),
      hr(),
      htmlOutput("explicacao")
    )
  )
)

# Server do Shiny
server <- function(input, output, session) {
  # Dados reativos
  dados <- reactive({
    simular_dados(transicao = input$transicao, intensidade = input$intensidade)
  })
  
  # Gráfico interativo dinâmico
  output$plot_interativo <- renderPlotly({
    df <- dados()
    p <- ggplot(df, aes(x = Investimento, y = Vendas, color = Fase)) +
      geom_point(size = 2, alpha = 0.7) +
      geom_smooth(method = "loess", se = FALSE, color = "black") +
      scale_color_manual(values = c("Linear" = "#1F77B4", "Transição" = "#FF7F0E", "Exponencial" = "#2CA02C")) +
      labs(
        title = "Mudança na relação entre Investimento e Vendas",
        subtitle = "Ajuste os controles para simular diferentes cenários!",
        x = "Investimento em Marketing (R$)", y = "Vendas (R$)",
        color = "Estágio"
      ) +
      theme_minimal(base_size = 15)
    ggplotly(p)
  })
  
  # Controle para mostrar animação
  output$show_anim <- reactive({
    input$animar %% 2 == 1
  })
  outputOptions(output, "show_anim", suspendWhenHidden = FALSE)
  
  # Animação gerada sob demanda
  output$animacao <- renderImage({
    req(input$animar)
    progresso <- seq(0, 1, length.out = 25)
    frames <- do.call(rbind, lapply(progresso, function(alpha) {
      n <- 100
      x <- seq(1, 10000, length.out = n)
      y_linear <- 0.08 * x + rnorm(n, 0, 200)
      y_exp <- 0.000015 * (x ^ input$intensidade)
      y <- (1 - alpha) * y_linear + alpha * y_exp
      data.frame(
        Investimento = x,
        Vendas = pmax(y, 0),
        Alpha = alpha,
        Fase = ifelse(alpha == 0, "Linear", ifelse(alpha == 1, "Exponencial", "Transição")),
        Progresso = round(alpha * 100)
      )
    }))
    p_anim <- ggplot(frames, aes(x = Investimento, y = Vendas, color = Fase)) +
      geom_point(size = 2, alpha = 0.7) +
      geom_smooth(method = "loess", se = FALSE, color = "black") +
      scale_color_manual(values = c("Linear" = "#1F77B4", "Transição" = "#FF7F0E", "Exponencial" = "#2CA02C")) +
      labs(
        title = "Evolução do padrão de vendas: de Linear a Exponencial",
        subtitle = "Transição: {round(frame_time*100)}%",
        x = "Investimento em Marketing (R$)", y = "Vendas (R$)",
        color = "Estágio"
      ) +
      theme_minimal(base_size = 15) +
      transition_time(Alpha) +
      ease_aes('cubic-in-out')
    anim_file <- tempfile(fileext = ".gif")
    animate(p_anim, duration = 4, width = 600, height = 300, renderer = gifski_renderer(), fps = 10, output = anim_file)
    list(src = anim_file, contentType = "image/gif")
  }, deleteFile = TRUE)
  
  # Explicação didática no dashboard
  output$explicacao <- renderUI({
    HTML(paste0("<b>Como usar:</b> Ajuste o 'Momento de Transição' para controlar quando o padrão linear começa a mudar para exponencial. Use o controle de intensidade para modificar o quão vigoroso será o crescimento exponencial. Experimente clicar em 'Animar evolução' e observe 'ao vivo' como mudanças de mercado e campanhas virais podem alterar drasticamente o padrão de vendas — visualizando, com animação, do linear ao exponencial. Perfeito para apresentações e análise interativa no LinkedIn ou reuniões!"))
  })
}

# Execute o aplicativo
shinyApp(ui, server)