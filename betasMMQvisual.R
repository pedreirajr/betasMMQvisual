library(shiny)
library(ggplot2)

#(0) Creating the toy dataset
set.seed(0)
x <- seq(0,100,length.out = 30)
y <- 0.5*x + runif(30,-25,25) + 25
coef <- lm(y ~ x)$coef
df <- as.data.frame(x = x, y = y)

#(1) Creating the range of values of b0 and b1 to be chosen
b0s <- seq(0,50,1)
b1s <- seq(-1,1,.1)
(bs <- expand.grid(b0s, b1s))
names(bs) <- c('b0s','b1s')
bs$ssr_bs <- NA

for (i in 1:nrow(bs)){
  bs[i,'ssr_bs'] <- round(sum((y - (bs[i,'b1s']*x + bs[i,'b0s']))^2),0)
}

# App:
ui <- fluidPage(
  theme = shinythemes::shinytheme('simplex'),
  withMathJax(),
  titlePanel('Você consegue estimar visualmente os valores de \\(\\hat\\beta_0\\) and \\(\\hat\\beta_1\\)?'),

  sidebarLayout(
    
    sidebarPanel(
      sliderInput('b0',withMathJax('Escolha o valor de \\(\\hat\\beta_0\\): '),
                  min(b0s),max(b0s),mean(y),1),
      sliderInput('b1',withMathJax('Escolha o valor de \\(\\hat\\beta_1\\): '),
                  min(b1s),max(b1s),0,0.1)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Reta com o seu 'chute'",
          plotOutput('regplot')#,
          # tags$h4('O quão longe você está dos coeficientes estimados por Mínimos Quadrados?'),
          # uiOutput('errb0'),
          # uiOutput('errb1'),
        ),
        tabPanel('Resíduos',
          plotOutput('regplotres'),
          uiOutput('ssr_pan1')
        ),
        tabPanel('Heatmap da Soma dos Quadrados dos Resíduos',
          plotOutput('bsheatmap'),
          uiOutput('ssr_pan2')
        )
      )
    )
  )
)
server <- function(input, output, session){
  baseplot <- reactive(ggplot(df, aes(x = x, y = y)) +
                         geom_point() +
                         geom_abline(intercept = input$b0, slope = input$b1,
                                     color = 'red', linewidth = 1.2) +
                         ylim(0,max(y)) +
                         theme(axis.title.x = element_text(size = 14, face = 'bold'),
                               axis.title.y = element_text(size = 14, face = 'bold'),
                               axis.text.x = element_text(size = 14),
                               axis.text.y = element_text(size = 14))
                       )
  
  distb0 <- reactive({round(input$b0 - coef[1], 2)})
  
  distb1 <- reactive({round(input$b1 - coef[2], 2)})
  
  ssr_calc <- reactive({round(sum((y - (input$b1*x + input$b0))^2),0)}) 
  
  output$regplot <- renderPlot({
      baseplot()
    })
  
  
  output$regplotres <- renderPlot({
    baseplot() +
      geom_segment(aes(x = x , y = y, xend = x, yend = input$b1*x + input$b0),
                   linetype='dashed')
  })
  
  output$bsheatmap <- renderPlot({
    ggplot(bs, aes(b0s, b1s, fill = ssr_bs)) + 
      geom_tile() +
      geom_point(x = input$b0, y = input$b1, size = 5) +
      scale_fill_distiller(name="SQR", type = "div", palette = 'Spectral', trans = 'log10') + 
      xlab(expression(paste(hat(beta)[0], ' escolhido'))) +
      ylab(expression(paste(hat(beta)[1], ' escolhido'))) +
      theme(axis.title.x = element_text(size = 14, face = 'bold'),
            axis.title.y = element_text(size = 14, face = 'bold'),
            axis.text.x = element_text(size = 14),
            axis.text.y = element_text(size = 14),
            legend.text = element_text(size = 13),
            legend.title = element_text(size = 14))
    })
  
  output$errb0 <- renderUI({
    withMathJax("Distância de \\(\\hat\\beta_0\\) = ", 
                ifelse(distb0()==0,distb0(),
                       ifelse(distb0()>0,paste("+", abs(distb0())),paste("-", abs(distb0())))))
  })
  output$errb1 <- renderUI({
    withMathJax("Distância de \\(\\hat\\beta_1\\) = ", 
                ifelse(distb1()==0,distb1(),
                       ifelse(distb1()>0,paste("+", abs(distb1())),paste("-", abs(distb1())))))
  })
  
  output$ssr_pan1 <- renderUI({
    withMathJax("Soma dos Quadrados dos Resíduos (SQR) = ", ssr_calc())
  })
  
  output$ssr_pan2 <- renderUI({
    withMathJax("Soma dos Quadrados dos Resíduos (SQR) = ", ssr_calc())
  })
  
}

shinyApp(ui = ui, server = server)
