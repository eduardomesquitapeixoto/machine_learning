#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

set.seed(123)
load("dataset_cardio.RData")
load("ACM1.RData")
load("ACM2.RData")
load("ACM3.RData")
load("Graficoad.RData")
load("KM.RData")
load("cancelada.RData")
load("TABELA_KM.RData")
load("tabelanb.RData")



library(shiny)
library(caret)
library(Amelia)
library(pROC)
library(mgcv)
library(neuralnet)
library(rpart.plot)
library(caret)
require(FactoMineR)
require(dplyr)
require(flextable)
require(factoextra)
require(shinyWidgets)
library(shinythemes)


# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("sandstone"),
                HTML('<center><img src="https://i0.wp.com/blogdoberta.com/wp-content/uploads/2020/04/sindicancia_capa.png?fit=840%2C239&ssl=1" width="400"></center>'),

    # Application title
    titlePanel("ENTENDENDO PADRÕES EM BANCOS DE DADOS DE REGULAÇÃO"),

        # Show a plot of the generated distribution
        mainPanel(
          
          selectInput("modelo","Selecione o modelo de exploração",choices = c("AD","KMODE","NB","ACM")),
           
           h3("Exploração de dados"),
          conditionalPanel( condition = "input.modelo == 'AD'",
          h3("Árvore de Decisão"),
          h5("usa profundidade máxima (rpart2)"),
           plotOutput("AD")),
          
          conditionalPanel(condition = "input.modelo == 'KMODE'",
                           h3("Modelo K-MODE"),
                           h5("Utiliza clusterização via Moda, similar ao Kmeans ou Kmedians, mas utiliza a moda como parâmetro sendo adequado para variáveis qualitativas também"),
                           tableOutput("texto")
                           
                           ),
          
          conditionalPanel(condition = "input.modelo == 'ACM'",
                           h3("Análise de Correspondência Múltipla"),
                           h5("Similar a uma análise de componentes principais, mas para variáveis qualitativas"),
                           h6("ACM Por Variável"),
                           
                           plotOutput("ACM1"),
                           h6("ACM Por fator"),
                           
                           plotOutput("ACM2"),
                           
                           h6("Cosseno quadrático (ver a contribuição de cada fator, interessante para categorizar)"),
                           
                           plotOutput("ACM3")
                           
          ),
          
          conditionalPanel(condition = "input.modelo == 'NB'",
                           h3("Naive Bayes"),
                           
                           h5("usa a frequência do NB como dimensão de freq uma nuvem de palavras para canceladas"),
        
                           plotOutput("cancelada"),
                           
                           h5("Tabela de Valores Naive Bayes"),
                           
                           tableOutput("try")
          )
          
          
        )
    )


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$texto<-  renderUI(tabelakm)

    output$AD <- renderPlot({
      rpart.plot::rpart.plot(dtFit$finalModel,  
                 extra = 4, # informações extras nos nós
                 type = 4,  # tipo de gráfico
                 box.palette = "RdYlGn") # cor
      
    
    })
    
    output$nb<-renderTable(nb)
    
    output$try<-renderUI(tabelanb)

    output$cancelada<-renderPlot(wordcloud::wordcloud(cancelada$Variavel,cancelada$Peso/2))
    
    output$ACM1<-renderPlot(acm1)
    output$ACM2<-renderPlot(acm2)
    output$ACM3<-renderPlot(acm3)
    
    
    }

# Run the application 
shinyApp(ui = ui, server = server)
