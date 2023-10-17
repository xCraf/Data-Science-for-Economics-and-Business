#####dati------                                     Just click on RUN APP in right upper corner
rm(list = ls())
#library(shiny)
library(readxl)
library(MASS)
library(rstudioapi)
library(dplyr)
#current_path <- getActiveDocumentContext()$path 
#setwd(dirname(current_path ))
data1  <- read_excel("data.xlsx", sheet=3)
data2  <- read_excel("data.xlsx", sheet=2)
datoni <- merge(data1, data2, by = "Record ID")
datoni <- datoni[!duplicated(datoni$Isin), ]
datoni$SUB_INDUSTRY<-as.factor(datoni$SUB_INDUSTRY)
datoni <- datoni %>% filter(!(datoni$NET_SALES<0)) ## non può essere negativo
datoni <- datoni %>% filter(!(datoni$ENTERPRISE_VALUE<0)) ## non può essere troppo negativo (ne abbiamo tolti un paio in più)
datoni <- datoni[datoni$EBITDA > 0 & datoni$`RETURN_ON_ EQUITY` > 0 | datoni$EBITDA < 0 & datoni$`RETURN_ON_ EQUITY` < 0,]## se ebitda è negativo anche roe deve esserlo e viceversa
datoni <- datoni[datoni$EBITDA > 0 & datoni$EPS > 0 | datoni$EBITDA < 0 & datoni$EPS < 0,]## se ebitda è negativo eps non può essere positivo e viceversa

celentano<- datoni[,-c(1,2,3,4,6,8,9,10,11)]
colnames(celentano)[4]<-"EPS"
celentano <- celentano %>% filter(!(EPS>15)) 
celentano$EPS<- -(celentano$EPS)
celentano <- celentano %>% filter(!(EPS>15)) 
celentano$EPS<- -(celentano$EPS)
celentano <- celentano %>% filter(!(EBITDA>12.064000))
celentano <- celentano %>% filter(!(RETURN_ON_ASSET>25))
celentano$RETURN_ON_ASSET<- -(celentano$RETURN_ON_ASSET)
celentano <- celentano %>% filter(!(RETURN_ON_ASSET>45))
celentano$RETURN_ON_ASSET<- -(celentano$RETURN_ON_ASSET)
alpha <- 0.05


####APP----
ui <- fluidPage(
  titlePanel("Calcolatore EPS"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("input_ebitda", "EBITDA:", value = 0),
      numericInput("input_roa", "RETURN ON ASSET:", value = 0),
      selectInput("input_sub_industry", "SUB INDUSTRY:", choices = levels(celentano$SUB_INDUSTRY)),
      actionButton("submit_button", "Calcola EPS"),
    ),
    
    mainPanel(
      textOutput("output_eps"),
      textOutput("output_conf"),
      textOutput("output_confsup")
    )
  )
)


server <- function(input, output) {
  
  calculate_eps <- function() {
    ebitda <- input$input_ebitda
    roa <- input$input_roa
    sub_industry <- input$input_sub_industry
    filtered_data <- celentano %>% filter(SUB_INDUSTRY == sub_industry)
    reg_lin <- lm(EPS ~ EBITDA + RETURN_ON_ASSET, data = filtered_data)
    user_data <- data.frame(EBITDA = ebitda, RETURN_ON_ASSET = roa)
    eps <- predict(reg_lin, newdata = user_data)
    return(eps)
  }
  calculate_conf <- function() {
    ebitda <- input$input_ebitda
    roa <- input$input_roa
    sub_industry <- input$input_sub_industry
    filtered_data <- celentano %>% filter(SUB_INDUSTRY == sub_industry)
    reg_lin <- lm(EPS ~ EBITDA + RETURN_ON_ASSET, data = filtered_data)
    se <- summary(reg_lin)$sigma
    user_data <- data.frame(EBITDA = ebitda, RETURN_ON_ASSET = roa)
    qt_val <- qt(1 - alpha/2, df = reg_lin$df.residual)
    RO <- predict(reg_lin,user_data)
    RO <- RO - qt_val * se
    
    #RO <- predict(reg_lin, user_data, interval = "confidence", level = 0.95)
    #RO<- RO[2]
    return(RO)
  }
  calculate_confsup <- function() {
    ebitda <- input$input_ebitda
    roa <- input$input_roa
    sub_industry <- input$input_sub_industry
    filtered_data <- celentano %>% filter(SUB_INDUSTRY == sub_industry)
    reg_lin <- lm(EPS ~ EBITDA + RETURN_ON_ASSET, data = filtered_data)
    se <- summary(reg_lin)$sigma
    user_data <- data.frame(EBITDA = ebitda, RETURN_ON_ASSET = roa)
    qt_val <- qt(1 - alpha/2, df = reg_lin$df.residual)
    RO1 <- predict(reg_lin,user_data)
    RO1 <- RO1 + qt_val * se
    #RO1 <- predict(reg_lin, user_data, interval = "confidence", level = 0.95)
    #RO1<- RO1[3]
    return(RO1)
    
  }
  
  observeEvent(input$submit_button, {
    eps <- calculate_eps()
    conf<- calculate_conf()
    confsup<- calculate_confsup()
    output$output_eps <- renderText(paste("EPS calcolato:", eps))
    output$output_conf <- renderText(paste("Intervallo di Confidenza inf:", conf))
    output$output_confsup <- renderText(paste("Intervallo di Confidenza sup:", confsup))
  })
  
}
shinyApp(ui = ui, server = server)
