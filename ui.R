# Author: Yubo Xiao
# Date: 11-Jul-2019 
# options(warn = -1)
# suppressWarnings(library(shiny))
# suppressWarnings(library(shinythemes))

library(shiny)
library(shinythemes)


shinyUI(
  fluidPage(
    theme = shinytheme("flatly"),
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "mystyle.css")),
    navbarPage(
      # title = ("Drug X PopPK Simulation"),
      title = strong(("Population-based approaches for dose selection:")),
      
      # second page: simulation -------------------------------------------------
      # tabPanel("模拟预测",
      tabPanel(strong("for 50% or 90% population ?"),
               #左栏
               fluidRow(
                 column(4,
                        style = "background: #ffffff",
                        # 给药方案的设定 ------------------------------------------
                        # titlePanel(h3(strong("给药方案"))),
                        titlePanel(h3(strong("Dosage regimen"))),
                        # hr(),
                        wellPanel(
                          style = "background: #ededed",
                          fluidRow(
                            column(4, numericInput("amt", h4(strong("Dose (mg)")), value = 120)),
                            column(4, numericInput("interval", h4(strong("Dose interval (h)")), value = 24)),
                            column(4, numericInput("n", h4(strong("Number of doses")), value = 24)),
                            tags$div(id = 'placeholder')
                          ),
                          # hr(),
                          # titlePanel(h5(strong("浓度范围 (mg/L)"))),
                          titlePanel(h4(strong("Observed time range (h)"))),
                          sliderInput("obs", label = NULL, min = 0, max = 168, step = 12, value = c(0,168)),
                          titlePanel(h4(strong("Theraputic concentration range (mg/L)"))),
                          sliderInput("mec", label = NULL, min = 0, max = 10, step = 1, value = c(1, 3))
                        ),
                        # 模型参数的设定 ----------------------------------------
                        # titlePanel(h3(strong("模型参数"))),
                        titlePanel(h3(strong("PopPK model parameters"))),
                        wellPanel(
                          style = "background: #ededed",
                          fluidRow(
                            column(6,numericInput("cl", "CL (L/h)", value = 2.7)),
                            column(6,numericInput("v", "V (L)", value = 100))
                          ),
                          fluidRow(
                            column(6,numericInput("ka", "Ka (/h)", value = 0.8)),
                            column(6,numericInput("f", "F ", value = 0.8))
                          ),
                          fluidRow(
                            column(4, numericInput("omega1", "BSV_CL", value = 0.2)),
                            column(4, numericInput("omega2", "BSV_V", value = 0.3)),
                            column(4, numericInput("omega3", "BSV_F", value = 0.3))
                          ),
                          h6("Notes: CL, clearance; V, volume of distribution; Ka, absorption constant; F, bioavailability; BSV, between subject variability.")
                        )
                 ),
                 column(8,
                        # 药时曲线呈现  ----------------------------------------
                        # titlePanel(h3(strong("药时曲线"))),
                        # titlePanel(h3(strong("Conc-Time Curve"))),
                        br(),
                        br(),
                        br(),
                        # absolutePanel(
                        #   top = 6, right = 580, width = 160, height = 10, draggable = FALSE,
                        #   checkboxGroupInput("res", "",
                        #                      choices = c("Average", "Pooled"),
                        #                      selected = "Average", inline = TRUE)
                        # ),
                        # conditionalPanel(condition = "input.res == 'Average'",
                        #                  plotOutput(outputId = "idvpkplot")),
                        # conditionalPanel(condition = "input.res == 'Pooled'",
                        #                  plotOutput(outputId = "poppkplot"))
                        plotOutput(outputId = "idvpkplot"),
                        plotOutput(outputId = "poppkplot")
                 )
               ),
               
               # absolutePanel(
               #   top = 160, right = 295, width = 335, height = 10, draggable = FALSE,
               #   titlePanel(h4(strong(textOutput(outputId = "proportion1", inline = T))))
               # ),
               absolutePanel(
                 top = 170, right = 60, width = 100, height = 10, draggable = TRUE,
                 img(src="LOGOdMed.png", height = 50)
               ),
               
               # absolutePanel(
               #   top = 560, right = 295, width = 335, height = 10, draggable = FALSE,
               #   titlePanel(h4(strong(textOutput(outputId = "proportion2", inline = T))))
               # ),
               absolutePanel(
                 top = 570, right = 60, width = 100, height = 10, draggable = TRUE,
                 img(src="LOGOdMed.png", height = 50)
               ),
               
               absolutePanel(
                 top = 90, right = 445, width = 160, height = 10, draggable = FALSE,
                 titlePanel(h4(strong("Subject")))
               ),
               absolutePanel(
                 top = 90, right = 370, width = 160, height = 10, draggable = FALSE,
                 sliderInput("idv", label = NULL, min = 0, max = 500, step = 50, value = 50)
               ),
               
               absolutePanel(
                 top = 90, right = 175, width = 160, height = 10, draggable = FALSE,
                 titlePanel(h4(strong("Propotion")))
               ),
               absolutePanel(
                 top = 90, right = 80, width = 160, height = 10, draggable = FALSE,
                 sliderInput("p", label = NULL, min = 0, max = 1, step = 0.05, value = c(0.05,0.95))
               )
               # ,
               # absolutePanel(
               #   top = 90, left = 280, width = 300, height = 10, draggable = FALSE,
               #   img(src="LOGOdMed.png", height = 50)
               # )
      ),
      # img(src="2sdh.png",height = 55, width = 225)
      # <code style='color:#ec4c3c;background-color:#F8F9F9'>
      footer = h5(HTML("dMed Copyright 2020 : 
                       <strong style='color:#ec4c3c;background-color:#F8F9F9'> E </strong>
                       arly <strong style='color:#ec4c3c;background-color:#F8F9F9'> D </strong> evelepment and 
                       <strong style='color:#ec4c3c;background-color:#F8F9F9'> C </strong> linical 
                       <strong style='color:#ec4c3c;background-color:#F8F9F9'> P </strong>harmacology"), align = "right")
    )
  )
)

