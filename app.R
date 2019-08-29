library(shiny)
library(shinydashboard)
library(DT)
library(bnlearn)
library(tidyverse)
library(lubridate)


#----------------------
# Load and wrangle data
#----------------------
rasffData <- readRDS('shiny080819_data.rds')
d_tan <- rasffData %>% select(reference, 
                              month,
                              #classification, 
                              #type,
                              #riskDecision,
                              #distributionStatus,
                              origin_country,
                              notifyingCountry,  
                              dist_uk,
                              hazard, 
                              product_level1,
                              #product_level1,
                              #product_level2,
                              uk_rasff_soon)

#---------------
# load the model
#---------------
bn_tan_fit <- readRDS('bn_tan_fit_210819.rds')


#-------------------------------------------------
# Calculate probabilities of non-UK RASFFs in data
#-------------------------------------------------
rasff_data_display <- readRDS('rasff_data_display.rds')
rasff_data_display <- rasff_data_display %>%
  mutate(date=dmy(date))


#----------------
# Shiny interface
#----------------
ui <- navbarPage(titlePanel("Non-UK RASFFs"),
                # recent RASFFs table
                tabPanel('Recent RASFFs',
                         ###
                         # Google Analytics Code
                         ###
                         tags$head(HTML("<!-- Global site tag (gtag.js) - Google Analytics -->
                                <script async src='https://www.googletagmanager.com/gtag/js?id=UA-146519820-1'></script>
                                <script>
                                window.dataLayer = window.dataLayer || [];
                                function gtag(){dataLayer.push(arguments);}
                                gtag('js', new Date());
                                gtag('config', 'UA-146519820-1');
                                </script> ")), 
                         ###
                         # Goodle Analytics Code end
                         ###
                         fluidPage(
                           fluidRow(
                             column(12, div(dataTableOutput('recent_rasff_table')))
                           ) # end of fluidRow
                          ) # end of fluidPage
                         ), # end of tabPanel
                # query
                tabPanel('Query',
                  sidebarLayout(
                    sidebarPanel(
                         ## month
                         selectInput(inputId='month', 
                                     label='Month', 
                                     choices=c('NA', sort(unique(d_tan$month))),
                                     multiple=FALSE),
                         ## product
                         selectInput(inputId='product', 
                                     label='Product', 
                                     choices=c('NA', 
                                               sort(as.character(unique(d_tan$product_level1)))),
                                     multiple=FALSE),
                         ## hazard
                         selectInput(inputId='hazard', 
                                     label='Hazard', 
                                     choices=c('NA', 
                                               sort(as.character(unique(d_tan$hazard)))),
                                     multiple=FALSE),
                         ## notifying country
                         selectInput(inputId='notifying_country', 
                                     label='Notifying Country', 
                                     choices=c('NA', 
                                               sort(as.character(unique(d_tan$notifyingCountry)))),
                                     multiple=FALSE),
                         ## origin country
                         selectInput(inputId='origin_country', 
                                     label='Origin Country', 
                                     choices=c('NA', 
                                               sort(as.character(unique(d_tan$origin_country)))),
                                     multiple=FALSE),
                         ## distributed to the UK
                         selectInput(inputId ='distributed_to_uk',
                                     label='Distributed to the UK',
                                     choices=c('NA', 'No', 'Yes'),
                                     multiple=FALSE)
                      ), # end of sidebarPanel
                    mainPanel(
                         ## output for the server function
                         span(textOutput('query_info')), 
                                style='background-color:cornflowerblue;
                                       color:white;
                                       text-align: center;
                                       font-size:200%')
                      ) # end of mainPanel
                   ) # end of sidebarLayout
                 ) # end of tabPanel

server <- function(input, output){
  # table of RASFFs output
  output$recent_rasff_table <- renderDT(
    expr=rasff_data_display,
    class='display nowrap compact',
    colnames=c('Date', 'RASFF Reference',
               'Description', 'Probability of UK RASFF in following 28 days'),
    filter='top',
    options = list(scrollX = TRUE)
    )
  
  # sorting evidence for cpquery
  input_query <- reactive({
    q <- list(month=input$month,
              product_level1=input$product,
              hazard=input$hazard,
              origin_country=input$origin_country,
              notifyingCountry=input$notifying_country,
              dist_uk=input$distributed_to_uk
              )
    ev <- q[q!='NA']
  return(ev)
  })
  
  output$query_info <- renderText({
    evidence <- input_query()
      if(length(evidence>0)){
        paste0('The probability of a UK RASFF in the next 28 days is ',
          round(cpquery(
            fitted=bn_tan_fit,
            event=uk_rasff_soon=='1',
            evidence=evidence,
            method='lw',
            n=1e5)
          ,2) # end of round
        ) # end of paste}
      } # end of if(length(evidence>0....
    }) # end of render UI
  
  } # end of server

shinyApp(ui, server)
