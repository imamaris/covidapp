library(shiny)
library(DT)
library(ggplot2)
library(shinythemes)
# source our helper script into our file
source("scripts/reader.R")
source("scripts/preprocess.R")

corona <- read_github()
corona_latest <- subset(corona, date == max(corona$date))
corona_latest$date <- as.character(corona_latest$date)
melted_latest <- melt_all(corona_latest)
options(scipen = 9999)

# navigational bar page
ui <- navbarPage(title="Covid-19 Global Response",
                 #tags$head(tags$link(rel="shortcut icon", href="favicon.ico"))
                 #find more theme more on https://rstudio.github.io/shinythemes/
                 theme = shinytheme("superhero"),
                 tabPanel("Global",
                          sidebarLayout(mainPanel(
                            fluidRow(
                              column(4, 
                                     h5("Total Cases"),
                                     wellPanel(
                                       h3(textOutput("totalCases"))
                                     )),
                              column(4, 
                                     h5("Total Recovered"),
                                     wellPanel(
                                       h3(textOutput("totalRecovered"))
                                     )),
                              column(4, 
                                     h5("Total Deaths"),
                                     wellPanel(
                                       h3(textOutput("totalDeaths"))
                                     ))
                            ),
                            plotOutput("globalPlot")
                          ), 
                          sidebarPanel(h2("Covid-19 Pandemic"),
                                       img(src="logo.png", width=140),
                                       helpText(
                                         textOutput("dateToday"),
                                         "Data source: COVID-19 Data Repository by the Center for Systems Science and Engineering (CSSE) at Johns Hopkins University. View the full list of data sources in Credits."),
                                       radioButtons("plotType",
                                                    label="Plot type",
                                                    choices=c("Bars", "Points")
                                       )
                          ))
                 ),
                 tabPanel("Latest Data",
                          dataTableOutput("todayTable")
                 ),
                 tabPanel("Predictive"),
                 tabPanel("Donation")
)


server <- function(input, output){
  
  output$totalCases <- renderText({
    prettyNum(sum(corona_latest$confirmed), big.mark = ",")
  })
  
  output$totalRecovered <- renderText({
    prettyNum(sum(corona_latest$recovered), big.mark = ",")
  })
  
  output$totalDeaths <- renderText({
    prettyNum(sum(corona_latest$deaths), big.mark = ",")
  })
  
  output$todayTable <- renderDataTable({
    corona_latest
  })
  
  output$dateToday <- renderText({
    paste("Updated as of: ", as.character(Sys.Date()))
  })
  
  output$globalPlot <- renderPlot({
    # pull the countries that are among the top 20 by confirmed column
    cntToPlot <- corona_latest %>% top_n(20, confirmed) %>% pull(country)    
    # subset data to only use a portion of the dataframe
    dat <- subset(melted_latest, country %in% cntToPlot)
    
    # geoms <- ifelse(as.character(input$plotType) == "Bars", geom_col, geom_point)
    geoms <- switch(input$plotType,
                    "Bars" = geom_col(),
                    "Points"=geom_point(aes(size=value)))
    
    ggplot(data=dat, aes(x=value, y=country, fill=variable, col=variable)) +
      geoms +
      labs(
        x="",
        y=""
      ) +
      theme_minimal() +
      theme(
        panel.background = element_blank(),
        plot.background = element_blank()
      )
  })
  
}


shinyApp(ui=ui, server=server)









