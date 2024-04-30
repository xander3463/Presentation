library(shiny)
library(shinydashboard)
library(tidyverse)
library(DT)
library(shinycssloaders)
library(ggplotlyExtra)
library(sp)
library(sf)
library(ggplot2)
library(viridis)
library(patchwork)
library(ggnewscale)
library(plotly)

load("Modified_data.RData")

# Define UI for application that draws a histogram
ui <- function(request){
  dashboardPage(
    skin = "purple",
  dashboardHeader(
    title =span(img(src="Blend.png",width=200,height=40,alt="Blend"))
  ),
  ##in header the logo of the United Nations   the logo of the University of Strathclyde
  dashboardSidebar(
    sidebarMenu(
      id='side_bar', 
      menuItem(text="Introduction",
               tabName="Introduction",selected=TRUE),
      menuItem(text = "The data",
               tabName = "The_data"),
      menuItem(text="Analysis",
               tabName="Analysis"),
      menuItem(text="Machine learning model",
               tabName="ML"),
      menuItem(text="Conclusions",
               tabName="Conclusions"))
  ),

        # Show a plot of the generated distribution
    dashboardBody(
      tags$head(
        tags$title("Analysis of machine performance"),
        tags$style(
          HTML(
            ".content-wrapper {
        background-color: #CBC3E3;
      }
      .skin-green .main-sidebar {
        background-color: #CBC3E3;
      }
      .skin-green .main-header .logo{
        color: #CBC3E3;
      }
      .shiny-output-error-validation {
        color: #946d6d;
        font-weight: bold;
      }
      .skin-purple .main-header .navbar .nav>li>a {
    color: #007fb0;                                      ##########warning triangle
      }
      .skin-green .main-header .navbar .sidebar-toggle {
        color: #CBC3E3;
      }"
          )
        )
      ),
     # use_tota11y(),
      tags$html(lang="en"),
      tags$head(tags$title("Analysis of machine performance")),
      tabItems(
        tabItem(
####Introduction####          
          tabName = "Introduction",
          tags$h1("Introduction"),
          tags$br(),
          fluidRow(column(img(src="Intro_Pic.PNG",width=600, style = "display: block; margin-left: auto; margin-right: auto;"),width = 8),
                   column(div(
                     htmlOutput("Intro"),
                     style = "font-size: 30px;"
                   ),width = 4))),


####The data####
          tabItem(
            tabName = "The_data",
            tags$h1("The data"),
            tags$br(),
            fluidRow(tags$h2("Taget"),div(
              htmlOutput("myList0"),
              style = "font-size: 20px;"
            )),
            fluidRow(tags$h2("Machine Type"),div(
              htmlOutput("myList"),
              style = "font-size: 20px;"
            )),
            fluidRow(tags$h2("Rotation Speed"),div(
              htmlOutput("myList2"),
              style = "font-size: 20px;"
            )),
            fluidRow(tags$h2("Time Stamp"),div(
              htmlOutput("myList3"),
              style = "font-size: 20px;"
            )),
            fluidRow(tags$h2("Air temperature"),div(
              htmlOutput("myList4"),
              style = "font-size: 20px;"
            ))
          ),
####Analysis####
          tabItem(
            tabName = "Analysis",
            tags$h1("Analysis"), 
            tags$br(),
            fluidRow(tags$h2("Time of day"),
                     column(selectInput(inputId = "hist_fail_time",label="Choose a type of failure",multiple = F,
                                        choices = unique(data$Failure.Type)),
                            withSpinner(plotOutput("Hist1"),image = "data.gif", image.width = 90),width=9)
              , column(tags$br(),tags$br(),tags$br(),tags$br(),tags$br(),
                       div(
                         htmlOutput("An1"),
                         style = "font-size: 30px;"
                       ),width = 3)),
            fluidRow(tags$h2("Machine type"),
                     column(withSpinner(plotlyOutput("Analysis2"),image = "data.gif", image.width = 90),width=9),
                     column(tags$br(),tags$br(),tags$br(),tags$br(),tags$br(),
                            div(
                              htmlOutput("An2"),
                              style = "font-size: 30px;"
                            ),width = 3)),
            fluidRow(tags$h2("Other statistically significant variables"),div(
              htmlOutput("An3"),
              style = "font-size: 30px;"
            )),
            fluidRow(tags$h2("Target"),div(
              htmlOutput("An4"),
              style = "font-size: 30px;"
            ))
            ),
          
####Machine learning####
            tabItem(
              tabName = "ML",
                    tags$h1("Machine learning model"), 
                    tags$br(),
                    fluidRow(column(img(src="NN.PNG",width=700),width = 6),
                             column(img(src="train_test.PNG",width=700),width = 6)),
                    fluidRow(div(
                      htmlOutput("ML"),
                      style = "font-size: 30px;"
                    ))),
####Conclusion####

        tabItem(tabName = "Conclusions",
                tags$h1("Conclusions"), 
                tags$br(),
                fluidRow(column(img(src="Conc_Pic.PNG",width=600, style = "display: block; margin-left: auto; margin-right: auto;"),width = 8),
                         column(div(
                           htmlOutput("Conc"),
                           style = "font-size: 30px;"
                         ),width = 4)),width=12)
      )
))}

####Server####
# Define server logic required to draw a histogram
server <- function(input, output,session) {

  
####Introduction####
  output$Intro<-renderUI(HTML("<ul><li>Features of the data</li><li>
                                      Analysis of the data and variables</li><li>
                                      Construction of neural network to predict failures
                              </li></ul>"))
 
####The data####

  output$myList0 <- renderUI(HTML("<ul><li>Apears to be a prediction of whether the machine will fail or not (97% accuracy)</li><li>
                              Removing this from statistical models has a large impact on the statisticall significance of other variables
                              </li></ul>"))
  
  output$myList <- renderUI(HTML("<ul><li>Could be miss coded</li><li>Through a Chi squared test statistically significant differences were found</li><li> 
                                 Could be mistakes are more likley to happen with machines when data is more likely to be miss coded or different machines</li></ul>"))
  output$myList2 <- renderUI(HTML("<ul><li>Missing values</li><li>MAR, MCAR or MNAR missingness</li><li> 
                                  Cannot impute with MICE or single value imputation without knowing why data is missing</li><li>
                                  High and statistically significant corelation between Rotation speed and torque so if missingness is MAR then mice will most likely be effective</li></ul>"))
  output$myList3 <- renderUI(HTML("<ul><li>Extreme outlier dates</li><li>All outliers exactly 10 years different than what would be expected</li><li>
                                  Recoded</li></ul>"))
  output$myList4 <- renderUI(HTML("<ul><li>New column created representing difference in temperature</li></ul>"))
  
  
#discuss data/ data modifications  

  
####Analyse#### 

  output$Hist1 <- renderPlot({
    validate(need(input$hist_fail_time,"Please select a failure type"))
    data%>%
       filter(Failure.Type%in%c(input$hist_fail_time))->dataA2
    dataA2<-as.numeric(dataA2$time)
    
    Hist1p<-hist(dataA2,breaks = 0:23)
    
    plot(Hist1p, main = "Histogram of Time",
         xlab = "Time of day", ylab = "Frequency", 
         col = "skyblue", border = "black")
  })
  
  output$An1 <- renderUI(HTML("<ul><li>Time of day has an effect, backed up by logistic modelling</li><li>Some ocolation especially in Heat dispersion, could indicate deteriating condition after a fix</li><li>
                                  Breakdowns ocurr half as often at midnight</li><li>less likely to breakdown at 4am</li></ul>"))
  output$Analysis2<-renderPlotly({
    failed_proportion <- data %>%
      group_by(Machine.Type) %>%
      summarize(proportion_failed = mean(fail == 1))
    
    plot_an_1<-ggplot(failed_proportion, aes(x = Machine.Type, y = proportion_failed)) +
      geom_bar(stat = "identity", fill = "skyblue", color = "black") +
      labs(x = "Machine Type", y = "Proportion Failed", title = "Proportion of Failures by Machine Type")
    ggplotly(plot_an_1, tooltip = c("text"))
    })
  
  output$An2 <- renderUI(HTML("<ul><li>Statistically significant difference between machine types</li><li>
                              Different machines or misscoding?</li></ul>"))
  output$An3 <- renderUI(HTML("<ul><li>Other variables were found to increase the likelyhood of a failure: torque and tool wear</li><li>
                              variables were found to decrease the likelyhood of a failure: time from start date; rotation speed and temperature difference  </li></ul>"))
  output$An4 <- renderUI(HTML("<ul><li>Apears to be prediction with Error not being classed as a failure</li><li>
                              Models including this prediction can show what variability the prediction is missing</li><li>
                              Prediction leaves room for variability to be explained by: torque; machine type and some time of the day</li><li>
                              The effect size of the missing variability incredably low</li></ul>"))

####Machine learning model####
  output$ML<-renderUI(HTML("<ul><li>Testing accuracy of 94%, 3% less than target variable
                              </li></ul>"))
####Conclusion####
  
  output$Conc<-renderText(HTML("<ul><li>Potential misscoding and missing data.
                                </li><li>Independant variables have statistically significance corelation with change of machine failure.
                                </li><li>Machine learning model wasn't as accurate as target variable</li></ul>"))
  
}
  

##### Run the application #####
  
shinyApp(ui = ui, server = server,enableBookmarking = "url")
