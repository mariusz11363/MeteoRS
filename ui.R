library(tmap)
library(shinydashboard)
library(dplyr)
library(readr)
library(raster)
library(compiler)
library("efficient")
height <<- 650
width <<- 650
library("profvis")
enableJIT(3)
tmap_mode("plot")

source("function.R")



if(exists("dane")==FALSE){
  dane <<- NULL
  dane <<- list()
}


a1 <- "Temperature"
a2 <- "Temp. max"
a3 <- "Temp. min"
a4 <- "Dew point temp."
a5 <- "Relative humidity"
a6 <- "Pressure"
a7 <- "Wind speed (GUST)"
a8 <- "CAPE"


sidebar <- dashboardSidebar(

  
  
  sidebarMenu(
    menuItem(a1, tabName = "dashboard1", icon = icon("dashboard")),
    menuItem(a2, tabName = "dashboard2", icon = icon("dashboard")),
    menuItem(a3, tabName = "dashboard3", icon = icon("dashboard")),
    menuItem(a4, tabName = "dashboard4", icon = icon("dashboard")),
    menuItem(a5, tabName = "dashboard5", icon = icon("dashboard")),
    menuItem(a6, tabName = "dashboard6", icon = icon("dashboard")),
    menuItem(a7, tabName = "dashboard7", icon = icon("dashboard")),
    menuItem(a8, tabName = "dashboard8", icon = icon("dashboard")),
    menuItem("Chart", icon = icon("th"), tabName = "widgets",
             badgeLabel = "new", badgeColor = "green"),
    box(width = 15,background = "black",
        title = "Animation",
        fluidRow(
        column(8, dateInput("date",
                  label = 'Date',
                  value = Sys.Date()
                  #value = "2018-02-25"
        )),
        column(3, selectInput("Hour",'' ,
                    choices = list("0" = "00", "6" = "06",
                                   "12" = "12", "18" = "18"), selected = 1))),
        actionButton("go", "Recall"),
        sliderInput("bins", "",
                    min = 0, max = 72,
                    value = 0, step = 1,
                    animate =
                      animationOptions(interval = 500, loop = T))
        
    )
  )
)

body <- dashboardBody(

  
  tabItems(
    tabItem(tabName = "dashboard1", "",
            tabBox(title = "Temperature",id = "dashboard1",
                   tabPanel("Maps",
                            fluidRow(uiOutput("UIPLOT_TMP")),
                            fluidRow(plotOutput("legend_TMP"))),
                   tabPanel("Profile","Tymczasowy")
                   )),
    
    tabItem(tabName = "dashboard2", "",
            tabBox(title = "Temp. max",id = "dashboard2",
            tabPanel("Maps",
                     fluidRow(uiOutput("UIPLOT_TMAX")),
                     fluidRow(plotOutput("legend_TMAX"))))),
    tabItem(tabName = "dashboard3", "",
            tabBox(title = "Temp. min",id = "dashboard3",
                   tabPanel("Maps",
                            fluidRow(uiOutput("UIPLOT_TMIN")),
                            fluidRow(plotOutput("legend_TMIN"))))),
    tabItem(tabName = "dashboard4", "",
            tabBox(title = "Dew point",id = "dashboard4",
                   tabPanel("Maps",
                            fluidRow(uiOutput("UIPLOT_DPT")),
                            fluidRow(plotOutput("legend_DPT"))))),
   tabItem(tabName = "dashboard5", "",
            tabBox(title = "Relative hum.",id = "dashboard5",
                   tabPanel("Maps",
                            fluidRow(uiOutput("UIPLOT_RH")),
                            fluidRow(plotOutput("legend_RH"))))),
   tabItem(tabName = "dashboard6", "",
           tabBox(title = "Press. see lvl",id = "dashboard6",
                  tabPanel("Maps",uiOutput("UIPLOT_PRMSL")))),
   tabItem(tabName = "dashboard7", "",
           tabBox(title = "Wind (gust)",id = "dashboard7",
                  tabPanel("Maps",uiOutput("UIPLOT_GUST")))),
    tabItem(tabName = "dashboard8", "",
          tabBox(title = "CAPE",id = "dashboard8",
                 tabPanel("Maps",uiOutput("UIPLOT_CAPE")))),
    
    
    
    tabItem(tabName = "widgets",
            h2("Chart"),
            tabBox(
              title = "Chart Box",
              # The id lets us use input$tabset1 on the server to find the current tab
              id = "tabset1", height = "550px",
              tabPanel("Temp",
                       plotOutput("widget_temp"))#,
              #tabPanel("Cape",
              #  plotOutput("widget_cape"))
            )
            
    )
  )
)

# Put them together into a dashboardPage
dashboardPage(
  
  
  dashboardHeader(title = "Meteo",
                  
                  
                  dropdownMenuOutput("messageMenu"),
                  dropdownMenu(type = "tasks", badgeStatus = "success",
                               taskItem(value = 90, color = "green",
                                        "Documentation"
                               ),
                               taskItem(value = 17, color = "aqua",
                                        "Project X"
                               ),
                               taskItem(value = 75, color = "yellow",
                                        "Server deployment"
                               ),
                               taskItem(value = 80, color = "red",
                                        "Overall project"
                               )
                  ),
                  dropdownMenu(type = "notifications",
                               notificationItem(
                                 text = "5 new users today",
                                 icon("users")
                               ),
                               notificationItem(
                                 text = "12 items delivered",
                                 icon("truck"),
                                 status = "success"
                               ),
                               notificationItem(
                                 text = "Server load at 86%",
                                 icon = icon("exclamation-triangle"),
                                 status = "warning"
                               )
                  )
  ),
  sidebar,
  body
)
