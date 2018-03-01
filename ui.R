library(tmap)
library(shinydashboard)
library(dplyr)
library(readr)
library(raster)
library(compiler)

#library("profvis")
source("function.R")

tmap_mode("plot")

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
    box(width = 15,
        title = "Animation", status = "primary", solidHeader = T,
        collapsible = TRUE,
        dateInput("date",
                  label = '',
                  #value = Sys.Date()
                  value = "2018-02-25"
        ),
        selectInput("Hour",'' ,
                    choices = list("00:00" = "00", "06:00" = "06",
                                   "12:00" = "12", "18:00" = "18"), selected = 1),
        actionButton("go", "Recall"),
        sliderInput("bins", "",
                    min = 0, max = 79,
                    value = 0, step = 1,
                    animate =
                      animationOptions(interval = 600, loop = T))
        
    )
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "dashboard1", h2(a1),
            box(width=2,title = "Legend", solidHeader = TRUE,
            collapsible = TRUE,
            imageOutput("legend_TMP")),
            box(uiOutput("UIPLOT_TMP"))),
    
    tabItem(tabName = "dashboard2", h2(a2),
            box(uiOutput("UIPLOT_TMAX"))),
    tabItem(tabName = "dashboard3", h2(a3),
            box(uiOutput("UIPLOT_TMIN"))),
    tabItem(tabName = "dashboard4", h2(a4),
            box(uiOutput("UIPLOT_DPT"))),
    tabItem(tabName = "dashboard5", h2(a5),
            box(uiOutput("UIPLOT_RH"))),
    tabItem(tabName = "dashboard6", h2(a6),
            box(uiOutput("UIPLOT_PRMSL"))),
    tabItem(tabName = "dashboard7", h2(a7),
            box(uiOutput("UIPLOT_GUST"))),
    tabItem(tabName = "dashboard8", h2(a8),
            box(uiOutput("UIPLOT_CAPE"))),
    
    
    
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
