sys.wersion <- "1.0beta"

#speed R
library(pryr)
#library(gpuR)
library("bigmemory")
library("ff")
############

library(tmap)
library(shinydashboard)
library(dplyr)
library(readr)
library(raster)
library(compiler)#jit

#contour
library(maptools)
library(akima)
#####
library("efficient")
height <<- 550
width <<- 550
library("profvis")

tmap_mode("plot")

source("function.R")


a0 <- "Introduction"
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
    menuItem(a0, tabName = "dashboard0", icon = icon("book")),
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
                    min = 1, max = 72,
                    value = 0, step = 1,
                    animate =
                      animationOptions(interval = 700, loop = T))
        
    )
  )
)

body <- dashboardBody(

  
  tabItems(
    tabItem(tabName = "dashboard0", "",
            fluidRow(
           column(6, box(
              title = "Introducion", width = NULL, solidHeader = TRUE,
              "
MeteoRS is an integrated meteorological information system. It gives you the opportunity to access weather forecasts, as well as various charts. Interactive system operation, facilitates work on data. MeteoRS is an advanced system that is constantly being developed."
            )),
            column(6,box(
              title = "Warning", width = NULL, background = "yellow",
              "
The system calculates data on the fly, it can affect the smooth operation of the system. In case of problems, please contact us at: x@x.com"
            ))),
           fluidRow(
             column(6,box(style="margin-left: 16px;margin-right: 16px;",
               title = "Application instructions", width = NULL, background = "light-blue",
               fluidRow("1. In the first step, select the weather parameter from the menu on the left."),
              fluidRow("2. To calculate the data, select the date and time, then click on the button."),
              fluidRow("3. The data will be downloaded and transferred, it may take a while."),
              fluidRow("4. To enable the animation, click play.")
              
             )),
             column(6,box(style="margin-left: 16px;margin-right: 16px;",
               title = "Authors and system version", width = NULL, background = "maroon",
               fluidRow("Sys.ver:",sys.wersion),
               br(),
               fluidRow("Authors:"),
               fluidRow("1..........."),
               fluidRow("2...........")
             ))
             
           )),
    tabItem(tabName = "dashboard1", "",
            tabBox(title = "Temperature",id = "dashboard1",height = 760,width = 800,
                   tabPanel("Maps", style="margin-left: 16px;",
                            fluidRow(uiOutput("UIPLOT_TMP")),
                            fluidRow(plotOutput("legend_TMP", height = 20))),
                   tabPanel("Profile","Tymczasowy")
                   )),
    
    tabItem(tabName = "dashboard2", "",
            tabBox(title = "Temp. max",id = "dashboard2",height = 760,width = 800,
            tabPanel("Maps",
                     fluidRow(uiOutput("UIPLOT_TMAX")),
                     fluidRow(plotOutput("legend_TMAX", height = 20))))),
    tabItem(tabName = "dashboard3", "",
            tabBox(title = "Temp. min",id = "dashboard3",height = 760,width = 800,
                   tabPanel("Maps",
                            fluidRow(uiOutput("UIPLOT_TMIN")),
                            fluidRow(plotOutput("legend_TMIN", height = 20))))),
    tabItem(tabName = "dashboard4", "",
            tabBox(title = "Dew point",id = "dashboard4",height = 760,width = 800,
                   tabPanel("Maps",
                            fluidRow(uiOutput("UIPLOT_DPT")),
                            fluidRow(plotOutput("legend_DPT", height = 20))))),
   tabItem(tabName = "dashboard5", "",
            tabBox(title = "Relative hum.",id = "dashboard5",height = 760,width = 800,
                   tabPanel("Maps",
                            fluidRow(uiOutput("UIPLOT_RH")),
                            fluidRow(plotOutput("legend_RH", height = 20))))),
   tabItem(tabName = "dashboard6", "",
           tabBox(title = "Press. see lvl",id = "dashboard6",height = 760,width = 800,
                  tabPanel("Maps",
                           fluidRow(uiOutput("UIPLOT_PRMSL")),
                           fluidRow(plotOutput("legend_PRMSL", height = 20))))),
   tabItem(tabName = "dashboard7", "",height = 760,width = 800,
           tabBox(title = "Wind (gust)",id = "dashboard7",
                  tabPanel("Maps",uiOutput("UIPLOT_GUST")))),
    tabItem(tabName = "dashboard8", "Cape",height = 760,width = 800,
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
  
  
  dashboardHeader(title = "MeteoRS",
                  
                  
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
