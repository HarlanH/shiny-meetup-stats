
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#

library(shiny)
library(shinydashboard)

shinyUI(dashboardPage(
  dashboardHeader(title="Shiny Meetup Stats App"),
  dashboardSidebar(
    htmlOutput("AuthMeetupURL"),
    selectInput("selectedMeetup", "Group", ""),
    sidebarMenu(
      menuItem("RSVPs", tabName="rsvps", icon=icon("thumbs-up")),
      menuItem("About", tabName="about", icon=icon("question-circle"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    tabItems(
      tabItem("rsvps",
              plotOutput("rsvpPlot")),
      tabItem("about",
              a(href="", "Source on Github"))
    )
    
  )
))
