library(shiny)
library(shinydashboard)

shinyUI(dashboardPage(
  dashboardHeader(title="Meetup Stats"),
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
              a(href="https://github.com/HarlanH/shiny-meetup-stats", "Source on Github"))
    )
    
  ),
  title="Meetup Stats",
  skin="red"
))
