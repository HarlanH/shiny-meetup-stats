library(shiny)
library(shinydashboard)

options(stringsAsFactors = FALSE)

shinyUI(dashboardPage(
  dashboardHeader(title="Meetup Stats"),
  dashboardSidebar(
    htmlOutput("AuthMeetupURL"),
    selectInput("selectedMeetup", "Group", ""),
    sidebarMenu(
      menuItem("RSVPs", tabName="rsvps", icon=icon("thumbs-up")),
      menuItem("Nametags", tabName="nametags", icon=icon("tags")),
      menuItem("Attendees", tabName="attendees", icon=icon("users")),
      menuItem("Pick Winner", tabName="pickwinner", icon=icon("trophy")),
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
      tabItem("nametags",
              downloadButton("downloadNametagsCSV", label="Download Nametag CSV")),
      tabItem("attendees",
              plotOutput("attendeeClusters")),
      tabItem("pickwinner",
              actionButton("winnerbutton", "Pick a Winner!", icon=icon("trophy"))),
      tabItem("about",
              a(href="https://github.com/HarlanH/shiny-meetup-stats", "Source on Github"))
    )
    
  ),
  title="Meetup Stats",
  skin="red"
))
