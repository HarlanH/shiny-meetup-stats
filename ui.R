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
      menuItem("Attendee Clusters", tabName="attendees", icon=icon("users")),
      menuItem("Nametags", tabName="nametags", icon=icon("tags")),
      menuItem("Pick Winner", tabName="pickwinner", icon=icon("trophy")),
      menuItem("Speakers on Twitter", tabName="twitters", icon=icon("twitter")),
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
              p("Attendees of next Meetup, restricted to people with Roles or Titles."),
              downloadButton("downloadNametagsCSV", label="Download Nametag CSV")
              ),
      tabItem("attendees",
              sliderInput("numClusts", "Clusters", min=2, max=10, value=2, step=1),
              dataTableOutput("attendeeClusters")),
      tabItem("pickwinner",
              actionButton("winnerbutton", "Pick a Winner!", icon=icon("trophy")),
              p("not yet implemented...")),
      tabItem("twitters",
              p("not yet implemented...")),
      tabItem("about",
              a(href="https://github.com/HarlanH/shiny-meetup-stats", "Source on Github"))
    )
    
  ),
  title="Meetup Stats",
  skin="red"
))
