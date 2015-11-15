library(shiny)
library(shinydashboard)
library(httr)
library(tidyr)
library(ggplot2)
library(dplyr)

source("auth.R")

MeetupBaseURL = "https://api.meetup.com"

securityCode <- createCode()

shinyServer(function(input, output, session) {
  AuthCode <- reactive({
    authReturnCode(session, securityCode)
    })
  
  appURL <- reactive({
    if (!is.null(session)) {
      ## build redirect URI
      paste0(session$clientData$url_protocol, "//", session$clientData$url_hostname, 
             ifelse(session$clientData$url_hostname == "127.0.0.1", 
                    ":", session$clientData$url_pathname), session$clientData$url_port)
    }
  })
  
  AccessToken <- reactive({
      validate(
        need(AuthCode(), "Log in to see your Meetup groups")
      )
      message(appURL())
      access_token <- MeetupGetToken(code = AuthCode(), 
                                     redirect.uri=appURL())
      token <- access_token$access_token
    })
    
  #output$authCode <- renderText(AuthCode())
  #output$accessToken <- renderText(AccessToken())
  #output$appurl <- renderText({appURL()})
  
  output$AuthMeetupURL <- renderUI({
    if (isolate(!is.null(AuthCode()))) {
      tags$button("Logged In", 
                  type="button", class="btn btn-success shinybtn")
    } else {
      a(tags$button("Log In With Meetup",
                    type="button", class="btn btn-primary shinybtn"), 
        target="_top",
        href=MeetupGetTokenURL(securityCode, 
                               redirect.uri=appURL()))
      
    }
  })
  
  userMeetups <- reactive({
    message("userMeetups")
    tok <- AccessToken()
    resp <- GET(url=paste0(MeetupBaseURL, "/2/groups"),
              query=list(member_id="self", only="name,urlname",
                         access_token=tok))
    mus <- content(resp)$results
    ret <- structure(sapply(mus, function(mu) mu$urlname),
                     names=sapply(mus, function(mu) mu$name))
    c("", ret[order(names(ret))])
  })
  
  rsvpData <- reactive({
    if (nchar(input$selectedMeetup) > 0) {
      tok <- AccessToken()
      resp <- GET(url=paste0(MeetupBaseURL, "/2/events"),
                  query=list(group_urlname=input$selectedMeetup, 
                             status="past",
                             only="name,time,yes_rsvp_count,headcount",
                             access_token=tok))
      res <- content(resp)$results
      data.frame(name=sapply(res, function(x) x$name),
                 time=as.POSIXct(sapply(res, function(x) x$time)/1e3, 
                                 origin=Sys.time() - as.numeric(Sys.time())),
                 yes_rsvp_count=sapply(res, function(x) x$yes_rsvp_count),
                 headcount=sapply(res, function(x) x$headcount))
    }
  })
  
  output$rsvpPlot <- renderPlot({
    message("rsvpPlot")
    validate(
      need(!is.null(AuthCode()), "Please log in."),
      need(!is.null(rsvpData()), "Please choose a Meetup.")
    )
    
    dat <- rsvpData() %>%
      rename("Yes RSVPs"=yes_rsvp_count, "Headcount"=headcount) %>%
      gather(metric, count, -time, -name)
    
    ggplot(dat, aes(time, count, fill=metric)) +
      xlab("Date") + ylab("Count") +
      scale_fill_manual("Metric", values=c("black", "red")) +
      geom_area(position="identity", alpha=.5) 
  })
  
  observe({
    message("updateSelectedMeetup")
    updateSelectInput(session, "selectedMeetup", choices = userMeetups())
  })
})
