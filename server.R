library(shiny)
library(shinydashboard)
library(httr)
library(tidyr)
library(ggplot2)
library(dplyr)
library(stringr)

options(stringsAsFactors = FALSE)

source("auth.R")

MeetupBaseURL = "https://api.meetup.com"

securityCode <- createCode()

shinyServer(function(input, output, session) {
  AuthCode <- reactive({
    authReturnCode(session, securityCode)
    })
  
  appURL <- reactive({
    if (!is.null(session)) {
      # if we're running on localhost, build the redirect URI, so we get the port right;
      # if we're running on a server, use the config option
      if (session$clientData$url_hostname == "127.0.0.1") {
        paste0(session$clientData$url_protocol, "//", session$clientData$url_hostname, 
              ":", session$clientData$url_port)
      } else getOption("shiny_meetup_stats.redirect_uri")
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
                               redirect.uri=appURL())
      )
    }
  })
  
  ################### sidebar #############################
  
  observe({
    message("updateSelectedMeetup")
    updateSelectInput(session, "selectedMeetup", choices = userMeetups())
  })
  
  ################### API queries #############################
  
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
  
  upcomingEvents <- reactive({
    if (nchar(input$selectedMeetup) > 0) {
      tok <- AccessToken()
      resp <- GET(url=paste0(MeetupBaseURL, "/2/events"),
                  query=list(group_urlname=input$selectedMeetup, 
                             status="upcoming",
                             only="name,id,time,yes_rsvp_count",
                             access_token=tok))
      res <- content(resp)$results
      data.frame(name=sapply(res, function(x) x$name),
                 id=sapply(res, function(x) x$id),
                 time=as.POSIXct(sapply(res, function(x) x$time)/1e3, 
                                 origin=Sys.time() - as.numeric(Sys.time())),
                 yes_rsvp_count=sapply(res, function(x) x$yes_rsvp_count)
      )
    }
  })
  
  nextEvent <- reactive({
    upcomingEvents() %>% arrange(time) %>% head(1)
  })
  
  getSomeRSVPs <- function(tok, event.id, offset=0, page=200) {
    resp <- GET(url=paste0(MeetupBaseURL, "/2/rsvps"),
                query=list(group_urlname=input$selectedMeetup, 
                           rsvp="yes",
                           event_id=event.id,
                           page=page, offset=offset,
                           only="member",
                           access_token=tok))
    res <- content(resp)$results
    sapply(res, function(x) x$member$member_id)
  }
  
  # get a list of members attending the next event
  nextEventRSVPs <- reactive({
    if (nchar(input$selectedMeetup) > 0) {
      tok <- AccessToken()
      event_rsvps <- nextEvent()$yes_rsvp_count
      page.size = 200 # could be higher?
      
      message("getting RSVPs")
      member_ids <- sapply(seq.int(from=0,to=floor(event_rsvps/page.size)),
                          function(o) getSomeRSVPs(tok, nextEvent()$id, 
                                                   offset=o, page=page.size))
      member_ids <- unlist(member_ids)
      message("got ", length(member_ids), " RSPVs")
      member_ids
    }
  })
  
  getSomeProfiles <- function(tok, id_str, offset=0, page=20) {
    resp <- GET(url=paste0(MeetupBaseURL, "/2/profiles"),
                query=list(group_urlname=input$selectedMeetup, 
                           member_id=id_str,
                           page=page, offset=offset,
                           only="name,role,title",
                           access_token=tok))
    res <- content(resp)$results
    data.frame(name=sapply(res, function(x) x$name),
               role=sapply(res, function(x) if ('role' %in% names(x)) x$role else ''),
               title=sapply(res, function(x) if ('title' %in% names(x)) x$title else ''))
  }
  
  # get profile info for VIPs at next event
  nextEventVIPs <- reactive({
    if (nchar(input$selectedMeetup) > 0) {
      tok <- AccessToken()
      member_ids <- nextEventRSVPs()
      
      member_ids_df <- data.frame(member_ids, group=floor(seq_along(member_ids)/20))
      message("getting profiles")
      profiles <- member_ids_df %>% group_by(group) %>%
        do(getSomeProfiles(tok, paste(.$member_ids, collapse=','))) %>%
        ungroup() %>%
        mutate(titlerole = ifelse(role != '', role, title)) %>%
        filter(titlerole != '') %>%
        select(name, titlerole)
      message("got ", nrow(profiles), " VIP profiles")
      
      profiles
    }
  })
  
  ################### RSVP Plot #############################
  
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
  
  ################### Nametags #############################
  output$downloadNametagsCSV <- downloadHandler(
    filename = function() {
      sprintf("%s-%s.csv", input$selectedMeetup, make.names(str_sub(nextEvent()$name, end=20)))
    },
    content = function(con) {
      write.csv(nextEventVIPs(), con)
    }
  )
  
})
