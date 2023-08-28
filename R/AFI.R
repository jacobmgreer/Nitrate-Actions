library(tidyverse)
library(rvest)
library(jsonlite)
library(httr)
library(lubridate)
library(magrittr)
library(numform)

options(readr.show_col_types = FALSE)
options(warn=-1)

AFIlineup <-
  read_html("https://silver.afi.com/Browsing/Movies/NowShowing") %>%
  html_nodes('#movies-list > .movie') %>%
  map_df(~{
    Poster <- .x  %>% html_node(.,'div.image-outer div img') %>% html_attr("src")
    Trailer= .x %>% html_nodes(.,'div.image-outer a.play') %>% html_attr("href")
    Title <- .x %>% html_nodes(.,'div.item-details h3.item-title') %>% html_text(.)
    Screening <- .x %>% html_nodes(.,'div.main-action a') %>% html_attr("href")
    tibble(Poster,Trailer,Title,Screening)
  }) %>%
  mutate(
    TheatreID = str_remove(Screening, "//silver.afi.com/Browsing/Movies/Details/"),
    Title = trimws(Title))

showtimes <- data.frame()
for (i in 1:nrow(AFIlineup)) {
  link <- read_html(paste0("http:",AFIlineup$Screening[i]))
  page <-
    data.frame(
      FilmID = AFIlineup$FilmID[i],
      TheatreID = AFIlineup$TheatreID[i],
      SessionID = gsub('^.*SessionId=\\s*|\\s*&vis.*$', '', link %>% html_nodes(.,'.film-showtimes .session .session-times a') %>% html_attr("href")),
      ScreenTitle = AFIlineup$Title[i],
      ScreenTrailer = AFIlineup$Trailer[i],
      ScreenPoster = AFIlineup$Poster[i],
      ScreenDate= link %>% html_node(.,'.film-showtimes .session .session-date') %>% html_text(.),
      ScreenTimestamp= link %>% html_nodes(.,'.film-showtimes .session .session-times time') %>% html_attr("datetime"),
      ScreenTime= link %>% html_nodes(.,'.film-showtimes .session .session-times time') %>% html_text(.),
      #ScreenTimeDetails= link %>% html_nodes(.,'.film-showtimes .session .session-times a'),
      ScreenTicketLink= link %>% html_nodes(.,'.film-showtimes .session .session-times a') %>% html_attr("href"),
      ScreenGenre= link %>% html_nodes(.,'.film-info-item > div:nth-child(2) > span') %>% html_text(.),
      ScreenRuntime= link %>% html_nodes(.,'.film-info-item > div:nth-child(1) > span') %>% html_text(.)
    )
  showtimes <- rbind(showtimes, page)
}

theatres <- data.frame()
for (i in 1:nrow(showtimes)) {
  link <- read_html(paste0("http:",showtimes$ScreenTicketLink[i]))
  page <-
    data.frame(
      TheatreID = showtimes$TheatreID[i],
      ScreenTicketLink= showtimes$ScreenTicketLink[i],
      ScreenTheatre= link %>% html_node(.,'.cinema-screen-name') %>% html_text(.)
    )
  theatres <- rbind(theatres, page)
}

showtimes <- left_join(showtimes, theatres, by = c("TheatreID", "ScreenTicketLink")) %T>%
  write.csv(., "datasets/AFI-Silver/Showtimes.csv", row.names = FALSE)

ScreeningCal <- function(showtimes2) {
  ics_header <- readLines("ics_template/template_header.ics", warn = F)
  ics_body <- readLines("ics_template/template_body.ics", warn = F)
  ics_footer <- readLines("ics_template/template_footer.ics", warn = F)

  ics_events <- ""
  for(i in 1:nrow(showtimes2)) {
    ics_body <- str_replace(ics_body, "UID:.*", paste0("UID:", showtimes2$SessionID[i]))
    ics_body <- str_replace(ics_body, "DTSTAMP:.*", paste0("DTSTAMP:", format(now(tzone = "EST"), "%Y%m%dT%H%M%S")))
    ics_body <- str_replace(ics_body, "DTSTART:.*", paste0("DTSTART:", format(ymd_hms(showtimes2$ScreenTimestamp[i]), "%Y%m%dT%H%M%S")))
    ics_body <- str_replace(ics_body, "DTEND:.*", paste0("DTEND:", format(ymd_hms(showtimes2$ScreenTimestamp[i]) + minutes(str_remove(showtimes2$ScreenRuntime[i],' Minutes')), "%Y%m%dT%H%M%S")))
    ics_body <- str_replace(ics_body, "SUMMARY:.*", paste0("SUMMARY:", iconv(showtimes2$ScreenTitle[i], "UTF-8", "ASCII", sub = "")))
    ics_body <- str_replace(ics_body, "LOCATION:.*", paste0("LOCATION:", showtimes2$ScreenTheatre[i]))
    ics_body <- str_replace(ics_body, "DESCRIPTION:.*", paste0("DESCRIPTION:http:", showtimes2$ScreenTicketLink[i]))
    ics_events <- append(ics_events, ics_body)
  }
  ics_events <- append(ics_header, ics_events)
  ics_events <- append(ics_events, ics_footer)
  return(ics_events)
}

ScreeningCal(showtimes %>% filter(grepl('1', ScreenTheatre))) %>%
  write(., file = "datasets/AFI-Silver/Theater1.ics")
ScreeningCal(showtimes %>% filter(grepl('2', ScreenTheatre))) %>%
  write(., file = "datasets/AFI-Silver/Theater2.ics")
ScreeningCal(showtimes %>% filter(grepl('3', ScreenTheatre))) %>%
  write(., file = "datasets/AFI-Silver/Theater3.ics")

rm(page,link,i,ics_body,ics_events,ics_footer,ics_header,ScreeningCal,theatres)
