library(tidyverse)
library(magrittr)
library(numform)
library(scales)
#library(rvest)
#library(jsonlite)
#library(httr)
#library(lubridate)

options(readr.show_col_types = FALSE)
options(warn=-1)

## Watchlist
myratings <-
  read.csv("ratings/ratings.csv") %>%
  bind_rows(., read.csv("https://www.imdb.com/list/ls003235325/export")) %>%
  left_join(., read.csv("https://www.imdb.com/list/ls507245240/export") %>% select(Const, Modified) %>% rename(AFI = Modified), by=join_by(Const)) %>%
  left_join(., read.csv("https://www.imdb.com/list/ls507032905/export") %>% select(Const, Modified) %>% rename(Theater = Modified), by=join_by(Const)) %>%
  select(-c(Position,Description,Created,Modified)) %>%
  distinct(Const, .keep_all = TRUE) %>%
  arrange(desc(Date.Rated)) %>%
  left_join(.,
            bind_rows(read_csv("raw-lists/Prime-Free-Oscar.csv"),
                      read_csv("raw-lists/Prime-Free-Times.csv")) %>%
              mutate(Service = "Prime") %>%
              distinct %>%
              bind_rows(., anti_join(bind_rows(
                read_csv("raw-lists/Prime-Rentals-Oscar.csv"),
                read_csv("raw-lists/Prime-Rentals-Times.csv")) %>% distinct,
                bind_rows(
                  read_csv("raw-lists/Prime-Free-Oscar.csv"),
                  read_csv("raw-lists/Prime-Free-Times.csv")) %>% distinct) %>% mutate(Service = "Prime Rentals")), by=join_by(Const)) %T>%
  write.csv(.,"ratings/formatted.csv", row.names = FALSE)

## Oscar Ceremony Data for Summary and Graph
OscarsCorrected <-
  left_join(read_csv("raw-lists/OscarCeremonies.csv"), myratings, by="Const") %T>%
  write.csv(.,"datasets/Oscars/OscarsTracking.csv", row.names = FALSE)
OscarSummary <-
  OscarsCorrected %>%
  filter(!is.na(Const)) %>%
  mutate(AwardWinner = ifelse(AwardWinner == "Winner",TRUE,FALSE)) %>%
  select(AwardCeremony, AwardWinner, Const, Your.Rating) %>%
  distinct %>%
  dplyr::group_by(Const) %>%
  dplyr::mutate(
    filmwon=ifelse(any(AwardWinner),TRUE,FALSE),
    filmwon=ifelse(all(is.na(filmwon)),FALSE,filmwon)
  ) %>%
  dplyr::mutate(keep_row=ifelse(filmwon,AwardWinner,TRUE)) %>%
  dplyr::filter(!(filmwon == TRUE & is.na(keep_row))) %>%
  ungroup %>%
  mutate(
    Seen = ifelse(is.na(Your.Rating), FALSE, TRUE),
    Year = sub('.*-', '', AwardCeremony),
    Ceremony = ifelse(f_ordinal(sub("\\-.*", "", str_remove(AwardCeremony, "^0+"))) == 13, "13th", f_ordinal(sub("\\-.*", "", str_remove(AwardCeremony, "^0+")))),
    Menu = paste0("<h5>",Ceremony," Academy Awards</h5><h1>",Year,"</h1>")) %>%
  dplyr::group_by(AwardCeremony, Year) %>%
  dplyr::summarise(
    Winner.Y =
      ifelse(
        any(Seen == TRUE & AwardWinner == TRUE),
        ifelse(
          any(AwardWinner == TRUE),
          n_distinct(Const[Seen == TRUE & AwardWinner == TRUE]),
          NA),
        NA),
    Winner.N =
      ifelse(
        any(Seen == FALSE & AwardWinner == TRUE),
        ifelse(
          any(AwardWinner == TRUE),
          n_distinct(Const[Seen == FALSE & AwardWinner == TRUE]),
          NA),
        NA),
    Nominee.Y =
      ifelse(
        any(Seen == TRUE & is.na(AwardWinner)),
        n_distinct(Const[Seen == TRUE & is.na(AwardWinner)]),
        NA),
    Nominee.N =
      ifelse(
        any(Seen == FALSE & is.na(AwardWinner)),
        n_distinct(Const[Seen == FALSE & is.na(AwardWinner)]),
        NA)) %>%
  arrange(Year) %>%
  select(-Year) %T>%
  write.csv(.,"datasets/Oscars/Oscars-Chart-Data.csv", row.names = FALSE)

## Oscar Summary by Ceremony
Oscars.Ceremonies <-
  OscarsCorrected %>%
  filter(!is.na(Const)) %>%
  filter(!AwardType %in% c("Writing, Title","Director,Assistant","Direction, Dance")) %>%
  select(AwardCeremony, AwardType, Const, Your.Rating) %>%
  distinct %>%
  dplyr::group_by(Const) %>%
  mutate(Seen = ifelse(is.na(Your.Rating), FALSE, TRUE)) %>%
  dplyr::group_by(AwardCeremony, AwardType) %>%
  dplyr::summarise(Percentage = round(n_distinct(Const[Seen == TRUE]) / n_distinct(Const), digits = 1)) %>%
  spread(., AwardType, Percentage, fill = NA) %>%
  left_join(
    .,
    OscarsCorrected %>%
      filter(!is.na(Const)) %>%
      filter(!AwardType %in% c("Writing, Title","Director,Assistant","Direction, Dance")) %>%
      select(AwardCeremony, Const, Your.Rating) %>%
      distinct %>%
      dplyr::group_by(Const) %>%
      mutate(Seen = ifelse(is.na(Your.Rating), FALSE, TRUE)) %>%
      dplyr::group_by(AwardCeremony) %>%
      dplyr::summarize(
        Films.Y = n_distinct(Const[Seen == TRUE]),
        Films.N = n_distinct(Const[Seen == FALSE]),
        Films = n_distinct(Const),
        Percentage = percent(n_distinct(Const[Seen == TRUE]) / n_distinct(Const), accuracy = 1))
  ) %>%
  bind_rows(
    .,
    OscarsCorrected %>%
      filter(!is.na(Const)) %>%
      filter(!AwardType %in% c("Writing, Title","Director,Assistant","Direction, Dance")) %>%
      select(AwardCeremony, AwardType, Const, Your.Rating) %>%
      distinct %>%
      dplyr::group_by(Const) %>%
      mutate(Seen = ifelse(is.na(Your.Rating), FALSE, TRUE)) %>%
      dplyr::group_by(AwardType) %>%
      dplyr::summarize(AwardCeremony = "Not Seen", Films.N = n_distinct(Const[Seen == FALSE])) %>%
      spread(., AwardType, Films.N, fill = NA)
  ) %>%
  bind_rows(
    .,
    OscarsCorrected %>%
      filter(!is.na(Const)) %>%
      filter(!AwardType %in% c("Writing, Title","Director,Assistant","Direction, Dance")) %>%
      select(AwardCeremony, AwardType, Const, Your.Rating) %>%
      distinct %>%
      dplyr::group_by(Const) %>%
      mutate(Seen = ifelse(is.na(Your.Rating), FALSE, TRUE)) %>%
      dplyr::group_by(AwardType) %>%
      dplyr::summarize(AwardCeremony = "Seen", Films.Y = n_distinct(Const[Seen == TRUE])) %>%
      spread(., AwardType, Films.Y, fill = NA)
  ) %>%
  bind_rows(
    .,
    OscarsCorrected %>%
      filter(!is.na(Const)) %>%
      filter(!AwardType %in% c("Writing, Title","Director,Assistant","Direction, Dance")) %>%
      select(AwardCeremony, AwardType, Const, Your.Rating) %>%
      distinct %>%
      dplyr::group_by(Const) %>%
      mutate(
        Seen = ifelse(is.na(Your.Rating), FALSE, TRUE)) %>%
      dplyr::group_by(AwardType) %>%
      dplyr::summarize(
        AwardCeremony = "Total",
        Film = n_distinct(Const)) %>%
      spread(., AwardType, Film, fill = NA) %>%
      left_join(.,
                OscarsCorrected %>%
                  filter(!is.na(Const)) %>%
                  filter(!AwardType %in% c("Writing, Title","Director,Assistant","Direction, Dance")) %>%
                  select(AwardCeremony, AwardType, Const, Your.Rating) %>%
                  distinct %>%
                  dplyr::group_by(Const) %>%
                  mutate(
                    Seen = ifelse(is.na(Your.Rating), FALSE, TRUE)) %>%
                  dplyr::group_by(AwardCeremony = "Total") %>%
                  dplyr::summarize(
                    Films.Y = n_distinct(Const[Seen == TRUE]),
                    Films.N = n_distinct(Const[Seen == FALSE]),
                    Films = n_distinct(Const),
                    Percentage = percent(n_distinct(Const[Seen == TRUE]) / n_distinct(Const), accuracy = 1)))
  ) %T>%
  write.csv(.,"datasets/Oscars/Oscars-Summary-Ceremonies.csv", row.names = FALSE)

## Oscar Summary by Award
Oscars.Awards <-
  OscarsCorrected %>%
  filter(!is.na(Const)) %>%
  mutate(AwardWinner = ifelse(AwardWinner == "Winner",TRUE,FALSE)) %>%
  select(AwardCeremony, AwardType, AwardWinner, Const, Your.Rating, Service) %>%
  distinct %>%
  dplyr::group_by(Const) %>%
  dplyr::mutate(
    filmwon=ifelse(any(AwardWinner),TRUE,FALSE),
    filmwon=ifelse(all(is.na(filmwon)),FALSE,filmwon)
  ) %>%
  dplyr::mutate(keep_row=ifelse(filmwon,AwardWinner,TRUE)) %>%
  dplyr::filter(!(filmwon == TRUE & is.na(keep_row))) %>%
  ungroup %>%
  mutate(
    Seen = ifelse(is.na(Your.Rating), FALSE, TRUE),
    Year = sub('.*-', '', AwardCeremony),
    Ceremony = ifelse(f_ordinal(sub("\\-.*", "", str_remove(AwardCeremony, "^0+"))) == 13, "13th", f_ordinal(sub("\\-.*", "", str_remove(AwardCeremony, "^0+")))),
    Menu = paste0("<h5>",AwardType,"</h5>")) %>%
  dplyr::group_by(AwardType) %>%
  dplyr::summarise(
    Films = n_distinct(Const),
    Films.Y = n_distinct(Const[Seen == TRUE]),
    Films.N = n_distinct(Const[Seen == FALSE]),
    Winner.Y =
      ifelse(
        any(Seen == TRUE & AwardWinner == TRUE),
        ifelse(
          any(AwardWinner == TRUE),
          n_distinct(Const[Seen == TRUE & AwardWinner == TRUE]),
          0),
        0),
    Winner.N =
      ifelse(
        any(Seen == FALSE & AwardWinner == TRUE),
        ifelse(
          any(AwardWinner == TRUE),
          n_distinct(Const[Seen == FALSE & AwardWinner == TRUE]),
          0),
        0),
    Winner.Per = round(Winner.Y/(Winner.Y+Winner.N), digits=2),
    Nominee.Y =
      ifelse(
        any(Seen == TRUE & is.na(AwardWinner)),
        n_distinct(Const[Seen == TRUE & is.na(AwardWinner)]),
        0),
    Nominee.N =
      ifelse(
        any(Seen == FALSE & is.na(AwardWinner)),
        n_distinct(Const[Seen == FALSE & is.na(AwardWinner)]),
        0),
    Nominee.Per = round(Nominee.Y/(Nominee.Y+Nominee.N), digits=2),
    On.Prime.Free = n_distinct(Const[Service == "Prime"]),
    Prime.Free.Y = n_distinct(Const[Seen == TRUE & Service == "Prime"]),
    Prime.Free.N = n_distinct(Const[Seen == FALSE & Service == "Prime"]),
    Prime.Free.Per = round(Prime.Free.Y/On.Prime.Free, digits=2),
    On.Prime.Rental = n_distinct(Const[Service == "Prime Rentals"]),
    Prime.Rental.Y = n_distinct(Const[Seen == TRUE & Service == "Prime Rentals"]),
    Prime.Rental.N = n_distinct(Const[Seen == FALSE & Service == "Prime Rentals"]),
    Prime.Rental.Per = round(Prime.Rental.Y/On.Prime.Rental, digits=2)) %T>%
  write.csv(.,"datasets/Oscars/Oscars-Summary-Awards.csv", row.names = FALSE)

Oscars.Films <-
  OscarsCorrected %>%
  group_by(AwardCeremony, Const, Title, Year, Your.Rating, Runtime..mins., Genres, Directors, Writer, Actors, Plot, Language, Country, Awards, Poster, IMDb.Rating, Num.Votes, Title.Type, Date.Rated, Service, AFI, Theater) %>%
  summarize(
    Nominations = n_distinct(AwardCategory),
    Losses = n_distinct(AwardCategory[is.na(AwardWinner)]),
    Wins = Nominations - Losses) %T>%
  write.csv(.,"datasets/Oscars/Oscars-Summary-Films.csv", row.names = FALSE)

## NYT-1000 Data for Summary and Graph
combinedNYT1000 <-
  left_join(read_csv("raw-lists/nyt1000.csv"), myratings, by="Const") %>%
    mutate(Seen = ifelse(is.na(Your.Rating), "No", "Yes")) %T>%
    write.csv(.,"datasets/NYT1000/NYT1000Data.csv", row.names = FALSE)
combinedNYT1000 %>%
  dplyr::group_by(Year) %>%
  dplyr::summarize(
    Y = n_distinct(Const[Seen == "Yes"]),
    N = n_distinct(Const[Seen == "No"])) %>%
  select(Year, Y, N) %>%
  write.csv(.,"datasets/NYT1000/NYT1000Summary.csv", row.names = FALSE)

## NBR Data for Graph
combinedNBR <-
  left_join(read_csv("raw-lists/NBR-awards.csv"), myratings, by=c("FilmID" = "Const")) %>%
  mutate(Seen = ifelse(is.na(Your.Rating), "No", "Yes")) %>%
  rename(AwardCeremony = Ceremony.Year) %T>%
  write.csv(.,"datasets/NBR/NBRData.csv", row.names = FALSE)
combinedNBR %>%
  dplyr::group_by(Year = AwardCeremony) %>%
  dplyr::summarize(
    Y = n_distinct(FilmID[Seen == "Yes"]),
    N = n_distinct(FilmID[Seen == "No"])) %>%
  select(Year, Y, N) %>%
  write.csv(.,"datasets/NBR/NBRSummary.csv", row.names = FALSE)

## Great Films Ebert
List.Ebert <-
  left_join(read_csv("raw-lists/ebert.csv"), myratings, by="Const") %>%
    mutate(Decade = paste0(10 * floor(Year/10),"s")) %>%
    mutate(Seen = ifelse(is.na(Your.Rating), "No", "Yes")) %T>%
    write.csv(.,"datasets/GreatFilmsEbert/Data.csv", row.names = FALSE)

## AFI Top 100 from 1998
List.AFI.1998 <-
  left_join(read_csv("raw-lists/afi1998.csv"), myratings, by="Const") %>%
    mutate(Decade = paste0(10 * floor(Year/10),"s")) %>%
    mutate(Seen = ifelse(is.na(Your.Rating), "No", "Yes")) %T>%
    write.csv(.,"datasets/AFITop100/1998/Data.csv", row.names = FALSE)

## AFI Top 100 from 2007
List.AFI.2007 <-
  left_join(read_csv("raw-lists/afi2007.csv"), myratings, by="Const") %>%
    mutate(Decade = paste0(10 * floor(Year/10),"s")) %>%
    mutate(Seen = ifelse(is.na(Your.Rating), "No", "Yes")) %T>%
    write.csv(.,"datasets/AFITop100/2007/Data.csv", row.names = FALSE)

#mutate(Decade = floor(as.numeric(ItemYear)/10)*10)
