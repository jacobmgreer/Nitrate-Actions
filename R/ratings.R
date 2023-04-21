library(tidyverse)
library(rvest)
library(jsonlite)
library(httr)
library(lubridate)
library(magrittr)
library(numform)

options(readr.show_col_types = FALSE)
options(warn=-1)

myratings <-
  read.csv("https://www.imdb.com/user/ur28723514/ratings/export") %>%
  rename(
    IMDBid = const,
    Rating = `Your Rating`,
    Rated.Date = `Date Rated`,
    ItemTitle = Title,
    Type = `Title Type`,
    Runtime = `Runtime (mins)`,
    Director = Directors,
    imdbVotes = `Num Votes`,
    imdbRating = `IMDb Rating`,
    Genre = Genres,
    Year = `Release Date`
  ) %>%
  mutate(Year = substr(Year, 1, 4)) %T>%
  write.csv(.,"datasets/ratings.csv", row.names = FALSE)

## WATCHLIST
watchlist <-
  read.csv("https://www.imdb.com/list/ls003235325/export") %T>%
  write.csv(.,"datasets/watchlist.csv", row.names = FALSE)

## Seen at AFI
Seen.AFISilver <-
  read.csv("https://www.imdb.com/list/ls507245240/export") %T>%
  write.csv(.,"datasets/AFI-Silver.csv", row.names = FALSE)

## Seen at a Theater
Seen.Theater <-
  read.csv("https://www.imdb.com/list/ls507032905/export") %T>%
  write.csv(.,"datasets/theater.csv", row.names = FALSE)

## Prime Availability
Streaming.Available <-
  bind_rows(read_csv("raw-lists/Prime-Free-Oscar.csv"),
            read_csv("raw-lists/Prime-Free-Times.csv")) %>%
  mutate(Service = "Prime") %>%
  distinct %>%
  bind_rows(., anti_join(bind_rows(
                       read_csv("raw-lists/Prime-Rentals-Oscar.csv"),
                       read_csv("raw-lists/Prime-Rentals-Times.csv")) %>% distinct,
                     bind_rows(
                       read_csv("raw-lists/Prime-Free-Oscar.csv"),
                       read_csv("raw-lists/Prime-Free-Times.csv")) %>% distinct) %>% mutate(Service = "Prime Rentals"))

### For testing
# myratings <- ratingslist

## Oscar Ceremony Data for Summary and Graph
OscarCeremonies.corrected <- read_csv("raw-lists/OscarCeremonies.csv")
OscarsCorrected <- left_join(OscarCeremonies.corrected, myratings %>% select(IMDBid, Rating, Rated.Date), by=c("FilmID" = "IMDBid")) %>%
  left_join(., Streaming.Available, by=c("FilmID" = "IMDBid")) %>%
  left_join(., Seen.AFISilver %>% mutate(IMDBid = Const, AFISilver = "Y") %>% select(IMDBid, AFISilver), by=c("FilmID" = "IMDBid")) %>%
  left_join(., Seen.Theater %>% mutate(IMDBid = Const, Theater = "Y") %>% select(IMDBid, Theater), by=c("FilmID" = "IMDBid")) %>%
  left_join(., watchlist %>% mutate(IMDBid = Const, Watchlist = "Y") %>% select(IMDBid, Watchlist), by=c("FilmID" = "IMDBid"))
write.csv(OscarsCorrected,"datasets/Oscars/OscarsTracking.csv", row.names = FALSE)
left_join(OscarCeremonies.corrected, myratings %>% select(IMDBid, Rating, Rated.Date), by=c("FilmID" = "IMDBid")) %>%
  filter(FilmID != "") %>%
  mutate(AwardWinner = ifelse(AwardWinner == "Winner",TRUE,FALSE)) %>%
  select(AwardCeremony, AwardWinner, FilmID, Rating) %>%
  distinct %>%
  dplyr::group_by(FilmID) %>%
  dplyr::mutate(
    filmwon=ifelse(any(AwardWinner),TRUE,FALSE),
    filmwon=ifelse(all(is.na(filmwon)),FALSE,filmwon)
  ) %>%
  dplyr::mutate(keep_row=ifelse(filmwon,AwardWinner,TRUE)) %>%
  dplyr::filter(!(filmwon == TRUE & is.na(keep_row))) %>%
  ungroup %>%
  mutate(
    Seen = ifelse(is.na(Rating), FALSE, TRUE),
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
          n_distinct(FilmID[Seen == TRUE & AwardWinner == TRUE]),
          NA),
        NA),
    Winner.N =
      ifelse(
        any(Seen == FALSE & AwardWinner == TRUE),
        ifelse(
          any(AwardWinner == TRUE),
          n_distinct(FilmID[Seen == FALSE & AwardWinner == TRUE]),
          NA),
        NA),
    Nominee.Y =
      ifelse(
        any(Seen == TRUE & is.na(AwardWinner)),
        n_distinct(FilmID[Seen == TRUE & is.na(AwardWinner)]),
        NA),
    Nominee.N =
      ifelse(
        any(Seen == FALSE & is.na(AwardWinner)),
        n_distinct(FilmID[Seen == FALSE & is.na(AwardWinner)]),
        NA)) %>%
  arrange(Year) %>%
  select(-Year) %>%
  write.csv(.,"datasets/Oscars/OscarsSummary.csv", row.names = FALSE)

## Award Category Summary
OscarsCorrected %>%
  filter(FilmID != "") %>%
  filter(!AwardType %in% c("Writing, Title","Director,Assistant","Direction, Dance")) %>%
  select(AwardCeremony, AwardType, FilmID, Rating) %>%
  distinct %>%
  dplyr::group_by(FilmID) %>%
  mutate(Seen = ifelse(is.na(Rating), FALSE, TRUE)) %>%
  dplyr::group_by(AwardCeremony, AwardType) %>%
  dplyr::summarise(Percentage = round(n_distinct(FilmID[Seen == TRUE]) / n_distinct(FilmID), digits = 1)) %>%
  spread(., AwardType, Percentage, fill = NA) %>%
  write.csv(.,"datasets/Oscars/AwardTypeSummary.csv", row.names = FALSE)

## Award Summary
OscarsCorrected %>%
  filter(FilmID != "") %>%
  mutate(AwardWinner = ifelse(AwardWinner == "Winner",TRUE,FALSE)) %>%
  select(AwardCeremony, AwardType, AwardWinner, FilmID, Rating, Service) %>%
  distinct %>%
  dplyr::group_by(FilmID) %>%
  dplyr::mutate(
    filmwon=ifelse(any(AwardWinner),TRUE,FALSE),
    filmwon=ifelse(all(is.na(filmwon)),FALSE,filmwon)
  ) %>%
  dplyr::mutate(keep_row=ifelse(filmwon,AwardWinner,TRUE)) %>%
  dplyr::filter(!(filmwon == TRUE & is.na(keep_row))) %>%
  ungroup %>%
  mutate(
    Seen = ifelse(is.na(Rating), FALSE, TRUE),
    Year = sub('.*-', '', AwardCeremony),
    Ceremony = ifelse(f_ordinal(sub("\\-.*", "", str_remove(AwardCeremony, "^0+"))) == 13, "13th", f_ordinal(sub("\\-.*", "", str_remove(AwardCeremony, "^0+")))),
    Menu = paste0("<h5>",AwardType,"</h5>")) %>%
  dplyr::group_by(AwardType) %>%
  dplyr::summarise(
    Films = n_distinct(FilmID),
    Films.Y = n_distinct(FilmID[Seen == TRUE]),
    Films.N = n_distinct(FilmID[Seen == FALSE]),
    Winner.Y =
      ifelse(
        any(Seen == TRUE & AwardWinner == TRUE),
        ifelse(
          any(AwardWinner == TRUE),
          n_distinct(FilmID[Seen == TRUE & AwardWinner == TRUE]),
          0),
        0),
    Winner.N =
      ifelse(
        any(Seen == FALSE & AwardWinner == TRUE),
        ifelse(
          any(AwardWinner == TRUE),
          n_distinct(FilmID[Seen == FALSE & AwardWinner == TRUE]),
          0),
        0),
    Winner.Per = round(Winner.Y/(Winner.Y+Winner.N), digits=2),
    Nominee.Y =
      ifelse(
        any(Seen == TRUE & is.na(AwardWinner)),
        n_distinct(FilmID[Seen == TRUE & is.na(AwardWinner)]),
        0),
    Nominee.N =
      ifelse(
        any(Seen == FALSE & is.na(AwardWinner)),
        n_distinct(FilmID[Seen == FALSE & is.na(AwardWinner)]),
        0),
    Nominee.Per = round(Nominee.Y/(Nominee.Y+Nominee.N), digits=2),
    On.Prime.Free = n_distinct(FilmID[Service == "Prime"]),
    Prime.Free.Y = n_distinct(FilmID[Seen == TRUE & Service == "Prime"]),
    Prime.Free.N = n_distinct(FilmID[Seen == FALSE & Service == "Prime"]),
    Prime.Free.Per = round(Prime.Free.Y/On.Prime.Free, digits=2),
    On.Prime.Rental = n_distinct(FilmID[Service == "Prime Rentals"]),
    Prime.Rental.Y = n_distinct(FilmID[Seen == TRUE & Service == "Prime Rentals"]),
    Prime.Rental.N = n_distinct(FilmID[Seen == FALSE & Service == "Prime Rentals"]),
    Prime.Rental.Per = round(Prime.Rental.Y/On.Prime.Rental, digits=2)) %>%
  write.csv(.,"datasets/Oscars/OscarsAwardSummary.csv", row.names = FALSE)

OscarFilmSummary <-
  OscarsCorrected %>%
  group_by(AwardCeremony, FilmID, FilmName, Year, Rated, Runtime, Genre, Director, Writer, Actors, Plot, Language, Country, Awards, Poster, Metascore, imdbRating, imdbVotes, imdbID, Type, totalSeasons, Season, Episode, seriesID, IMDb, Metacritic, RottenTomatoes, Rating, Rated.Date, Service, AFISilver, Theater, Watchlist) %>%
  summarize(
    Nominations = n_distinct(AwardCategory),
    Losses = n_distinct(AwardCategory[is.na(AwardWinner)]),
    Wins = Nominations - Losses) %>%
  write.csv(.,"datasets/Oscars/OscarsFilmSummary.csv", row.names = FALSE)

## NYT-1000 Data for Summary and Graph
combinedNYT1000 <-
  left_join(read_csv("raw-lists/nyt1000.csv"), myratings %>% select(IMDBid, Rating, Rated.Date), by="IMDBid") %>%
    mutate(Seen = ifelse(is.na(Rating), "No", "Yes")) %>%
    left_join(., Streaming.Available, by="IMDBid") %>%
  left_join(., Seen.AFISilver %>% mutate(IMDBid = Const, AFISilver = "Y"), by="IMDBid") %>%
  left_join(., Seen.Theater %>% mutate(IMDBid = Const, Theater = "Y"), by="IMDBid") %>%
  left_join(., watchlist %>% mutate(IMDBid = Const, Watchlist = "Y"), by="IMDBid") %T>%
    write.csv(.,"datasets/NYT1000/NYT1000Data.csv", row.names = FALSE)
combinedNYT1000 %>%
  dplyr::group_by(ItemYear = as.numeric(ItemYear)) %>%
  dplyr::summarize(
    Y = n_distinct(IMDBid[Seen == "Yes"]),
    N = n_distinct(IMDBid[Seen == "No"])) %>%
  select(ItemYear, Y, N) %>%
  write.csv(.,"datasets/NYT1000/NYT1000Summary.csv", row.names = FALSE)

## Great Films Ebert
left_join(read_csv("raw-lists/ebert.csv"), myratings %>% select(IMDBid, Rating, Rated.Date), by="IMDBid") %>%
  mutate(Decade = paste0(10 * floor(as.numeric(ItemYear)/10),"s")) %>%
  mutate(Seen = ifelse(is.na(Rating), "No", "Yes")) %>%
  left_join(., Streaming.Available, by="IMDBid") %>%
  write.csv(.,"datasets/GreatFilmsEbert/Data.csv", row.names = FALSE)
## AFI Top 100 from 1998
left_join(read_csv("raw-lists/afi1998.csv"), myratings %>% select(IMDBid, Rating, Rated.Date), by="IMDBid") %>%
  mutate(Decade = paste0(10 * floor(as.numeric(ItemYear)/10),"s")) %>%
  mutate(Seen = ifelse(is.na(Rating), "No", "Yes")) %>%
  left_join(., Streaming.Available, by="IMDBid") %>%
  write.csv(.,"datasets/AFITop100/1998/Data.csv", row.names = FALSE)
## AFI Top 100 from 2007
left_join(read_csv("raw-lists/afi2007.csv"), myratings %>% select(IMDBid, Rating, Rated.Date), by="IMDBid") %>%
  mutate(Decade = paste0(10 * floor(as.numeric(ItemYear)/10),"s")) %>%
  mutate(Seen = ifelse(is.na(Rating), "No", "Yes")) %>%
  left_join(., Streaming.Available, by="IMDBid") %>%
  write.csv(.,"datasets/AFITop100/2007/Data.csv", row.names = FALSE)

#mutate(Decade = floor(as.numeric(ItemYear)/10)*10)
