library(tidyverse)
library(magrittr)
library(numform)
library(scales)

options(readr.show_col_types = FALSE)
options(warn=-1)

dir.create("_site", showWarnings = FALSE)
dir.create("_site/Oscars", showWarnings = FALSE)
dir.create("_site/AMPAS-International", showWarnings = FALSE)
dir.create("_site/NBR", showWarnings = FALSE)
dir.create("_site/NYT1000", showWarnings = FALSE)
dir.create("_site/GreatFilmsEbert", showWarnings = FALSE)

## Watchlist
lists <- 
  list.files("lists", full.names = TRUE) %>%
  as_tibble() %>%
  rowwise() %>%
  reframe(
    value,
    cols = length(as.list(strsplit(readLines(value, n=1), ",")[[1]])),
    file = 
      case_when(
        cols == 14 ~ "ratings",
        cols == 18 ~ "watchlist"
      )
  )

list_ratings <-
  lists %>%
  filter(file == "ratings") %>%
  pull(value) %>%
  read.csv(.) %>%
  mutate(IMDb.Rating = as.numeric(IMDb.Rating))

list_watchlist <-
  lists %>%
  filter(file == "watchlist") %>%
  pull(value) %>%
  read.csv(.) %>%
  filter(!Const %in% list_ratings$Const)

myratings <-
  list_ratings %>%
  bind_rows(., list_watchlist) %>%
  select(-c(Position, Description, Created, Modified, Genres, Directors, Release.Date, URL)) %>%
  rename(Runtime = Runtime..mins.) %>%
  distinct(Const, .keep_all = TRUE) %>%
  arrange(desc(Date.Rated)) %T>%
  write_csv(., "_site/formatted.csv")

## Oscar Ceremony Data for Summary and Graph
OscarsCorrected <-
  left_join(readRDS("RData/OscarCeremonies.rds"), myratings, by=c("FilmID" = "Const")) %>%
  select(!c(Runtime, Year, Title.Type, Title, Date.Rated)) %T>%
  write_csv(., "_site/Oscars/OscarsTracking.csv")
OscarSummary <-
  OscarsCorrected %>%
  filter(!is.na(FilmID)) %>%
  mutate(AwardWinner = ifelse(AwardWinner == "Winner",TRUE,FALSE)) %>%
  select(AwardCeremony, AwardWinner, FilmID, Your.Rating) %>%
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
  select(-Year) %T>%
  write_csv(., "_site/Oscars/Oscars-Chart-Data.csv")

## Oscar Summary by Ceremony
OCSumQuery <-
  OscarsCorrected %>%
  filter(!is.na(FilmID)) %>%
  filter(!AwardType %in% c("Writing, Title", "Director,Assistant", "Direction, Dance")) %>%
  select(AwardCeremony, AwardType, FilmID, Your.Rating) %>%
  distinct %>%
  dplyr::group_by(FilmID) %>%
  mutate(Seen = ifelse(is.na(Your.Rating), FALSE, TRUE))

OscarsCeremonies <-
  OCSumQuery %>%
  dplyr::group_by(AwardCeremony, AwardType) %>%
  dplyr::summarise(Percentage = round(n_distinct(FilmID[Seen == TRUE]) / n_distinct(FilmID), digits = 1)) %>%
  spread(., AwardType, Percentage, fill = NA) %>%
  left_join(
    OCSumQuery %>%
    dplyr::group_by(AwardCeremony) %>%
    dplyr::summarize(
      Films.Y = n_distinct(FilmID[Seen == TRUE]),
      Films.N = n_distinct(FilmID[Seen == FALSE]),
      Films = n_distinct(FilmID),
      Percentage = percent(n_distinct(FilmID[Seen == TRUE]) / n_distinct(FilmID), accuracy = 1))) %>%
  bind_rows(
    OCSumQuery %>%
    dplyr::group_by(AwardType) %>%
    dplyr::summarize(
      AwardCeremony = "Not Seen",
      Films.N = n_distinct(FilmID[Seen == FALSE])) %>%
    spread(., AwardType, Films.N, fill = NA)) %>%
  bind_rows(
    OCSumQuery %>%
    dplyr::group_by(AwardType) %>%
    dplyr::summarize(
      AwardCeremony = "Seen",
      Films.Y = n_distinct(FilmID[Seen == TRUE])) %>%
    spread(., AwardType, Films.Y, fill = NA)) %>%
  bind_rows(
    OCSumQuery %>%
    dplyr::group_by(AwardType) %>%
    dplyr::summarize(
      AwardCeremony = "Total",
      Film = n_distinct(FilmID)) %>%
    spread(., AwardType, Film, fill = NA) %>%
    left_join(
      OCSumQuery %>%
        dplyr::group_by(AwardCeremony = "Total") %>%
        dplyr::summarize(
          Films.Y = n_distinct(FilmID[Seen == TRUE]),
          Films.N = n_distinct(FilmID[Seen == FALSE]),
          Films = n_distinct(FilmID),
          Percentage = percent(n_distinct(FilmID[Seen == TRUE]) / n_distinct(FilmID), accuracy = 1)))) %>%
  arrange(AwardCeremony) %T>%
  write_csv(., "_site/Oscars/Oscars-Summary-Ceremonies.csv")

## Oscar Summary by Award
OscarsAwards <-
  OscarsCorrected %>%
  filter(!is.na(FilmID)) %>%
  mutate(AwardWinner = ifelse(AwardWinner == "Winner", TRUE, FALSE)) %>%
  select(AwardCeremony, AwardType, AwardWinner, FilmID, Your.Rating) %>%
  distinct %>%
  dplyr::group_by(FilmID) %>%
  dplyr::mutate(
    filmwon=ifelse(any(AwardWinner), TRUE, FALSE),
    filmwon=ifelse(all(is.na(filmwon)), FALSE, filmwon)
  ) %>%
  dplyr::mutate(
    keep_row=ifelse(filmwon, AwardWinner, TRUE)) %>%
  dplyr::filter(!(filmwon == TRUE & is.na(keep_row))) %>%
  ungroup %>%
  mutate(
    Seen = ifelse(is.na(Your.Rating), FALSE, TRUE),
    Year = sub('.*-', '', AwardCeremony),
    Ceremony = ifelse(f_ordinal(sub("\\-.*", "", str_remove(AwardCeremony, "^0+"))) == 13, "13th", f_ordinal(sub("\\-.*", "", str_remove(AwardCeremony, "^0+")))),
    Menu = paste0("<h5>", AwardType, "</h5>")) %>%
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
    Nominee.Per = round(Nominee.Y/(Nominee.Y+Nominee.N), digits=2)) %T>%
  write_csv(., "_site/Oscars/Oscars-Summary-Awards.csv")

OscarsFilms <-
  OscarsCorrected %>%
  group_by(AwardCeremony, FilmID, FilmName, Your.Rating, IMDb.Rating, Num.Votes) %>%
  summarize(
    Nominations = n_distinct(AwardCategory),
    Losses = n_distinct(AwardCategory[is.na(AwardWinner)]),
    Wins = Nominations - Losses) %T>%
  write_csv(., "_site/Oscars/Oscars-Summary-Films.csv")

## AMPAS-International Data for Summary and Graph
combinedAMPASIntl <-
  left_join(readRDS("RData/International-Submissions.rds"), myratings, by="Const") %>%
  mutate(
    Seen = ifelse(is.na(Your.Rating), "No", "Yes"),
    Ceremony = paste0(Ceremony, "-", Ceremony + 1928)) %T>%
  write_csv(., "_site/AMPAS-International/Data.csv")
combinedAMPASIntl %>%
  dplyr::group_by(Ceremony) %>%
  dplyr::summarize(
    Y = n_distinct(Const[Seen == "Yes"]),
    N = n_distinct(Const[Seen == "No"])) %>%
  select(Ceremony, Y, N) %>%
  write_csv(., "_site/AMPAS-International/Summary.csv")

## NYT-1000 Data for Summary and Graph
combinedNYT1000 <-
  left_join(readRDS("RData/NYT1000.rds"), myratings %>% select(-Year), by="Const") %>%
    mutate(Seen = ifelse(is.na(Your.Rating), "No", "Yes")) %T>%
    write_csv(., "_site/NYT1000/NYT1000Data.csv")
combinedNYT1000 %>%
  dplyr::group_by(Year) %>%
  dplyr::summarize(
    Y = n_distinct(Const[Seen == "Yes"]),
    N = n_distinct(Const[Seen == "No"])) %>%
  select(Year, Y, N) %>%
  write_csv(., "_site/NYT1000/NYT1000Summary.csv")

## NBR Data for Graph
combinedNBR <-
  left_join(readRDS("RData/NBR-Awards.rds"), myratings, by=c("FilmID" = "Const")) %>%
  mutate(Seen = ifelse(is.na(Your.Rating), "No", "Yes")) %T>%
  write_csv(., "_site/NBR/NBRData.csv")
combinedNBR %>%
  dplyr::group_by(Year = AwardCeremony) %>%
  dplyr::summarize(
    Y = n_distinct(FilmID[Seen == "Yes"]),
    N = n_distinct(FilmID[Seen == "No"])) %>%
  select(Year, Y, N) %>%
  write_csv(., "_site/NBR/NBRSummary.csv")

## NBR Summary by Ceremony
NBRCeremonies <-
  combinedNBR %>%
  filter(!is.na(FilmID)) %>%
  select(AwardCeremony, GeneralizedCategory, FilmID, Your.Rating) %>%
  distinct %>%
  dplyr::group_by(FilmID) %>%
  mutate(Seen = ifelse(is.na(Your.Rating), FALSE, TRUE)) %>%
  dplyr::group_by(AwardCeremony, GeneralizedCategory) %>%
  dplyr::summarise(Percentage = round(n_distinct(FilmID[Seen == TRUE]) / n_distinct(FilmID), digits = 1)) %>%
  spread(., GeneralizedCategory, Percentage, fill = NA) %>%
  left_join(
    .,
    combinedNBR %>%
      filter(!is.na(FilmID)) %>%
      select(AwardCeremony, FilmID, Your.Rating) %>%
      distinct %>%
      dplyr::group_by(FilmID) %>%
      mutate(Seen = ifelse(is.na(Your.Rating), FALSE, TRUE)) %>%
      dplyr::group_by(AwardCeremony) %>%
      dplyr::summarize(
        Films.Y = n_distinct(FilmID[Seen == TRUE]),
        Films.N = n_distinct(FilmID[Seen == FALSE]),
        Films = n_distinct(FilmID),
        Percentage = percent(n_distinct(FilmID[Seen == TRUE]) / n_distinct(FilmID), accuracy = 1))
  ) %>%
  bind_rows(
    .,
    combinedNBR %>%
      filter(!is.na(FilmID)) %>%
      select(AwardCeremony, GeneralizedCategory, FilmID, Your.Rating) %>%
      distinct %>%
      dplyr::group_by(FilmID) %>%
      mutate(Seen = ifelse(is.na(Your.Rating), FALSE, TRUE)) %>%
      dplyr::group_by(GeneralizedCategory) %>%
      dplyr::summarize(AwardCeremony = "Not Seen", Films.N = n_distinct(FilmID[Seen == FALSE])) %>%
      spread(., GeneralizedCategory, Films.N, fill = NA)
  ) %>%
  bind_rows(
    .,
    combinedNBR %>%
      filter(!is.na(FilmID)) %>%
      select(AwardCeremony, GeneralizedCategory, FilmID, Your.Rating) %>%
      distinct %>%
      dplyr::group_by(FilmID) %>%
      mutate(Seen = ifelse(is.na(Your.Rating), FALSE, TRUE)) %>%
      dplyr::group_by(GeneralizedCategory) %>%
      dplyr::summarize(AwardCeremony = "Seen", Films.Y = n_distinct(FilmID[Seen == TRUE])) %>%
      spread(., GeneralizedCategory, Films.Y, fill = NA)
  ) %>%
  bind_rows(
    .,
    combinedNBR %>%
      filter(!is.na(FilmID)) %>%
      select(AwardCeremony, GeneralizedCategory, FilmID, Your.Rating) %>%
      distinct %>%
      dplyr::group_by(FilmID) %>%
      mutate(
        Seen = ifelse(is.na(Your.Rating), FALSE, TRUE)) %>%
      dplyr::group_by(GeneralizedCategory) %>%
      dplyr::summarize(
        AwardCeremony = "Total",
        Film = n_distinct(FilmID)) %>%
      spread(., GeneralizedCategory, Film, fill = NA) %>%
      left_join(.,
                combinedNBR %>%
                  filter(!is.na(FilmID)) %>%
                  select(AwardCeremony, GeneralizedCategory, FilmID, Your.Rating) %>%
                  distinct %>%
                  dplyr::group_by(FilmID) %>%
                  mutate(
                    Seen = ifelse(is.na(Your.Rating), FALSE, TRUE)) %>%
                  dplyr::group_by(AwardCeremony = "Total") %>%
                  dplyr::summarize(
                    Films.Y = n_distinct(FilmID[Seen == TRUE]),
                    Films.N = n_distinct(FilmID[Seen == FALSE]),
                    Films = n_distinct(FilmID),
                    Percentage = percent(n_distinct(FilmID[Seen == TRUE]) / n_distinct(FilmID), accuracy = 1)))
  ) %T>%
  write_csv(.,"_site/NBR/NBR-Summary-Ceremonies.csv")

## Great Films Ebert
## note, Year needs to be added to rds so that it doesn't pull rereleases from ratings data
ListEbert <-
  left_join(readRDS("RData/Ebert-Great-Movies.rds"), myratings, by="Const") %>%
    mutate(Decade = paste0(10 * floor(Year/10),"s")) %>%
    mutate(Seen = ifelse(is.na(Your.Rating), "No", "Yes")) %T>%
    write_csv(., "_site/GreatFilmsEbert/Data.csv")
