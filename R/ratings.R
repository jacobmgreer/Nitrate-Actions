library(tidyverse)
library(magrittr)
library(numform)
library(scales)

options(readr.show_col_types = FALSE)
options(warn=-1)

## Watchlist
myratings <-
  read.csv("ratings/ratings.csv") %>%
  bind_rows(., read.csv("ratings/watchlist.csv")) %>%
  #left_join(., read.csv("https://www.imdb.com/list/ls507245240/export") %>% select(Const, Modified) %>% rename(AFI = Modified), by=join_by(Const)) %>%
  #left_join(., read.csv("https://www.imdb.com/list/ls507032905/export") %>% select(Const, Modified) %>% rename(Theater = Modified), by=join_by(Const)) %>%
  select(-c(Position, Description, Created, Modified, Genres, Directors, Release.Date, URL)) %>%
  rename(Runtime = Runtime..mins.) %>%
  distinct(Const, .keep_all = TRUE) %>%
  arrange(desc(Date.Rated)) %T>%
  write.csv(., "ratings/formatted.csv", row.names = FALSE)

## Oscar Ceremony Data for Summary and Graph
OscarsCorrected <-
  left_join(read_csv("raw-lists/OscarCeremonies.csv"), myratings, by=c("FilmID" = "Const")) %>%
  select(!c(Runtime, Year, Title.Type, Title, Date.Rated)) %T>%
  write.csv(., "datasets/Oscars/OscarsTracking.csv", row.names = FALSE)
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
  write.csv(.,"datasets/Oscars/Oscars-Chart-Data.csv", row.names = FALSE)

## Oscar Summary by Ceremony
OCSumQuery <-
  OscarsCorrected %>%
  filter(!is.na(FilmID)) %>%
  filter(!AwardType %in% c("Writing, Title", "Director,Assistant", "Direction, Dance")) %>%
  select(AwardCeremony, AwardType, FilmID, Your.Rating) %>%
  distinct %>%
  dplyr::group_by(FilmID) %>%
  mutate(Seen = ifelse(is.na(Your.Rating), FALSE, TRUE))

Oscars.Ceremonies <-
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
  write.csv(.,"datasets/Oscars/Oscars-Summary-Ceremonies.csv", row.names = FALSE)

## Oscar Summary by Award
Oscars.Awards <-
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
  write.csv(.,"datasets/Oscars/Oscars-Summary-Awards.csv", row.names = FALSE)

Oscars.Films <-
  OscarsCorrected %>%
  group_by(AwardCeremony, FilmID, FilmName, Your.Rating, IMDb.Rating, Num.Votes) %>%
  summarize(
    Nominations = n_distinct(AwardCategory),
    Losses = n_distinct(AwardCategory[is.na(AwardWinner)]),
    Wins = Nominations - Losses) %T>%
  write.csv(.,"datasets/Oscars/Oscars-Summary-Films.csv", row.names = FALSE)

## AMPAS-International Data for Summary and Graph
combinedAMPASIntl <-
  left_join(read_csv("raw-lists/International-Submissions.csv"), myratings, by="Const") %>%
  mutate(
    Seen = ifelse(is.na(Your.Rating), "No", "Yes"),
    Ceremony = paste0(Ceremony, "-", Ceremony + 1928)) %T>%
  write.csv(.,"datasets/AMPAS-International/Data.csv", row.names = FALSE)
combinedAMPASIntl %>%
  dplyr::group_by(Ceremony) %>%
  dplyr::summarize(
    Y = n_distinct(Const[Seen == "Yes"]),
    N = n_distinct(Const[Seen == "No"])) %>%
  select(Ceremony, Y, N) %>%
  write.csv(.,"datasets/AMPAS-International/Summary.csv", row.names = FALSE)

## NYT-1000 Data for Summary and Graph
combinedNYT1000 <-
  left_join(read_csv("raw-lists/NYT1000.csv"), myratings, by="Const") %>%
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
  left_join(read_csv("raw-lists/NBR-Awards.csv"), myratings, by=c("FilmID" = "Const")) %>%
  mutate(Seen = ifelse(is.na(Your.Rating), "No", "Yes")) %T>%
  write.csv(.,"datasets/NBR/NBRData.csv", row.names = FALSE)
combinedNBR %>%
  dplyr::group_by(Year = AwardCeremony) %>%
  dplyr::summarize(
    Y = n_distinct(FilmID[Seen == "Yes"]),
    N = n_distinct(FilmID[Seen == "No"])) %>%
  select(Year, Y, N) %>%
  write.csv(.,"datasets/NBR/NBRSummary.csv", row.names = FALSE)

## NBR Summary by Ceremony
NBR.Ceremonies <-
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
  write.csv(.,"datasets/NBR/NBR-Summary-Ceremonies.csv", row.names = FALSE)

## Great Films Ebert
List.Ebert <-
  left_join(read_csv("raw-lists/Ebert-Great-Movies.csv"), myratings, by="Const") %>%
    mutate(Decade = paste0(10 * floor(Year/10),"s")) %>%
    mutate(Seen = ifelse(is.na(Your.Rating), "No", "Yes")) %T>%
    write.csv(.,"datasets/GreatFilmsEbert/Data.csv", row.names = FALSE)
