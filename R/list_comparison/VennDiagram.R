library("SuperExactTest")
library("tidyverse")
library("jsonlite")

x <- list(
  AFI1 = lists$AFI.1998$Const %>% unique(),
  AFI2 = lists$AFI.2007$Const %>% unique(),
  AMPAS = lists$AMPAS.Awards$Const %>% unique(),
  NBR = lists$NBR.Awards$Const %>% unique(),
  EBERT = lists$Ebert.Great.Movies$Const %>% unique(),
  LAFCA = lists$LAFCA.Awards$Const %>% unique(),
  NSFC = lists$NSFC.Awards$Const %>% unique(),
  NYFCC = lists$NYFCC.Awards$Const %>% unique(),
  INTL = lists$International.Submissions$imdb %>% unique(),
  NYT = lists$NYT1000$Const %>% unique()
)

ress <- summary(supertest(x))

Set.Edges <-
  data.frame(ress$Table, row.names = NULL) %>%
  mutate(
    Label = case_when(
      Degree == 1 & Intersections == "NYT" ~
        "NYT 1000 Essential Films",
      Degree == 1 & Intersections == "AFI1" ~
        "AFI Top 100 1998",
      Degree == 1 & Intersections == "AFI2" ~
        "AFI Top 100 2007",
      Degree == 1 & Intersections == "EBERT" ~
        "Roger Ebert's 'Great Films'",
      Degree == 1 & Intersections == "NBR" ~
        "National Board of Review",
      Degree == 1 & Intersections == "AMPAS" ~
        "Academy Awards",
      Degree == 1 & Intersections == "INTL" ~
        "International Submissions to the Academy Awards",
      Degree == 1 & Intersections == "NYFCC" ~
        "New York Film Critic Circle",
      Degree == 1 & Intersections == "LAFCA" ~
        "Los Angeles Film Critic Association",
      Degree == 1 & Intersections == "NSFC" ~
        "National Society of Film Critics"),
    Warnings = gsub(" & ", ",", Intersections),
    Intersections = gsub(" & ", "','", Intersections),
    Intersections = paste0("['", Intersections, "']")) %>%
  select(Intersections, Label, Observed.Overlap, everything()) %>%
  arrange(Degree) %>%
  rename(
    sets = Intersections,
    label = Label,
    size = Observed.Overlap
  ) %>%
  filter(size != 0)

cat(sprintf(
      read_file('R/json/template.json'),
        gsub("\\]\"", "\\]",
        gsub("\"\\[", "\\[",
           prettify(toJSON(Set.Edges %>% select(sets,label,size,Warnings)),
               indent = 4)))),
    file="output/Venn.json")

rm(x, ress)
