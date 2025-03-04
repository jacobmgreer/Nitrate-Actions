library(tidyverse, arrow)

intl <- read_csv("/Users/jakeob/Documents/GitHub/Nitrate-Actions/RData/International-Submissions.csv")

saveRDS(intl, "/Users/jakeob/Documents/GitHub/Nitrate-Actions/RData/International-Submissions.rds")



ampas <- read_csv("RData/OscarCeremonies.csv")

saveRDS(ampas, "RData/OscarCeremonies.rds")

nbr <- read_csv("/Users/jakeob/Documents/GitHub/Nitrate-Actions/RData/NBR-Awards.csv")

saveRDS(nbr, "RData/NBR-Awards.rds")