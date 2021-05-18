#' L'enegistrement en csv du data2_carr_nat est trop voumineux dans le script initial
#' en faisant un script qiu charge juste cette données et l'enregistre - en changeant la limite 
#' de la mémoire, ca passe...


library(tidyverse)
memory.limit(32000)
load("~/R/odaf3/data/processed_data/obs_artif_nat_carroyee_tidy.RData")  # données tidy pour tous les carreaux
data2_carr_nat %>% write_csv2("data/processed_data/obs_artif_nat_carroyee_tidy2.csv")
