#' Objectif du fichier : COnvertir les données de l'observatoire sous un format 
#' exploitable (tidy) et création de plusieurs fichiers : 
#' * un avec les référentiels (tous les zonages sans données) (3)
#' * un avec les données de synthese (2)
#' * un avec les données sous forme tidy (1)
#'  IL S'AGIT DE LA VERSION CARROYEE DES DONNEES
#' 
# Auteur : Romain CADOT
# Date: 25/01/2021


################################################################################
#             LANCER LE SCRIPT preparation_donnees_nationales.R                #
#                  POUR LES DONNEES NON CARROYEES AVANT                        #
################################################################################

memory.limit(16000)
# chargement des librairies
library(tidyverse)
library(sf)
library(lubridate)
# Import des données PNB
carr <- st_read("data/raw_data/obs_artif_conso_carroyage_2009_2019.shp")
carr_com <- st_read("data/raw_data/carr_com.shp") # réalisé via qgis
load("data/processed_data/fr.RData") #sortie de l'exploitation du fichier non carroyé de preparation_donnees_nationales.R


################################   TIDY  (1)  #####################################
# contenu des données carroyées sous forme tidy
data_carr_nat <- carr %>% 
  as_tibble() %>% 
  select(-geometry) %>% 
  select(idcarreau,naf09art10:art18inc19) %>% 
  pivot_longer(cols = naf09art10:art18inc19,names_to = "annee",values_to = "valeur") %>% 
  separate(annee, into = c("calcul_annee1","destination_annee2"),sep = 5) %>% 
  separate(calcul_annee1, into = c("calcul","annee1"),sep = 3) %>% 
  separate(destination_annee2, into = c("destination","annee2"),sep = 3) %>% 
  select(-calcul) %>% 
  pivot_wider(names_from = destination,values_from = valeur) %>% 
  rename(artificialisation = art,
         activite = act,
         habitat = hab,
         mixte = mix,
         inconnu=inc) %>% 
  mutate(annee1=as.Date(paste0(20,annee1),"%Y"),
         annee2=as.Date(paste0(20,annee2),"%Y"))


# sélection des données à remettre sous forme exploitable V2 (avec la destination en une seule colonne et sans le total)
data2_carr_nat <- carr %>% 
  as_tibble() %>% 
  select(-geometry) %>% 
  select(idcarreau,naf09art10:art18inc19) %>% 
  pivot_longer(cols = naf09art10:art18inc19,names_to = "annee",values_to = "valeur") %>% 
  separate(annee, into = c("calcul_annee1","destination_annee2"),sep = 5) %>% 
  separate(calcul_annee1, into = c("calcul","annee1"),sep = 3) %>% 
  separate(destination_annee2, into = c("destination","annee2"),sep = 3) %>% 
  filter(calcul=="art") %>% 
  select(-calcul)  %>% 
  mutate(annee1=as.Date(paste0(20,annee1),"%Y"),
         annee2=as.Date(paste0(20,annee2),"%Y"))

data2_carr_nat$destination <- fct_recode(data2_carr_nat$destination,
                                    "Habitat" = "hab",
                                    "Activité" = "act",
                                    "Inconnu" = "inc",
                                    "Mixte" = "mix")



# export des données tidy
save(data_carr_nat,data2_carr_nat,file = "data/processed_data/obs_artif_nat_carroyee_tidy.RData")
data_carr_nat %>% write_csv2("data/processed_data/obs_artif_nat_carroyee_tidy.csv")
#data2_carr_nat %>% write_csv2("data/processed_data/obs_artif_nat_carroyee_tidy2.csv")  #trop volumineux ?

################## données de synthèse par carreaux (2) ########################

referentiel_carr_nat <- carr %>%         # ajout des évolutions entre 09 et 18 en absolu et relatif
  as_tibble() %>%       # caclculs de référence mais sans le référentiel geo
  select(-geometry) %>% 
  mutate(evolnaf09art19 =    naf18art19 - naf09art10,
         pcevolnaf09art19 = (naf18art19 - naf09art10)/naf09art10,
         evolart09act19 =    art18act19 - art09act10,
         pcevolart09act19 = (art18act19 - art09act10)/art09act10,
         evolart09hab19 =    art18hab19 - art09hab10,
         pcevolart09hab19 = (art18hab19 - art09hab10)/art09hab10,
         evolart09inc19 =    art18inc19 - art09inc10,
         pcevolart09inc19 = (art18inc19 - art09inc10)/art09inc10,
         evolart09mix19 =    art18mix19 - art09mix10,
         pcevolart09mix19 = (art18mix19 - art09mix10)/art09mix10) %>% 
  select(idcarreau,naf09art19:art09inc19,pcevolnaf09art19:pcevolart09mix19)

# export des données
save(referentiel_carr_nat,file = "data/processed_data/referentiel_carr_nat.RData")
referentiel_carr_nat %>% write_csv2("data/processed_data/referentiel_carr_nat.csv")




###################### creation d'un référentiel carroyé (3) ###################
#carr_com <- carr_com %>% as_tibble() %>% select(-geometry) #simplification de la table carr_com pour avoir juste les passages entre idcarreau et idcom
fr <- fr %>% as_tibble() %>% select(-geometry) #simplication du référentiel sous format tibble et sans geometry
fr_carr<- left_join(carr_com,fr,by=c("IDCOM"="IDCOM")) %>% filter(!is.na(IDCOM)) #jointure des données du référentiel au SF carr_ref, avec suppression des NA car en dehors des frontièeres françaises

# export pour exploitations
save(fr_carr, file = "data/processed_data/fr_carroyee.RData")
st_write(fr_carr,"data/processed_data/fr_caroyee.shp" , layer = "fr_carroyee", OVERWRITE=TRUE, append = FALSE)





# tests
#version 59
#test <- left_join(carr_ref,carr_ref_data)%>% filter(IDDEP=="59")
#mapview::mapView(test,zcol=evolnaf09art19)


#essai de la jointure directement dans R

#carr <- st_transform(carr,crs = 4326)
#fr <- st_transform(fr,crs = 4326)
#ref <- st_join(carr,fr)
#mapview::mapView(carr_ref)
#mapview::mapview(fr)



#reprendre données 0919 de carr
#faire une jointure gauche avec les donées tibble de fr
