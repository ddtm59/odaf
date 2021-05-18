#' Objectif du fichier : COnvertir les données de l'observatoire sous un format 
#' exploitable (tidy) et création de plusieurs fichiers : 
#' * un avec les référentiels (tous les zonages sans données) (3)
#' * un avec les données de synthese (2)
#' * un avec les données sous forme tidy (1)
#' 
# Auteur : Romain CADOT
# Date: 25/01/2021

# load in libraries
library(tidyverse)
library(sf)
library(lubridate)

# load in the data
obs_artif_conso_com_2009_2019.csv <- read_delim("data/raw_data/obs_artif_conso_com_2009_2019.csv", 
                                            ";", escape_double = FALSE, col_types = cols(epci20 = col_character()), 
                                            trim_ws = TRUE)

########################## tydification des données (1)#########################

data_nat <- obs_artif_conso_com_2009_2019.csv %>%
  select(idcom,idreg,naf09art10:art18inc19) %>%
  filter(idreg <=100) %>%  #pour supprimer les données dom-tom
  pivot_longer(cols = naf09art10:art18inc19,names_to = "annee",values_to = "valeur")%>% 
  separate(annee, into = c("calcul_annee1","destination_annee2"),sep = 5) %>% 
  separate(calcul_annee1, into = c("calcul","annee1"),sep = 3) %>% 
  separate(destination_annee2, into = c("destination","annee2"),sep = 3) %>% 
  select(-calcul) %>% 
  pivot_wider(names_from = destination,values_from = valeur) %>%
  select(-idreg) %>% 
  rename(artificialisation = art,
         activite = act,
         habitat = hab,
         mixte = mix,
         inconnu=inc) %>% 
  mutate(annee1=as.Date(paste0(20,annee1),"%Y"),
         annee2=as.Date(paste0(20,annee2),"%Y"))
  
# sélection des données à remettre sous forme exploitable V2 (avec la destination en une seule colonne et sans le total)
data2_nat <- obs_artif_conso_com_2009_2019.csv %>%
  select(idcom,idreg,naf09art10:art18inc19) %>%
  filter(idreg <=100) %>% 
  pivot_longer(cols = naf09art10:art18inc19,names_to = "annee",values_to = "valeur")%>% 
  separate(annee, into = c("calcul_annee1","destination_annee2"),sep = 5) %>% 
  separate(calcul_annee1, into = c("calcul","annee1"),sep = 3) %>% 
  separate(destination_annee2, into = c("destination","annee2"),sep = 3) %>% 
  filter(calcul=="art") %>% 
  select(-idreg,-calcul)  %>% 
  mutate(annee1=as.Date(paste0(20,annee1),"%Y"),
         annee2=as.Date(paste0(20,annee2),"%Y"))
data2_nat$destination <- fct_recode(data2_nat$destination,
             "Habitat" = "hab",
             "Activité" = "act",
             "Inconnu" = "inc",
             "Mixte" = "mix")

# export pour exploitations
save(data_nat,data2_nat,file = "data/processed_data/obs_artif_nat_tidy.RData")
data_nat %>% write_csv2("data/processed_data/obs_artif_nat_tidy.csv")
data2_nat %>% write_csv2("data/processed_data/obs_artif_nat_tidy_2.csv")


############################### DONNEES DE SYNTHSES (2) ########################
# Gestion du référentiel des données artificialisation, c'est à dire les données artificialisatoin agrégées ----
# selection des données à garder mais à compléter pour le référentiel (déjà en format geo)
# creation de referentiel_nat incorporant les données agrégées intiialement dans le fichier

referentiel_nat <- obs_artif_conso_com_2009_2019.csv %>% 
  select(idcom,nafart0919:surfcom20) %>% 
  mutate(artpop1217=as.numeric(artpop1217))
  

# calcul de l'artificalisation sur la période 12-17
# on choisit annee1 car c'est en fait l'année pendant laquelle a été artificialisée les espaces
data_nat_periodes <- data_nat %>% 
  filter(year(annee1) %in% c("2012","2013","2014","2015","2016","2017")) %>% 
  group_by(idcom) %>% 
  summarise(nafart1217 = sum(artificialisation,na.rm = TRUE),
            arthab1217 = sum(habitat,na.rm = TRUE),
            artact1217 = sum(activite,na.rm = TRUE),
            artmix1217 = sum(mixte,na.rm = TRUE),
            artinc1217 = sum(inconnu,na.rm = TRUE))

# jointure entre les données intiiales et les données calculées
referentiel_nat<- left_join(referentiel_nat,data_nat_periodes, by=c("idcom"="idcom"))




#' ANCIEN CODE POUR CLC2018
#' #' Incorporation des données sur l'artificialisation initiale par commune ####################################
#' En provenance ddu site : https://www.observatoire-des-territoires.gouv.fr/outils/cartographie-interactive/#c=indicator&f=1&i=occup_sol.p_surf_occup_sol&s=2018&view=map36
# 
# part_artif_clc2018 <- read_delim("~/R/odaf3/data/raw_data/part_artif_clc2018.csv", 
#                                  ";", escape_double = FALSE, col_types = cols(X3 = col_double()), 
#                                  trim_ws = TRUE, skip = 2)
#' ajout de la données part deja artif en 2018 dans le referentiel nat et 
#' de sa valeur absolue et déduction de la valeur avant les données d'artifification
# referentiel_nat <- referentiel_nat %>% 
#   left_join(part_artif_clc2018,by=c("idcom"="Code")) %>% 
#   rename(partart18=`Part des surfaces selon l’occupation du sol 2018`) %>% 
#   mutate(partart18=na_if(partart18,0),
#          partart18_en_m2=(partart18/100)*surfcom20, #/100 car parart18 est en % pas en 0,01
#          partart08=partart18_en_m2-nafart0919,
#          partart12=partart18_en_m2-nafart1217,
#          partart08=ifelse(partart08>0,partart08,NA),
#          partart12=ifelse(partart12>0,partart12,NA))

#' En provenance du SDES : 
#'  https://www.statistiques.developpement-durable.gouv.fr/corine-land-cover-0

clc_etat_com_n1 <- read_delim("data/raw_data/clc_etat_com_n1.csv", 
                              ";", escape_double = FALSE, col_types = cols(ANNEE = col_date(format = "%Y")), 
                              trim_ws = TRUE, skip = 3)

referentiel_nat <- referentiel_nat %>% 
  left_join(clc_etat_com_n1 %>% filter(year(ANNEE)=="2018") %>% select(NUM_COM,CLC_1),by=c("idcom"="NUM_COM")) %>% 
  rename(partart18=CLC_1) %>% 
  mutate(partart18=partart18*10000) %>% 
  left_join(clc_etat_com_n1 %>% filter(year(ANNEE)=="2012",base =="CLC 2012 révisée") %>% select(NUM_COM,CLC_1),by=c("idcom"="NUM_COM")) %>% 
  rename(partart12=CLC_1) %>% 
  mutate(partart12=partart12*10000)
  
  
# export pour exploitations
save(referentiel_nat, file = "data/processed_data/referentiel_nat.RData")
write_csv2(referentiel_nat, file = "data/processed_data/referentiel_nat.csv")




############## creation d'un réfetniel avec tous les zonages (3) ###############
# creation d'un fichier avec tous les perimetres et corrections pour avoir les epci du nord a jour----


# import shp pour référentiel
obs_artif_conso_com_2009_2019.shp <- st_read("data/raw_data/obs_artif_conso_com_2009_2019.shp")
fr <- obs_artif_conso_com_2009_2019.shp %>% select(IDCOM:CATEAAV202)

# ajout des perimetres suplémentaires
load("data/raw_data/nomenclature_insee.RData")
appartenance <- appartenance %>% 
  as_tibble() %>% 
  select(CODGEO,lib_arr,lib_cv,lib_uu2020:LIB_TDUU2017) %>%
  rename(LIB_CATEAAV2020 = LIB_CATEAAV1010) #correction faute de frappe 1010 au lieu de 2020

# jointure des deux référentiels
fr <- left_join(fr,appartenance,by=c("IDCOM"="CODGEO"))

# ajout des nouvelles intercos pour le 59 dans le fichier nationale
shp <- shp %>% as_tibble() %>% select(insee_comm,Nepci,nomepci)
fr <- left_join(fr,shp,by=c("IDCOM"="insee_comm"))
fr <- fr %>% mutate(EPCI20 = if_else(IDDEP=="59",Nepci,EPCI20),
                    EPCI20TXT=if_else(IDDEP=="59",nomepci,EPCI20TXT)) %>%
  select(-Nepci,-nomepci)

# ajout des scot59 dans le fichier national
scot_59 <-
  st_read("data/raw_data/L_SCOT_COMMUNE_S_D59.shp") %>% as_tibble() %>% select(ccocom, scot)
fr <- fr %>% left_join(scot_59, by = c("IDCOM" = "ccocom")) %>%
  mutate(
    lib_scot = case_when(
      scot == "591557" ~ "SCoT de Sambre Avesnois",
      scot == "59264"  ~ "SCoT du Cambrésis",
      scot == "595"    ~ "SCoT de la MEL",
      scot == "59207"  ~ "SCoT de Flandres-Dunkerque",
      scot == "59105"  ~ "SCoT de Flandre Intérieure",
      scot == "596"    ~ "SCoT de Valenciennes",
      scot == "59901"  ~ "SCoT du grand Douaisis",
      EPCI20TXT == "CC Flandre Lys" ~ "SCoT de Flandre Intérieure"
    )
  )

# export pour exploitations
save(fr, file = "data/processed_data/fr.RData")
st_write(fr,"data/processed_data/fr.shp" , layer = "fr", OVERWRITE=TRUE, append = FALSE)
