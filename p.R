

library(tidyverse)
library(knitr)
#Groupement de 4 parcelles
 
read_csv("data/2023-09-29_ParacouP13AllYears.csv") %>% 
   bind_rows(read_csv("data/2023-09-29_ParacouP14AllYears.csv")) %>% 
  bind_rows(read_csv("data/2023-09-29_ParacouP15AllYears.csv")) %>% 
  bind_rows(read_csv("data/2023-09-29_ParacouP16AllYears.csv")) %>% 
  print() ->
  paracou



#calcule de diamètre
paracou  %>%
  select(idTree, CensusYear, Genus, Species, CircCorr) %>% 
  pivot_wider(
    names_from = CensusYear,
    values_from = CircCorr
  ) %>% 
  select(idTree, `1991`, `2022`, Genus, Species) %>% 
  mutate(diametre_2022 = `2022` / pi)  %>% 
  mutate(diametre_1991 = `1991` / pi)  %>% 
  print() ->
  paracoutest

 
# Calcule de la biomasse 
install.packages("BIOMASS")
library(BIOMASS)
install.packages("httr2")
  library("httr2")
  
  #Correction de la taxonomie
  
  Taxo <- correctTaxo(genus = paracou$Genus, species = paracou$Species, useCache = F, verbose = FALSE)
  
  paracou$genusCorr <- Taxo$genusCorrected
  paracou$speciesCorr <- Taxo$speciesCorrected
  
  # récupération  des familles et des ordres APG III à partir des noms de genres
  
  APG <- getTaxonomy(paracou$genusCorr, findOrder = TRUE)
  paracou$familyAPG <- APG$family
  paracou$orderAPG <- APG$order
  
  # obtention des densités du bois
  
  dataWD <- getWoodDensity(
    genus = .$Genus,
    species =.$Species,
    family = .$Family,
    stand = .$idTree,
  )
  
  
  
  #Ajout des colonnes Longitude et Latitude
  paracou  %>%
    select(idTree, CensusYear, Genus, Species, CircCorr,Lon,Lat) %>% 
    pivot_wider(
      names_from = CensusYear,
      values_from = CircCorr
    ) %>% 
    select(idTree, `1991`, `2022`, Genus, Species,Lon,Lat) %>% 
    mutate(diametre_2022 = `2022` / pi)  %>% 
    mutate(diametre_1991 = `1991` / pi)  %>% 
    print() ->
    paracoutest


  #Ajout des colonnes de la densité moyenne
  paracoutest %>% 
    mutate(moy_densite = dataWD$meanWD) %>% 
    print () -> 
    paracou_agb
  
  # Calcule de la biomasse sans information sur la hauteur en 2022
  paracou_agb %>%
    mutate(
      biomasse = computeAGB(
        D =  .$diametre_2022, 
        WD =  .$moy_densite,
        coord =   .[, c("Lon", "Lat")]
      ) 
    ) %>%
    print () ->
    paracou_agbfT
  
  
  # Calcule de la biomasse sans information sur la hauteur en 1991
  
  
  paracou_agb %>%
    mutate(
      biomasse = computeAGB(
        D =  .$diametre_1991, 
        WD =  .$moy_densite,
        coord =   .[, c("Lon", "Lat")]
      ) 
    ) %>%
    print () ->
    paracou_agbfT1
  
  # Calcule de l'accroissement en  biomasse en fontion de l'espéce sans information sur la hauteur entre 1991-2022
  
  paracoutest %>%
    select(idTree, `1991`, `2022`, Species) %>% 
    mutate(biomasse_22 = paracou_agbfT$biomasse) %>%
    mutate(biomasse_91 = paracou_agbfT1$biomasse) %>% 
    mutate(accroissement = `biomasse_22` - `biomasse_91`) %>% 
    filter(!is.na(accroissement)) %>% 
    group_by(Species) %>%
    # summarise(accroissement_sp = mean(accroissement) / 31) %>% 
    print() ->
    paracou2
  
  
  
  
    # Calcule de l'accroissement en biomasse en fonction de la taille sans information sur la hauteur
  
  
  paracoutest %>%
    select(idTree, `1991`, `2022`, diametre_2022) %>% 
    mutate(biomasse_22 = paracou_agbfT$biomasse) %>%
    mutate(biomasse_91 = paracou_agbfT1$biomasse) %>% 
    mutate(accroissement = `biomasse_22` - `biomasse_91`) %>% 
    filter(!is.na(accroissement)) %>% 
    group_by(idTree,diametre_2022) %>%
    # summarise(accroissement_sp = mean(accroissement) / 31) %>% 
    print() ->
    paracou3
  
  
  
  
  
  
  
  ## Calcule de la biomasse avec la hauteur. NB: ce n'est pas dans les objectifs
  
  #Calcule de la hauteur avec les données Chave et al. (2012) tout en utilisant l'equation 6
  
  
  
  
  dataHchave <- retrieveH(
    D = paracoutest $ diametre_2022,
    coord = paracoutest[, c("Lon", "Lat")]
  )
  
  
  
  
  #Ajout de mean et H
  
  paracoutest %>% 
    mutate(dataWD$meanWD) %>% 
    mutate(dataHchave$H) %>% 
    print () -> 
    paracou_agb
  
            
  
  
  #Calcule de la biomasse par arbre
  
  AGBtree <- computeAGB(
    D = paracou_agb$diametre_2022,
    WD = paracou_agb$`dataWD$meanWD`,
    H = paracou_agb$`dataHchave$H`
  )
  
  print(AGBtree)
  
  
  paracou_agb  %>% 
    mutate(AGBtree) %>% 
    print () -> 
    paracou_agb_t
  
  
  # Comparaison  des modeles HD avec le diamètre en 2022 
  
  
  
  
  H_22<-dataHchave$H
  H_22
  
  # Modèle log2
  
  HDmodellog22 <- modelHD(
    D =paracou_agb$diametre_2022,
    H = H_22,
    method = "log2",
    useWeight = TRUE
  ) 
  
  
  # Modèle michaelis
  
  HDmodelmi22 <- modelHD(
    D =paracou_agb$diametre_2022,
    H = H_22,
    method = "michaelis",
    useWeight = TRUE
  )  
  
  
  
  
  # Modèle weibull
  
  HDmodelw22 <- modelHD(
    D =paracou_agb$diametre_2022,
    H = H_22,
    method = "weibull",
    useWeight = TRUE
  )  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  