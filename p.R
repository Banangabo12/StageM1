

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
  
  paracoutest  %>%
    select(idTree, `1991`, `2022`, Species) %>% 
    mutate(paracou_agbfT1) %>% 
    mutate(paracou_agbfT) %>% 
    mutate(accroissement = `2022` - `1991`) %>% 
    filter(!is.na(accroissement)) %>% 
    group_by(Species) %>%
    summarise(accroissement_sp = mean(accroissement) / 31) %>% 
    print() ->
    paracou2
  
  
  
  
  # Calcule de l'accroissement en biomasse en fonction de la taille sans information sur la hauteur
  
  
  paracoutest  %>%
    select(idTree, `1991`, `2022`, Species) %>% 
    mutate(paracou_agbfT1) %>% 
    mutate(paracou_agbfT) %>% 
    mutate(accroissement = `2022` - `1991`) %>% 
    filter(!is.na(accroissement)) %>% 
    group_by(diametre_2022) %>%
    summarise(accroissement_sp = mean(accroissement) / 31) %>% 
    print() ->
    paracou3
  
  
  

  
  ## Calcule de la biomasse avec la hauteur
  
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
  
  
  
  
  
  library(BIOMASS)
  
  # Acroissement moyen en fonction de l'espèce
  
  paracou  %>%
    select(idTree, CensusYear, Species, CircCorr) %>% 
    pivot_wider(
      names_from = CensusYear,
      values_from = CircCorr
    ) %>% 
    select(idTree, `1991`, `2022`, Species) %>% 
    mutate(accroissement = `2022` - `1991`) %>% 
    filter(!is.na(accroissement)) %>% 
    group_by(Species) %>%
    summarise(accroissement_sp = mean(accroissement) / 31) %>% 
    print() ->
    paracou2
  
  
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
  
  
  
  #Calcule de l'accroissement moyen du diamtre en foction de l'epèce 
  
  
  paracoutest  %>%
    mutate(espece = paste(Genus, Species)) %>% 
    select(idTree, `1991`, `2022`, espece,  diametre_2022) %>% 
    mutate(accroissement = `2022` - `1991`) %>% 
    filter(!is.na(accroissement)) %>% 
    group_by(espece) %>%
    summarise(accroissement_sp = mean(accroissement) / 31) %>% 
    print() ->
    paracoutest2
  
  
  
  #Calcule de l'accroissement moyen du diamtre en fonction de l'individu
  
  paracoutest  %>%
    
    select(idTree, `1991`, `2022`, Species, diametre_2022) %>% 
    mutate(accroissement = `2022` - `1991`) %>% 
    filter(!is.na(accroissement)) %>% 
    group_by(Species) %>%
    summarise(accroissement_sp = mean(accroissement) / 31) %>% 
    print() ->
    paracoutest3
  
  
  
  # calcule de l'accroissemenent en fonction de l'individu
  
  
  paracoutest  %>%
    
    select(idTree, `1991`, `2022`,  diametre_2022) %>% 
    mutate(accroissement = `2022` - `1991`) %>% 
    filter(!is.na(accroissement)) %>% 
    group_by(idTree) %>%
    summarise(accroissement_sp = (accroissement)) %>% 
    print() ->
    paracoutest4
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  install.packages("httr2")
  
  
  Taxo <- correctTaxo(genus = paracou$Genus, species = paracou$Species, useCache = FALSE, verbose = FALSE)
  
  KarnatakaForestsub$genusCorr <- Taxo$genusCorrected
  KarnatakaForestsub$speciesCorr <- Taxo$speciesCorrected
  
  # If needed, retrieve APG III families and orders from genus names
  
  APG <- getTaxonomy(KarnatakaForestsub$genusCorr, findOrder = TRUE)
  KarnatakaForestsub$familyAPG <- APG$family
  KarnatakaForestsub$orderAPG <- APG$order
  
  #If needed, retrieve APG III families and orders from genus names
  
  
  dataWD <- getWoodDensity(
    genus = paracoutest$Genus,
    species =paracoutest$Species,
    family = paracoutest$Family,
    stand = paracoutest$idTree,
  )
  
  #Below the number of wood density value estimated at the species, genus and plot level:
  
  # At species level
  sum(dataWD$levelWD == "species")
  
  #[1] 293284
  
  
  # At genus level
  sum(dataWD$levelWD == "genus")
  
  #[1] 72460
  
  
  # At plot level
  sum(!dataWD$levelWD %in% c("genus", "species"))
  
  #  44306
  
  
  
  #Retrieve height data from Chave et al. (2012) equation 6
  
  
  
  
  dataHchave <- retrieveH(
    D = paracoutest $ diametre_2022,
    coord = paracoutest[, c("Lon", "Lat")]
  )
  
  
  paracoutest %>% 
    mutate(dataWD$meanWD) %>% 
    mutate(dataHchave$H) %>% 
    print () -> 
    paracou_agb
  
  
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
  
  paracoutest  %>%
    mutate(Species = paste(Genus, Species, BIOMASS)) %>% 
    select(idTree, `1991`, `2022`, Species,  diametre_2022, BIOMASS) %>% 
    mutate(accroissement = `2022` - `1991`) %>% 
    filter(!is.na(accroissement)) %>% 
    group_by(Species) %>%
    summarise(accroissement_sp = mean(accroissement) / 31) %>% 
    print() ->
    paracoutest5
  
  
  
  
  
