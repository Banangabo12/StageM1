

library(tidyverse)
#Groupement de 4 parcelles

X2023_09_29_ParacouP14AllYears <- read_csv("2023-09-29_ParacouP14AllYears.csv")
read_csv("data/2023-09-29_ParacouP13AllYears.csv") %>% 
  bind_rows(read_csv("data/2023-09-29_ParacouP14AllYears.csv")) %>% 
  bind_rows(read_csv("data/2023-09-29_ParacouP15AllYears.csv")) %>% 
  bind_rows(read_csv("data/2023-09-29_ParacouP16AllYears.csv")) %>% 
  print() ->
  paracou



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
  select(idTree, CensusYear, Species, CircCorr) %>% 
  pivot_wider(
    names_from = CensusYear,
    values_from = CircCorr
  ) %>% 
  select(idTree, `1991`, `2022`, Species) %>% 
  mutate(diametre_2022 = `2022` / pi)  %>% 
  mutate(diametre_1991 = `1991` / pi)  %>% 
  print() ->
  paracoutest














paracou  %>%
  select(idTree, CensusYear, CircCorr) 
  pivot_wider(
    names_from = CensusYear,
    values_from = CircCorr
  ) 
   view Paracou test
  
  select(idTree, `1991`, `2022`)   
  mutate(diametre = CircCorr/π ) %>% 
 
  mean() %>%
  `/`(31)
*`paracou2



paracou  %>%
  select(idTree, CensusYear, Species, CircCorr) %>% 
  pivot_wider(
    names_from = CensusYear,
    values_from = CircCorr
  ) %>% 
  select(idTree, `1991`, `2022`, Species) %>% 
  mutate(diametre = CircCorr/π ) %>% 
  filter(!is.na(diametre = CircCorr/π )) %>% 
  group_by(Species) %>%
  summarise(diametre = CircCorr/π = mean(diametre = CircCorr/π) / 31)


CircCorr/π = paste(paracou$`2022` - `1991` )

 `
paracou %>% 
  CircCorr/π = paste(`2022` - `1991` ))
  mutate(diametre = CircCorr/π = paste(`2022` - `1991` )) %>%  
  print() ->
  paracou2
  
  
  paracou_2022_p15_bota |> 
    mutate(espece = paste(Genus, Species)) |> 
    print() ->
    paracou_2022_p15_bota_sp
 



