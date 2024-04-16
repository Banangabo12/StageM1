

library(tidyverse)
#Groupement de 4 parcelles

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




































































