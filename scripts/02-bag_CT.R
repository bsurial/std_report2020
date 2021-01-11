library(tidyverse)
library(rvest)

content <- read_html("https://www.bag.admin.ch/bag/de/home/zahlen-und-statistiken/zahlen-zu-infektionskrankheiten.exturl.html/aHR0cHM6Ly9tZWxkZXN5c3RlbWUuYmFnYXBwcy5jaC9pbmZyZX/BvcnRpbmcvZGF0ZW5kZXRhaWxzL2QvY2hsYW15ZGlhLmh0bWw_/d2ViZ3JhYj1pZ25vcmU=.html")

tables <- content %>%
  html_table(fill = TRUE)


# Chlamydia Cases per year
cases_ct <- tables[[1]] %>%
  janitor::clean_names() %>% 
  select(-1) %>% 
  as_tibble() %>% 
  pivot_longer(x2010:x2020, names_to = "year") %>% 
  filter(value != "Fallmeldungen") %>% 
  mutate(year = as.numeric(str_remove(year, "x"))) %>% 
  mutate(std = "Chlamydia") %>% 
  mutate(metric = "No. cases")


# Chlamydien Inzidenz pro 100'000 aggregiert pro Jahr
incidence_ct <- tables[[2]] %>% 
  janitor::clean_names() %>% 
  select(-x) %>% 
  as_tibble() %>% 
  pivot_longer(x2010:x2020, names_to = "year") %>% 
  mutate(std = "Chlamydia") %>% 
  mutate(metric = "Inzidenz pro 100'000") %>% 
  filter(!str_detect(value, "Inzidenz")) %>% 
  mutate(year = as.numeric(str_remove(year, "x")))


rownames(tables[[3]]) <- tables[[3]][,1]

# Chlamydien Cases nach Geschlecht, pro Jahr
cases_sex_ct <- tables[[3]] %>% 
  select(-1) %>% 
  t() %>% 
  as_tibble(rownames = "year") %>% 
  select(-2) %>% 
  mutate(std = "Chlamydia") %>% 
  mutate(metric = "No. cases")



# Chlamydien Inzidenz nach Geschlecht, pro Jahr
rownames(tables[[4]]) <- tables[[4]][,1]

incidence_sex_ct <- tables[[4]] %>% 
  select(-1) %>% 
  t() %>% 
  as_tibble(rownames = "year") %>% 
  select(-2) %>% 
  mutate(std = "Chlamydia") %>% 
  mutate(metric = "Inzidenz pro 100'000") %>% 
  mutate(year = as.numeric(str_remove(year, "\\*")))


# Chlamydien Cases nach Kanton
rownames(tables[[5]]) <- tables[[5]][,1]

cases_canton_ct <- tables[[5]] %>% 
  select(-1) %>% 
  t() %>% 
  as_tibble(rownames = "year") %>% 
  select(-2) %>% 
  mutate(across(Aargau:unbekannt, as.numeric)) %>% 
  mutate(std = "Chlamydia", 
         metric = "Cases")



