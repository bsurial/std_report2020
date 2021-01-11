library(tidyverse)
library(rvest)

content <- read_html("https://www.bag.admin.ch/bag/de/home/zahlen-und-statistiken/zahlen-zu-infektionskrankheiten.exturl.html/aHR0cHM6Ly9tZWxkZXN5c3RlbWUuYmFnYXBwcy5jaC9pbmZyZX/BvcnRpbmcvZGF0ZW5kZXRhaWxzL2QvYWlkcy5odG1sP3dlYmdy/YWI9aWdub3Jl.html")

tables <- content %>%
  html_table(fill = TRUE)


# HIV Cases per year
cases_hiv <- tables[[1]] %>%
  janitor::clean_names() %>% 
  select(-1) %>% 
  as_tibble() %>% 
  pivot_longer(x2010:x2020, names_to = "year") %>% 
  filter(value != "Fallmeldungen") %>% 
  mutate(year = as.numeric(str_remove(year, "x"))) %>% 
  mutate(std = "HIV") %>% 
  mutate(metric = "No. cases")


# HIV Inzidenz pro 100'000 aggregiert pro Jahr
incidence_hiv <- tables[[2]] %>% 
  janitor::clean_names() %>% 
  select(-x) %>% 
  as_tibble() %>% 
  pivot_longer(x2010:x2020, names_to = "year") %>% 
  mutate(std = "HIV") %>% 
  mutate(metric = "Inzidenz pro 100'000") %>% 
  filter(!str_detect(value, "Inzidenz")) %>% 
  mutate(year = as.numeric(str_remove(year, "x")))


rownames(tables[[3]]) <- tables[[3]][,1]

# HIV Cases nach Geschlecht, pro Jahr
cases_sex_hiv <- tables[[3]] %>% 
  select(-1) %>% 
  t() %>% 
  as_tibble(rownames = "year") %>% 
  select(-2) %>% 
  mutate(std = "HIV") %>% 
  mutate(metric = "No. cases")



# HIV Inzidenz nach Geschlecht, pro Jahr
rownames(tables[[4]]) <- tables[[4]][,1]

incidence_sex_hiv <- tables[[4]] %>% 
  select(-1) %>% 
  t() %>% 
  as_tibble(rownames = "year") %>% 
  select(-2) %>% 
  mutate(std = "HIV") %>% 
  mutate(metrci = "Inzidenz pro 100'000") %>% 
  mutate(year = as.numeric(str_remove(year, "\\*")))


# HIV Cases nach Kanton
rownames(tables[[5]]) <- tables[[5]][,1]
  
cases_canton_hiv <- tables[[5]] %>% 
  select(-1) %>% 
  t() %>% 
  as_tibble(rownames = "year") %>% 
  select(-2) %>% 
  mutate(across(Aargau:unbekannt, as.numeric)) %>% 
  mutate(std = "HIV", 
         metric = "Cases")



         