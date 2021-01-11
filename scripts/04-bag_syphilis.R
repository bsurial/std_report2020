library(tidyverse)
library(rvest)

# Webscraping 
content <- read_html("https://www.bag.admin.ch/bag/de/home/zahlen-und-statistiken/zahlen-zu-infektionskrankheiten.exturl.html/aHR0cHM6Ly9tZWxkZXN5c3RlbWUuYmFnYXBwcy5jaC9pbmZyZX/BvcnRpbmcvZGF0ZW5kZXRhaWxzL2Qvc3lwaF9mcnVlaC5odG1s/P3dlYmdyYWI9aWdub3Jl.html")

tables <- content %>%
  html_table(fill = TRUE)


# Syphilis (Früh) Cases per year
cases_syphilis <- tables[[1]] %>%
  janitor::clean_names() %>% 
  select(-1) %>% 
  as_tibble() %>% 
  pivot_longer(x2010:x2020, names_to = "year") %>% 
  filter(value != "Fallmeldungen") %>% 
  mutate(year = as.numeric(str_remove(year, "x"))) %>% 
  mutate(value = na_if(value, "")) %>% 
  mutate(std = "Syphilis") %>% 
  mutate(metric = "No. cases")


# Syphilis (Früh) Inzidenz pro 100'000 aggregiert pro Jahr
incidence_syphilis <- tables[[2]] %>% 
  janitor::clean_names() %>% 
  select(-x) %>% 
  as_tibble() %>% 
  pivot_longer(x2010:x2020, names_to = "year") %>% 
  mutate(std = "Syphilis") %>% 
  mutate(metric = "Inzidenz pro 100'000") %>% 
  mutate(value = na_if(value, "")) %>% 
  filter(!str_detect(value, "Inzidenz")) %>% 
  mutate(year = as.numeric(str_remove(year, "x")))


rownames(tables[[3]]) <- tables[[3]][,1]

# Syphilis (Früh) Cases nach Geschlecht, pro Jahr
cases_sex_syphilis <- tables[[3]] %>% 
  select(-1) %>% 
  t() %>% 
  as_tibble(rownames = "year") %>% 
  select(-2) %>% 
  mutate(std = "Gonorrhea") %>% 
  mutate(metric = "No. cases") %>% 
  mutate(across(c(männlich:unbekannt), ~na_if(.x, "")))



# Syphilis (Früh) Inzidenz nach Geschlecht, pro Jahr
rownames(tables[[4]]) <- tables[[4]][,1]

incidence_sex_syphilis <- tables[[4]] %>% 
  select(-1) %>% 
  t() %>% 
  as_tibble(rownames = "year") %>% 
  select(-2) %>% 
  mutate(std = "Syphilis") %>% 
  mutate(metric = "Inzidenz pro 100'000") %>% 
  mutate(year = as.numeric(str_remove(year, "\\*"))) %>% 
  mutate(across(c(männlich:metric), ~na_if(.x, "")))

