library(tidyverse)
library(here)
library(lubridate)


# hiv_2017_stat <- function(sheet, month) {
#   df <- readxl::read_excel(here("daten", "2017_HIV_Statistik.xlsx"), sheet = sheet) %>% 
#     janitor::clean_names() %>% 
#     filter(datum == "Total") %>% 
#     select(contains("full_sti"), contains("st_is_ohne_hiv"), 
#            contains("st_is_ohne_syphilis"), contains("clamydien_gono_pcr"), 
#            gono_pos, chlamydien_pos) %>% 
#     mutate(datum = month) %>% 
#     relocate(datum)
#   names(df) <- c("datum", "full_sti", "st_is_ohne_hiv", "st_is_ohne_syphilis",                                                                          
#                  "clamydien_gono_pcr", "gono_pos", "chlamydien_pos")
#   df %>% 
#     mutate(across(full_sti:chlamydien_pos, as.numeric))
# }
# 
# 
# st2017 <- bind_rows(
#   hiv_2017_stat( 1, "Januar"), 
#   hiv_2017_stat( 2, "Februar"), 
#   hiv_2017_stat( 3, "März"), 
#   hiv_2017_stat( 4, "April"), 
#   hiv_2017_stat( 5, "Mai"), 
#   hiv_2017_stat( 6, "Juni"), 
#   hiv_2017_stat( 7, "Juli"), 
#   hiv_2017_stat( 8, "August"), 
#   hiv_2017_stat( 9, "September"), 
#   hiv_2017_stat(10, "Oktober"), 
#   hiv_2017_stat(11, "November"), 
#   hiv_2017_stat(12, "Dezember"), 
# ) %>% 
#   mutate(year = 2017)
# 
# 
# 
# st2018 <- readxl::read_excel(here("daten", "2018_HIV_Statistik.xlsx"), sheet = "Gesamt 2018") %>% 
#   janitor::clean_names() %>% 
#   select(datum, full_sti_75chf, st_is_ohne_hiv_65chf, st_is_ohne_syphilis_65chf, clamydien_gono_pcr_50chf, 
#          gono_pos, chlamydien_pos) %>% 
#   slice(1:12) %>% 
#   mutate(year = 2018)
# 
# names(st2018) <- c("datum", "full_sti", "st_is_ohne_hiv", "st_is_ohne_syphilis",                                                                          
#                    "clamydien_gono_pcr", "gono_pos", "chlamydien_pos", "year")
#   
# 
# st2019 <- readxl::read_excel(here("daten", "2019_HIV_Statistik.xlsx"), sheet = "Gesamt 2019") %>% 
#   janitor::clean_names() %>% 
#   select(datum, full_sti_75_chf, st_is_known_syphilis_65_chf, st_is_ohne_hiv_65_chf, clamydien_gono_pcr_50_chf, 
#          gono_pos, chlamydien_pos) %>% 
#   slice(1:12) %>% 
#   mutate(year = 2019)
# 
# names(st2019) <- names(st2018) <- c("datum", "full_sti", "st_is_ohne_hiv", "st_is_ohne_syphilis",                                                                          
#                                     "clamydien_gono_pcr", "gono_pos", "chlamydien_pos", "year")
# 




# Data from 2018-2019 (old Berda)

result <- readxl::read_excel(here("daten", "berda_2018_2019.xlsx"), range = "HP1:HQ10056") %>% 
  janitor::clean_names()

date <- readxl::read_excel(here("daten", "berda_2018_2019.xlsx"), range = "B1:C10056") %>% 
  janitor::clean_names()

all18_19 <- bind_cols(date, result) %>% 
  filter(!is.na(vct_interner_code)) %>% 
  mutate(year = year(erstellt_datum),
         month = month(erstellt_datum, label = TRUE, abbr = TRUE)) %>%
  mutate(
    resultat_gonorrhoe_test = case_when(
      str_detect(resultat_gonorrhoe_test, "Negativ") ~ "Negativ", 
      str_detect(resultat_gonorrhoe_test, "Positiv") ~ "Positiv"
    ),
    resultat_chlamydien_test = case_when(
      str_detect(resultat_chlamydien_test, "Negativ") ~ "Negativ", 
      str_detect(resultat_chlamydien_test, "Positiv") ~ "Positiv", 
    )
  )

gono18_19 <- all18_19 %>% 
  group_by(year, month) %>% 
  count(resultat_gonorrhoe_test) %>% 
  filter(!is.na(resultat_gonorrhoe_test)) %>% 
  pivot_wider(names_from = resultat_gonorrhoe_test, 
              values_from = n, 
              values_fill = 0) %>% 
  mutate(Total = Negativ + Positiv,
         pos_rate_per100 = Positiv/Total * 100)


ct_18_19 <- all18_19 %>% 
  group_by(year, month) %>% 
  count(resultat_chlamydien_test) %>% 
  filter(!is.na(resultat_chlamydien_test)) %>% 
  pivot_wider(names_from = resultat_chlamydien_test, 
              values_from = n, 
              values_fill = 0) %>% 
  mutate(Total = Negativ + Positiv,
         pos_rate_per100 = Positiv/Total * 100)

names(ct_18_19) <- c("year", "month", "neg", "pos", "Total", "pos_rate_per100")

ct_18_19 <- ct_18_19 %>% 
  mutate(test = "Chlamydien") %>% 
  relocate(pos, .before = neg)



names(gono18_19) <- c("year", "month", "neg", "pos", "Total", "pos_rate_per100")

gono18_19 <- gono18_19 %>% 
  mutate(test = "Gonokokken") %>% 
  relocate(pos, .before = neg)

full18_19 <- bind_rows(
  ct_18_19, 
  gono18_19
)

# Data from 2020 (new Berda)

ct2020 <- read_delim(here("daten", "berda2020", "ergebnis-des-chlamydient.csv"), delim = ";") %>% 
  rename(date = 1)

ngo2020 <- read_delim(here("daten", "berda2020", "ergebnis-des-gonorrhoe-t.csv"), delim = ";") %>% 
  rename(date = 1)

hiv2020 <- read_delim(here("daten", "berda2020", "ergebnis-des-hiv-laborte.csv"), delim = ";") %>% 
  rename(date = 1)

n_clients2020 <- read_delim(here("daten", "berda2020", "gendersex.csv"), delim = ";") %>% 
  janitor::clean_names() %>% 
  rename(date = 1) %>% 
  pivot_longer(-1) %>% 
  group_by(date) %>% 
  summarise(n = sum(value))


ct2020 <- ct2020 %>% 
  mutate(month = month(date, label = T, abbr = T),
         year = year(date)) %>% 
  group_by(year, month) %>% 
  summarise(pos = sum(Positiv), 
            neg = sum(Negativ)) %>% 
  mutate(Total = pos + neg, 
         pos_rate_per100 = pos / Total * 100) %>% 
  mutate(test =  "Chlamydien") %>% 
  filter(year == 2020)

ngo2020 <- ngo2020 %>% 
  mutate(month = month(date, label = T, abbr = T),
         year = year(date)) %>% 
  group_by(year, month) %>% 
  summarise(pos = sum(Positiv), 
            neg = sum(Negativ)) %>% 
  mutate(Total = pos + neg, 
         pos_rate_per100 = pos / Total * 100) %>% 
  mutate(test = "Gonokokken") %>% 
  filter(year == 2020)


full2020 <- ct2020 %>% 
  bind_rows(ngo2020)


full <- bind_rows(
  full18_19,
  full2020)



