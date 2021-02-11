
get_html_tables <- function(url) {
  read_html(url) %>% 
    html_table(fill = TRUE)
}
  


create_overall_table <- function(table, std, var = x2011:x2020) {
  table %>%
    janitor::clean_names() %>% 
    select({{ var }}) %>% 
    as_tibble() %>% 
    pivot_longer({{ var }}, names_to = "year") %>% 
    filter(value != "Fallmeldungen") %>% 
    mutate(year = as.numeric(str_remove(year, "x"))) %>% 
    mutate(std = std) %>% 
    filter(!str_detect(value, "Inzidenz")) %>% 
    mutate(year = as.numeric(str_remove(year, "x")),
           value = as.numeric(value))
}


create_sex_table <- function(table, std) {
  rownames(table) <- table[,1]
  table %>% 
    t() %>% 
    as_tibble(rownames = "year") %>% 
    select(-2) %>% 
    mutate(std = std) %>% 
    filter(parse_number(year) != 2021)
}


cases_graph <- function(df, incidence = FALSE, text_distance = -750) {
  std <- unique(df$std)
    if(incidence == FALSE) {
      y_lab <- "No. of cases"
      title_lab <- glue::glue("CH: Number of new {std} cases")
    } else {
      y_lab <- "Incidence per 100'000" 
      title_lab <- glue::glue("CH: Incidence of new {std} cases, per 100'000")
    }
    
    df %>%
      mutate(
        pct_change = (value - lag(value)) / lag(value)
      ) %>%
      mutate(pct_change_ft = scales::percent(pct_change, accuracy = 0.1)) %>% 
      ggplot(aes(x = year, y = value)) +
      geom_text(aes(
        label = pct_change_ft, y = value + text_distance,
        color = pct_change >= 0
      ),
      family = "Roboto", size = 3, show.legend = FALSE
      ) +
      geom_line(aes(group = 1)) +
      geom_point(aes(color = pct_change >= 0),
                 show.legend = FALSE,
                 size = 2
      ) +
      expand_limits(y = 0) +
      scale_color_manual(values = c("firebrick", "darkgreen")) +
      scale_x_continuous(
        breaks = seq(2010, 2020, 1),
        labels = scales::comma_format(
          accuracy = 1,
          big.mark = ""
        )
      ) +
      scale_y_continuous(labels = scales::comma_format(big.mark = "'")) + 
      labs(x = "\nCalendar year", 
           y = y_lab,
           title = title_lab,
           caption = "Source: Bundesamt für Gesundheit, Stand 5.1.21") +
      theme_light(base_family = "Roboto") + 
      theme(panel.grid.minor.x = element_blank(),
            plot.title.position = "panel", 
            plot.title = element_text(face = "bold"),
            plot.caption = element_text(face = "italic"))
  }





cases_sex_graph <- function(df, incidence = FALSE, text_distance) {
  std <- unique(df$std)
  if(incidence == FALSE) {
    y_lab <- "No. of cases"
    title_lab <- glue::glue("CH: Number of new {std} cases (by gender)")
  } else {
    y_lab <- "Incidence per 100'000" 
    title_lab <- glue::glue(
      "CH: Incidence of new {std} cases, per 100'000 (by gender)"
      )
  }
  df %>% 
    select(-c(std)) %>% 
    pivot_longer(-year) %>%
    mutate(value = as.numeric(value)) %>% 
    filter(name != "unbekannt") %>% 
    arrange(name, year) %>% 
    group_by(name) %>% 
    mutate(pct_change = (value - lag(value)) / lag(value)) %>%
    mutate(pct_change_ft = scales::percent(pct_change, accuracy = 0.1)) %>% 
    ggplot(aes(x = year, y = value)) + 
    geom_line(aes(linetype = name, group = name)) +
    geom_text(aes(label = pct_change_ft, color = pct_change >= 0, 
                  y = value + text_distance),
              family = "Roboto", size = 3, show.legend = FALSE) +
    geom_point(aes(color = pct_change >= 0), 
               size = 2, show.legend = FALSE) +
    expand_limits(y = 0)  + 
    scale_color_manual(values = c("firebrick", "darkgreen")) + 
    labs(linetype = "Gender", x = "\nCalendar year", 
         y = y_lab, 
         title = title_lab,
         caption = "Source: Bundesamt für Gesundheit, Stand 5.1.21") +
    theme_light(base_family = "Roboto") + 
    theme(panel.grid.minor.x = element_blank(),
          plot.title.position = "panel", 
          plot.title = element_text(face = "bold"),
          plot.caption = element_text(face = "italic"))
}






# Data from 2018-2019 and 2020
get_berda_data <- function() {
  
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
  
  ct2020 <- read_delim(here("daten", "ergebnis-des-chlamydient.csv"), delim = ";") %>% 
    rename(date = 1)
  
  ngo2020 <- read_delim(here("daten", "ergebnis-des-gonorrhoe-t.csv"), delim = ";") %>% 
    rename(date = 1)
  
  hiv2020 <- read_delim(here("daten", "ergebnis-des-hiv-laborte.csv"), delim = ";") %>% 
    rename(date = 1)
  
  n_clients2020 <- read_delim(here("daten", "gendersex.csv"), delim = ";") %>% 
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
  
  full
}



graph_teststelle_cases <- function(df) {
  df %>% 
    mutate(year = factor(year)) %>%
    ggplot(aes(x = month, y = pos_rate_per100)) +
    geom_line(aes(group = year, color = year)) +
    facet_wrap(~test, nrow = 2) +
    geom_point(aes(color = year)) +
    # scale_color_manual(values = c("grey80", "firebrick")) +
    theme(strip.background = element_blank(),
          strip.text = element_text(color = "black", face = "bold"))  +
    scale_color_brewer(palette = 7) +
    labs(title = "Teststelle Insel: Positive Resultate pro 100 Tests",
         x = "",
         y = "Pos. Rate pro 100 Tests\n") +
    theme_light(base_family = "Roboto") + 
    theme(panel.grid.minor.x = element_blank(),
          panel.grid.major.x = element_blank(),
          strip.background = element_blank(), 
          strip.text = element_text(color = "black", face = "bold"),
          plot.title.position = "panel", 
          plot.title = element_text(face = "bold"),
          plot.caption = element_text(face = "italic"))
} 

graph_teststelle_anzahltests <- function(df) {
  df %>% 
    mutate(year = factor(year)) %>%
    filter(test == "Chlamydien") %>% 
    ggplot(aes(x = month, y = Total)) +
    geom_line(aes(group = year, color = year)) +
    geom_point(aes(color = year)) +
    # scale_color_manual(values = c("grey80", "firebrick")) +
    theme(strip.background = element_blank(),
          strip.text = element_text(color = "black", face = "bold"))  +
    scale_color_brewer(palette = 7) +
    labs(title = "Teststelle Insel: Anzahl Chlamydien/Gonokokken Tests",
         x = "",
         y = "Anzahl Tests pro Monat\n", 
         color = "\nCalendar year") +
    theme_light(base_family = "Roboto") + 
    theme(panel.grid.minor.x = element_blank(),
          panel.grid.major.x = element_blank(),
          strip.background = element_blank(), 
          strip.text = element_text(color = "black", face = "bold"),
          plot.title.position = "panel", 
          plot.title = element_text(face = "bold"),
          plot.caption = element_text(face = "italic"))
}  


graph_teststelle_visits <- function(df) {
  df %>% 
    filter(test == "Chlamydien") %>% 
    group_by(year) %>% 
    summarise(Total = sum(Total)) %>% 
    ggplot(aes(x = year, y = Total)) + 
    geom_col() +
    geom_text(aes(y = Total + 100, 
                  label = scales::comma(Total, big.mark = "'")), 
              family = "Roboto") + 
    scale_y_continuous(limits = c(0, 2500)) + 
    labs(x = "Calendar year\n", 
         y = "\nNumber of visit", 
         title = "Teststelle Insel: Anzahl Besuche pro Jahr") + 
    theme_light(base_family = "Roboto") + 
    theme(panel.grid.minor.x = element_blank(),
          panel.grid.major.x = element_blank(),
          strip.background = element_blank(), 
          strip.text = element_text(color = "black", face = "bold"),
          plot.title.position = "panel", 
          plot.title = element_text(face = "bold"),
          plot.caption = element_text(face = "italic"))
}

pos_test_bars <- function(df, test) {
  df %>% 
    filter(test == {{ test }}) %>% 
    ggplot(aes(x = month, y = pos)) + 
    geom_col() + 
    facet_wrap(~year, nrow = 1) + 
    # scale_color_manual(values = c("grey80", "firebrick")) +
    theme(strip.background = element_blank(),
          strip.text = element_text(color = "black", face = "bold"))  +
    scale_color_brewer(palette = 7) +
    labs(title = glue("Teststelle Insel: Positive Gonokokken Tests"),
         x = "",
         y = "Anzahl positive Tests\n", 
         color = "\nCalendar year") +
    theme_light(base_family = "Roboto") + 
    theme(panel.grid.minor.x = element_blank(),
          panel.grid.major.x = element_blank(),
          strip.background = element_blank(), 
          strip.text = element_text(color = "black", face = "bold"),
          plot.title.position = "panel", 
          plot.title = element_text(face = "bold"),
          plot.caption = element_text(face = "italic"))
}



# IFIK data ---------------------------------------------------------------

# Function to read IFIK files

ifik_file <- function(excel) {
  readxl::read_excel(excel, 
                     sheet = 2) %>% 
    janitor::clean_names() %>% 
    mutate(datum = as.Date(datum)) %>% 
    mutate(resultat = factor(resultat, 
                             levels = c("positiv", "negativ")))
}


# Read data from IFIK and parse it

ifik <- function(std) {
  # Check Input
  if(!std %in% c("ct", "ngo")) {
    stop(glue::glue("Value of std must be 'ct' or 'ngo', not '{std}'"))
  }
  
  # Read file
  ct <- ifik_file(here("daten", glue::glue("ifik-{std}-pcr-2015-2020.xlsx")))
  
  # Frauenklinik makes a lot of screenings -> remove them
  ct_subset <- ct %>% 
    filter(!(str_detect(einsender, "Frauen")))
  
  # Aggregate results by month
  ct_monthly <- ct_subset %>% 
    mutate(period = floor_date(datum, "month")) %>% 
    group_by(period) %>% 
    count(resultat) %>% 
    filter(!is.na(resultat)) %>% 
    pivot_wider(names_from = resultat, values_from = n, values_fill = 0) %>% 
    mutate(total = positiv + negativ, 
           p_rate = positiv/total)
  
  # Calculate average of Years 2015 - 2019
  ct_avg_monthly <- ct_monthly %>% 
    mutate(year = year(period), 
           month = month(period)) %>% 
    filter(year != 2020) %>% 
    group_by(month) %>% 
    summarise(avg_p_rate = mean(p_rate), 
              avg_total = mean(total), 
              min_p_rate = min(p_rate), 
              max_p_rate = max(p_rate), 
              min_total = min(total),
              max_total = max(total))
  
  # Join them together
  ct_monthly <- ct_monthly %>% 
    mutate(month = month(period)) %>% 
    left_join(ct_avg_monthly)
  
  ct_monthly
}



ifik_test_plot <- function(df, title) {
  df %>% 
    ggplot(aes(x = period, y = total)) + 
    geom_ribbon(aes(ymax = max_total, 
                    ymin = min_total),
                fill = "grey80", alpha = 0.3) +
    geom_line(aes(y = max_total), 
              color = "grey30", size = 0.11) + 
    geom_line(aes(y = min_total), 
              color = "grey30", size = 0.11) + 
    geom_line(aes(x = period, y = avg_total), 
              size = 0.75, color = "firebrick") + 
    geom_col(fill = "grey80",
             color = "black", alpha = 0.6, 
             width = 15) +
    scale_x_date(date_labels = "%b") +
    facet_wrap(~year(period), scales = "free_x", 
               ncol = 1) + 
    theme(panel.grid.major.x = element_blank(), 
          panel.grid.minor.x = element_blank(), 
          strip.text = element_text(face = "bold", 
                                    hjust = 0.95, 
                                    size = 12)) + 
    labs(x = NULL, y = "**No. of tests performed**, n", 
         title = glue("**{title}**, results from IFIK Bern"),
         subtitle = "<span style='color: firebrick'>**Mean no. of tests** (line)</span> and <span style='color: grey30'>**range** (ribbon)</span> from 2015 - 2019 </span>")
}


ifik_rate_plot <-function(df) {
  df %>% 
    ggplot(aes(x = period, y = p_rate*100)) + 
    geom_ribbon(aes(ymax = max_p_rate * 100, 
                    ymin = min_p_rate * 100),
                fill = "grey80", alpha = 0.3) +
    geom_line(aes(y = max_p_rate*100), 
              color = "grey30", size = 0.11) + 
    geom_line(aes(y = min_p_rate*100), 
              color = "grey30", size = 0.11) + 
    geom_line(aes(x = period, y = avg_p_rate * 100), 
              size = 0.75, color = "firebrick") + 
    geom_col(fill = "grey80",
             color = "black", alpha = 0.6, 
             width = 15) +
    scale_x_date(date_labels = "%b") +
    facet_wrap(~year(period), scales = "free_x", 
               ncol = 1) + 
    theme(panel.grid.major.x = element_blank(), 
          panel.grid.minor.x = element_blank(), 
          strip.text = element_text(face = "bold", 
                                    hjust = 0.95, 
                                    size = 12)) + 
    labs(x = NULL, y = "**Test positivity rate**, per 100 tests", 
         subtitle = "<span style='color: firebrick'>**Mean rate** (line)</span> and <span style='color: grey30'>**range** (ribbon)</span> from 2015 - 2019 </span>")
}

# Combine plots
ifik_plot <- function(df, title) {
  plot1 <- ifik_test_plot(df, title = title)
  plot2 <- ifik_rate_plot(df)
  
  theme_set(theme_minimal(base_family = "Open Sans"))
  theme_update(plot.title = element_markdown(face = "plain", 
                                             size = 18, 
                                             margin = margin(b = 4, unit = "mm")),
               plot.subtitle = element_markdown(face = "plain", 
                                                size = 8, color = "grey30", 
                                                lineheight = 1.24),
               axis.title.y = element_markdown(margin = margin(r = 4, 
                                                               unit = "mm")),
               axis.title.x = element_markdown(margin = margin(t = 4, 
                                                               unit = "mm")))
  
  plot1 + plot2 &
    plot_annotation(caption = "Swabs from Frauenklinik excluded.")
}
