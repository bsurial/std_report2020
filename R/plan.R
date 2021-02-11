plan <- drake_plan(
  # Get data from BAG homepage
  hiv = 
    "https://www.bag.admin.ch/bag/de/home/zahlen-und-statistiken/zahlen-zu-infektionskrankheiten.exturl.html/aHR0cHM6Ly9tZWxkZXN5c3RlbWUuYmFnYXBwcy5jaC9pbmZyZX/BvcnRpbmcvZGF0ZW5kZXRhaWxzL2QvaGl2Lmh0bWw_d2ViZ3Jh/Yj1pZ25vcmU=.html",
  ct = 
    "https://www.bag.admin.ch/bag/de/home/zahlen-und-statistiken/zahlen-zu-infektionskrankheiten.exturl.html/aHR0cHM6Ly9tZWxkZXN5c3RlbWUuYmFnYXBwcy5jaC9pbmZyZX/BvcnRpbmcvZGF0ZW5kZXRhaWxzL2QvY2hsYW15ZGlhLmh0bWw_/d2ViZ3JhYj1pZ25vcmU=.html",
  ngo = 
    "https://www.bag.admin.ch/bag/de/home/zahlen-und-statistiken/zahlen-zu-infektionskrankheiten.exturl.html/aHR0cHM6Ly9tZWxkZXN5c3RlbWUuYmFnYXBwcy5jaC9pbmZyZX/BvcnRpbmcvZGF0ZW5kZXRhaWxzL2QvZ29ub3JyaG9lLmh0bWw_/d2ViZ3JhYj1pZ25vcmU=.html",
  syph = 
    "https://www.bag.admin.ch/bag/de/home/zahlen-und-statistiken/zahlen-zu-infektionskrankheiten.exturl.html/aHR0cHM6Ly9tZWxkZXN5c3RlbWUuYmFnYXBwcy5jaC9pbmZyZX/BvcnRpbmcvZGF0ZW5kZXRhaWxzL2Qvc3lwaGlsaXMuaHRtbD93/ZWJncmFiPWlnbm9yZQ==.html",
  
  
  # HIV
  hiv_data = get_html_tables(hiv),
  hiv_plot_cases = create_overall_table(hiv_data[[1]], 
                                        std = "HIV") %>% 
    cases_graph(text_distance = -30, incidence = FALSE),
  
  hiv_plot_incidence = create_overall_table(hiv_data[[2]], 
                                            std = "HIV") %>% 
    cases_graph(text_distance = -1, incidence = TRUE),
  
  hiv_plot_cases_sex = create_sex_table(hiv_data[[3]], 
                                        std = "HIV") %>% 
    cases_sex_graph(text_distance = -15, incidence = FALSE),
  
  hiv_plot_incidence_sex = create_sex_table(hiv_data[[4]], 
                                            std = "HIV") %>% 
    cases_sex_graph(text_distance = -0.5, incidence = TRUE),
  
  # C. trachomatis
  ct_data = get_html_tables(ct),
  
  ct_plot_cases = create_overall_table(ct_data[[1]], 
                                       std = "Chlamydia") %>% 
    cases_graph(text_distance = -450, incidence = FALSE), 
  
  ct_plot_incidence = create_overall_table(ct_data[[2]], 
                                       std = "Chlamydia") %>% 
    cases_graph(text_distance = -10, incidence = TRUE),
  
  ct_plot_cases_sex = create_sex_table(ct_data[[3]], 
                                       std = "Chlamydia") %>% 
    cases_sex_graph(text_distance = -250, incidence = FALSE),
  
  ct_plot_incidence_sex = create_sex_table(ct_data[[4]], 
                                       std = "Chlamydia") %>% 
    cases_sex_graph(text_distance = -100, incidence = TRUE),
  
  # Gonorrhea
  ngo_data = get_html_tables(ngo),
  
  ngo_plot_cases = create_overall_table(ngo_data[[1]], 
                                       std = "Gonorrhea") %>% 
    cases_graph(text_distance = -200, incidence = FALSE), 
  
  ngo_plot_incidence = create_overall_table(ngo_data[[2]], 
                                           std = "Gonorrhea") %>% 
    cases_graph(text_distance = -3, incidence = TRUE),
  
  ngo_plot_cases_sex = create_sex_table(ngo_data[[3]], 
                                       std = "Gonorrhea") %>% 
    cases_sex_graph(text_distance = -100, incidence = FALSE),
  
  ngo_plot_incidence_sex = create_sex_table(ngo_data[[4]], 
                                           std = "Gonorrhea") %>% 
    cases_sex_graph(text_distance = -3, incidence = TRUE),
  
  # Syphilis
  syph_data = get_html_tables(syph),
  
  syph_plot_cases = create_overall_table(syph_data[[1]], 
                                        std = "Syphilis") %>% 
    cases_graph(text_distance = -100, incidence = FALSE), 
  
  syph_plot_incidence = create_overall_table(syph_data[[2]], 
                                            std = "Syphilis") %>% 
    cases_graph(text_distance = -1.5, incidence = TRUE),
  
  syph_plot_cases_sex = create_sex_table(syph_data[[3]], 
                                        std = "Syphilis") %>% 
    cases_sex_graph(text_distance = -75, incidence = FALSE),
  
  syph_plot_incidence_sex = create_sex_table(syph_data[[4]], 
                                            std = "Syphilis") %>% 
    cases_sex_graph(text_distance = -1.5, incidence = TRUE),
  
  # BERDA Daten
  berda_graph_visits = get_berda_data() %>% 
    graph_teststelle_visits(),
  
  berda_graph_cases = get_berda_data() %>% 
    graph_teststelle_cases(),
  
  berda_graph_tests = get_berda_data() %>% 
    graph_teststelle_anzahltests(),
  
  pos_chlam = get_berda_data() %>% 
    pos_test_bars(test = "Chlamydien"),
  
  pos_gono = get_berda_data() %>% 
    pos_test_bars(test = "Gonokokken"),
  
  
  # IFIK Daten
  ifik_ct_data = ifik("ct"), 
  ifik_ngo_data = ifik("ngo"),
  
  ifik_ct_plot = ifik_ct_data %>% ifik_plot(title = "Chlamydia trachomatis"),
  ifik_ngo_plot = ifik_ngo_data %>% ifik_plot(title = "Neisseria gonorrhea"),
  
  ifik_ct_year_cases_plot = ifik_ct_data %>% 
    ifik_year_cases_plot(title = "Chlamydia trachomatis"),
  ifik_ct_year_rate_plot = ifik_ct_data %>% 
    ifik_year_rate_plot(title = "Chlamydia trachomatis"),
  
  ifik_ngo_year_cases_plot = ifik_ngo_data %>% 
    ifik_year_cases_plot(title = "Neisseria gonorrhea"),
  ifik_ngo_year_rate_plot = ifik_ngo_data %>% 
    ifik_year_rate_plot(title = "Neisseria gonorrhea"),
  
  report = rmarkdown::render(
    knitr_in("report.Rmd"),
    output_file = file_out("report.md"),
    quiet = TRUE
  )
)

