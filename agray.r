# For cleaning and summarizing AgRay grader data
# Ben Bradford, bbradford@wisc.edu
#
# If it doesn't run, check for missing plot numbers or too many cull weights

library(tidyverse)

agray <- function(file, alias) {
  require(tidyverse)
  
  dir.create("out", showWarnings = F)
  
  # read raw csv
  df <- suppressMessages(suppressWarnings(read_csv(file)))
  
  # clean up raw csv and remove culls
  df_clean <- df %>%
    filter(
      !is.na(Researcher),
      Researcher != "Researcher",
      Researcher != "Culls Weight") %>%
    type_convert() %>%
    mutate(Size = pmap_dbl(list(Width, Length, Height), ~ sort(c(..1, ..2, ..3))[2])) %>%
    mutate(
      Grade = case_when(
        Size >= 1.875 ~ "A",
        Size >= 1.5 ~ "B",
        T ~ "C"))
  
  message("Loaded file '", file, "' with alias '", alias, "'")
  
  # get ordered plot list
  plots <- unique(df_clean$Plot)
  message("\nPlots:")
  print(plots)
  
  if (is.character(df_clean$Plot)) message("Warning: Non-numeric plot number(s) detected.") 
  
  # save tuber list
  outfile <- paste0("out/", alias, "_tubers.csv")
  df_clean %>% write_csv(outfile)
  message("\nSaved graded tuber list to '", outfile, "'")
  
  # get culls
  culls <- df %>%
    filter(Researcher == "Culls Weight") %>%
    select(Trial) %>%
    rename(cull_wt = Trial) %>%
    cbind(tibble(Plot = plots), .)
  
  # write culls list
  outfile <- paste0("out/", alias, "_culls.csv")
  culls %>% write_csv(outfile)
  message("\nSaved culls list to '", outfile, "'")
  
  # generate total summaries
  summary1 <- df_clean %>%
    group_by(Plot) %>%
    summarise(
      n_tubers = n(),
      total_wt = sum(Weight),
      mean_wt = mean(Weight),
      prp_hollow = sum(Hollow),
      prp_double = sum(Double),
      prp_knob = sum(Knob),
      .groups = "drop") %>%
    mutate(prp_defect = prp_hollow + prp_double + prp_knob) %>%
    mutate(across(c(prp_hollow, prp_double, prp_knob, prp_defect), ~ .x / n_tubers))
  
  # generate summaries by grade
  summary2 <- df_clean %>%
    group_by(Plot, Grade) %>%
    summarise(
      n_tubers = n(),
      total_wt = sum(Weight),
      mean_wt = mean(Weight),
      prp_hollow = sum(Hollow),
      prp_double = sum(Double),
      prp_knob = sum(Knob),
      .groups = "drop") %>%
    mutate(prp_defect = prp_hollow + prp_double + prp_knob) %>%
    mutate(across(c(prp_hollow, prp_double, prp_knob, prp_defect), ~ .x / n_tubers)) %>%
    pivot_wider(
      names_from = "Grade",
      values_from = c("n_tubers", "total_wt", "mean_wt", "prp_hollow", "prp_double", "prp_knob", "prp_defect")
    )
  
  # join the summaries and the culls
  summary <- summary1 %>%
    left_join(summary2, by = "Plot") %>%
    left_join(culls, by = "Plot")
  
  # write summary file
  outfile <- paste0("out/", alias, "_summary.csv")
  summary %>% write_csv(outfile)
  message("\nGrading summary saved to '", outfile, "'")
}


# Run example data
# arguments are [file], [output file name prefix]
agray("example-data.csv", "example")


