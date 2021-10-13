# For cleaning and summarizing AgRay grader data
# Ben Bradford, bbradford@wisc.edu
#
# If it doesn't run, check for missing plot numbers or too many cull weights

library(tidyverse)

agray <- function(file, alias) {
  
  require(tidyverse)
  
  message("Loading file '", file, "' with alias '", alias, "'...", "\n")
  
  dir.create(alias, showWarnings = F)
  
  # read raw csv
  df <- suppressMessages(read_csv(file))
  
  # clean up raw csv and remove culls
  df_clean <- df %>%
    filter(!is.na(Researcher),
      Researcher != "Researcher",
      Researcher != "Culls Weight") %>%
    type_convert() %>%
    mutate(Size = pmap_dbl(list(Width, Length, Height), ~ sort(c(..1, ..2, ..3))[2])) %>%
    mutate(Grade = case_when(
      Size >= 1.875 ~ "A",
      Size >= 1.5 ~ "B",
      T ~ "C"))
  
  # get ordered plot list
  plots <- unique(df_clean$Plot)
  message("\nPlots:")
  print(plots)
  
  if (is.character(df_clean$Plot)) { message("Warning: Non-numeric plot number detected.") }  
  
  # save tuber list
  tuber_path <- file.path(alias, paste(alias, "tubers.csv"))
  df_clean %>% write_csv(tuber_path)
  message("\nSaved graded tuber list to '", tuber_path, "'")
  
  # get culls
  culls <- df %>%
    filter(Researcher == "Culls Weight") %>%
    select(Trial) %>%
    rename(cull_wt = Trial) %>%
    cbind(tibble(Plot = plots), .)
  
  culls_path <- file.path(alias, paste(alias, "culls.csv"))
  culls %>% write_csv(culls_path)
  message("\nSaved culls list to '", culls_path, "'")
  
  # total summary
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
  
  # summary by grade
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
  
  col_names_sorted <- sort(names(summary)[-1])
  
  summary <- summary %>%
    select("Plot", all_of(col_names_sorted)) %>%
    select(
      "Plot",
      starts_with("total_"),
      starts_with("cull_"),
      starts_with("mean_"),
      starts_with("n_"),
      starts_with("prp_"),
      everything())
  
  summary_path <- file.path(alias, paste(alias, "grading summary.csv"))
  summary %>% write_csv(summary_path)
  message("\nGrading summary saved to '", summary_path, "'")
  
  
  # simple plot
  plot <- summary %>%
    ggplot(aes(x = as.character(Plot), y = total_wt)) +
    geom_col(aes(fill = total_wt)) +
    scale_y_continuous(expand = expansion(c(0, 0.1))) +
    labs(
      title = "Total plot weights",
      x = "Plot",
      y = "Total weight (oz)",
    ) +
    theme_classic() +
    theme(
      legend.position = "none",
      axis.text.x = element_text(angle = -90, hjust = 0, vjust = 0.5))
  show(plot)
  
  plot_path <- file.path(alias, paste(alias, "total weights plot.png"))
  ggsave(plot_path, plot, type = "cairo")
}


# Run example data
# arguments are [file], [output file name prefix]
agray("example-data.csv", "example")
