# For cleaning and summarizing AgRay grader data
# Ben Bradford, bbradford@wisc.edu
#
# If it doesn't run, check for missing plot numbers or too many cull weights
# Grades can be provided as a named list in descending order of size

library(tidyverse)

grade <- function(file, name = tools::file_path_sans_ext(basename(file)), grades = list("A" = 1.875, "B" = 1.5, "C" = 0)) {
  require(tidyverse)
  
  # Checks
  if (!file.exists(file)) stop("File '", file, "' not found.")
  message("Input file: ", file)
  
  if (is.null(name)) stop("Trial name is required.")
  name <- as.character(name)
  if (length(name) == 0) stop("You must provide a name for this trial.")
  out_dir <- file.path(dirname(file), name)
  message("Saving outputs to: ", out_dir)
  
  if (!is.list(grades) | is.null(names(grades))) stop("Tuber grades must be a named list.")
  grading_criteria <- paste0(paste(names(grades), grades, sep = " >= ", collapse = '", '), '" diameter')
  message("Grades: ", grading_criteria)
  
  assignGrade <- function(size) {
    for (grade in names(grades)) {
      if (size >= grades[grade]) return(grade)
    }
    "No grade"
  }
  
  # read csv
  dir.create(out_dir, showWarnings = F)
  df <- suppressWarnings(read_csv(file, col_types = cols(.default = "c"), progress = F))
  
  # clean up raw csv and remove culls
  message("\nCleaning input file...")
  df_clean <- df %>%
    filter(
      !is.na(Researcher),
      Researcher != "Researcher",
      Researcher != "Culls Weight") %>%
    type_convert(col_types = cols()) %>%
    mutate(
      Size = pmap_dbl(list(Width, Length, Height), ~ sort(c(..1, ..2, ..3))[2]),
      Grade = mapply(assignGrade, Size))
  
  # save tuber list
  tuber_path <- file.path(out_dir, paste(name, "- graded tuber list.csv"))
  df_clean %>% write_csv(tuber_path)
  cat("Saving graded tuber list to:", tuber_path, "\n")
  
  # get ordered plot list
  message("\nGetting plots...")
  plots <- unique(df_clean$Plot)
  if (anyNA(plots)) {
    stop("Missing value(s) in plot name column, check data!")
  }
  cat("As run:", paste(plots, collapse = ", "), "\n")
  cat("Sorted:", paste(sort(plots), collapse = ", "), "\n")
  cat("Total plots:", length(plots), "\n")
  if (is.character(df_clean$Plot)) {
    message("Non-numeric plot number detected, confirm plot names!")
  }
  
  # get culls
  message("\nGetting cull weights...")
  culls <- df %>%
    filter(Researcher == "Culls Weight") %>%
    select(Trial) %>%
    rename(cull_wt = Trial)
  
  # check for culls error
  if (length(plots) != nrow(culls)) {
    culls <- tibble(Plot = plots, cull_wt = NA)
    cat("ERROR: Failed to parse cull weights, check for extra cull weights or unnamed plots in data!\n")
    warning("Failed to parse cull weights, check for extra cull weights or unnamed plots in data!")
  } else {
    culls <- cbind(tibble(Plot = plots), culls)
    culls_file <- file.path(out_dir, paste(name, "- cull weights.csv"))
    cat("Saving cull weights to:", culls_file, "\n")
    write_csv(culls, culls_file)
  }
  
  # total summary
  message("\nSummarizing dataset...")
  totals_summary <- df_clean %>%
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
  grade_summary <- df_clean %>%
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
    mutate(across(c(prp_hollow, prp_double, prp_knob, prp_defect), ~ .x / n_tubers))

  grade_summary_wide <- grade_summary %>%
    pivot_wider(
      names_from = "Grade",
      values_from = c("n_tubers", "total_wt", "mean_wt", "prp_hollow", "prp_double", "prp_knob", "prp_defect")
    )
  
  # join the summaries and the culls
  summary <- totals_summary %>%
    left_join(grade_summary_wide, by = "Plot") %>%
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
  
  summary_path <- file.path(out_dir, paste(name, "- plot summaries.csv"))
  summary %>% write_csv(summary_path)
  cat("Saving plot-wise grading summaries to:", summary_path, "\n")
  
  
  # simple plot
  message("\nGenerating summary plot...")
  if (is.numeric(summary$Plot)) {
    summary <- summary %>%
      arrange(Plot) %>%
      mutate(Plot = fct_inorder(as.character(Plot)))
  }
  
  plot <- grade_summary %>%
    group_by(Plot) %>%
    mutate(pct_wt = total_wt / sum(total_wt)) %>%
    ggplot(aes(x = Plot, y = total_wt, fill = Grade, label = scales::percent(pct_wt, 1))) +
    geom_col(color = "black", size = 0.25, position = "stack") +
    geom_text(position = position_stack(vjust = 0.5)) +
    scale_y_continuous(expand = expansion(c(0, 0.1))) +
    labs(
      title = paste(name, "- Total plot weights"),
      subtitle = paste("Grading criteria:", grading_criteria),
      x = "Plot",
      y = "Total weight (oz)",
    ) +
    theme_classic() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
  show(plot)
  
  plot_path <- file.path(out_dir, paste(name, "- total weights plot.png"))
  cat("Saving plot image to:", plot_path, "\n")
  suppressMessages(ggsave(plot_path, plot, type = "cairo"))
  
  assign("all_tubers", df_clean, envir = .GlobalEnv)
  assign("grade_summary", grade_summary, envir = .GlobalEnv)
  assign("totals_summary", summary, envir = .GlobalEnv)
  assign("plot", plot, envir = .GlobalEnv)
  
  message("\nAlso data and plots to: all_tubers, grade_summary, totals_summary, plot.")
}
