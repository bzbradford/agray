#' For cleaning and summarizing AgRay grader data
#' Ben Bradford, bbradford@wisc.edu
#'
#' If it doesn't run, check for missing plot numbers or too many cull weights
#' 
#' Grades can be provided as a named list in descending order of size, in inches or ounces
#' Provide `grading_metric = "Weight"` to switch to weight-based grading

grade <- function(
  file,
  name = tools::file_path_sans_ext(basename(file)),
  grades = list("A" = 1.875, "B" = 1.5, "C" = 0),
  grading_type = "Size"
) {
  
# Argument checks ---------------------------------------------------------
  
  if (!require(tidyverse)) install.packages("tidyverse")
  if (!require(janitor)) install.packages("janitor")
  
  if (!file.exists(file)) stop("File '", file, "' not found.")
  message("Input file: ", file)
  
  if (is.null(name)) stop("Trial name is required.")
  name <- as.character(name)
  if (length(name) == 0) stop("You must provide a name for this trial.")
  out_dir <- file.path(dirname(file), name)
  message("Saving outputs to: ", out_dir)
  
  if (!is.list(grades) | is.null(names(grades))) stop("Tuber grades must be a named list.")
  if (!(grading_type %in% c("Size", "Weight"))) stop("Grading type must be one of 'Size', 'Weight'")
  
  if (grading_type == "Size") {
    grading_criteria <- paste0(paste(names(grades), grades, sep = " > ", collapse = "\", "), "\" diameter")
  } else {
    grading_criteria <- paste0(paste(names(grades), grades, sep = " > ", collapse = " oz, "), " oz weight")
  }
  message("Grades: ", grading_criteria)
  

# Define functions --------------------------------------------------------
  
  assignGrade <- function(size) {
    for (grade in names(grades)) {
      if (size >= grades[grade]) return(grade)
    }
    "No grade"
  }
  
  writeCsvWithHeader <- function(df, file, header) {
    write_csv(tibble(lines = c(header, "")), file, col_names = F)
    write_csv(df, file, col_names = T, append = T)
  }
  

# Read and grade data -----------------------------------------------------

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
      Grade = factor(mapply(assignGrade, .data[[grading_type]]), levels = c(names(grades), "No grade")))
  
  # save tuber list
  tuber_file <- file.path(out_dir, paste(name, "- graded tuber list.csv"))
  df_clean %>%
    writeCsvWithHeader(
      tuber_file,
      header =  c(
        paste(name, "- Graded tubers"),
        paste("Grading criteria:", grading_criteria)))
  cat("- Saved graded tuber list to:", tuber_file, "\n")
  

# Get plot names -----------------------------------------------------------

  message("\nGetting plots...")
  
  plots <- unique(df_clean$Plot)
  if (anyNA(plots)) stop("Missing value(s) in plot name column, check data!")
  
  cat("- As run:", paste(plots, collapse = ", "), "\n")
  cat("- Sorted:", paste(sort(plots), collapse = ", "), "\n")
  cat("- Total plots:", length(plots), "\n")
  

# Get culls ---------------------------------------------------------------

  message("\nGetting cull weights...")
  
  culls <- df %>%
    filter(Researcher == "Culls Weight") %>%
    select(Trial) %>%
    rename(cull_wt = Trial)
  
  # check for culls error
  if (length(plots) != nrow(culls)) {
    message("ERROR: Plots/culls mismatch (", length(plots), " plots, ", nrow(culls), " culls)")
    print(plots)
    culls <- tibble(Plot = plots, cull_wt = NA)
    cat("- ERROR: Failed to parse cull weights, check for extra cull weights or unnamed plots in data!\n")
    warning("Failed to parse cull weights, check for extra cull weights or unnamed plots in data!")
  } else {
    culls <- cbind(tibble(Plot = plots), culls)
    culls_file <- file.path(out_dir, paste(name, "- cull weights.csv"))
    cat("- Saved cull weights to:", culls_file, "\n")
    write_csv(culls, culls_file)
  }
  

# Summarize dataset -------------------------------------------------------
  
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
      .groups = "drop_last") %>%
    mutate(prp_tubers = n_tubers / sum(n_tubers), .after = "n_tubers") %>%
    mutate(prp_total_wt = total_wt / sum(total_wt), .after = "total_wt") %>%
    ungroup() %>%
    mutate(prp_defect = prp_hollow + prp_double + prp_knob) %>%
    mutate(across(c(prp_hollow, prp_double, prp_knob, prp_defect), ~ .x / n_tubers))
  
  # save to file
  summary_file <- file.path(out_dir, paste(name, "- grading summary (long format).csv"))
  grade_summary %>%
    janitor::clean_names(case = "big_camel") %>%
    writeCsvWithHeader(
      summary_file,
      header = c(
        paste(name, "- Grading summary (long format)"),
        paste("Grading criteria:", grading_criteria)))
  cat("- Saved grading summary (long format) to:", summary_file, "\n")

  # wide format
  grade_summary_wide <- grade_summary %>%
    pivot_wider(
      names_from = "Grade",
      values_from = -c("Plot", "Grade"))
  
  # join the summaries and the culls
  summary <- totals_summary %>%
    left_join(grade_summary_wide, by = "Plot") %>%
    left_join(culls, by = "Plot")
  
  # reorganize columns
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
  
  # save to file
  summary_file <- file.path(out_dir, paste(name, "- grading summary (wide format).csv"))
  summary %>%
    janitor::clean_names(case = "big_camel") %>%
    writeCsvWithHeader(
      summary_file,
      header = c(
        paste(name, "- Grading summary (wide format)"),
        paste("Grading criteria:", grading_criteria)))
  cat("- Saved grading summary (wide format) to:", summary_file, "\n")
  

# Create plot ------------------------------------------------------------

  message("\nGenerating summary plot...")
  
  if (is.numeric(summary$Plot)) {
    summary <- summary %>%
      arrange(Plot) %>%
      mutate(Plot = fct_inorder(as.character(Plot)))
  }
  
  plot <- grade_summary %>%
    group_by(Plot) %>%
    mutate(Plot = fct_inorder(as.character(Plot))) %>%
    mutate(pct_wt = total_wt / sum(total_wt)) %>%
    ggplot(aes(x = Plot, y = total_wt, fill = Grade, label = scales::percent(pct_wt, 1))) +
    geom_col(color = "black", size = 0.25, position = "stack") +
    geom_text(position = position_stack(vjust = 0.5)) +
    scale_y_continuous(expand = expansion(c(0, 0.1))) +
    labs(
      title = paste(name, "- Total plot weights"),
      subtitle = paste("Grading criteria:", grading_criteria),
      x = "Plot",
      y = "Total weight (oz)"
    ) +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
  
  show(plot)
  
  plot_file <- file.path(out_dir, paste(name, "- total weights plot.png"))
  cat("- Saved plot image to:", plot_file, "\n")
  suppressMessages(ggsave(plot_file, plot))
  

# Finish up ---------------------------------------------------------------

  assign("all_tubers", df_clean, envir = .GlobalEnv)
  assign("grade_summary", grade_summary, envir = .GlobalEnv)
  assign("totals_summary", summary, envir = .GlobalEnv)
  assign("plot", plot, envir = .GlobalEnv)
  
  message("\nAlso saved data and plots to: all_tubers, grade_summary, totals_summary, plot")
}
