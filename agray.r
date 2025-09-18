
# For cleaning and summarizing AgRay grader data
# Ben Bradford, bbradford@wisc.edu

# If it doesn't run, check for missing plot numbers or too many cull weights

library(tidyverse)

#*
#* @param file string, The csv file from the AgRay grader
#* @param name A folder name for outputs, if blank will use csv file name
#* @param grades A named list indicating lower cutoffs for each grade.
#* @param grade_by Indicates whether grading criteria is by 'Size' (in.) or by 'Weight' (oz.)
#* @param area Optionally include the harvested area (sq. ft.) to add yield values in cwt/ac
#*
grade <- function(
  file,
  name = tools::file_path_sans_ext(basename(file)),
  grades = list("A" = 1.875, "B" = 1.5, "C" = 0),
  grade_by = c("Size", "Weight"),
  area = NULL
) {
  
  # Argument checks ----
  
  if (!require(tidyverse)) install.packages("tidyverse")
  if (!require(janitor)) install.packages("janitor")
  
  stopifnot(is.character(file), length(file) > 0)
  if (!file.exists(file)) stop("File '", file, "' not found.")
  message("Input file: ", file)
  
  stopifnot(is.character(name), length(name) > 0)
  trial_name <- name
  out_dir <- file.path(dirname(file), trial_name)
  message("Saving outputs to: ", out_dir)
  
  if (!is.list(grades) | is.null(names(grades))) stop("Tuber grades must be a named list.")
  grades <- lapply(grades, sort, decreasing = TRUE)
  grade_by <- match.arg(grade_by)
  message("Grading by: ", grade_by)
  
  grading_criteria <- switch(grade_by,
    Size = paste0(paste(names(grades), grades, sep = " >= ", collapse = "\", "), "\" diameter"),
    Weight = paste0(paste(names(grades), grades, sep = " >= ", collapse = " oz, "), " oz weight")
  )
  message("Grades: ", grading_criteria)
  
  acreage <- if (is.numeric(area)) {
    message("Harvested area: ", area, " sq. ft.")
    area / 43560
  } else {
    message("No harvested area specified, yields cannot be calculated from plot weights.")
    NULL
  }
  
  csv_headers <- c(
    paste("Grading criteria:", grading_criteria),
    { if (is.numeric(area)) paste("Harvested area:", area, "sq. ft.") }
  )
  
  
  # Define functions ----
  
  assign_grade <- function(size) {
    for (grade in names(grades)) {
      if (size >= grades[grade]) return(grade)
    }
    "No grade"
  }
  
  write_csv_with_header <- function(df, file, header) {
    write_csv(tibble(lines = c(header, "")), file, col_names = F)
    write_csv(df, file, col_names = T, append = T, na = "0")
  }
  
  
  # Read and grade data ----
  
  # read csv
  dir.create(out_dir, showWarnings = F)
  df <- suppressWarnings(read_csv(file, col_types = cols(.default = "c"), progress = F))
  
  # clean up raw csv and remove culls
  cat("\nCleaning input file...\n")
  df_clean <- df %>%
    filter(
      !is.na(Researcher),
      Researcher != "Researcher",
      Researcher != "Culls Weight"
    ) %>%
    type_convert(col_types = cols()) %>%
    select(-Shape) %>%
    rename(any_of(c("Knob" = "KnobForeign"))) %>% # for 2025 Knob was named KnobForeign
    rowwise() %>%
    mutate(
      Defect = as.numeric(Double | Hollow | Knob),
      Size = median(Width, Length, Height),
      Grade = assign_grade(.data[[grade_by]])
    ) %>%
    ungroup() %>%
    mutate(Grade = factor(Grade, levels = c(names(grades), "No grade")))
  
  # save tuber list
  tuber_file <- file.path(out_dir, paste(trial_name, "- Graded tuber list.csv"))
  df_clean %>%
    write_csv_with_header(
      tuber_file,
      header = c(paste(trial_name, "- Graded tubers"), csv_headers)
    )
  cat("- Saved graded tuber list to:", tuber_file, "\n")
  
  
  # Get plot names ----
  
  cat("\nGetting plots...\n")
  
  plots <- unique(df_clean$Plot)
  if (anyNA(plots)) stop("Missing value(s) in plot name column, check data!")
  
  cat("- As run:", paste(plots, collapse = ", "), "\n")
  cat("- Sorted:", paste(sort(plots), collapse = ", "), "\n")
  cat("- Total plots:", length(plots), "\n")
  
  
  # Get culls ----
  
  cat("\nGetting cull weights...\n")
  
  culls <- df %>%
    filter(Researcher == "Culls Weight") %>%
    select(Trial) %>%
    rename(cull_wt_lbs = Trial) %>%
    mutate(across(cull_wt_lbs, as.numeric))
  
  # check for culls error
  if (length(plots) != nrow(culls)) {
    message("ERROR: Plots/culls mismatch (", length(plots), " plots, ", nrow(culls), " culls)")
    print(plots)
    culls <- tibble(Plot = plots, cull_wt_lbs = NA)
    msg <- "Failed to parse cull weights, check for extra cull weights, duplicate plot numbers, or unnamed plots in data!"
    cat("- ERROR: ", msg, "\n")
    warning(msg)
  } else {
    culls <- cbind(tibble(Plot = plots), culls)
    culls_file <- file.path(out_dir, paste(trial_name, "- Cull weights.csv"))
    cat("- Saved cull weights to:", culls_file, "\n")
    write_csv(culls, culls_file)
  }
  
  
  # Summarize dataset ----
  
  cat("\nSummarizing dataset...\n")
  cat("- Total tubers:", nrow(df_clean), "\n")
  cat("- Total weight:", round(sum(df_clean$Weight) / 16), "lbs.\n")
  cat("- Mean weight:", round(mean(df_clean$Weight), 1), "oz.\n")
  lapply(names(grades), function(grade) {
    prp <- nrow(filter(df_clean, Grade == grade)) / nrow(df_clean)
    cat(paste0("- Proportion ", grade, ":"), sprintf("%.1f%%", prp * 100), "\n")
  })
  
  # total summary
  totals_summary <- df_clean %>%
    group_by(Plot) %>%
    summarise(
      tuber_count = n(),
      total_wt_oz = sum(Weight),
      mean_tuber_wt_oz = mean(Weight),
      prp_hollow = sum(Hollow),
      prp_double = sum(Double),
      prp_knob = sum(Knob),
      prp_defect = sum(Defect),
      .groups = "drop"
    ) %>%
    mutate(across(c(prp_hollow, prp_double, prp_knob, prp_defect), ~ .x / tuber_count))
  
  # summary by grade
  grade_summary <- df_clean %>%
    group_by(Plot, Grade) %>%
    summarise(
      tuber_count = n(),
      total_wt_oz = sum(Weight),
      mean_wt_oz = mean(Weight),
      prp_hollow = sum(Hollow),
      prp_double = sum(Double),
      prp_knob = sum(Knob),
      prp_defect = sum(Defect),
      .groups = "drop_last"
    ) %>%
    mutate(prp_tubers = tuber_count / sum(tuber_count), .before = tuber_count) %>%
    mutate(prp_total_wt = total_wt_oz / sum(total_wt_oz), .before = total_wt_oz) %>%
    ungroup() %>%
    mutate(across(c(prp_hollow, prp_double, prp_knob, prp_defect), ~ .x / tuber_count))
  
  # add yields if plot size given
  if (is.numeric(acreage)) {
    fn <- function(df) {
      df %>%
        mutate(total_yield = total_wt_oz / 1600 / acreage, .before = total_wt_oz) %>%
        mutate(tubers_per_acre = tuber_count / acreage, .before = tuber_count)
    }
    totals_summary <- fn(totals_summary)
    grade_summary <- fn(grade_summary)
  }
  
  # save to file
  summary_file <- file.path(out_dir, paste(trial_name, "- Grading summary (long format).csv"))
  grade_summary %>%
    janitor::clean_names(case = "big_camel") %>%
    write_csv_with_header(
      summary_file,
      header = c(paste(trial_name, "- Grading summary (long format)"), csv_headers)
    )
  cat("- Saved grading summary (long format) to:", summary_file, "\n")
  
  # wide format
  grade_summary_wide <- grade_summary %>%
    pivot_wider(
      names_from = "Grade",
      values_from = -c("Plot", "Grade")
    )
  
  # join the summaries and the culls
  summary <- totals_summary %>%
    left_join(grade_summary_wide, by = "Plot") %>%
    left_join(culls, by = "Plot")
  
  # add culled yield
  if (is.numeric(acreage)) {
    summary <- summary %>%
      mutate(culled_yield = cull_wt_lbs / 100 / acreage, .after = cull_wt_lbs)
  }
  
  # reorganize columns
  col_names_sorted <- sort(names(summary)[-1])
  summary <- summary %>%
    select("Plot", all_of(col_names_sorted)) %>%
    select(
      "Plot",
      starts_with("total"),
      starts_with("cull"),
      starts_with("mean"),
      starts_with("tuber"),
      starts_with("prp_total"),
      starts_with("prp_tubers"),
      starts_with("prp"),
      everything()
    )
  
  # save to file
  summary_file <- file.path(out_dir, paste(trial_name, "- Grading summary (wide format).csv"))
  summary %>%
    janitor::clean_names(case = "big_camel") %>%
    write_csv_with_header(
      summary_file,
      header = c(paste(trial_name, "- Grading summary (wide format)"), csv_headers)
    )
  cat("- Saved grading summary (wide format) to:", summary_file, "\n")
  
  
  # Create plot ----
  
  cat("\nGenerating summary plot...\n")
  
  if (is.numeric(summary$Plot)) {
    summary <- summary %>%
      arrange(Plot) %>%
      mutate(Plot = fct_inorder(as.character(Plot)))
  }
  
  plt_data <- grade_summary %>%
    # group_by(Plot) %>%
    mutate(across(Plot, ~fct_inorder(as.character(.x)))) %>%
    mutate(pct_wt = total_wt_oz / sum(total_wt_oz), .by = Plot) %>%
    mutate(across(pct_wt, ~if_else(.x >= .025, scales::percent(.x, 1), "")))
  
  plt_opts <- if (is.numeric(acreage)) {
    list(
      var = "total_yield",
      y = "Total yield (cwt/ac)",
      subtitle = sprintf("\nHarvested area: %s sq. ft. (%s acres)", area, signif(acreage, 4))
    )
  } else {
    list(
      var = "total_wt_oz",
      name = "Total weight (oz)",
      subtitle = ""
    )
  }
  
  plt <- plt_data %>%
    ggplot(aes(x = Plot, y = .data[[plt_opts$var]], fill = Grade, label = pct_wt)) +
    geom_col(color = "black", linewidth = 0.25, position = "stack") +
    geom_text(position = position_stack(vjust = 0.5)) +
    scale_y_continuous(
      expand = expansion(c(0, 0.1)),
      breaks = scales::breaks_pretty()
    ) +
    labs(
      title = paste(trial_name, "- Yield summary"),
      subtitle = paste0("Grading criteria: ", grading_criteria, plt_opts$subtitle),
      x = "Plot",
      y = plt_opts$name
    ) +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
  
  show(plt)
  
  plt_file <- file.path(out_dir, paste(trial_name, "- Yield summary.png"))
  cat("- Saved plot image to:", plt_file, "\n")
  suppressMessages(ggsave(plt_file, plt, h = 7, w = 10))
  
  
  # Finish up ----
  
  assign("all_tubers", df_clean, envir = .GlobalEnv)
  assign("grade_summary", grade_summary, envir = .GlobalEnv)
  assign("totals_summary", summary, envir = .GlobalEnv)
  assign("plt", plt, envir = .GlobalEnv)
  
  cat("\nAlso saved to local environment: all_tubers, grade_summary, totals_summary, plt")
}
