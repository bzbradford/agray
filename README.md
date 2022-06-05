# AgRay data wrangling

For processing output from the AgRay potato grading machine at Hancock, WI

## How to use

1.  Install R, Rstudio, tidyverse package

2.  Run the `grade` function definition or source the file with `source("agray.r")` from your analysis script.

3.  Call the `grade(file, name, grades)` with your data.

    -   `file` is the AgRay csv output file location (in quotes). This is required.
    -   `name` is either taken from the filename, or can be supplied.
    -   `grades` is a named list of grades and sizes, or left blank to use the default. Last grade should be 0. Units must be inches.

4.  The script will output a graded tuber list, the culls list, and a per-plot summary of counts, weights, and quality by tuber grade. Outputs will be written to the same directory as the csv, under a sub-directory with the same name as the input csv, or the user-supplied name.

5. Weights are given in ounces and sizes in inches.

## Example usage

    # my_analysis_script.R

    source("agray.r")


    ### Using default name and grading scheme ###
    # This will output to a folder called 'my_agray_file' within the current project directory

    grade(file = "my_agray_file.csv")


    ### Supplying your own trial name ###
    # This will output to a folder called 'CPB Trial' within the current project directory

    grade(
      file = "my_agray_file.csv",
      name = "CPB Trial"
    )


    ### Supplying your own tuber grades ###

    my_grades <- list(
      "Large" = 3,
      "Med" = 2,
      "Small" = 1,
      "Undersize" = 0
    )

    grade(
      file = "my_agray_file.csv",
      name = "Alternate tuber grades trial",
      grades = my_grades
    )
