# example_grading.R

source("agray.r")


## Using default name and grading scheme ----

grade(file = "example_data.csv")

# This will output to a folder called 'example_data' within the current project directory


## Supplying your own trial name ----

grade(
  file = "example_data.csv",
  name = "Example trial"
)

# This will output to a folder called 'CPB Trial' within the current project directory


## Supplying the harvested area ----

grade(
  file = "example_data.csv",
  name = "Example trial with yield",
  area = 60
)

# Now the output files will include additional columns for calculated yields


## Supplying your own tuber grades ----

# remember the values are the minimum size for each class
size_grades <- list(
  "Large" = 3,
  "Med" = 2,
  "Small" = 1,
  "Undersize" = 0
)

grade(
  file = "example_data.csv",
  name = "Example trial - custom size grades",
  grades = size_grades,
  area = 60
)


# You can also grade by weight
weight_grades <- list(
  "8+ oz" = 8,
  "4-8 oz" = 4,
  "2-4 oz" = 2,
  "<2 oz" = 0
)

grade(
  file = "example_data.csv",
  name = "Example trial - custom weight grades",
  grades = weight_grades,
  grade_by = "Weight",
  area = 60
)
