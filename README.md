# AgRay data wrangling
For processing output from the AgRay potato grading machine at Hancock, WI

### How to use
1. Install R, Rstudio, tidyverse
2. Run the agray function definition or source the file with `source("agray.r")`.
3. Call the `agray(file)` function with arguments where `file` is the AgRay csv output file.
4. The script will output a graded tuber list, the culls list, and a per-plot summary of counts, weights, and quality by tuber grade. Outputs will be written to the same directory as the csv, under a sub-directory with the same name as the input csv.
5. You can alter the grading scheme by changing the values in the `case_when()` call on lines 30-32. Values should be in inches. Start with the largest size and work downwards. After changing these values you must re-run the function definition or call `source("agray.r")`.
