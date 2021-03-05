# AgRay data wrangling
For processing output from the AgRay potato grading machine at Hancock, WI

### How to use
1. Install R, Rstudio, tidyverse
2. Run the agray function definition
3. Call the `agray(file, alias)` function with arguments where `file` is the AgRay csv output file, and `alias` is the trial name to be appended to the output filenames
4. The script will output a graded tuber list, the culls list, and a per-plot summary of counts, weights, and quality by tuber grade. Outputs will be written to the `/out` directory within the agray project directory.
5. You can alter the grading scheme by changing the values in the `case_when()` call on lines 25-28. Values should be in inches. Start with the largest size and work downwards. After changing these values you must re-run the function definition.