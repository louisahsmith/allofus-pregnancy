# Identification and description of pregnancies in All of Us

This repository contains the code and data used to identify and describe pregnancies in the All of Us Research Program. The algorithm used to identify pregnancy episodes is from Jones et al. 2023<sup>1</sup>, available in [this repository](https://github.com/jonessarae/n3c_pregnancy_cohort). The code was translated into R and adapted to work on the All of Us Researcher Workbench with some small modifications.

The repository is organized as follows:

- `code/`: R code to identify pregnancies and describe the cohort. These are in the form of quarto files. `01-run-hipps.qmd` contains the code to run the algorithm; the rest of the files just look at the results in various ways. These are rendered as html files, so the easiest way to view them is to follow these links:
  - https://htmlpreview.github.io/?https://github.com/louisahsmith/allofus-pregnancy/blob/main/code/01_run-hipps.html
  - https://htmlpreview.github.io/?https://github.com/louisahsmith/allofus-pregnancy/blob/main/code/02_examine-hipps.html
  - https://htmlpreview.github.io/?https://github.com/louisahsmith/allofus-pregnancy/blob/main/code/03_examine-hipps-restricted.html
  - https://htmlpreview.github.io/?https://github.com/louisahsmith/allofus-pregnancy/blob/main/code/04_validate-hipps.html
  - https://htmlpreview.github.io/?https://github.com/louisahsmith/allofus-pregnancy/blob/main/code/05_validate-hipps-restricted.html
  - https://htmlpreview.github.io/?https://github.com/louisahsmith/allofus-pregnancy/blob/main/code/06_predictors.html
  - https://htmlpreview.github.io/?https://github.com/louisahsmith/allofus-pregnancy/blob/main/code/07_compare-to-US-stats.html
  - https://htmlpreview.github.io/?https://github.com/louisahsmith/allofus-pregnancy/blob/main/code/08_available-data.html
  - https://htmlpreview.github.io/?https://github.com/louisahsmith/allofus-pregnancy/blob/main/code/09_make-maps.html
  - https://htmlpreview.github.io/?https://github.com/louisahsmith/allofus-pregnancy/blob/main/code/10_session-info.html
- `code/algorithm`: Jones et al.<sup>1</sup> algorithm translated to R and adapted to work on the All of Us Researcher Workbench
- `data/`: concept sets from Jones et al.<sup>1</sup> and national vital statistics data from CDC Wonder<sup>2</sup>
- `results/`: tables/figures/other results that do not contain individual-level data

The code creates datasets with individual-level data and saves them to `workbench/`, a directory that remains on the All of Us Researcher Workbench for privacy protection.


<sup>1</sup> Jones, Sara E, Katie R Bradwell, Lauren E Chan, Julie A McMurry, Courtney Olson-Chen, Jessica Tarleton, Kenneth J Wilkins, et al. "Who Is Pregnant? Defining Real-World Data-Based Pregnancy Episodes in the National COVID Cohort Collaborative (N3C)." JAMIA Open 6, no. 3 (July 4, 2023): ooad067. https://doi.org/10.1093/jamiaopen/ooad067.

<sup>2</sup> Centers for Disease Control and Prevention, National Center for Health Statistics. National Vital Statistics System, Natality on CDC WONDER Online Database. Data are from the Natality Records 2016-2022, as compiled from data provided by the 57 vital statistics jurisdictions through the Vital Statistics Cooperative Program. Accessed at http://wonder.cdc.gov/natality-expanded-current.html on Nov 27, 2023 12:43:06 PM.
