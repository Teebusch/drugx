
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Drug X Clinical Trial

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

This is a Shiny app using some (simulated) clinical data to visualize
the results of a clinical trial for the fictive drug “Drug X”.

## Installation and Use

An online demo of the app can be found at
<https://teebusch.shinyapps.io/drugx/>

You can install and run the app locally from
[Github](https://github.com/teebusch/drugx) with:

``` r
# Install devtools if needed
if(!require(devtools)) install.packages("devtools")
devtools::install_github("Teebusch/drugx")
library(drugx)
drugx::run_app()
```

## Drug X Clinical Trial

### Study Design

  - One study, 9492 observations
  - 3 arms (A = drug, B = placebo, C = combination)
  - ~150 patients in each arm
  - 7 Repeated measures: screening, baseline, 5 weekly follow-ups (day
    8-36)
  - consistent, complete data (except some “undifferentiated” / “other”
    patient info)

#### Measurements

##### Patient Info

  - Age
  - Sex
  - Race

##### Measured once at baseline:

  - **Biomarker 1** (numerical; Mean around 6, range 0.3-22.4)
  - **Biomarker 2** (categorical; low, medium, high)

##### Measured at each visit:

  - **ALT** is increased with liver damage and is used to screen for
    and/or monitor liver disease
  - **CRP** is a blood test marker for inflammation in the body (e.g.,
    chronic inflammatory diseases such as lupus, vasculitis, or
    rheumatoid arthritis (RA)
  - **IGA** high levels might be caused by allergies, chronic
    infections, autoimmune disorders such as RA

### Research questions (assumed)

##### Effect of Drug X on Lab values…

  - change compared to baseline
  - difference between drug X / placebo / combination (group difference
    between arms)

##### controlling for / as a function of / correlation with…

  - Age
  - Sex
  - 2 Biomarkers
  - Screening / Baseline values

## Requirements for the Shiny App

### Deliverable

  - Shiny application to explore the properties of the data sources.
  - minimum of 2 different visualization plots
  - use of a version control
  - formated as a RStudio project, or gzip R Package.
  - delivered within 8 days of receiving these instructions.
  - 4-5 hours of work

### High level goals

  - help explore associations between background variables and treatment
  - filter data to visualize subgroups
  - allow high level overview as well as investigating subgroups and
    individuals
  - comparing data in different study arms

### Necessary

  - \[x\] sanity checks / cleaning of supplied data
  - \[x\] graphs of background variables (for all data and by study arm)
  - \[x\] allow to filter data by age, race, sex, biomarkers 1 and 2,
    performance at baseline subjects
  - \[x\] graph that shows development of ALT, CRP, IGA for individuals
  - \[x\] graph that shows development of ALT, CRP, IGA by study arm
    (summary)
  - \[x\] deliver as R package

## ToDo / Possible next steps

  - \[ \] allow data to filter by performance of screening, individual
    patient
  - \[ \] complete function documentation
  - \[ \] optimize loading times by refactoring data transforms and
    filtering
  - \[ \] comprehensive test suite
  - \[ \] statistical models with significance tests
  - \[ \] interactive plotly plots with hover functionality
  - \[ \] tables with summary statistics

## Open Questions

  - What is being done in the “combination” condition?
