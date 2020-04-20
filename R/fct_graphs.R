#' Plot of Histogram by study arm
#'
#' @param df 
#' @param var 
#'
#' @noRd 
plotHistogramByArm <- function(df, var) {
  ggplot(df, aes({{ var }})) +
    geom_histogram(aes(fill = ACTARM), bins = 100) +
    facet_wrap(~ ACTARM, ncol = 1) +
    theme(
      legend.position = "none",
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      axis.title.y = element_blank()
    )
}



#' Show lab measurements
#'
#' @param df  df with lab maeasurements
#' @param visit Visit to display (one of SCREENING, BASELINE, ...)
#' @param test Test to display (one of IGA, CRP, ALT)
#' 
#' @noRd 
plotLabMeasurementAtVisit <- function(df, visit, test) {
  df %>% 
    filter(
      AVISIT == visit,
      LBTESTCD == test
    ) %>% 
    plotHistogramByArm(AVAL)
}



#' Plot of proportions by study arm
#'
#' @param df 
#' @param var 
#'
#' @noRd 
plotProportionsByArm <- function(df, var) {
  df %>% 
    group_by(ACTARM, {{ var }}) %>% 
    summarise(
      count = n()
    ) %>% 
    mutate(
      prop = count / sum(count)
    ) %>% 
    ggplot(aes(prop, {{ var }}, color = ACTARM)) +
    geom_point(size = 4, alpha = .7) +
    labs(x = "Proportion") +
    theme(
      panel.grid.minor.x = element_blank(),
      panel.grid.major.x = element_blank(),
      axis.title.y = element_blank()
    )
}  