theme_update(
  panel.background = element_rect(fill = "#FBFAF9"),
  text = element_text(size = 14)
)

#' Custom color scale
#'
#' @noRd 
scale_color_drugx <- function(...)
  scale_colour_manual(..., values = c('#2C3256', '#C2728D', '#CAB17A'))

#' Custom fill scale
#'
#' @noRd 
scale_fill_drugx <- function(...)
  scale_fill_manual(..., values = c('#2C3256', '#C2728D', '#CAB17A'))


#' Plot of Histogram by study arm
#'
#' @param df 
#' @param var 
#'
#' @noRd 
plotHistogramByArm <- function(df, var) {
  ggplot(df, aes({{ var }}, fill = ACTARM)) +
    geom_vline(aes(xintercept = mean( {{ var}} )), 
               color = "#E8E7E6", linetype = "solid")+
    geom_histogram(binwidth = 1) + 
    facet_wrap(~ ACTARM, ncol = 1) +
    scale_fill_drugx() +
    theme(
      legend.position = "none",
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank()
    )
}



#' Show lab measurements as histogram
#'
#' @param df  df with lab maeasurements
#' @param var variable to display
#' @param visit Visit to display (one of SCREENING, BASELINE, ...)
#' @param test Test to display (one of IGA, CRP, ALT)
#' 
#' @noRd 
plotLabMeasurementAtVisit <- function(df, var, visit, test) {
  df %>% 
    filter(
      AVISIT == visit,
      LBTESTCD == test
    ) %>% 
    plotHistogramByArm( {{ var}} )
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
    scale_color_drugx() +
    labs(x = "Proportion in Sample") +
    theme(
      panel.grid.minor.x = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(color = "#E8E7E6"),
      axis.title.y = element_blank(),
      legend.position = "none"
    )
}  



#' Plot timeline of all visits
#'
#' @param df 
#' @param var
#' @param max_sample 
#'
#' @noRd
plotAllVisits <- function(df, var, max_sample = 0) {
  # random sample of patients to display in background
  df_sample <- df %>%
    select(USUBJID, ACTARM) %>%
    group_by(ACTARM) %>%
    sample_n(min(max_sample, n())) %>%
    left_join(df, by = c("USUBJID", "ACTARM"))
  
  ggplot(df, aes(AVISIT, {{ var }}, group = USUBJID, color = ACTARM)) +
    geom_line(data = df_sample, alpha = .1) + 
    stat_summary(
      aes(group = 1),
      geom = "line",
      fun = mean,
      size = 1.5
    ) +
    facet_grid(ACTARM ~ LBTEST) +
    scale_color_drugx() +
    scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
    theme(
      legend.position = "none",
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank()
    )
}



#' Group Difference Plot
#'
#' @param df 
#' @param var measurement variable to be plotted (one of AVAL, AVAL_change)
#'
#' @noRd
plotGroupDifferences <- function(df, var) {
  df %>%  
    ggplot(aes(AVISIT, {{ var }}, group = ACTARM, color = ACTARM, fill = ACTARM)) +
    stat_summary(
      geom = "line", 
      position = position_dodge(width = 0.4),
      fun = mean
    ) +
    stat_summary(
      geom = "pointrange", 
      fun.data = "mean_se", 
      position = position_dodge(width = 0.4),
      alpha = .4
    ) +
    facet_wrap(~LBTEST) +
    scale_color_drugx() +
    scale_fill_drugx() +
    scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
    theme(
      legend.position = "bottom",
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank()
    )
}
