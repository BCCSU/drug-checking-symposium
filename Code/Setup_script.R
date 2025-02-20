#Setup script

#Packages
library(tidyverse)
library(tidygeocoder)
library(tmap)
library(sf)
library(leaflet)
library(htmlwidgets)

#Functions

#Function for grabbing key descriptive statistics (which are ultimately appended to the original dataset)
#Arguments required for dataframe and grouping variable
grouping_fun <- function(df, groupvar) {
  df %>% 
    group_by({{groupvar}}) %>%
    summarise(med = median(Quant),
              mean = mean(Quant),
              interquartile = IQR(Quant),
              count = n())
}

#creating a theme for the combined plots
#arguments required are dataframe, grouping variable and size of y-axis
timeseries_theme <- function(df, x, num) {
  df %>%
    ggplot(aes(
      yearmonth,
      y = medcont,
      group = {{x}},
      color = {{x}}
    )) +
    #geom_line() +
    #geom_point() +
    geom_smooth(se = F, method = 'loess') +
    theme_bw() +
    scale_y_continuous(limits = c(0, num)) +
    theme(
      axis.text.x = element_text(
        angle = 45,
        vjust = 0.5,
        hjust = .5
      ),
      legend.title = element_blank(),
      legend.position = 'bottom',
      legend.text = element_text(size = 10)
    ) +
    labs(x = "", y = "Median fentanyl concentration (%)") +
    guides(colour = guide_legend(nrow = 1))
}

#function to extract/format monthly median concentration and combine with overall
#Requires dataframe and groupinb variable
combinewithoverall <- function(df, var){
  df %>%
    mutate(yearmonth = format(as.Date(`Visit Date`), "%Y-%m")) %>%
    group_by(yearmonth, {{var}}) %>%
    summarise(medcont = median(Quant))
}
