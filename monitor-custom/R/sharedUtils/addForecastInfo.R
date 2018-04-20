addForecastInfo <- function(ft) {
  
  ft <- ft %>% 
    bg(i = ~ Yesterday == 1, j = ~ Yesterday, bg = AQI$colors[1]) %>% 
    bg(i = ~ Yesterday == 2, j = ~ Yesterday, bg = AQI$colors[2]) %>% 
    bg(i = ~ Yesterday == 3, j = ~ Yesterday, bg = AQI$colors[3]) %>% 
    bg(i = ~ Yesterday == 4, j = ~ Yesterday, bg = AQI$colors[4]) %>% 
    bg(i = ~ Yesterday == 5, j = ~ Yesterday, bg = AQI$colors[5]) %>% 
    bg(i = ~ Yesterday == 6, j = ~ Yesterday, bg = AQI$colors[6]) %>% 
    
    bg(i = ~ Today == 1, j = ~ Today, bg = AQI$colors[1]) %>% 
    bg(i = ~ Today == 2, j = ~ Today, bg = AQI$colors[2]) %>% 
    bg(i = ~ Today == 3, j = ~ Today, bg = AQI$colors[3]) %>% 
    bg(i = ~ Today == 4, j = ~ Today, bg = AQI$colors[4]) %>% 
    bg(i = ~ Today == 5, j = ~ Today, bg = AQI$colors[5]) %>% 
    bg(i = ~ Today == 6, j = ~ Today, bg = AQI$colors[6]) %>%
    
    bg(i = ~ Tomorrow == 1, j = ~ Tomorrow, bg = AQI$colors[1]) %>% 
    bg(i = ~ Tomorrow == 2, j = ~ Tomorrow, bg = AQI$colors[2]) %>% 
    bg(i = ~ Tomorrow == 3, j = ~ Tomorrow, bg = AQI$colors[3]) %>% 
    bg(i = ~ Tomorrow == 4, j = ~ Tomorrow, bg = AQI$colors[4]) %>% 
    bg(i = ~ Tomorrow == 5, j = ~ Tomorrow, bg = AQI$colors[5]) %>% 
    bg(i = ~ Tomorrow == 6, j = ~ Tomorrow, bg = AQI$colors[6])
  
  return(ft)
    
}
