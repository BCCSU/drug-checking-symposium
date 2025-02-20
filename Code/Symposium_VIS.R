#symposium VISUALISATION
#first, visualising all the services

#reading in Shapefile
healthauthority_SF<- read_sf("../Data/shapefile/B.C._Health_Authority_Boundaries_(with_Provincial_Health_Services_Boundary)/B.C._Health_Authority_Boundaries_(with_Provincial_Health_Services_Boundary).shp") %>% 
  select(3, 9:12)

initmap <- left_join(healthauthority_SF,
          (finalbasedataset_vis %>% 
             mutate(
               HA_Name = case_when(
                 `Health Authority` == "Fraser Health" ~ "Fraser",
                 `Health Authority` == "Interior Health" ~ "Interior",
                 `Health Authority` == "Island Health" ~ "Vancouver Island",
                 `Health Authority` == "Northern Health" ~ "Northern",
                 T ~ `Health Authority`
               )
             ) %>% 
             group_by(HA_Name, `Collection Site`, latitude, longitude) %>% 
             summarise(medcont = round(median(Quant),1),
                       numberofsamples = n())),
           # select(HA_Name,`Collection Site`, 11,12) %>% 
           # distinct()),
          by = "HA_Name") %>% 
  slice(-1) 

#MUCH EASIER WITH LEAFLET THAN TMAP
library(leaflet)

#FIG 1: Point map
Fig1 <- leaflet(initmap) %>% 
  addTiles() %>% 
  addMarkers(~longitude, 
             ~latitude,
             label = ~as.character(`Collection Site`), 
             popup = ~as.character(paste0("Median fentanyl concentration: ",medcont, '%; ',
                                          "Number of samples tested: ", numberofsamples)))

saveWidget(Fig1, "../Output/initmap.html")

#FIG 1a: circle map (isn't this fun!)
leaflet(
  (initmap %>% filter(`Collection Site` != 'Get Your Drugs Tested'))) %>% 
  addTiles() %>% 
  addCircleMarkers(~longitude,
                   ~latitude,
                   radius = ~numberofsamples*.05)

####################################################################

#Summary of data

#Figure 2: histogram of all samples, showing near bi-modal distribution
FIG2 <- finalbasedataset_vis  %>% 
  ggplot(aes(x=Quant)) +
  geom_histogram() +
  theme_bw() +
  geom_vline(aes(xintercept = median(Quant))) +
  labs(x = 'Fentanyl concentration (%)',
       y = "Count",
       title = 'Figure 1: Fentanyl concentration across all services, 2022-24') +
  scale_y_continuous(label = scales::comma) +
  annotate("text", x = 31, y=1600, label = "Median concentration: 18.6%")

ggsave("../Output/images/fig2.png", height = 8, width = 10)

#Figure 3: density chart, showing that Vancouver Coastal is more or less responsible. Need to disaggregate (consider faceting?)
FIG3 <- finalbasedataset_vis %>% 
  ggplot(aes(x = Quant, group = `Health Authority`, fill = `Health Authority`)) +
  geom_density(adjust = 1.5, alpha = .5) +
  # facet_wrap(~`Health Authority`) +
  theme_bw() +
  theme(legend.position = 'bottom')  +
  labs(x = 'Fentanyl concentration (%)',
       y = '',
       title = 'Figure 2: Fentanyl concentration by health authority, 2022-24')

ggsave("../Output/images/fig3.png", height = 8, width = 10)

#Figure 4: boxplot

#need to create a function for grabbing key descriptives
#SEE Setup_script.R for grouping_fun() function

#this grabs descriptive statistics by health authority, which I'm going to append....
checking <- grouping_fun(finalbasedataset_vis, `Health Authority`)

#....here:
fig4 <- finalbasedataset_vis %>%
  left_join(checking, by = "Health Authority") %>%
  mutate(`Health Authority` = paste0(`Health Authority`, "\n (n = ",scales::comma(count),")")) %>% 
  ggplot(aes(reorder(`Health Authority`, med), y = Quant)) +
  geom_boxplot() +
  coord_flip() +
  labs(y = "Fentanyl concentration (%)",
       x = "Health Authority",
       title = 'Figure 3: Fentanyl concentration by Health Authority (plus GYDT)',
       subtitle = "N = 13,768") +
  theme_bw()

ggsave("../Output/images/fig4.png", height = 6, width = 10)

###################################

#TIME SERIES - OVERALL
Overall <- finalbasedataset_vis %>% 
  mutate(yearmonth = format(as.Date(`Visit Date`), "%Y-%m")) %>% 
  group_by(yearmonth) %>% 
  summarise(medcont = median(Quant)) %>% 
  mutate(`Health Authority` = "OVERALL") %>% 
  select(yearmonth, `Health Authority`, medcont)

#See Setup.script.R for timeseries_theme() function

#THREE SEPARATE TIME SERIES CHARTS
#THIS IS THE FIRST CHART - overall fentanyl concentration
fig5 <- Overall %>% 
  timeseries_theme(`Health Authority`, 33) +
  geom_line() +
  geom_point() +
  scale_color_manual(values = 'black') 

ggsave("../Output/images/fig5.png", height = 8, width = 10)

#See Setup_script.R for combinewithoverall() function

#THIS IS THE SECOND CHART - overall fentanyl concentration overlaid with other health authorities
fig6 <- bind_rows(Overall,
                  combinewithoverall(finalbasedataset_vis, `Health Authority`)) %>%
  timeseries_theme(`Health Authority`, 33) +
  geom_line() +
  geom_point() +
  scale_color_manual(values = c("#F8766D", "#A3A500",  "#00BF7D", "#00B0F6", "black", "#E76BF3")) 

ggsave("../Output/images/fig6.png", height = 8, width = 10)

#THIS IS THE THIRD CHART - Same as previous but with only smoothing function remaining
fig7 <- bind_rows(Overall,
                  combinewithoverall(finalbasedataset_vis, `Health Authority`)) %>%
  timeseries_theme(`Health Authority`, 33) +
  scale_color_manual(values = c("#F8766D", "#A3A500",  "#00BF7D", "#00B0F6", "black", "#E76BF3")) 

ggsave("../Output/images/fig7.png", height = 8, width = 10)

##############################################
#Now looking at rural/urban

#Creating boxplot for rural/urban
fig8 <- left_join(finalbasedataset_vis,
                  grouping_fun(finalbasedataset_vis, `Geographic area type`),
                  by = "Geographic area type") %>% 
  mutate(`Geographic area type` = paste0(`Geographic area type`, "\n (n = ",scales::comma(count),")")) %>% 
  ggplot(aes(reorder(`Geographic area type`, med), y = Quant)) +
  geom_boxplot() +
  coord_flip() +
  labs(y = "Fentanyl concentration (%)",
       x = "Urban/Rural Index",
       title = 'Figure 7: Fentanyl concentration by urban/rural index, 2022-2024',
       subtitle = "N = 13,768") +
  theme_bw()

ggsave("../Output/images/fig8.png", height = 6, width = 10)

#with the other classificaitons overlaid
fig9 <- bind_rows((Overall %>% 
                     rename(`Geographic area type` = `Health Authority`)),
                  combinewithoverall(finalbasedataset_vis, `Geographic area type`)) %>%
  timeseries_theme(`Geographic area type`, 33) +
  geom_point()+
  geom_line()+
  scale_color_manual(values = c("#F8766D", "#B79F00", 'black', "#C77CFF"))

ggsave("../Output/images/fig9.png", height = 8, width = 10)

#And previous figure with only the smoothing function left
fig10 <- bind_rows((Overall %>% 
                     rename(`Geographic area type` = `Health Authority`)),
                  combinewithoverall(finalbasedataset_vis, `Geographic area type`)) %>%
  timeseries_theme(`Geographic area type`, 33) +
  scale_color_manual(values = c("#F8766D", "#B79F00", 'black', "#C77CFF"))

ggsave("../Output/images/fig10.png", height = 8, width = 10)

##########################
##########################
##########################
##########################
##########################
##########################
#LET US MAP

#HSDA lookup (which I had to do manually (yuck))
HSDA_lookup <- readxl::read_excel("../Data/HSDA.xlsx")

#merging onto base dataset
HSDA_Mapping <- left_join(finalbasedataset_vis,
                          HSDA_lookup,
                          by = c('Collection Site', 'Health Authority')) %>%
  mutate(
    HSDA = case_when(
      `Collection Site` == "SafePoint" ~ 'Fraser South',
      `Collection Site` == "BCG Williams Lake (Remote)" ~ 'Northern Interior',
      T ~ HSDA))

#Now summarising for median concentration by HSDA, as well as health authority
map_datajoin <- left_join((HSDA_Mapping %>%
                             group_by(HSDA, `Health Authority`) %>%
                             summarise(HSDA_med = median(Quant))),
                          (HSDA_Mapping %>% 
                             group_by(`Health Authority`) %>% 
                             summarise(HA_med = median(Quant))), 
                          by = 'Health Authority')  %>%
  mutate(overallmed = median(finalbasedataset_vis$Quant),
         `HSDA - HA median` = round(HSDA_med - HA_med, 1),
         `HSDA - BC median` = round(HSDA_med - overallmed, 1))

#Grabbing shapefile from: https://www150.statcan.gc.ca/n1/pub/82-402-x/2023001/hrbf-flrs/carto/ArcGIS/HR_059b22a_e.zip
HSDA <- read_sf("../Data/HR_059b22a_e/HR_059b22a_e.shp")  

#Joining to shapefile
HSDA_mapdata <- left_join(HSDA, (map_datajoin %>%
                                   mutate(HSDA = paste0(HSDA, " Health Service Delivery Area")) %>%
                                   rename(ENGNAME = HSDA)), by = 'ENGNAME') %>% 
  pivot_longer(10:11, names_to = 'cat', values_to = 'diff')

#MAP
#FIrst map - comparing HSDA to BC median
Map1 <- 
  HSDA_mapdata %>% 
  filter(cat == "HSDA - BC median") %>% 
  tm_shape() +
  tm_polygons(
    'diff',
    palette = 'seq',
    style = "cont",
    title = "Difference between \nHSDA and BC (pp.)",
    lwd = .5
  ) +
  tm_layout(aes.palette = list(seq = "-RdBu"),
            legend.outside = T,
            legend.text.size = 1) 

#Inset for first map
Map1_inset <- HSDA_mapdata %>% 
  filter(cat == "HSDA - BC median",
         str_detect(FRENAME, "Vancouver|Garibaldi|Island|Fraser|Richmond")) %>% 
  tm_shape() +
  tm_polygons(
    'diff',
    palette = 'seq',
    style = "cont",
    title = "Difference between \nHSDA and BC (pp.)",
    lwd = .5
  ) +
  tm_layout(aes.palette = list(seq = "-RdBu"),
            legend.outside = T,
            legend.text.size = 1,
            legend.show = F) 

#Map2 = Comparing HSDA median to HA median
Map2 <- tm_shape(HSDA_mapdata) +
  tm_polygons(
    'diff',
    palette = 'seq',
    style = "cont",
    title = "Difference (pp.)",
    lwd = .5
  ) +
  tm_layout(aes.palette = list(seq = "-RdBu"),
            legend.outside = T,
            legend.text.size = 1) +
  tm_facets(by = "cat")

#Saving
tmap_save(Map1, '../Output/images/map.png', width = 6, height = 8, dpi = 300, units = 'in')
tmap_save(Map1_inset, '../Output/images/map1a.png', width = 6, height = 8, dpi = 300, units = 'in')
tmap_save(Map2, '../Output/images/map2.png', width = 8, height = 8, dpi = 300, units = 'in')
