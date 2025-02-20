
#################
#REVISED ANALYSIS
#################

#Be sure to run Setup.script.R first.

######################

#FIRST - reading in data from 2022,2023,2024
fentfiles <- list.files(path = "../Data/UPDATED_fentanylquant/")

fentanyl_quant <- read_csv(paste0("../Data/UPDATED_fentanylquant/", fentfiles)) %>% 
  rename(Sample.ID = `Sample ID`) %>% 
  filter(!is.na(`City/Town`))

#############################################################

#matching on service address ddress

#there are 108 services - this will need some tidying
sitestowns <- table(fentanyl_quant$`Collection Site`, fentanyl_quant$`City/Town`) %>%  
  data.frame() %>% 
  filter(Freq != 0)

#list of addresses
sites <- read_csv("../Data/Sites.csv")

#This massive code chunk:
postcodedataset <- sitestowns %>% 
  rename(Name = Var1,
         `City/Town`= Var2) %>% 
  select(-Freq) %>% 
  #formats the list of sites/towns from the quant dataset so they match the sites datset (extremely fussy, lots of manual changes)
  mutate(Name = case_when(Name == "Abby Drug War Survivors" ~ "Abby Drug War Survivors (NOMAD)",
                          Name == "The Hub" ~ "Phoenix Society The Hub",
                          Name == "CMHA" ~ "CMHA (Albert St.)",
                          Name == "Fraser Canyon Clinic" ~ "Fraser Canyon Clinic (Hope)",
                          Name == "Public Health Unit Chilliwack" ~ "Public Health Unit (Chilliwack)",
                          Name == "Ruth and Naomi's Mission" ~ "Ruth and Naomi's (Chilliwack)",
                          Name == "The Haven" ~ "MCS The Haven",
                          T ~ Name)) %>% 
  #matches the dataset of addresses for those sites
  left_join((sites %>% 
               select(Name, `City/Town`, Address)),
            by = c("Name", "City/Town")) %>% 
  #further manual munging of names and also grabbing addresses manually
  mutate(Address = case_when(Name == "Pacific Community Resources Society" ~ "45921 Hocking Ave, Chilliwack, BC V2P 1B5",
                             Name == "Northwest ICMT" ~ "103-4624 Greig Ave., Terrace, BC V8G 1M9",
                             Name == "Phoenix Society The Hub" & `City/Town` == "Nelson" ~ "521 vernon St, Nelson, BC V1L 1S5",
                             Name == "Phoenix Society The Hub" & `City/Town` == "Vancouver" ~ "13686 94a Ave, Surrey, BC V3V 1N1",
                             Name == "OneSky Education Event 2023" ~ "330 Ellis Street Penticton, BC V2A 4L7",
                             Name == "OPS" ~ "4910 Joyce Ave, Powell River, BC V8A 0S5",
                             Name == "Sechelt Hospital MHSU 2" ~ "5000 Joyce Ave, Third floor Powell River, BC V8A 5R3",
                             Name == "Turning Points NW" ~ "3862 Broadway Ave, Smithers, BC V0J 2N0",
                             Name == "SOWINS Drop-In Centre" ~ "1027 Westminster Ave W, Penticton, BC V2A 1L4",
                             Name == "Victory Shelter (Penticton)" ~ "250 Winnipeg St, Penticton, BC V2A 5M3",
                             Name == "FVDED in the Park 2022" ~ "13428 Old Yale Rd, Surrey, BC V3T 3C7",
                             Name == "Osborn Shelter" ~ "27 W Hastings St, BC V6B 1G5",
                             Name == "Maple Ridge Mods" ~ "22548 Royal Crescent, Maple Ridge, BC V2X 2M3",
                             str_detect(Address, "Vancouver|Vernon|Van Vliet") ~ str_remove(Address, "Vancouver|Vernon|Van Vliet"),
                             T ~ Address)) %>% 
  distinct() %>% 
  #extracting postcodes into their own column
  mutate(postcode = str_match_all(Address, "V.*$"),
         postcode = str_remove(postcode, ","),
         postcode = str_remove(postcode, 'Canada')) %>% 
  rename(`Collection Site` = Name)

###############################################

#NOW MATCHING ADDRESS INFO BACK TO BASE FENTANYL CONCENTRATION DATASET

final_basedataset <- fentanyl_quant %>% 
  #Having to reformat the names in the fentanyl dataset to make sure they are consistent
  mutate(`Collection Site` = case_when(`Collection Site` == "Abby Drug War Survivors" ~ "Abby Drug War Survivors (NOMAD)",
                          `Collection Site` == "The Hub" ~ "Phoenix Society The Hub",
                          `Collection Site` == "CMHA" ~ "CMHA (Albert St.)",
                          `Collection Site` == "Fraser Canyon Clinic" ~ "Fraser Canyon Clinic (Hope)",
                          `Collection Site` == "Public Health Unit Chilliwack" ~ "Public Health Unit (Chilliwack)",
                          `Collection Site` == "Ruth and Naomi's Mission" ~ "Ruth and Naomi's (Chilliwack)",
                          `Collection Site` == "The Haven" ~ "MCS The Haven",
                          T ~ `Collection Site`)) %>% 
  left_join(postcodedataset, by = c("Collection Site", "City/Town")) %>%   
  mutate(`City/Town` = case_when(`City/Town` == "Trail" ~ "Trail - Fruitvale", 
                                 T ~ `City/Town`)) %>% 
  #these two services have the same name in the same town. Removing because they are duplicated in the join procedure
  filter(!Address %in% c("13459 107a Ave, Surrey, BC V3T 4E3", "1440 - 14th Avenue , BC , V1B 2T1"))   #%>% 
  #grabbing lat/lon for descriptive map. Takes about a minute to run 
  #geocode(Address, method = 'arcgis', lat = latitude, long = longitude)

#grabbing coordinates for future use
# COORDINATES <- final_basedataset %>%
#   select(`Collection Site`, `City/Town`, Address, latitude, longitude)


#So of the overall sample there are six with no address reported (which are all mobile sites)
final_basedataset %>% 
  filter(is.na(latitude)) 

###############################################
#Population centre info
#https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=9810001101&geocode=A000011124
popcentre <- read_csv("../Data/Populationcentre.csv") %>% 
  select(1:2) %>% 
  slice(9:n()) %>% 
  janitor::row_to_names(row_number = 1) %>% 
  rename(`City/Town` = `Geographic name`) 

#merging (and final name fixing)
finalbasedataset_vis <- left_join(final_basedataset, popcentre, by = 'City/Town') %>% 
  mutate(`Geographic area type` = case_when(`City/Town` %in% c("Surrey", "New Westminster", "Burnaby", "Coquitlam", "Maple Ridge", "Langley") ~ "Large urban population centre",
                                            T ~ `Geographic area type`),
         `Collection Site` = case_when(str_detect(`Collection Site`, "Gordon") ~ "Gordon Shelter",
                                       str_detect(`Collection Site`, "Terrace") ~ "Terrace ICMT",
                                       `Collection Site` == 'GYDT (West)' ~ "Get Your Drugs Tested",
                                       T ~ `Collection Site`),
  ) %>% 
  filter(!is.na(latitude))
