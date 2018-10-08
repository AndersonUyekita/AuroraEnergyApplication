#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
#                                                         #
#   Author: AH Uyekita                                    #
#   Document: Job Application                             #
#   Position: Data and Information Manager                #
#   Code: QJ45141                                         #
#                                                         #
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++#

# 1. Setting Work directory #################################################################################### 
setwd("~")
setwd("~/Workplace")
Sys.setlocale("LC_ALL","English")

# 2. Libraries #############################################################################################
library(readxl)      # Necessary to import the .xlsx
library(dplyr)       # Data frame manipulating
library(datasets)    # Data frame manipulating
library(ggplot2)     # Plot graphics
library(ggrepel)     
library(grid)        
library(ggfortify)   
library(ggthemes)
library(tidyr)

# 3. Functions #############################################################################################


# 4. Importing files #######################################################################################

       ## 4.1. Importing Dataset from ComCom archive 
db_raw <- read_xlsx(path = "~/Workplace/Electricity-distributors-information-disclosures-2013March-2018.xlsm" , sheet = "Database" , col_names = TRUE)

       ## 4.2 Renaming column to remove spaces
colnames(x = db_raw) <- c("EDB",
                                  "Network",
                                  "Disclosure_year",
                                  "Schedule",
                                  "Schedule_reference",
                                  "Section",
                                  "Category",
                                  "Subcategory",
                                  "Description",
                                  "Year",
                                  "Forecast_year",
                                  "Units",
                                  "Value",
                                  "Text_input")

       ## 4.3. Importing the Polygons of New Zealand Regions
nz_region <- geojsonio::geojson_read("~/Workplace/nz_region.geojson",
                                     what = "sp")

       ## 4.4. Importing the Polygons of New Zealand Territories
nz_territories <- geojsonio::geojson_read("~/Workplace/nz_territories.geojson",
                                          what = "sp")

# 5. Creating a Neaty Data ####

# Ordering the raw data
db_raw <- db_raw %>% arrange(-Year,EDB,Network)

# Creating a new database with only EDB, Network, and Year.
# I used Section, Category, Subcategory, and Description to subset
# the unique pair EDB and Network.
db_neat <- db_raw %>% filter(Section %in% "9c: Overhead lines and underground cables",
                             Category %in% "Total overhead length",
                             Subcategory %in% "Circuit length (km)",
                             Description %in% "Total overhead length") %>%
                                   select(EDB,Network,Year)

# 6. Data Manipulation Explanation #########################################################

# I am going to fill the db_neat with data, each column is a new characteristc.
# For any kind of characteristc I will give prefixes in your variable, for instance:
#
#      PREFIXES                                  SUFIXES
#
#      Asset:        a                           Percentage: p
#      Reliability:  r
#      Economic:     e
#      Aurora:       au
#      All EDB:      edb
#      Table:        tb
#      Graphic:      gp
#
# List of variables:
#      r_un:         Interruptions Unplanned (Quantity)
#      r_pl:         Interruptions Planned (Quantity)
#      r_less_3:     Consumer restored in less than 3 hours
#      r_more_3:     Consumer restored in more than 3 hours
#      r_tp:         Transpower Interruptions
#      r_un_tp_saidi: Transpower SAIDI Unplanned
#      r_pl_tp_saidi: Transpower SAIDI Planned
#      r_un_tp_saifi: Transpower SAIFI Unplanned
#      r_pl_tp_saifi: Transpower SAIFI Planned
#      r_un_saidi:   Unplanned SAIDI
#      r_pl_saidi:   Planned SAIDI
#      r_saidi:      Total SAIDI
#      r_un_saifi:   Unplanned SAIFI
#      r_pl_saifi:   Planned SAIFI
#      r_saifi:      Total SAIFI
#      r_ae_saidi:   Adverse Enviroment cause of SAIDI
#      r_aw_saidi:   Adverse weather cause of SAIDI
#      r_idk_saidi:  Cause unknown cause of SAIDI
#      r_eqp_saidi:  Defective equipment cause of SAIDI
#      r_hum_saidi:  Human error cause of SAIDI
#      r_lig_saidi:  Lightning cause of SAIDI
#      r_tpi_saidi:  Third party interference  cause of SAIDI
#      r_veg_saidi:  Vegetation cause of SAIDI
#      r_wlf_saidi:  Wildlife cause of SAIDI
#      r_ae_saifi:   Adverse environment cause of SAIFI
#      r_aw_saifi:   Adverse weather cause of SAIFI
#      r_idk_saifi:  Cause unknown cause of SAIFI
#      r_eqp_saifi:  Defective equipment cause of SAIFI
#      r_hum_saifi:  Human error cause of SAIFI
#      r_lig_saifi:  Lightning cause of SAIFI
#      r_tpi_saifi:  Third party interference cause of SAIFI 
#      r_veg_saifi:  Vegetation cause of SAIFI
#      r_wlf_saifi:  Wildlife cause of SAIFI
#      a_wp:         Wood Pole
#      a_cp:         Concrete Pole or Steel Structure
#      a_ot:         Other type of Pole
#      a_bc:         Capacitor Banks
#      a_vr:         Voltage Regulator
#      a_li:         Length of Network
#      a_li_st:      Length of line used in Street Lighting
#      au_tb_li:     Line Length Table in Aurora
#      au_tb_li_st:  Street Lighting Line Table in Aurora
#      a_lv_li:      Low Voltage Lines
#      au_tb_lv_li:  Low Voltage Table in Aurora
#      a_lv_cb:      Low Voltage Cable
#      au_tb_lv_cb:  Low Voltage Cable Table in Aurora
#      au_tb_li_cb_st: Comparasion between LV Lines, Cables, and Street.
#      a_hv_li_oh:   Overhead High Voltage Line (wire conductor)
#      a_hv_li_swer: Single Wired earth return
#      a_hv_li_acc:  High Voltage Aerial Cable Conductor
#      a_hv_sb_li:   High Voltage Subtransmission Line
#      au_tb_hv_sb_li: High Voltage Subtransmission Lines in Aurora
#      a_hv_cb_xlpe:   HV Isolated Cable 
#      a_hv_cb_pilc: PILC Cable
#      a_hv_cb_sub:  Submarine Cable
#      a_li_tree:    Lines on areas with Tree Management
#      au_tb_li_tree: Length of Line under Tree Management
#      a_li_coge:    Line Length under Coastal and Geothermal effect
#      au_tb_li_coge: Table with Line Length under influency of Coastal and Geothermal
#      a_lv_con:     LV Consumer
#      a_li_st_oh:   Overhead Street Light lines
#      a_li_st_ug:   Underground Street Light Cables
#      a_li_oh:      Lines Overhead


# 6. Data Cleaning ###############################################

# In this chapter I will performe a extense calculation of ratios, infos, data, etc.


## 6.1. Reliability ####

       ### 5.1.1. Interruptions Unplanned #### 

db_neat  <- bind_cols(db_neat,
                     db_raw %>% 
                            filter(Section %in% "10(i): Interruptions",
                            Subcategory %in% "Number of interruptions",
                            Description %in% "Class C (unplanned interruptions on the network)") %>%
                                   select(r_un= Value)
                     )

       ### 5.1.2. Interruptions Planned ####

db_neat  <- bind_cols(db_neat,
                      db_raw %>% 
                             filter(Section %in% "10(i): Interruptions",
                                    Category %in% "Interruptions by class",
                                    Description %in% "Class B (planned interruptions on the network)") %>%
                             select(r_pl= Value)
                     )

       ### 5.1.3. Consumer restored in less than 3 hours ####

db_neat  <- bind_cols(db_neat,
                      db_raw %>% 
                             filter(Section %in% "10(i): Interruptions",
                                    Category %in% "Interruption restoration",
                                    !Description %in% ">3hrs") %>%            # ATENTION!! => DIFFERENT of Greater than 3 hour
                             select(r_less_3= Value)                          # It means: Less than 3 hours
                     )

       ### 5.1.5. Consumer restored in more than 3 hours ####

db_neat  <- bind_cols(db_neat,
                      db_raw %>% 
                             filter(Section %in% "10(i): Interruptions",
                                    Category %in% "Interruption restoration",
                                    Description %in% ">3hrs") %>%
                             select(r_more_3= Value)
                     )

       ### 5.1.7. Transpower influency in outages ####

## Interruptions Quantity
db_neat  <- bind_cols(db_neat,
                      db_raw %>% 
                             filter(Section %in% "10(i): Interruptions",
                                    Category %in% "Interruptions by class",
                                    Description %in% "Class A (planned interruptions by Transpower)") %>%
                                          select(r_tp= Value) +
                      db_raw %>% 
                            filter(Section %in% "10(i): Interruptions",
                                   Category %in% "Interruptions by class",
                                   Description %in% "Class D (unplanned interruptions by Transpower)") %>%
                                          select(r_tp= Value)
                     )

## SAIDI Unplanned
db_neat  <- bind_cols(db_neat,
                      db_raw %>% 
                             filter(Section %in% "10(i): Interruptions",
                                    Subcategory %in% "SAIDI",
                                    Description %in% "Class D (unplanned interruptions by Transpower)") %>%
                             select(r_un_tp_saidi= Value)
)

## SAIDI Planned
db_neat  <- bind_cols(db_neat,
                      db_raw %>% 
                             filter(Section %in% "10(i): Interruptions",
                                    Subcategory %in% "SAIDI",
                                    Description %in% "Class A (planned interruptions by Transpower)") %>%
                             select(r_pl_tp_saidi= Value)
)

## SAIFI Unplanned
db_neat  <- bind_cols(db_neat,
                      db_raw %>% 
                             filter(Section %in% "10(i): Interruptions",
                                    Subcategory %in% "SAIFI",
                                    Description %in% "Class D (unplanned interruptions by Transpower)") %>%
                             select(r_un_tp_saifi= Value)
)

## SAIFI Planned
db_neat  <- bind_cols(db_neat,
                      db_raw %>% 
                             filter(Section %in% "10(i): Interruptions",
                                    Subcategory %in% "SAIFI",
                                    Description %in% "Class A (planned interruptions by Transpower)") %>%
                             select(r_pl_tp_saifi= Value)
)

       ### 5.1.8. SAIDI ####

## Unplanned
db_neat  <- bind_cols(db_neat,
                      db_raw %>% 
                             filter(Section %in% "10(i): Interruptions",
                                    Subcategory %in% "SAIDI",
                                    Description %in% "Class C (unplanned interruptions on the network)") %>%
                             select(r_un_saidi= Value)
                     )

## Planned
db_neat  <- bind_cols(db_neat,
                      db_raw %>% 
                             filter(Section %in% "10(i): Interruptions",
                                    Subcategory %in% "SAIDI",
                                    Description %in% "Total") %>%
                             select(r_pl_saidi= Value)
                     )

## Total
db_neat  <- bind_cols(db_neat,
                      db_raw %>% 
                             filter(Section %in% "10(i): Interruptions",
                                    Subcategory %in% "SAIDI",
                                    Description %in% "Class B (planned interruptions on the network)") %>%
                             select(r_saidi= Value)
)

       ### 5.1.8. SAIFI ####

## Unplanned
db_neat  <- bind_cols(db_neat,
                      db_raw %>% 
                             filter(Section %in% "10(i): Interruptions",
                                    Subcategory %in% "SAIFI",
                                    Description %in% "Class C (unplanned interruptions on the network)") %>%
                             select(r_un_saifi= Value)
                     )

## Planned
db_neat  <- bind_cols(db_neat,
                      db_raw %>% 
                             filter(Section %in% "10(i): Interruptions",
                                    Subcategory %in% "SAIFI",
                                    Description %in% "Class B (planned interruptions on the network)") %>%
                             select(r_pl_saifi= Value)
                     )

## Total
db_neat  <- bind_cols(db_neat,
                      db_raw %>% 
                             filter(Section %in% "10(i): Interruptions",
                                    Subcategory %in% "SAIFI",
                                    Description %in% "Total") %>%
                             select(r_saifi= Value)
)









       
       ### 5.1.8.1. SAIDI by Causes ####

# There are too many causes, for this reason, I will use for statement to put it in automatic
# Causes
type_desc <-  db_raw %>%    filter(Section %in% "10(ii): Class C Interruptions and Duration by Cause",
                                   Subcategory %in% "SAIDI") %>%
                                   select(Description) %>%
                                          count(Description)
# Creating Acronyms for each Cause
type_desc <- bind_cols(type_desc,Acronyms = c("ae","aw","idk","eqp","hum","lig","tpi","veg","wlf"))

# Loop
for (i in 1:nrow(type_desc))
       {
       # Save the column names
       temp <- colnames(db_neat)
       
       # Subsetting the raw data and binding with dt_neat
       db_neat  <- bind_cols(db_neat,
                             db_raw %>% 
                                    filter(Section %in% "10(ii): Class C Interruptions and Duration by Cause",
                                           Subcategory %in% "SAIDI",
                                           Description %in% type_desc[i,1]) %>%
                                    select(Value)
                            )
       
       # Naming the columns
       colnames(db_neat) <- c(temp,paste("r_",type_desc[i,3],"_saidi",sep = ""))
       }

# Excluding
rm(type_desc);rm(temp)

       ### 5.1.8.2. SAIFI by Causes #####

# Analog with the "SAIDI by Causes"
# Causes
type_desc <-  db_raw %>%    filter(Section %in% "10(ii): Class C Interruptions and Duration by Cause",
                                   Subcategory %in% "SAIFI") %>%
                                          select(Description) %>%
                                          count(Description)
# Creating Acronyms for each Cause
type_desc <- bind_cols(type_desc,Acronyms = c("ae","aw","idk","eqp","hum","lig","tpi","veg","wlf"))

# Loop
for (i in 1:nrow(type_desc))
{
       # Save the column names
       temp <- colnames(db_neat)
       
       # Subsetting the raw data and binding with dt_neat
       db_neat  <- bind_cols(db_neat,
                             db_raw %>% 
                                    filter(Section %in% "10(ii): Class C Interruptions and Duration by Cause",
                                           Subcategory %in% "SAIFI",
                                           Description %in% type_desc[i,1]) %>%
                                    select(Value)
       )
       
       # Naming the columns
       colnames(db_neat) <- c(temp,paste("r_",type_desc[i,3],"_saifi",sep = ""))
}

# Excluding temporaly variable
rm(type_desc);rm(temp)


## 5.2. Assets ####

       ### 5.10.1. Poles ####

## Wood Pole
db_neat  <- bind_cols(db_neat,
                      db_raw %>% 
                             filter(Section %in% "9a: Asset Register",
                                    Category %in% "All - Overhead  Line",
                                    Subcategory %in% "Wood poles",
                                    Description %in% "Items at end of year (quantity)") %>%
                                          select(a_wp= Value)
)

## Concrete Pole or Steel Structure
db_neat  <- bind_cols(db_neat,
                      db_raw %>% 
                             filter(Section %in% "9a: Asset Register",
                                    Category %in% "All - Overhead  Line",
                                    Subcategory %in% "Concrete poles / steel structure",
                                    Description %in% "Items at end of year (quantity)") %>%
                                          select(a_cp= Value)
)

## Other Pole
db_neat  <- bind_cols(db_neat,
                      db_raw %>% 
                             filter(Section %in% "9a: Asset Register",
                                    Category %in% "All - Overhead  Line",
                                    Subcategory %in% "Other pole types",
                                    Description %in% "Items at end of year (quantity)") %>%
                                          select(a_ot= Value)
)

       ### 5.10.1.1 Aging Pole ####

## Wood Poles

# The same problem of the SAIDI/SAIFI causes.
# Due to of variables number I will save this in a separated dataset.

type_desc <- db_raw %>% 
              filter(Section %in% "9b: Asset Age Profile",
                     Subcategory %in% "Wood poles",
                     !Description %in% c("Data accuracy (1-4)","Items at end of year (quantity)","Total assets at year end")) %>%
                            count(Description)

type_desc$Description <- gsub(pattern = "Number of assets at disclosure year end by installation date - ",replacement = "",x = temp$Description,ignore.case = FALSE)

rm(type_desc)

db_wp <- db_raw %>% 
       filter(Section %in% "9b: Asset Age Profile",
              Subcategory %in% "Wood poles",
              !Description %in% c("Data accuracy (1-4)","Items at end of year (quantity)","Total assets at year end"))

## Graphs                 

gp_a_db_wp <- db_wp %>% select(EDB,Network,Description,Year,Value) %>% 
                                   filter(EDB %in% "Aurora Energy", Year %in% "2017", !Network %in% "All")

ggplot(gp_a_db_wp, aes(x=Description , y= Value, fill= Network)) +
       geom_bar(stat= "identity", width = .6) +
       coord_flip() +
       labs(title="Wood Poles") +
       theme_tufte() + # Tufte theme from ggfortify
       theme(plot.title = element_text(hjust = .5),
       axis.ticks = element_blank()) + # Centre plot title
       scale_fill_brewer(palette = "Dark2") # Color palette

gp_a_db_wp <- gp_a_db_wp %>%
              filter(Description %in% c(1990:2020))

ggplot(gp_a_db_wp, aes(x=Description , y= Value, fill= Network)) +
       geom_bar(stat= "identity", width = .6) +
       coord_flip() +
       labs(title="Wood Poles") +
       theme_tufte() + # Tufte theme from ggfortify
       theme(plot.title = element_text(hjust = .5),
             axis.ticks = element_blank()) + # Centre plot title
       scale_fill_brewer(palette = "Dark2") # Color palette

## Concrete Poles

# The same problem of the SAIDI/SAIFI causes.
# Due to of variables number I will save this in a separated dataset.

type_desc <- db_raw %>% 
       filter(Section %in% "9b: Asset Age Profile",
              Subcategory %in% "Concrete poles / steel structure",
              !Description %in% c("Data accuracy (1-4)","Items at end of year (quantity)","Total assets at year end")) %>%
       count(Description)

type_desc$Description <- gsub(pattern = "Number of assets at disclosure year end by installation date - ",replacement = "",x = temp$Description,ignore.case = FALSE)



db_cp <- db_raw %>% 
       filter(Section %in% "9b: Asset Age Profile",
              Subcategory %in% "Concrete poles / steel structure",
              !Description %in% c("Data accuracy (1-4)","Items at end of year (quantity)","Total assets at year end"))

rm(type_desc)

## Graphs                 

gp_a_db_cp <- db_cp %>% select(EDB,Network,Description,Year,Value) %>% 
       filter(EDB %in% "Aurora Energy", Year %in% "2017", !Network %in% "All")

ggplot(gp_a_db_cp, aes(x=Description , y= Value, fill= Network)) +
       geom_bar(stat= "identity", width = .6) +
       coord_flip() +
       labs(title="Wood Poles") +
       theme_tufte() + # Tufte theme from ggfortify
       theme(plot.title = element_text(hjust = .5),
             axis.ticks = element_blank()) + # Centre plot title
       scale_fill_brewer(palette = "Dark2") # Color palette

gp_a_db_cp <- gp_a_db_cp %>%
       filter(Description %in% c(1990:2020))

ggplot(gp_a_db_cp, aes(x=Description , y= Value, fill= Network)) +
       geom_bar(stat= "identity", width = .6) +
       coord_flip() +
       labs(title="Wood Poles") +
       theme_tufte() + # Tufte theme from ggfortify
       theme(plot.title = element_text(hjust = .5),
             axis.ticks = element_blank()) + # Centre plot title
       scale_fill_brewer(palette = "Dark2") # Color palette


       ### 5.10.2. Capacitor Banks ####

db_neat  <- bind_cols(db_neat,
                      db_raw %>% 
                             filter(Section %in% "9a: Asset Register",
                                    Category %in% "All - Capacitor Banks",
                                    Description %in% "Items at end of year (quantity)") %>%
                             select(a_bc= Value)
                     )

       ### 5.10.2.1. Capacitor Banks in Aurora Energy <================ (REMOVE - Put on the Report body) ####

#au_tb_bc <-   spread(db_neat %>% 
#                            filter(EDB %in% "Aurora Energy") %>%
#                            select(Network,Year,a_bc),
#                            Network,a_bc)

       ### 5.10.3. Voltage Regulator ####

db_neat  <- bind_cols(db_neat,
                      db_raw %>% 
                             filter(Section %in% "9a: Asset Register",
                                    Category %in% "HV - Distribution Transformer",
                                    Subcategory %in% "Voltage regulators",
                                    Description %in% "Items at end of year (quantity)") %>%
                                          select(a_vr= Value)
                     )

       ### 5.10.3.1. Voltage Regulator in Aurora Energy <================ (REMOVE - Put on the Report body) ####

#au_tb_vr <-   spread(db_neat %>% 
#                            filter(EDB %in% "Aurora Energy") %>%
#                            select(Network,Year,a_vr),
#                            Network,a_vr)
#

       ### 5.3.1. Network Length ####

db_neat  <- bind_cols(db_neat,
                      db_raw %>% 
                             filter(Subcategory %in% "Total circuit length (km)",
                                    Description %in% "Total circuit length (for supply)") %>%
                                          select(a_li= Value)
                     )

       ### 5.3.1.1. Network by Regions in Aurora Energy <================ (REMOVE - Put on the Report body)####

# Creating a table to insert on the report
au_tb_li <-   spread(db_neat %>%                                 # x = db_neat
                           filter(EDB %in% "Aurora Energy") %>%  # key = Network
                            select(Network,Year,a_li),           # value = a_li
                     Network,a_li)

# re-ordering the sequence of columns
au_tb_li <- au_tb_li[,c(1,3,4,2)]

# Table with absolute numbers and percentage
au_tb_li_p <- bind_cols(au_tb_li[,1:2],
                            au_tb_li[,2]/au_tb_li[,4],
                            au_tb_li[,3],
                            au_tb_li[,3]/au_tb_li[,4],
                            au_tb_li[,4])

# Creating a dataset to use in graphics
gp_au_li <- db_neat %>%
                     filter(EDB %in% "Aurora Energy") %>%
                            select(Network,Year,Value = a_li)

## Graph all Aurora Length
ggplot(gp_au_li, aes(x= Year, y = Value , fill= "tomato3") )+
       geom_bar(width = .5, stat = "identity")

## Graph network comparing
ggplot(gp_au_li,aes(x=Year, y=Value,fill=Network)) + 
       geom_bar(position = position_dodge() , stat = "identity") +
              scale_fill_brewer(palette = "Paired") +
              geom_smooth(mapping = aes(linetype ="r2"),
              method = "lm",
              formula = y ~ x + log(x), se = FALSE,
              color = "black")



       ### 5.3.1.1. Street lighting ####

## Total
db_neat  <- bind_cols(db_neat,
                      db_raw %>% 
                             filter(Section %in% "9a: Asset Register",
                                    Category %in% "LV - LV Street lighting",
                                    Description %in% "Items at start of year (quantity)") %>%
                                          select(a_li_st= Value)
)

## Overhead
db_neat  <- bind_cols(db_neat,
                      db_raw %>% 
                             filter(Section %in% "9c: Overhead lines and underground cables",
                                    Subcategory %in% "Overhead (km)",
                                    Description %in% "Dedicated street lighting circuit length (km)") %>%
                             select(a_li_st_oh= Value)
)

## Undergound
db_neat  <- bind_cols(db_neat,
                      db_raw %>% 
                             filter(Section %in% "9c: Overhead lines and underground cables",
                                    Subcategory %in% "Underground (km)",
                                    Description %in% "Dedicated street lighting circuit length (km)") %>%
                             select(a_li_st_ug= Value)
)

       ### 5.2.1.1.a. Street lighting in Aurora Energy  <================ (REMOVE - Put on the Report body) ####

au_tb_li_st <-   spread(db_neat %>%                                 # x = db_neat
                            filter(EDB %in% "Aurora Energy") %>%    # key = Network
                            select(Network,Year,a_li_st),           # value = a_li
                     Network,a_li_st)

       ### 5.3.1.2. LV Line ####

db_neat  <- bind_cols(db_neat,
                      db_raw %>% 
                             filter(Section %in% "9a: Asset Register",
                                    Category %in% "LV - LV Line",
                                    Description %in% "Items at start of year (quantity)") %>%
                             select(a_lv_li= Value)
                     )

       ### 5.2.1.2.a. LV Line in Aurora Energy <================ (REMOVE - Put on the Report body) ####

au_tb_lv_li <-   spread(db_neat %>%                                    # x = db_neat
                               filter(EDB %in% "Aurora Energy") %>%    # key = Network
                               select(Network,Year,a_lv_li),           # value = a_lv_li
                        Network,a_lv_li)

       ### 5.3.1.3. LV Cable ####

db_neat  <- bind_cols(db_neat,
                      db_raw %>% 
                             filter(Section %in% "9a: Asset Register",
                                    Category %in% "LV - LV Cable",
                                    Description %in% "Items at start of year (quantity)") %>%
                             select(a_lv_cb= Value)
                     )

       ### 5.2.1.3.a. LV Cable in Aurora Energy <================ (REMOVE - Put on the Report body) ####

au_tb_lv_cb <-   spread(db_neat %>%                                    # x = db_neat
                               filter(EDB %in% "Aurora Energy") %>%    # key = Network
                               select(Network,Year,a_lv_cb),           # value = a_lv_cb
                        Network,a_lv_cb)

       ### 5.2.1.3.a. Comparasion LV-LV Lines, Cables. and Street Lighting in Aurora <===== (REMOVE - Put on the Report body) ####

au_tb_li_cb_st <- db_neat %>% select(EDB,
                                        Network,
                                        Year,
                                        "LV Lines" = a_lv_li,
                                        "LV Cables" = a_lv_cb,
                                        "Street Light" = a_li_st) %>%
                                          filter(EDB %in% "Aurora Energy")

       ### 5.3.1.4. HV Distribution Lines ####

## HV Lines
db_neat  <- bind_cols(db_neat,
                      db_raw %>% 
                             filter(Section %in% "9a: Asset Register",
                                    Category %in% "HV - Distribution Line",
                                    Subcategory %in% "Distribution OH Open Wire Conductor",
                                    Description %in% "Items at start of year (quantity)") %>%
                             select(a_hv_li_oh= Value)
                     )

## SWER - Single Wired with Earth return
db_neat  <- bind_cols(db_neat,
                      db_raw %>% 
                             filter(Section %in% "9a: Asset Register",
                                    Category %in% "HV - Distribution Line",
                                    Subcategory %in% "SWER conductor",
                                    Description %in% "Items at start of year (quantity)") %>%
                             select(a_hv_li_swer= Value)
)

## Aerial Cable Condutor
db_neat  <- bind_cols(db_neat,
                      db_raw %>% 
                             filter(Section %in% "9a: Asset Register",
                                    Category %in% "HV - Distribution Line",
                                    Subcategory %in% "Distribution OH Aerial Cable Conductor",
                                    Description %in% "Items at start of year (quantity)") %>%
                             select(a_hv_li_acc= Value)
)

       ### 5.3.1.5. HV Subtransmission Lines ####

db_neat  <- bind_cols(db_neat,
                      db_raw %>% 
                             filter(Section %in% "9a: Asset Register",
                                    Category %in% "HV - Subtransmission Line",
                                    Subcategory %in% "Subtransmission OH 110kV+ conductor",
                                    Description %in% "Items at start of year (quantity)") %>%
                             select(a_hv_sb_li= Value)
                     )

       ### 5.3.1.5. HV Subtransmission Lines in Aurora <================ (REMOVE - Put on the Report body) ####

au_tb_hv_sb_li <-   spread(db_neat %>%                                 # x = db_neat
                               filter(EDB %in% "Aurora Energy") %>%    # key = Network
                               select(Network,Year,a_hv_sb_li),        # value = a_hv_sb_li
                        Network,a_hv_sb_li)

       ### 5.3.1.6. HV Distribution Cables ####

## XLPE Underground
db_neat  <- bind_cols(db_neat,
                      db_raw %>% 
                             filter(Section %in% "9a: Asset Register",
                                    Category %in% "HV - Distribution Cable",
                                    Subcategory %in% "Distribution UG XLPE or PVC",
                                    Description %in% "Items at start of year (quantity)") %>%
                             select(a_hv_cb_xlpe= Value)
                     )

## PILC Underground
db_neat  <- bind_cols(db_neat,
                      db_raw %>% 
                             filter(Section %in% "9a: Asset Register",
                                    Category %in% "HV - Distribution Cable",
                                    Subcategory %in% "Distribution UG PILC",
                                    Description %in% "Items at start of year (quantity)") %>%
                             select(a_hv_cb_pilc= Value)
)

## Submarine
db_neat  <- bind_cols(db_neat,
                      db_raw %>% 
                             filter(Section %in% "9a: Asset Register",
                                    Category %in% "HV - Distribution Cable",
                                    Subcategory %in% "Distribution Submarine Cable",
                                    Description %in% "Items at start of year (quantity)") %>%
                             select(a_hv_cb_sub= Value)
)

       ### 5.3.1.7. HV Subtransmission Cables ####

# Too many cases in Subcategory. "For" instance to save codes lines.
# Causes
type_desc <-  db_raw %>%    filter(Section %in% "9a: Asset Register",
                                   Category %in% "HV - Subtransmission Cable",
                                   Description %in% "Items at end of year (quantity)") %>%
                                          count(Subcategory)

# Creating Acronyms for each Cause
type_desc <- bind_cols(type_desc,Acronyms = c("sub","ug_110_gas","ug_110_oil","110_pilc","110_xlpe",
                                              "66_gas","66_oil","66_pilc","66_xlpe"))

# Loop
for (i in 1:nrow(type_desc))
{
       # Save the column names
       temp <- colnames(db_neat)
       
       # Subsetting the raw data and binding with dt_neat
       db_neat  <- bind_cols(db_neat,
                             db_raw %>% 
                                    filter(Section %in% "9a: Asset Register",
                                           Category %in% "HV - Subtransmission Cable",
                                           Description %in% "Items at end of year (quantity)",
                                           Subcategory %in% type_desc[i,1]) %>%
                                    select(Value)
       )
       
       # Naming the columns
       colnames(db_neat) <- c(temp,paste("a_",type_desc[i,3],"_hv_sb_",sep = ""))
}

# Excluding temporaly variable
rm(type_desc);rm(temp)

       ### 5.3.1.1. Length of Circuit with Tree Management ####

## For some reason I can not explain, two part of the same informations were split in differents filter()
temp <-bind_rows(db_raw %>%
                     filter(Section %in% "9c: Overhead lines and underground cables",             #
                            Category %in% "Overhead circuit requiring vegetation management",     # 175 rows
                            Subcategory %in% "Circuit length (km)",                               #
                            Description %in% "Overhead circuit requiring vegetation management"),
              
              db_raw %>% filter(Section %in% "9c: Overhead lines and underground cables",         #
                     Category %in% "Total overhead length",                                       # 20 rows
                            Subcategory %in% "Circuit length (km)",                               #
                             Description %in% "Overhead circuit requiring vegetation management")
                     )

## Ordering to ensure the ideal sequence of row
temp <- temp %>% arrange(-Year,EDB,Network)

## Copying the value to the db_neat
db_neat  <- bind_cols(db_neat,
                      temp %>% 
                             select(a_li_tree= Value)
                     )

#Excluding the temporaly variable
rm(temp)

       ### 5.3.1.1.a. Length of Circuit with Tree Management in Aurora  <========= (REMOVE - Put on the Report body)####

au_tb_li_tree <- spread(db_neat %>%                                 # x = db_neat
                            filter(EDB %in% "Aurora Energy") %>%    # key = Network
                            select(Network,Year,a_li_tree),         # value = a_li_tree
                                   Network,a_li_tree)

       ### 5.3.1.1. Length of Circuit within 10km of coastline or geothermal areas ####

## The same problem faced in Tree Management
temp <-bind_rows(db_raw %>%
                        filter(Section %in% "9c: Overhead lines and underground cables",                                         #
                               Category %in% "Length of circuit within 10km of coastline or geothermal areas (where known)",     # 175 rows
                               Subcategory %in% "Circuit length (km)"),
                 
                 db_raw %>% filter(Section %in% "9c: Overhead lines and underground cables",         #
                                   Category %in% "Total overhead length",                            # 20 rows
                                   Subcategory %in% "Circuit length (km)",                           #
                                   Description %in% "Length of circuit within 10km of coastline or geothermal areas (where known)")
                )

## Ordering to ensure the ideal sequence of row
temp <- temp %>% arrange(-Year,EDB,Network)

## Copying the value to the db_neat
db_neat  <- bind_cols(db_neat,
                      temp %>% 
                             select(a_li_coge= Value)
)

#Excluding the temporaly variable
rm(temp)

       ### 5.3.1.1.a. Length of Circuit within 10km of coastline or geothermal areas in Aurora Energy <================ (REMOVE - Put on the Report body) ####

au_tb_li_coge <- spread(db_neat %>%                                    # x = db_neat
                               filter(EDB %in% "Aurora Energy") %>%    # key = Network
                               select(Network,Year,a_li_coge),         # value = a_li_coge
                        Network,a_li_coge)

       ### 5.2.2. Consumers ####

db_neat  <- bind_cols(db_neat,
                      db_raw %>% 
                             filter(Section %in% "9a: Asset Register",
                                    Category %in% "LV - Connections",
                                    Description %in% "Items at end of year (quantity)") %>%
                             select(a_lv_con= Value)
                     )

       ### 5.X.X. Overhead lines ####

db_neat  <- bind_cols(db_neat,
                      db_raw %>% 
                             filter(Section %in% "9c: Overhead lines and underground cables",
                                    Subcategory %in% "Overhead (km)",
                                    Description %in% "Total circuit length (for supply)") %>%
                             select(a_li_oh= Value)
)

       ### 5.X.X. Underground lines ####

db_neat  <- bind_cols(db_neat,
                      db_raw %>% 
                             filter(Section %in% "9c: Overhead lines and underground cables",
                                    Subcategory %in% "Underground (km)",
                                    Description %in% "Total circuit length (for supply)") %>%
                             select(a_li_ug= Value)
)




















# 6. Aurora Energy - Descriptive ####




   ## 6.1. Proportion of restoration in less or above 3 hours in Aurora Energy ####
# Less
temp <- rest_cons_less_3h %>%
       select(EDB,Network,Year,Value) %>%
       filter(EDB %in% "Aurora Energy" , Network %in% c("Central Otago","Dunedin"))
temp <- arrange(.data = temp, Year, Network)
# Over
temp1 <- rest_cons_over_3h %>%
       select(EDB,Network,Year,Value) %>%
       filter(EDB %in% "Aurora Energy" , Network %in% c("Central Otago","Dunedin"))
temp1 <- arrange(.data = temp1, Year, Network)

aurora_out_net_less_over_3h <- cbind(temp[,-1],temp1[,4])
colnames(aurora_out_net_less_over_3h) <- c("Network","Year","less 3h","over 3h")

      ### 6.1.1 Graphs of Interruptions in Aurora ####
p1 <- ggplot(data = aurora_out_net_less_over_3h,
             aes(x = aurora_out_net_less_over_3h$`less 3h`,
                 y= aurora_out_net_less_over_3h$`over 3h`)) +
       geom_point()

p1 + geom_text_repel(aes(label=aurora_out_net_less_over_3h$Year), size=4) +
       geom_point(aes(shape=aurora_out_net_less_over_3h$Network), size = 4) +
       scale_x_continuous(name = "Less 3 hours") +
       scale_y_continuous(name = "more than 3 hours") +
       theme(legend.position = "bottom")



   ## 6.3. Interruptions in Aurora Energy ####

aurora_unplan <- dt_int_unplan %>%
                     select(EDB, Network, Year, Value) %>%
                            filter(EDB %in% "Aurora Energy")
aurora_unplan <- arrange(.data = aurora_unplan,Year,Network)
colnames(aurora_unplan)[4] <- "Unplanned_Outage"


   ## 6.4. Outages Planned and Unplanned  ####
aurora_unplan_length <- cbind(aurora_unplan,aurora_length[,4])

aurora_unplan_length <- cbind(aurora_unplan_length,ratio = aurora_unplan_length[4]/aurora_unplan_length[5])
colnames(aurora_unplan_length)[6] <- "ratio"

aurora_plan <- dt_int_plan %>%
                     select(EDB,Network,Year,Description,Value) %>%
                            filter(EDB %in% "Aurora Energy" , Description %in% "Class B (planned interruptions on the network)")
aurora_plan <- aurora_plan %>%
                     select(EDB,Network,Year,Value)
aurora_plan <- arrange(.data = aurora_plan, Year, Network)


aurora_plan_unplan <- cbind(aurora_unplan,aurora_plan[,4],aurora_unplan[,4]/(aurora_unplan[,4]+aurora_plan[,4]),aurora_plan[,4]/(aurora_plan[,4]+aurora_unplan[,4]))
colnames(aurora_plan_unplan) <- c("EDB","Network","Year","Unplanned_Outage","Planned","%_Unplanned","%_Planeed")

aurora_plan_unplan_all <- aurora_plan_unplan %>%
                                   filter(Network %in% "All")

aurora_plan_unplan_dunedin <- aurora_plan_unplan %>%
                                   filter(Network %in% "Dunedin")

aurora_plan_unplan_central_otago <- aurora_plan_unplan %>%
                                          filter(Network %in% "Central Otago")
 
   ## 6.5. Aurora SAIDI in details ####
aurora_SAIDI_causes <- SAIDI_causes %>%
                            select(EDB,Network,Year,Description,Value) %>%
                                   filter(EDB %in% "Aurora Energy") %>%
                                          arrange(desc(Year),EDB,Network,Description)

      ### 6.5.1. Graphs ####
rm(temp);rm(temp1);rm(temp2);rm(temp3)
temp <- aurora_SAIDI_causes %>%
       select(EDB,Network,Year,Description,Value) %>%
              filter(Year %in% 2017, Network %in% "All")

temp1 <- cbind(temp[,1:4],100*temp[,5]/sum(temp[,5]))

# Barplot
temp2 <- ggplot(temp1, aes(x="SAIDI", y = Value , fill = Description) )+
              geom_bar(width = 1, stat = "identity")
temp2

# Pie
temp3 <- temp2 + coord_polar("y" , start = 0)
temp3

rm(temp);rm(temp1);rm(temp2);rm(temp3)

   ## 6.6. Aurora SAIFI in details ####
aurora_SAIFI_causes <- SAIFI_causes %>%
                            select(EDB,Network,Year,Description,Value) %>%
                                   filter(EDB %in% "Aurora Energy") %>%
                                          arrange(desc(Year),EDB,Network,Description)

      ### 6.6.1. Graphs ####
rm(temp);rm(temp1);rm(temp2);rm(temp3)
temp <- aurora_SAIFI_causes %>%
              select(EDB,Network,Year,Description,Value) %>%
                     filter(Year %in% 2017, Network %in% "All")

temp1 <- cbind(temp[,1:4],100*temp[,5]/sum(temp[,5]))

# Barplot
temp2 <- ggplot(temp1, aes(x="SAIFI", y = Value , fill = Description) )+
       geom_bar(width = 1, stat = "identity")
temp2

# Pie
temp3 <- temp2 + coord_polar("y" , start = 0)
temp3

rm(temp);rm(temp1);rm(temp2);rm(temp3)

   ## 6.7. Aurora Poles ####
rm(temp);rm(temp1);rm(temp2);rm(temp3)

aurora_pole <- poles %>%
                     select(EDB,Network,Year,Subcategory,Description,Value) %>%
                            filter(EDB %in% "Aurora Energy")

# Net change of wood poles
temp1 <- filter(.data = aurora_pole, Network == "All" , Subcategory == "Wood poles" , Description == "Net change")

# Net change of concrete poles
temp2 <- filter(.data = aurora_pole, Network == "All" , Subcategory == "Concrete poles / steel structure" , Description == "Net change")

rm(temp);rm(temp1);rm(temp2);rm(temp3)


# 7. New Zealand Lines Companies Study ####

