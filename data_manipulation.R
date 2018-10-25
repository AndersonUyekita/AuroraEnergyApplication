data_manipulation<- function(x = db_raw,y = db_neat)
{
       # 4. Index #########################################################
       
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
       #      r_i_un:       Interruptions Unplanned (Quantity)
       #      r__ipl:       Interruptions Planned (Quantity)
       #      r_i_tot:      Total interruptions
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
       #      r_lig_saidi:  Lighting cause of SAIDI
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
       #      a_li_coge:    Line Length under Coastline and Geothermal effect
       #      au_tb_li_coge: Table with Line Length under influency of Coastline and Geothermal
       #      a_lv_con:     LV Consumer
       #      s_tot_enr:    Total Energy Supplied
       #      s_gxp_enr:    Energy delivered in GXPs (Grid Exit Points)
       #      s_dr_enr:     Energy delivered by Distributed Resources
       #      s_exp_enr:    Exported Energy to GXPs
       #      s_ent_enr:    Total Energy Entering the Aurora Energy Systems.
       #      r_pf:         Regulatory Profit
       #      r_li_ch:      Lines Charge
       
       
       
       
       # 5. Data Manipulation #########################################################
       
       
       ## 5.1. Assets ####
       
       ### 5.1.1. Network Assets ####
       
       ### 5.1.1.1. Consumers ####
       
       db_neat  <- bind_cols(db_neat,
                             db_raw %>% 
                                    filter(Section %in% "9a: Asset Register",
                                           Category %in% "LV - Connections",
                                           Description %in% "Items at end of year (quantity)") %>%
                                    select(a_lv_con= Value)
       )
       
       ### 5.1.1.2. Network Length ####
       
       db_neat  <- bind_cols(db_neat,
                             db_raw %>% 
                                    filter(Subcategory %in% "Total circuit length (km)",
                                           Description %in% "Total circuit length (for supply)") %>%
                                    select(a_li= Value)
       )
       
       ### 5.1.1.3. Overhead lines ####
       
       db_neat  <- bind_cols(db_neat,
                             db_raw %>% 
                                    filter(Section %in% "9c: Overhead lines and underground cables",
                                           Subcategory %in% "Overhead (km)",
                                           Description %in% "Total circuit length (for supply)") %>%
                                    select(a_li_oh= Value)
       )
       
       ### 5.1.1.4. Underground lines ####  
       
       db_neat  <- bind_cols(db_neat,
                             db_raw %>% 
                                    filter(Section %in% "9c: Overhead lines and underground cables",
                                           Subcategory %in% "Underground (km)",
                                           Description %in% "Total circuit length (for supply)") %>%
                                    select(a_li_ug= Value)
       )
       
       ### 5.1.1.5. Rural Overhead lines ####
       
       db_neat  <- bind_cols(db_neat,
                             db_raw %>% 
                                    filter(Section %in% "9c: Overhead lines and underground cables",
                                           Subcategory %in% "Circuit length (km)",
                                           Description %in% "Rural") %>%
                                    select(a_li_ru= Value)
       )
       
       ### 5.1.1.6. Urban Overhead lines ####
       
       db_neat  <- bind_cols(db_neat,
                             db_raw %>% 
                                    filter(Section %in% "9c: Overhead lines and underground cables",
                                           Subcategory %in% "Circuit length (km)",
                                           Description %in% "Urban") %>%
                                    select(a_li_ur= Value)
       )
       
       ### 5.1.1.7. Rugged Overhead lines ####
       
       db_neat <- mutate(db_neat,a_li_rrr = a_li_oh - a_li_ur - a_li_ru)
       
       ### 5.1.1.8. LV Street lighting ####
       
       db_neat  <- bind_cols(db_neat,
                             db_raw %>% 
                                    filter(Section %in% "9a: Asset Register",
                                           Category %in% "LV - LV Street lighting",
                                           Description %in% "Items at end of year (quantity)") %>%
                                    select(a_li_st= Value)
       )
       
       ### 5.1.1.9. Tree Management ####
       
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
       
       ### 5.1.1.10. Coastline or Geothermal Areas ####
       
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
       
       
       
       
       ### 5.1.1.11. LV Line ####
       
       db_neat  <- bind_cols(db_neat,
                             db_raw %>% 
                                    filter(Section %in% "9a: Asset Register",
                                           Category %in% "LV - LV Line",
                                           Description %in% "Items at end of year (quantity)") %>%
                                    select(a_lv_li= Value)
       )
       
       ### 5.1.1.12. LV Cable ####
       
       db_neat  <- bind_cols(db_neat,
                             db_raw %>% 
                                    filter(Section %in% "9a: Asset Register",
                                           Category %in% "LV - LV Cable",
                                           Description %in% "Items at end of year (quantity)") %>%
                                    select(a_lv_cb= Value)
       )
       
       ### 5.1.1.12.a. Total LV
       
       db_neat <- mutate(db_neat,a_lv_tot = db_neat$a_lv_li + db_neat$a_lv_cb)
       
       ### 5.1.1.13. HV Subtransmission Lines ####
       
       db_neat  <- bind_cols(db_neat,
                             db_raw %>% 
                                    filter(Section %in% "9a: Asset Register",
                                           Category %in% "HV - Subtransmission Line",
                                           Subcategory %in% "Subtransmission OH 110kV+ conductor",
                                           Description %in% "Items at end of year (quantity)") %>%
                                    select(a_hv_sb_li= Value)
       )
       
       
       ### 5.1.1.14. HV Subtransmission Cables ####
       
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
       
       ### 5.1.1.X. Subtransmission Total Cable Length
       
       temp <- db_raw %>%  filter(Section %in% "9a: Asset Register",
                                  Category %in% "HV - Subtransmission Cable",
                                  Description %in% "Items at end of year (quantity)") %>% 
              select(EDB,Network,Year,Subcategory,Value) %>% 
              group_by(EDB,Network,Year) %>% 
              mutate(a_hv_sb_cb_tot = sum(Value)) %>%
              select(EDB,Network,Year,a_hv_sb_cb_tot) %>%
              distinct() %>% ungroup()
       
       db_neat <- left_join(x = temp,y = db_neat,by = NULL)
       
       rm(temp)
       
       ### 5.1.1.X. Subtransmission Total Line Length
       
       temp <- db_raw %>%  filter(Section %in% "9a: Asset Register",
                                  Category %in% "HV - Subtransmission Line",
                                  Description %in% "Items at end of year (quantity)") %>% 
              select(EDB,Network,Year,Subcategory,Value) %>% 
              group_by(EDB,Network,Year) %>% 
              mutate(a_hv_sb_li_tot = sum(Value)) %>%
              select(EDB,Network,Year,a_hv_sb_li_tot) %>%
              distinct() %>% ungroup()
       
       db_neat <- left_join(x = temp,y = db_neat,by = NULL)
       
       rm(temp)
       
       ### 5.1.1.X. Subtransmission Total Length
       
       db_neat <- mutate(db_neat, a_hv_sb_tot = a_hv_sb_li_tot + a_hv_sb_cb_tot)
       
       
       ### 5.1.1.15. HV Distribution Lines - Overhead ####
       
       db_neat  <- bind_cols(db_neat,
                             db_raw %>% 
                                    filter(Section %in% "9a: Asset Register",
                                           Category %in% "HV - Distribution Line",
                                           Subcategory %in% "Distribution OH Open Wire Conductor",
                                           Description %in% "Items at end of year (quantity)") %>%
                                    select(a_hv_li_oh= Value)
       )
       
       ### 5.1.1.16. HV Distribution Lines - SWER ####
       ## SWER - Single Wired with Earth return
       db_neat  <- bind_cols(db_neat,
                             db_raw %>% 
                                    filter(Section %in% "9a: Asset Register",
                                           Category %in% "HV - Distribution Line",
                                           Subcategory %in% "SWER conductor",
                                           Description %in% "Items at end of year (quantity)") %>%
                                    select(a_hv_li_swer= Value)
       )
       
       ### 5.1.1.17. HV Distribution Aerial Cable - Overhead ####
       
       db_neat  <- bind_cols(db_neat,
                             db_raw %>% 
                                    filter(Section %in% "9a: Asset Register",
                                           Category %in% "HV - Distribution Line",
                                           Subcategory %in% "Distribution OH Aerial Cable Conductor",
                                           Description %in% "Items at end of year (quantity)") %>%
                                    select(a_hv_li_acc= Value)
       )
       
       ### 5.1.1.18. HV Distribution Total - Overhead ####
       
       db_neat <- db_neat %>% mutate(a_hv_li = a_hv_li_oh + a_hv_li_swer + a_hv_li_acc) 
       
       ### 5.1.1.19. HV Distribution Cables ####
       
       ## XLPE Underground
       db_neat  <- bind_cols(db_neat,
                             db_raw %>% 
                                    filter(Section %in% "9a: Asset Register",
                                           Category %in% "HV - Distribution Cable",
                                           Subcategory %in% "Distribution UG XLPE or PVC",
                                           Description %in% "Items at end of year (quantity)") %>%
                                    select(a_hv_cb_xlpe= Value)
       )
       
       ## PILC Underground
       db_neat  <- bind_cols(db_neat,
                             db_raw %>% 
                                    filter(Section %in% "9a: Asset Register",
                                           Category %in% "HV - Distribution Cable",
                                           Subcategory %in% "Distribution UG PILC",
                                           Description %in% "Items at end of year (quantity)") %>%
                                    select(a_hv_cb_pilc= Value)
       )
       
       ## Submarine
       db_neat  <- bind_cols(db_neat,
                             db_raw %>% 
                                    filter(Section %in% "9a: Asset Register",
                                           Category %in% "HV - Distribution Cable",
                                           Subcategory %in% "Distribution Submarine Cable",
                                           Description %in% "Items at end of year (quantity)") %>%
                                    select(a_hv_cb_sub= Value)
       )
       
       ## Total
       db_neat <- mutate(db_neat,a_hv_cb_tot = a_hv_cb_xlpe + a_hv_cb_pilc + a_hv_cb_sub)
       
       
       
       ### 5.1.1.X Poles ####
       
       ### 5.1.1.X.1 Wood Pole ####
       db_neat  <- bind_cols(db_neat,
                             db_raw %>% 
                                    filter(Section %in% "9a: Asset Register",
                                           Category %in% "All - Overhead  Line",
                                           Subcategory %in% "Wood poles",
                                           Description %in% "Items at end of year (quantity)") %>%
                                    select(a_wp= Value)
       )
       
       ### 5.1.1.X.2 Concrete Pole or Steel Structure ####
       db_neat  <- bind_cols(db_neat,
                             db_raw %>% 
                                    filter(Section %in% "9a: Asset Register",
                                           Category %in% "All - Overhead  Line",
                                           Subcategory %in% "Concrete poles / steel structure",
                                           Description %in% "Items at end of year (quantity)") %>%
                                    select(a_cp= Value)
       )
       
       ### 5.1.1.X.3 Other Pole ####
       db_neat  <- bind_cols(db_neat,
                             db_raw %>% 
                                    filter(Section %in% "9a: Asset Register",
                                           Category %in% "All - Overhead  Line",
                                           Subcategory %in% "Other pole types",
                                           Description %in% "Items at end of year (quantity)") %>%
                                    select(a_ot= Value)
       )
       
       ### 5.1.1.X.Y Total Pole ####
       
       db_neat  <- mutate(db_neat, a_pole_tot = a_wp + a_cp + a_ot)
       
       ## Wood Poles Age
       
       db_raw %>% 
              filter(Section %in% "9b: Asset Age Profile",
                     Subcategory %in% "Wood poles",
                     !Description %in% c("Data accuracy (1-4)","Items at end of year (quantity)","Total assets at year end")) -> temp_wp
       
       gsub(x = temp_wp$Description,
            pattern = "Number of assets at disclosure year end by installation date - ",
            replacement = "") -> temp_wp$Description
       
       
       gsub(x = temp_wp$Description,
            pattern = "\r\n",
            replacement = "") -> temp_wp$Description
       
       gsub(x = temp_wp$Description,
            pattern = "pre-1940",
            replacement = "1900-1939") -> temp_wp$Description
       
       gsub(x = temp_wp$Description,
            pattern = "Number with age unknown",
            replacement = "999999") -> temp_wp$Description
       
       gsub(x = temp_wp$Description,
            pattern = "Number with default dates",
            replacement = "777777") -> temp_wp$Description
       
       gsub(x = temp_wp$Description,
            pattern = "\\D",
            replacement = "_") -> temp_wp$Description
       
       gsub(x = temp_wp$Description,
            pattern = "999999",
            replacement = "unknown") -> temp_wp$Description
       
       gsub(x = temp_wp$Description,
            pattern = "777777",
            replacement = "default_dates") -> temp_wp$Description
       
       db_neat  <- temp_wp %>%
              
              filter(Section %in% "9b: Asset Age Profile",
                     Subcategory %in% "Wood poles",
                     Description %in% "unknown") %>%
              select(EDB,
                     Network,
                     Year,
                     a_wp_unknown = Value) %>% 
              
              left_join(db_neat,.,by = NULL)

       db_neat <- temp_wp %>% filter(Section %in% "9b: Asset Age Profile",
                     Subcategory %in% "Wood poles",
                     Description %in% "default_dates") %>% 
       
              select(EDB,
                     Network,
                     Year,
                     a_wp_def_date = Value) %>% 
              
              left_join(db_neat,.,by = NULL)       
       
       db_neat  <- temp_wp %>%
              
                     filter(Section %in% "9b: Asset Age Profile",
                            Subcategory %in% "Wood poles",
                            Description %in% "1900_1939") %>%
                            select(EDB,
                                   Network,
                                   Year,
                                   a_wp_1900_1939 = Value) %>% 
              
              left_join(db_neat,.,by = NULL) 
       
       db_neat  <- temp_wp %>%
              
              filter(Section %in% "9b: Asset Age Profile",
                     Subcategory %in% "Wood poles",
                     Description %in% "1940_1949") %>%
              select(EDB,
                     Network,
                     Year,
                     a_wp_1940_1949 = Value) %>%
              
              left_join(db_neat,.,by = NULL)
       
       db_neat  <- temp_wp %>%
              
              filter(Section %in% "9b: Asset Age Profile",
                     Subcategory %in% "Wood poles",
                     Description %in% "1950_1959") %>%
              select(EDB,
                     Network,
                     Year,
                     Value) %>% 
              
              left_join(db_neat,.,by = NULL) %>% rename(a_wp_1950_1959 = Value)
       
       db_neat  <-  temp_wp %>%
              
              filter(Section %in% "9b: Asset Age Profile",
                     Subcategory %in% "Wood poles",
                     Description %in% "1960_1969") %>%
              select(EDB,
                     Network,
                     Year,
                     Value) %>% 
              
              left_join(db_neat,.,by = NULL) %>% rename(a_wp_1960_1969 = Value)
       
       db_neat  <- temp_wp %>%
              
              filter(Section %in% "9b: Asset Age Profile",
                     Subcategory %in% "Wood poles",
                     Description %in% "1970_1979") %>%
              select(EDB,
                     Network,
                     Year,
                     Value) %>% 
              
              left_join(db_neat,.,by = NULL) %>% rename(a_wp_1970_1979 = Value)
       
       db_neat  <- temp_wp %>%
              
              filter(Section %in% "9b: Asset Age Profile",
                     Subcategory %in% "Wood poles",
                     Description %in% "1980_1989") %>%
              select(EDB,
                     Network,
                     Year,
                     Value) %>% 
              
              left_join(db_neat,.,by = NULL) %>% rename(a_wp_1980_1989 = Value)
       
       db_neat  <- temp_wp %>%
              
              filter(Section %in% "9b: Asset Age Profile",
                     Subcategory %in% "Wood poles",
                     Description %in% "1990_1999") %>%
              select(EDB,
                     Network,
                     Year,
                     Value) %>% 
              
              left_join(db_neat,.,by = NULL) %>% rename(a_wp_1990_1999 = Value)
       
       db_neat  <- temp_wp %>%
              
              filter(Section %in% "9b: Asset Age Profile",
                     Subcategory %in% "Wood poles",
                     Description %in% "2000") %>%
              select(EDB,
                     Network,
                     Year,
                     Value) %>% 
              
              left_join(db_neat,.,by = NULL) %>% rename(a_wp_2000 = Value)
       
       db_neat  <- temp_wp %>%
              
              filter(Section %in% "9b: Asset Age Profile",
                     Subcategory %in% "Wood poles",
                     Description %in% "2001") %>%
              select(EDB,
                     Network,
                     Year,
                     a_wp_2001 =Value) %>% 
              
              left_join(db_neat,.,by = NULL)   
       
       db_neat  <- temp_wp %>%
              
              filter(Section %in% "9b: Asset Age Profile",
                     Subcategory %in% "Wood poles",
                     Description %in% "2002") %>%
              select(EDB,
                     Network,
                     Year,
                     a_wp_2002 =Value) %>% 
              
              left_join(db_neat,.,by = NULL)      
       
       
       db_neat  <- temp_wp %>%
              
              filter(Section %in% "9b: Asset Age Profile",
                     Subcategory %in% "Wood poles",
                     Description %in% "2003") %>%
              select(EDB,
                     Network,
                     Year,
                     a_wp_2003 =Value) %>% 
              
              left_join(db_neat,.,by = NULL)    
       
       db_neat  <- temp_wp %>%
              
              filter(Section %in% "9b: Asset Age Profile",
                     Subcategory %in% "Wood poles",
                     Description %in% "2004") %>%
              select(EDB,
                     Network,
                     Year,
                     a_wp_2004 = Value) %>% 
              
              left_join(db_neat,.,by = NULL)  
       
       db_neat  <- temp_wp %>%
              
              filter(Section %in% "9b: Asset Age Profile",
                     Subcategory %in% "Wood poles",
                     Description %in% "2005") %>%
              select(EDB,
                     Network,
                     Year,
                     a_wp_2005 = Value) %>% 
              
              left_join(db_neat,.,by = NULL)       
       
       db_neat  <- temp_wp %>%
              
              filter(Section %in% "9b: Asset Age Profile",
                     Subcategory %in% "Wood poles",
                     Description %in% "2006") %>%
              select(EDB,
                     Network,
                     Year,
                     a_wp_2006 = Value) %>% 
              
              left_join(db_neat,.,by = NULL)         
       
       db_neat  <- temp_wp %>%
              
              filter(Section %in% "9b: Asset Age Profile",
                     Subcategory %in% "Wood poles",
                     Description %in% "2007") %>%
              select(EDB,
                     Network,
                     Year,
                     a_wp_2007 = Value) %>% 
              
              left_join(db_neat,.,by = NULL)     
       
       db_neat  <- temp_wp %>%
              
              filter(Section %in% "9b: Asset Age Profile",
                     Subcategory %in% "Wood poles",
                     Description %in% "2008") %>%
              select(EDB,
                     Network,
                     Year,
                     a_wp_2008 = Value) %>% 
              
              left_join(db_neat,.,by = NULL) 
       
       db_neat  <- temp_wp %>%
              
              filter(Section %in% "9b: Asset Age Profile",
                     Subcategory %in% "Wood poles",
                     Description %in% "2009") %>%
              select(EDB,
                     Network,
                     Year,
                     a_wp_2009 = Value) %>% 
              
              left_join(db_neat,.,by = NULL) 
       
       db_neat  <- temp_wp %>%
              
              filter(Section %in% "9b: Asset Age Profile",
                     Subcategory %in% "Wood poles",
                     Description %in% "2010") %>%
              select(EDB,
                     Network,
                     Year,
                     a_wp_2010 = Value) %>% 
              
              left_join(db_neat,.,by = NULL) 
       
       db_neat  <- temp_wp %>%
              
              filter(Section %in% "9b: Asset Age Profile",
                     Subcategory %in% "Wood poles",
                     Description %in% "2011") %>%
              select(EDB,
                     Network,
                     Year,
                     a_wp_2011 = Value) %>% 
              
              left_join(db_neat,.,by = NULL) 
       
       db_neat  <- temp_wp %>%
              
              filter(Section %in% "9b: Asset Age Profile",
                     Subcategory %in% "Wood poles",
                     Description %in% "2012") %>%
              select(EDB,
                     Network,
                     Year,
                     a_wp_2012 = Value) %>% 
              
              left_join(db_neat,.,by = NULL) 
       
       
       db_neat  <- temp_wp %>%
              
              filter(Section %in% "9b: Asset Age Profile",
                     Subcategory %in% "Wood poles",
                     Description %in% "2013") %>%
              select(EDB,
                     Network,
                     Year,
                     a_wp_2013 = Value) %>% 
              
              left_join(db_neat,.,by = NULL) 
       
       
       db_neat  <- temp_wp %>%
              
              filter(Section %in% "9b: Asset Age Profile",
                     Subcategory %in% "Wood poles",
                     Description %in% "2014") %>%
              select(EDB,
                     Network,
                     Year,
                     a_wp_2014 = Value) %>% 
              
              left_join(db_neat,.,by = NULL) 
       
       
       db_neat  <- temp_wp %>%
              
              filter(Section %in% "9b: Asset Age Profile",
                     Subcategory %in% "Wood poles",
                     Description %in% "2015") %>%
              select(EDB,
                     Network,
                     Year,
                     a_wp_2015 = Value) %>% 
              
              left_join(db_neat,.,by = NULL) 
       
       
       db_neat  <- temp_wp %>%
              
              filter(Section %in% "9b: Asset Age Profile",
                     Subcategory %in% "Wood poles",
                     Description %in% "2016") %>%
              select(EDB,
                     Network,
                     Year,
                     a_wp_2016 = Value) %>% 
              
              left_join(db_neat,.,by = NULL)    
       
       
       db_neat  <- temp_wp %>%
              
              filter(Section %in% "9b: Asset Age Profile",
                     Subcategory %in% "Wood poles",
                     Description %in% "2017") %>%
              select(EDB,
                     Network,
                     Year,
                     a_wp_2017 = Value) %>% 
              
              left_join(db_neat,.,by = NULL)  
       
       
       
       db_neat  <- temp_wp %>%
              
              filter(Section %in% "9b: Asset Age Profile",
                     Subcategory %in% "Wood poles",
                     Description %in% "2018") %>%
              select(EDB,
                     Network,
                     Year,
                     a_wp_2018 = Value) %>% 
              
              left_join(db_neat,.,by = NULL)  
       
       
       ## Concrete poles / steel structure Age
       
       db_raw %>% 
              filter(Section %in% "9b: Asset Age Profile",
                     Subcategory %in% "Concrete poles / steel structure",
                     !Description %in% c("Data accuracy (1-4)","Items at end of year (quantity)","Total assets at year end")) -> temp_cp
       
       gsub(x = temp_cp$Description,
            pattern = "Number of assets at disclosure year end by installation date - ",
            replacement = "") -> temp_cp$Description
       
       
       gsub(x = temp_cp$Description,
            pattern = "\r\n",
            replacement = "") -> temp_cp$Description
       
       gsub(x = temp_cp$Description,
            pattern = "pre-1940",
            replacement = "1900-1939") -> temp_cp$Description
       
       gsub(x = temp_cp$Description,
            pattern = "Number with age unknown",
            replacement = "999999") -> temp_cp$Description
       
       gsub(x = temp_cp$Description,
            pattern = "Number with default dates",
            replacement = "777777") -> temp_cp$Description
       
       gsub(x = temp_cp$Description,
            pattern = "\\D",
            replacement = "_") -> temp_cp$Description
       
       gsub(x = temp_cp$Description,
            pattern = "999999",
            replacement = "unknown") -> temp_cp$Description
       
       gsub(x = temp_cp$Description,
            pattern = "777777",
            replacement = "default_dates") -> temp_cp$Description
       
       db_neat  <- temp_cp %>%
              
              filter(Section %in% "9b: Asset Age Profile",
                     Subcategory %in% "Concrete poles / steel structure",
                     Description %in% "unknown") %>%
              select(EDB,
                     Network,
                     Year,
                     a_cp_unknown = Value) %>% 
              
              left_join(db_neat,.,by = NULL)
       
       db_neat <- temp_cp %>% filter(Section %in% "9b: Asset Age Profile",
                                     Subcategory %in% "Concrete poles / steel structure",
                                     Description %in% "default_dates") %>% 
              
              select(EDB,
                     Network,
                     Year,
                     a_cp_def_date = Value) %>% 
              
              left_join(db_neat,.,by = NULL)       
       
       db_neat  <- temp_cp %>%
              
              filter(Section %in% "9b: Asset Age Profile",
                     Subcategory %in% "Concrete poles / steel structure",
                     Description %in% "1900_1939") %>%
              select(EDB,
                     Network,
                     Year,
                     a_cp_1900_1939 = Value) %>% 
              
              left_join(db_neat,.,by = NULL) 
       
       db_neat  <- temp_cp %>%
              
              filter(Section %in% "9b: Asset Age Profile",
                     Subcategory %in% "Concrete poles / steel structure",
                     Description %in% "1940_1949") %>%
              select(EDB,
                     Network,
                     Year,
                     a_cp_1940_1949 = Value) %>%
              
              left_join(db_neat,.,by = NULL)
       
       db_neat  <- temp_cp %>%
              
              filter(Section %in% "9b: Asset Age Profile",
                     Subcategory %in% "Concrete poles / steel structure",
                     Description %in% "1950_1959") %>%
              select(EDB,
                     Network,
                     Year,
                     Value) %>% 
              
              left_join(db_neat,.,by = NULL) %>% rename(a_cp_1950_1959 = Value)
       
       db_neat  <-  temp_cp %>%
              
              filter(Section %in% "9b: Asset Age Profile",
                     Subcategory %in% "Concrete poles / steel structure",
                     Description %in% "1960_1969") %>%
              select(EDB,
                     Network,
                     Year,
                     Value) %>% 
              
              left_join(db_neat,.,by = NULL) %>% rename(a_cp_1960_1969 = Value)
       
       db_neat  <- temp_cp %>%
              
              filter(Section %in% "9b: Asset Age Profile",
                     Subcategory %in% "Concrete poles / steel structure",
                     Description %in% "1970_1979") %>%
              select(EDB,
                     Network,
                     Year,
                     Value) %>% 
              
              left_join(db_neat,.,by = NULL) %>% rename(a_cp_1970_1979 = Value)
       
       db_neat  <- temp_cp %>%
              
              filter(Section %in% "9b: Asset Age Profile",
                     Subcategory %in% "Concrete poles / steel structure",
                     Description %in% "1980_1989") %>%
              select(EDB,
                     Network,
                     Year,
                     Value) %>% 
              
              left_join(db_neat,.,by = NULL) %>% rename(a_cp_1980_1989 = Value)
       
       db_neat  <- temp_cp %>%
              
              filter(Section %in% "9b: Asset Age Profile",
                     Subcategory %in% "Concrete poles / steel structure",
                     Description %in% "1990_1999") %>%
              select(EDB,
                     Network,
                     Year,
                     Value) %>% 
              
              left_join(db_neat,.,by = NULL) %>% rename(a_cp_1990_1999 = Value)
       
       db_neat  <- temp_cp %>%
              
              filter(Section %in% "9b: Asset Age Profile",
                     Subcategory %in% "Concrete poles / steel structure",
                     Description %in% "2000") %>%
              select(EDB,
                     Network,
                     Year,
                     Value) %>% 
              
              left_join(db_neat,.,by = NULL) %>% rename(a_cp_2000 = Value)
       
       db_neat  <- temp_cp %>%
              
              filter(Section %in% "9b: Asset Age Profile",
                     Subcategory %in% "Concrete poles / steel structure",
                     Description %in% "2001") %>%
              select(EDB,
                     Network,
                     Year,
                     a_cp_2001 =Value) %>% 
              
              left_join(db_neat,.,by = NULL)   
       
       db_neat  <- temp_cp %>%
              
              filter(Section %in% "9b: Asset Age Profile",
                     Subcategory %in% "Concrete poles / steel structure",
                     Description %in% "2002") %>%
              select(EDB,
                     Network,
                     Year,
                     a_cp_2002 =Value) %>% 
              
              left_join(db_neat,.,by = NULL)      
       
       
       db_neat  <- temp_cp %>%
              
              filter(Section %in% "9b: Asset Age Profile",
                     Subcategory %in% "Concrete poles / steel structure",
                     Description %in% "2003") %>%
              select(EDB,
                     Network,
                     Year,
                     a_cp_2003 =Value) %>% 
              
              left_join(db_neat,.,by = NULL)    
       
       db_neat  <- temp_cp %>%
              
              filter(Section %in% "9b: Asset Age Profile",
                     Subcategory %in% "Concrete poles / steel structure",
                     Description %in% "2004") %>%
              select(EDB,
                     Network,
                     Year,
                     a_cp_2004 = Value) %>% 
              
              left_join(db_neat,.,by = NULL)  
       
       db_neat  <- temp_cp %>%
              
              filter(Section %in% "9b: Asset Age Profile",
                     Subcategory %in% "Concrete poles / steel structure",
                     Description %in% "2005") %>%
              select(EDB,
                     Network,
                     Year,
                     a_cp_2005 = Value) %>% 
              
              left_join(db_neat,.,by = NULL)       
       
       db_neat  <- temp_cp %>%
              
              filter(Section %in% "9b: Asset Age Profile",
                     Subcategory %in% "Concrete poles / steel structure",
                     Description %in% "2006") %>%
              select(EDB,
                     Network,
                     Year,
                     a_cp_2006 = Value) %>% 
              
              left_join(db_neat,.,by = NULL)         
       
       db_neat  <- temp_cp %>%
              
              filter(Section %in% "9b: Asset Age Profile",
                     Subcategory %in% "Concrete poles / steel structure",
                     Description %in% "2007") %>%
              select(EDB,
                     Network,
                     Year,
                     a_cp_2007 = Value) %>% 
              
              left_join(db_neat,.,by = NULL)     
       
       db_neat  <- temp_cp %>%
              
              filter(Section %in% "9b: Asset Age Profile",
                     Subcategory %in% "Concrete poles / steel structure",
                     Description %in% "2008") %>%
              select(EDB,
                     Network,
                     Year,
                     a_cp_2008 = Value) %>% 
              
              left_join(db_neat,.,by = NULL) 
       
       db_neat  <- temp_cp %>%
              
              filter(Section %in% "9b: Asset Age Profile",
                     Subcategory %in% "Concrete poles / steel structure",
                     Description %in% "2009") %>%
              select(EDB,
                     Network,
                     Year,
                     a_cp_2009 = Value) %>% 
              
              left_join(db_neat,.,by = NULL) 
       
       db_neat  <- temp_cp %>%
              
              filter(Section %in% "9b: Asset Age Profile",
                     Subcategory %in% "Concrete poles / steel structure",
                     Description %in% "2010") %>%
              select(EDB,
                     Network,
                     Year,
                     a_cp_2010 = Value) %>% 
              
              left_join(db_neat,.,by = NULL) 
       
       db_neat  <- temp_cp %>%
              
              filter(Section %in% "9b: Asset Age Profile",
                     Subcategory %in% "Concrete poles / steel structure",
                     Description %in% "2011") %>%
              select(EDB,
                     Network,
                     Year,
                     a_cp_2011 = Value) %>% 
              
              left_join(db_neat,.,by = NULL) 
       
       db_neat  <- temp_cp %>%
              
              filter(Section %in% "9b: Asset Age Profile",
                     Subcategory %in% "Concrete poles / steel structure",
                     Description %in% "2012") %>%
              select(EDB,
                     Network,
                     Year,
                     a_cp_2012 = Value) %>% 
              
              left_join(db_neat,.,by = NULL) 
       
       
       db_neat  <- temp_cp %>%
              
              filter(Section %in% "9b: Asset Age Profile",
                     Subcategory %in% "Concrete poles / steel structure",
                     Description %in% "2013") %>%
              select(EDB,
                     Network,
                     Year,
                     a_cp_2013 = Value) %>% 
              
              left_join(db_neat,.,by = NULL) 
       
       
       db_neat  <- temp_cp %>%
              
              filter(Section %in% "9b: Asset Age Profile",
                     Subcategory %in% "Concrete poles / steel structure",
                     Description %in% "2014") %>%
              select(EDB,
                     Network,
                     Year,
                     a_cp_2014 = Value) %>% 
              
              left_join(db_neat,.,by = NULL) 
       
       
       db_neat  <- temp_cp %>%
              
              filter(Section %in% "9b: Asset Age Profile",
                     Subcategory %in% "Concrete poles / steel structure",
                     Description %in% "2015") %>%
              select(EDB,
                     Network,
                     Year,
                     a_cp_2015 = Value) %>% 
              
              left_join(db_neat,.,by = NULL) 
       
       
       db_neat  <- temp_cp %>%
              
              filter(Section %in% "9b: Asset Age Profile",
                     Subcategory %in% "Concrete poles / steel structure",
                     Description %in% "2016") %>%
              select(EDB,
                     Network,
                     Year,
                     a_cp_2016 = Value) %>% 
              
              left_join(db_neat,.,by = NULL)    
       
       
       db_neat  <- temp_cp %>%
              
              filter(Section %in% "9b: Asset Age Profile",
                     Subcategory %in% "Concrete poles / steel structure",
                     Description %in% "2017") %>%
              select(EDB,
                     Network,
                     Year,
                     a_cp_2017 = Value) %>% 
              
              left_join(db_neat,.,by = NULL)  
       
       db_neat  <- temp_cp %>%
              
              filter(Section %in% "9b: Asset Age Profile",
                     Subcategory %in% "Concrete poles / steel structure",
                     Description %in% "2018") %>%
              select(EDB,
                     Network,
                     Year,
                     a_cp_2018 = Value) %>% 
              
              left_join(db_neat,.,by = NULL)  
       
       
       ### 5.1.1.X Capacitor Banks ####
       
       db_neat  <- bind_cols(db_neat,
                             db_raw %>% 
                                    filter(Section %in% "9a: Asset Register",
                                           Category %in% "All - Capacitor Banks",
                                           Description %in% "Items at end of year (quantity)") %>%
                                    select(a_bc= Value)
       )
       
       ### 5.1.1.X. Voltage Regulator ####
       
       db_neat  <- bind_cols(db_neat,
                             db_raw %>% 
                                    filter(Section %in% "9a: Asset Register",
                                           Category %in% "HV - Distribution Transformer",
                                           Subcategory %in% "Voltage regulators",
                                           Description %in% "Items at end of year (quantity)") %>%
                                    select(a_vr= Value)
       )
       
       ### 5.1.1.X. HV - Zone substation Buildings ####
       
       db_neat  <- bind_cols(db_neat,
                             db_raw %>% 
                                    filter(Section %in% "9a: Asset Register",
                                           Category %in% "HV - Zone substation Buildings",
                                           Subcategory %in% "Zone substations up to 66kV",
                                           Description %in% "Items at end of year (quantity)") %>%
                                    select(a_hv_zon= Value)
       )
       
       ### 5.1.1.X. HV - Zone Substation Transformer ####
       
       db_neat  <- bind_cols(db_neat,
                             db_raw %>% 
                                    filter(Section %in% "9a: Asset Register",
                                           Category %in% "HV - Zone Substation Transformer",
                                           Subcategory %in% "Zone Substation Transformers",
                                           Description %in% "Items at end of year (quantity)") %>%
                                    select(a_hv_trafo= Value)
       )
       
       ### 5.1.1.X. Switches
       
       temp <- db_raw %>%  filter(Section %in% "9a: Asset Register",
                                  Category %in% "HV - Zone substation switchgear",
                                  Description %in% "Items at end of year (quantity)") %>% 
              select(EDB,Network,Year,Subcategory,Value) %>% 
              group_by(EDB,Network,Year) %>% 
              mutate(a_zon_swt = sum(Value)) %>%
              select(EDB,Network,Year,a_zon_swt) %>%
              distinct() %>% ungroup()
       
       db_neat <- left_join(x = temp,y = db_neat,by = NULL)
       
       rm(temp)
       
       ### 5.1.1.X. SCADA and communications equipment operating as a single system
       
       db_neat <- bind_cols(db_neat,
                            db_raw %>% 
                                   filter(Section %in% "9a: Asset Register",
                                          Category %in% "All - SCADA and communications",
                                          Subcategory %in% "SCADA and communications equipment operating as a single system",
                                          Description %in% "Items at end of year (quantity)") %>%
                                   select(a_scada= Value))
       
       ### 5.1.1.X. Protection
       
       db_neat <- bind_cols(db_neat,
                            db_raw %>% 
                                   filter(Section %in% "9a: Asset Register",
                                          Category %in% "All - Protection",
                                          Subcategory %in% "Protection relays (electromechanical, solid state and numeric)",
                                          Description %in% "Items at end of year (quantity)") %>%
                                   select(a_prot_rel= Value))
       
       ### 5.1.1.X. Relays
       
       db_neat <- bind_cols(db_neat,
                            db_raw %>% 
                                   filter(Section %in% "9a: Asset Register",
                                          Category %in% "All - Load Control",
                                          Subcategory %in% "Relays",
                                          Description %in% "Items at end of year (quantity)") %>%
                                   select(a_ld_rel= Value)
       )
       
       ### 5.1.1.X. Centralised plant
       
       db_neat <- bind_cols(db_neat,
                            db_raw %>% 
                                   filter(Section %in% "9a: Asset Register",
                                          Category %in% "All - Load Control",
                                          Subcategory %in% "Centralised plant",
                                          Description %in% "Items at end of year (quantity)") %>%
                                   select(a_ld_cen_plant= Value)
       )
       
       
       ### 5.1.2. Non-network Assets ####
       
       
       
       
       
       
       ## 5.2. Reliability ####
       
       ### 5.2.1. Interruptions Total #### 
       
       db_neat  <- bind_cols(db_neat,
                             db_raw %>% 
                                    filter(Section %in% "10(i): Interruptions",
                                           Category %in% "Interruptions by class",
                                           Description %in% "Total") %>%
                                    select(r_i_tot= Value)
       )
       
       ### 5.2.1.1. Interruptions SAIDI Total #### 
       
       db_neat  <- bind_cols(db_neat,
                             db_raw %>% 
                                    filter(Section %in% "10(i): Interruptions",
                                           Subcategory %in% "SAIDI",
                                           Description %in% "Total") %>%
                                    select(r_saidi= Value)
       )
       
       ### 5.2.1.2. Interruptions SAIFI Total #### 
       db_neat  <- bind_cols(db_neat,
                             db_raw %>% 
                                    filter(Section %in% "10(i): Interruptions",
                                           Subcategory %in% "SAIFI",
                                           Description %in% "Total") %>%
                                    select(r_saifi= Value)
       )
       
       ### 5.2.2. Planned #### 
       
       db_neat  <- bind_cols(db_neat,
                             db_raw %>% 
                                    filter(Section %in% "10(i): Interruptions",
                                           Category %in% "Interruptions by class",
                                           Description %in% "Class B (planned interruptions on the network)") %>%
                                    select(r_i_pl= Value)
       )
       
       ### 5.2.2.1. Planned SAIDI #### 
       
       db_neat  <- bind_cols(db_neat,
                             db_raw %>% 
                                    filter(Section %in% "10(i): Interruptions",
                                           Subcategory %in% "SAIDI",
                                           Description %in% "Class B (planned interruptions on the network)") %>%
                                    select(r_pl_saidi= Value)
       )
       ### 5.2.2.2. Planned SAIFI #### 
       
       db_neat  <- bind_cols(db_neat,
                             db_raw %>% 
                                    filter(Section %in% "10(i): Interruptions",
                                           Subcategory %in% "SAIFI",
                                           Description %in% "Class B (planned interruptions on the network)") %>%
                                    select(r_pl_saifi= Value)
       )
       
       ### 5.2.3. Unplanned ####
       
       db_neat  <- bind_cols(db_neat,
                             db_raw %>% 
                                    filter(Section %in% "10(i): Interruptions",
                                           Subcategory %in% "Number of interruptions",
                                           Description %in% "Class C (unplanned interruptions on the network)") %>%
                                    select(r_i_un= Value)
       )
       
       ### 5.2.3.1. Unplanned SAIDI ####
       
       db_neat  <- bind_cols(db_neat,
                             db_raw %>% 
                                    filter(Section %in% "10(i): Interruptions",
                                           Subcategory %in% "SAIDI",
                                           Description %in% "Class C (unplanned interruptions on the network)") %>%
                                    select(r_un_saidi= Value)
       )
       
       ### 5.2.3.2. Unplanned SAIFI ####
       
       db_neat  <- bind_cols(db_neat,
                             db_raw %>% 
                                    filter(Section %in% "10(i): Interruptions",
                                           Subcategory %in% "SAIFI",
                                           Description %in% "Class C (unplanned interruptions on the network)") %>%
                                    select(r_un_saifi= Value)
       )
       
       ### 5.2.3.3. Unplanned SAIDI Causes ####
       
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
       
       ### 5.2.3.3. Unplanned SAIDI Causes ####
       
       
       
       
       
       
       
       
       ### 5.2.3.4. Unplanned SAIFI Causes ####
       
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
       
       
       ### 5.2.3.4. Unplanned SAIFI Causes by Equipements
       
       
       
       
       ### 5.2.3.4. Unplanned SAIFI Causes by Equipements - Subtransmission lines
       db_neat <- db_raw %>%
              filter(Section %in% "10(iv): Class C Interruptions and Duration by Main Equipment Involved",
                     Subcategory %in% "SAIDI",
                     Description %in% "Subtransmission lines") %>%
              select(EDB,
                     Network,
                     Year,
                     r_i_cau_equ_sub_li = Value) %>%
              
              left_join(db_neat,.,by = NULL)
       
       ### 5.2.3.4. Unplanned SAIFI Causes by Equipements - Subtransmission cables
       
       db_neat <- db_raw %>%
              filter(Section %in% "10(iv): Class C Interruptions and Duration by Main Equipment Involved",
                     Subcategory %in% "SAIDI",
                     Description %in% "Subtransmission cables") %>%
              select(EDB,
                     Network,
                     Year,
                     r_i_cau_equ_sub_cb = Value) %>%
              
              left_join(db_neat,.,by = NULL)

       ### 5.2.3.4. Unplanned SAIFI Causes by Equipements - Subtransmission other
       

       db_neat <- db_raw %>%
              filter(Section %in% "10(iv): Class C Interruptions and Duration by Main Equipment Involved",
                     Subcategory %in% "SAIDI",
                     Description %in% "Subtransmission other") %>%
              select(EDB,
                     Network,
                     Year,
                     r_i_cau_equ_sub_oth = Value) %>%
              
              left_join(db_neat,.,by = NULL)
       
       ### 5.2.3.4. Unplanned SAIFI Causes by Equipements - Distribution lines (excluding LV)
       
       
       db_neat <- db_raw %>%
              filter(Section %in% "10(iv): Class C Interruptions and Duration by Main Equipment Involved",
                     Subcategory %in% "SAIDI",
                     Description %in% "Distribution lines (excluding LV)") %>%
              select(EDB,
                     Network,
                     Year,
                     r_i_cau_equ_dis_li_exc_lv = Value) %>%
              
              left_join(db_neat,.,by = NULL)
                
       ### 5.2.3.4. Unplanned SAIFI Causes by Equipements - Distribution cables (excluding LV)
       
       
       db_neat <- db_raw %>%
              filter(Section %in% "10(iv): Class C Interruptions and Duration by Main Equipment Involved",
                     Subcategory %in% "SAIDI",
                     Description %in% "Distribution cables (excluding LV)") %>%
              select(EDB,
                     Network,
                     Year,
                     r_i_cau_equ_dis_cb_exc_lv = Value) %>%
              
              left_join(db_neat,.,by = NULL)
       
       ### 5.2.3.4. Unplanned SAIFI Causes by Equipements - Distribution other (excluding LV)
       
       
       db_neat <- db_raw %>%
              filter(Section %in% "10(iv): Class C Interruptions and Duration by Main Equipment Involved",
                     Subcategory %in% "SAIDI",
                     Description %in% "Distribution other (excluding LV)") %>%
              select(EDB,
                     Network,
                     Year,
                     r_i_cau_equ_dis_oth_exc_lv = Value) %>%
              
              left_join(db_neat,.,by = NULL)
       
       
       
       
       
       
       ### 5.2.4. Restoration #### 
       
       ### 5.2.4.1 Consumer restored in less than 3 hours ####
       
       db_neat  <-   bind_cols(db_neat,
                               db_raw %>% 
                                      filter(Section %in% "10(i): Interruptions",
                                             Category %in% "Interruption restoration",
                                             !Description %in% ">3hrs") %>%            # ATENTION!! => DIFFERENT of Greater than 3 hour
                                      select(r_less_3= Value)                   # It means: Less than 3 hours
       )
       
       ### 5.2.4.2 Consumer restored in more than 3 hours ####
       
       db_neat  <- bind_cols(db_neat,
                             db_raw %>% 
                                    filter(Section %in% "10(i): Interruptions",
                                           Category %in% "Interruption restoration",
                                           Description %in% ">3hrs") %>%
                                    select(r_more_3= Value)
       )
       
       ### 5.2.5. Transpower influency in outages ####
       
       ### 5.2.5.1. Transpower Interruptions Frequence/Quantity ####
       
       db_neat <- bind_cols(db_neat,
                            db_raw %>%
                                   filter(Section %in% "10(i): Interruptions",
                                          Category %in% "Interruptions by class",
                                          Description %in% "Class A (planned interruptions by Transpower)") %>%
                                   select(r_tp_class_a = Value),
                            
                            db_raw %>%
                                   filter(Section %in% "10(i): Interruptions",
                                          Category %in% "Interruptions by class",
                                          Description %in% "Class D (unplanned interruptions by Transpower)") %>%
                                   select(r_tp_class_d = Value)) %>%
              
              mutate(r_tp = r_tp_class_a + r_tp_class_d)
       
       ### 5.2.5.2. Transpower SAIDI Unplanned ####
       db_neat  <- bind_cols(db_neat,
                             db_raw %>% 
                                    filter(Section %in% "10(i): Interruptions",
                                           Subcategory %in% "SAIDI",
                                           Description %in% "Class D (unplanned interruptions by Transpower)") %>%
                                    select(r_un_tp_saidi= Value)
       )
       
       ### 5.2.5.3. Transpower SAIDI Planned ####
       db_neat  <- bind_cols(db_neat,
                             db_raw %>% 
                                    filter(Section %in% "10(i): Interruptions",
                                           Subcategory %in% "SAIDI",
                                           Description %in% "Class A (planned interruptions by Transpower)") %>%
                                    select(r_pl_tp_saidi= Value)
       )
       
       ### 5.2.5.4. Transpower SAIFI Unplanned ####
       db_neat  <- bind_cols(db_neat,
                             db_raw %>% 
                                    filter(Section %in% "10(i): Interruptions",
                                           Subcategory %in% "SAIFI",
                                           Description %in% "Class D (unplanned interruptions by Transpower)") %>%
                                    select(r_un_tp_saifi= Value)
       )
       
       ### 5.2.5.5. Transpower SAIFI Planned ####
       db_neat  <- bind_cols(db_neat,
                             db_raw %>% 
                                    filter(Section %in% "10(i): Interruptions",
                                           Subcategory %in% "SAIFI",
                                           Description %in% "Class A (planned interruptions by Transpower)") %>%
                                    select(r_pl_tp_saifi= Value)
       )
       
       
       
       
       
       
       ### 5.3. Network
       
       
       ### 5.X.X. Total Energy Supplied and Losses ####
       
       ## Electricity entering in the system
       
       db_neat  <- bind_cols(db_neat,
                             db_raw %>% 
                                    filter(Section %in% "9e(ii): System Demand",
                                           Category %in% "Electricity volumes carried",
                                           Description %in% "Electricity entering system for supply to consumers' connection points") %>%
                                    select(s_ent_enr= Value)
       )
       
       ## Electricity supplied from GXPs (Grids Exits Points)
       db_neat  <- bind_cols(db_neat,
                             db_raw %>% 
                                    filter(Section %in% "9e(ii): System Demand",
                                           Category %in% "Electricity volumes carried",
                                           Description %in% "Electricity supplied from GXPs") %>%
                                    select(s_gxp_enr= Value)
       )
       
       ## Electricity supplied from distributed generation
       db_neat  <- bind_cols(db_neat,
                             db_raw %>% 
                                    filter(Section %in% "9e(ii): System Demand",
                                           Category %in% "Electricity volumes carried",
                                           Description %in% "Electricity supplied from distributed generation") %>%
                                    select(s_dr_enr= Value)
       )
       
       ## Electricity Exported
       
       db_neat  <- bind_cols(db_neat,
                             db_raw %>% 
                                    filter(Section %in% "9e(ii): System Demand",
                                           Category %in% "Electricity volumes carried",
                                           Description %in% "Electricity exports to GXPs") %>%
                                    select(s_exp_enr= Value)
       )
       
       ## Net electricity supplied to (from) other EDBs
       
       db_neat  <- bind_cols(db_neat,
                             db_raw %>% 
                                    filter(Section %in% "9e(ii): System Demand",
                                           Category %in% "Electricity volumes carried",
                                           Description %in% "Net electricity supplied to (from) other EDBs") %>%
                                    select(s_oth_edb= Value)
       )
       
       ## Total Energy Consumers Delivered
       
       db_neat  <- bind_cols(db_neat,
                             db_raw %>% 
                                    filter(Section %in% "9e(ii): System Demand",
                                           Category %in% "Electricity volumes carried",
                                           Description %in% "Total energy delivered to ICPs") %>%
                                    select(s_tot_enr= Value)
       )
       
       
       ## 5.4. Regulatory
       
       ## 5.4.1. RAB Opening Year ####
       
       db_neat <- db_raw %>%
              filter(Section %in% "4(ii): Unallocated Regulatory Asset Base",
                     Subcategory %in% "RAB",
                     Description %in% "Total opening RAB value") %>%
                     select(EDB,
                            Network,
                            Year,
                            rg_rab_open = Value) %>%
              
                            left_join(db_neat,.,by = NULL)
       
       ## 5.4.1. RAB Opening Year - Subtransmission lines####

       db_neat <- db_raw %>%
              filter(Section %in% "4(vii): Disclosure by Asset Category",
                     Subcategory %in% "Total opening RAB value",
                     Description %in% "Subtransmission lines") %>%
              select(EDB,
                     Network,
                     Year,
                     rg_rab_open_sub_li = Value) %>%
              
              left_join(db_neat,.,by = NULL)
       
       ## 5.4.1. RAB Opening Year - Subtransmission cables ####
       
       db_neat <- db_raw %>%
              filter(Section %in% "4(vii): Disclosure by Asset Category",
                     Subcategory %in% "Total opening RAB value",
                     Description %in% "Subtransmission cables") %>%
              select(EDB,
                     Network,
                     Year,
                     rg_rab_open_sub_cb = Value) %>%
              
              left_join(db_neat,.,by = NULL)
       
       ## 5.4.1. RAB Opening Year - Distribution and LV lines ####
       
       db_neat <- db_raw %>%
              filter(Section %in% "4(vii): Disclosure by Asset Category",
                     Subcategory %in% "Total opening RAB value",
                     Description %in% "Distribution and LV lines") %>%
              select(EDB,
                     Network,
                     Year,
                     rg_rab_open_dis_lv_li = Value) %>%
              
              left_join(db_neat,.,by = NULL)

       ## 5.4.1. RAB Opening Year - Distribution switchgear ####
       
       db_neat <- db_raw %>%
              filter(Section %in% "4(vii): Disclosure by Asset Category",
                     Subcategory %in% "Total opening RAB value",
                     Description %in% "Distribution switchgear") %>%
              select(EDB,
                     Network,
                     Year,
                     rg_rab_open_dis_swi = Value) %>%
              
              left_join(db_neat,.,by = NULL)

       ## 5.4.1. RAB Opening Year - Distribution and LV cables ####
       
       db_neat <- db_raw %>%
              filter(Section %in% "4(vii): Disclosure by Asset Category",
                     Subcategory %in% "Total opening RAB value",
                     Description %in% "Distribution and LV cables") %>%
              select(EDB,
                     Network,
                     Year,
                     rg_rab_open_dis_lv_cb = Value) %>%
              
              left_join(db_neat,.,by = NULL)
       
       ## 5.4.1. RAB Opening Year - Distribution substations and transformers ####
       
       db_neat <- db_raw %>%
              filter(Section %in% "4(vii): Disclosure by Asset Category",
                     Subcategory %in% "Total opening RAB value",
                     Description %in% "Distribution substations and transformers") %>%
              select(EDB,
                     Network,
                     Year,
                     rg_rab_open_dis_sub_tran = Value) %>%
              
              left_join(db_neat,.,by = NULL)
       
       ## 5.4.1. RAB Opening Year - Zone substations ####
       
       db_neat <- db_raw %>%
              filter(Section %in% "4(vii): Disclosure by Asset Category",
                     Subcategory %in% "Total opening RAB value",
                     Description %in% "Zone substations") %>%
              select(EDB,
                     Network,
                     Year,
                     rg_rab_open_z_sub = Value) %>%
              
              left_join(db_neat,.,by = NULL)
       
       ## 5.4.1. RAB Opening Year - Other network assets ####
       
       db_neat <- db_raw %>%
              filter(Section %in% "4(vii): Disclosure by Asset Category",
                     Subcategory %in% "Total opening RAB value",
                     Description %in% "Other network assets") %>%
              select(EDB,
                     Network,
                     Year,
                     rg_rab_open_a_net_othe = Value) %>%
              
              left_join(db_neat,.,by = NULL)
       
       ## 5.4.1. RAB Opening Year - Non-network assets ####
       
       db_neat <- db_raw %>%
              filter(Section %in% "4(vii): Disclosure by Asset Category",
                     Subcategory %in% "Total opening RAB value",
                     Description %in% "Non-network assets") %>%
              select(EDB,
                     Network,
                     Year,
                     rg_rab_open_a_non_net = Value) %>%
              
              left_join(db_neat,.,by = NULL)
       
       ## 5.4.1. RAB CLOSING Year TOTAL ####
       
       db_neat <- db_raw %>%
              filter(Section %in% "4(ii): Unallocated Regulatory Asset Base",
                     Category %in% "Total closing RAB value",
                     Subcategory %in% "Unallocated RAB",
                     Description %in% "Total closing RAB value") %>%
              select(EDB,
                     Network,
                     Year,
                     rg_rab_closed_tot = Value) %>%
              
              left_join(db_neat,.,by = NULL)
       
       ## 5.4.1. RAB Closing Year - Distribution and LV lines ####
       
       db_neat <- db_raw %>%
              filter(
                     Section %in% "4(vii): Disclosure by Asset Category",
                     Subcategory %in% "Total closing RAB value",
                     Description %in% "Distribution and LV lines") %>%
              select(EDB,
                     Network,
                     Year,
                     rg_rab_clo_li_lv = Value) %>%
              
              left_join(db_neat,.,by = NULL)
       
       ## 5.4.1. RAB Closing Year - Distribution and LV cables ####
       
       db_neat <- db_raw %>%                                                                                #
              filter(                                                   #
                     Section %in% "4(vii): Disclosure by Asset Category",                        #
                     Subcategory %in% "Total closing RAB value",                                 # Selecting and 
                     Description %in% "Distribution and LV cables") %>%                          # Filtering 
              select(EDB,
                     Network,
                     Year,
                     rg_rab_clo_cb_lv = Value) %>%
              
              left_join(db_neat,.,by = NULL)
       
       ## 5.4.1. RAB Closing Year - Distribution substations and transformers ####
       
       db_neat <- db_raw %>%                                                                                #
              filter(                                                   #
                     Section %in% "4(vii): Disclosure by Asset Category",                        #
                     Subcategory %in% "Total closing RAB value",                                 # Selecting and 
                     Description %in% "Distribution substations and transformers") %>%           # Filtering 
              select(EDB,
                     Network,
                     Year,
                     rg_rab_clo_sb_tr = Value) %>%
              
              left_join(db_neat,.,by = NULL)
       
       ## 5.4.1. RAB Closing Year - Distribution switchgear ####
       
       db_neat <- db_raw %>%                                                                                #
              filter(                                                   #
                     Section %in% "4(vii): Disclosure by Asset Category",                        #
                     Subcategory %in% "Total closing RAB value",                                 # Selecting and 
                     Description %in% "Distribution switchgear") %>%                             # Filtering 
              select(EDB,
                     Network,
                     Year,
                     rg_rab_clo_swi = Value) %>%
              
              left_join(db_neat,.,by = NULL)
       
       ## 5.4.1. RAB Closing Year - Subtransmission lines ####
       
       db_neat <- db_raw %>%                                                                                #
              filter(                                                   #
                     Section %in% "4(vii): Disclosure by Asset Category",                        #
                     Subcategory %in% "Total closing RAB value",                                 # Selecting and 
                     Description %in% "Subtransmission lines") %>%                               # Filtering 
              select(EDB,
                     Network,
                     Year,
                     rg_rab_clo_li_sb = Value) %>%
              
              left_join(db_neat,.,by = NULL)
       
       ## 5.4.1. RAB Closing Year - Subtransmission cables ####
       
       db_neat <- db_raw %>%                                                                                #
              filter(                                                   #
                     Section %in% "4(vii): Disclosure by Asset Category",                        #
                     Subcategory %in% "Total closing RAB value",                                 # Selecting and 
                     Description %in% "Subtransmission cables") %>%                              # Filtering 
              select(EDB,
                     Network,
                     Year,
                     rg_rab_clo_cb_sb = Value) %>%
              
              left_join(db_neat,.,by = NULL)
       
       ## 5.4.1. RAB Closing Year - Non-network assets ####
       
       db_neat <- db_raw %>%                                                                                #
              filter(                                                   #
                     Section %in% "4(vii): Disclosure by Asset Category",                        #
                     Subcategory %in% "Total closing RAB value",                                 # Selecting and 
                     Description %in% "Non-network assets") %>%                                  # Filtering 
              select(EDB,
                     Network,
                     Year,
                     rg_rab_clo_non_net = Value) %>%
              
              left_join(db_neat,.,by = NULL)
       
       ## 5.4.1. RAB Closing Year - Other network assets ####
       
       db_neat <- db_raw %>%                                                                                #
              filter(                                                   #
                     Section %in% "4(vii): Disclosure by Asset Category",                        #
                     Subcategory %in% "Total closing RAB value",                                 # Selecting and 
                     Description %in% "Other network assets") %>%                                # Filtering 
              select(EDB,
                     Network,
                     Year,
                     rg_rab_clo_net_ot = Value) %>%
              
              left_join(db_neat,.,by = NULL)
       
       ## 5.4.1. RAB Closing Year - Zone substations ####
       
       db_neat <- db_raw %>%                                                                                #
              filter(                                                   #
                     Section %in% "4(vii): Disclosure by Asset Category",                        #
                     Subcategory %in% "Total closing RAB value",                                 # Selecting and 
                     Description %in% "Zone substations") %>%                                    # Filtering 
              select(EDB,
                     Network,
                     Year,
                     rg_rab_clo_z_sub = Value) %>%
              
              left_join(db_neat,.,by = NULL)
       
       ## 5.4.1. RAB Closing Year - Total ####
       
       db_neat <- db_raw %>%                                                                                #
              filter(                                                   #
                     Section %in% "4(vii): Disclosure by Asset Category",                        #
                     Subcategory %in% "Total closing RAB value",                                 # Selecting and 
                     Description %in% "Total") %>%                                               # Filtering 
              select(EDB,
                     Network,
                     Year,
                     rg_a_rab_clo = Value) %>%
              
              left_join(db_neat,.,by = NULL)
       
       ## 5.4.1. RAB - Asset Commissioned - Assets commissioned (other than below)####
       
       db_neat <- db_raw %>%
              filter(
                     Section %in% "4(ii): Unallocated Regulatory Asset Base",
                     Subcategory %in% "RAB",
                     Description %in% "Assets commissioned (other than below)") %>%
              select(EDB,
                     Network,
                     Year,
                     rg_a_com_oth = Value)%>%
              
              left_join(db_neat,.,by = NULL)
       
       ## 5.4.1. RAB - Asset Commissioned - Assets acquired from a regulated supplier ####
       
       db_neat <- db_raw %>%
              filter(
                     Section %in% "4(ii): Unallocated Regulatory Asset Base",
                     Subcategory %in% "RAB",
                     Description %in% "Assets acquired from a regulated supplier") %>%
              select(EDB,
                     Network,
                     Year,
                     rg_a_com_acq_sup = Value)%>%
              
              left_join(db_neat,.,by = NULL)
       
       ## 5.4.1. RAB - Asset Commissioned - Assets acquired from a related party ####
       
       db_neat <- db_raw %>%
              filter(
                     Section %in% "4(ii): Unallocated Regulatory Asset Base",
                     Subcategory %in% "RAB",
                     Description %in% "Assets acquired from a related party") %>%
              select(EDB,
                     Network,
                     Year,
                     rg_a_com_acq_rp = Value)%>%
              
              left_join(db_neat,.,by = NULL)
       
       ## 5.4.1. RAB - Asset Commissioned Total ####
       
       db_neat <- db_raw %>%
              filter(
                     Section %in% "4(ii): Unallocated Regulatory Asset Base",
                     Subcategory %in% "RAB",
                     Description %in% "Assets commissioned") %>%
              select(EDB,
                     Network,
                     Year,
                     rg_a_com_tot = Value)%>%
              
              left_join(db_neat,.,by = NULL)

       ## 5.4.1. RAB - Asset Commissioned - Distribution and LV lines ####
       

       db_neat <- db_raw %>%                                                                  #
              filter(                                                   #
                     Section %in% "4(vii): Disclosure by Asset Category",                        #
                     Subcategory %in% "Assets commissioned",                                     # Selecting and 
                     Description %in% "Distribution and LV lines") %>%                           # Filtering 
              select(EDB,
                     Network,
                     Year,                                                                # variable 1
                     rg_a_com_li_lv = Value) %>%
              
              left_join(db_neat,.,by = NULL)

       ## 5.4.1. RAB - Asset Commissioned - Distribution and LV cables ####
       
       db_neat <- db_raw %>%                                                                                #
              filter(                                                   #
                     Section %in% "4(vii): Disclosure by Asset Category",                        #
                     Subcategory %in% "Assets commissioned",                                     # Selecting and 
                     Description %in% "Distribution and LV cables") %>%                          # Filtering 
              select(EDB,
                     Network,
                     Year,
                     rg_a_com_cb_lv = Value) %>%
              
              left_join(db_neat,.,by = NULL)
       
       ## 5.4.1. RAB - Asset Commissioned - Distribution substations and transformers ####
       
       
       db_neat <- db_raw %>%                                                                                #
              filter(                                                   #
                     Section %in% "4(vii): Disclosure by Asset Category",                        #
                     Subcategory %in% "Assets commissioned",                                     # Selecting and 
                     Description %in% "Distribution substations and transformers") %>%           # Filtering 
              select(EDB,
                     Network,
                     Year,
                     rg_a_com_sb_tr = Value) %>%
              
              left_join(db_neat,.,by = NULL)
       
       ## 5.4.1. RAB - Asset Commissioned - Distribution switchgear ####
       
       
       db_neat <- db_raw %>%                                                                                #
              filter(                                                   #
                     Section %in% "4(vii): Disclosure by Asset Category",                        #
                     Subcategory %in% "Assets commissioned",                                     # Selecting and 
                     Description %in% "Distribution switchgear") %>%                             # Filtering 
              select(EDB,
                     Network,
                     Year,
                     rg_a_com_swi = Value) %>%
              
              left_join(db_neat,.,by = NULL)
       
       ## 5.4.1. RAB - Asset Commissioned - Subtransmission lines ####
       
       db_neat <- db_raw %>%                                                                                #
              filter(                                                   #
                     Section %in% "4(vii): Disclosure by Asset Category",                        #
                     Subcategory %in% "Assets commissioned",                                     # Selecting and 
                     Description %in% "Subtransmission lines") %>%                               # Filtering 
              select(EDB,
                     Network,
                     Year,
                     rg_a_com_li_sb = Value) %>%
              
              left_join(db_neat,.,by = NULL)
       
       ## 5.4.1. RAB - Asset Commissioned - Subtransmission cables ####
       
       
       db_neat <- db_raw %>%                                                                                #
              filter(                                                   #
                     Section %in% "4(vii): Disclosure by Asset Category",                        #
                     Subcategory %in% "Assets commissioned",                                     # Selecting and 
                     Description %in% "Subtransmission cables") %>%                              # Filtering 
              select(EDB,
                     Network,
                     Year,
                     rg_a_com_cb_sb = Value) %>%
              
              left_join(db_neat,.,by = NULL)
       
       ## 5.4.1. RAB - Asset Commissioned - Non-network assets ####
       
       db_neat <- db_raw %>%                                                                                #
              filter(                                                   #
                     Section %in% "4(vii): Disclosure by Asset Category",                        #
                     Subcategory %in% "Assets commissioned",                                     # Selecting and 
                     Description %in% "Non-network assets") %>%                                  # Filtering 
              select(EDB,
                     Network,
                     Year,
                     rg_a_com_non_net = Value) %>%
              
              left_join(db_neat,.,by = NULL)
       
       ## 5.4.1. RAB - Asset Commissioned - Other network assets ####
       
       
       db_neat <- db_raw %>%                                                                                #
              filter(                                                   #
                     Section %in% "4(vii): Disclosure by Asset Category",                        #
                     Subcategory %in% "Assets commissioned",                                     # Selecting and 
                     Description %in% "Other network assets") %>%                                # Filtering 
              select(EDB,
                     Network,
                     Year,
                     rg_a_com_net_ot = Value) %>%
              
              left_join(db_neat,.,by = NULL)
       
       ## 5.4.1. RAB - Asset Commissioned - Zone substations ####
       
       
       db_neat <- db_raw %>%                                                                                #
              filter(                                                   #
                     Section %in% "4(vii): Disclosure by Asset Category",                        #
                     Subcategory %in% "Assets commissioned",                                     # Selecting and 
                     Description %in% "Zone substations") %>%                                    # Filtering 
              select(EDB,
                     Network,
                     Year,
                     rg_a_com_z_sub = Value) %>%
              
              left_join(db_neat,.,by = NULL)
       
       ## 5.4.1. RAB - Asset Disposal - Asset disposals (other than below) ####
       
       db_neat <- db_raw %>%                                                                  #
              filter(                                                   #
                     Section %in% "4(ii): Unallocated Regulatory Asset Base",                    #
                     Subcategory %in% "RAB",                                                     # Selecting and 
                     Description %in% "Asset disposals (other than below)") %>%                  # Filtering 
              select(EDB,
                     Network,
                     Year,
                     rg_a_dis_ot = Value) %>%
              
              left_join(db_neat,.,by = NULL)
       
       ## 5.4.1. RAB - Asset Disposal - Asset disposals to a regulated supplier ####
       
       db_neat <- db_raw %>%                                                                                #
              filter(                                                   #
                     Section %in% "4(ii): Unallocated Regulatory Asset Base",                    #
                     Subcategory %in% "RAB",                                                     # Selecting and 
                     Description %in% "Asset disposals to a regulated supplier") %>%             # Filtering
              select(EDB,
                     Network,
                     Year,
                     rg_a_dis_sup = Value) %>%
              
              left_join(db_neat,.,by = NULL)
       
       ## 5.4.1. RAB - Asset Disposal - Asset disposals to a related party ####
       
       db_neat <- db_raw %>%                                                                                #
              filter(                                                   #
                     Section %in% "4(ii): Unallocated Regulatory Asset Base",                    #
                     Subcategory %in% "RAB",                                                     # Selecting and
                     Description %in% "Asset disposals to a related party") %>%
              select(EDB,
                     Network,
                     Year,
                     rg_a_dis_rp = Value) %>%
              
              left_join(db_neat,.,by = NULL)
       
       ## 5.4.1. RAB - Asset Disposal - Asset disposals to Total ####
       
       db_neat <- db_raw %>%                                                                                #
              filter(                                                   #
                     Section %in% "4(ii): Unallocated Regulatory Asset Base",                    #
                     Subcategory %in% "RAB",                                                     # Selecting and
                     Description %in% "Asset disposals") %>%
              select(EDB,
                     Network,
                     Year,
                     rg_a_dis_tot = Value) %>%
              
              left_join(db_neat,.,by = NULL)
       
       ## 5.4.1. RAB - Asset Adjustment - Lost and found assets adjustment ####
       
       db_neat <- db_raw %>%
              filter(
                     Section %in% "4(ii): Unallocated Regulatory Asset Base",
                     Subcategory %in% "RAB",
                     Description %in% "Lost and found assets adjustment") %>%
              select(EDB,
                     Network,
                     Year,
                     rg_a_adj_lf = Value) %>%
              
              left_join(db_neat,.,by = NULL)
       
       ## 5.4.1. RAB - Asset Adjustment - Adjustment resulting from asset allocation ####
       
       db_neat <- db_raw %>%
              filter(
                     Section %in% "4(ii): Unallocated Regulatory Asset Base",
                     Subcategory %in% "RAB",
                     Description %in% "Adjustment resulting from asset allocation") %>%
              select(EDB,
                     Network,
                     Year,
                     rg_a_adj_aln = Value) %>%
              
              left_join(db_neat,.,by = NULL)
       
       ## 5.4.1. RAB - Asset Adjustment Total ####
       
       db_neat <- mutate(db_neat,rg_a_adj_tot = rg_a_adj_lf + rg_a_adj_aln)
       
       ## 5.4.1. Regulatory Profit (after tax) ####
       
       db_neat <- db_raw %>%
              filter(Section %in% "3(i): Regulatory Profit",
                     Description %in% c("Regulatory profit / (loss)",
                                        "Regulatory profit/(loss) including financial incentives and wash-ups")) %>%
              select(EDB,
                     Network,
                     Year,
                     rg_pf = Value) %>%
              
              left_join(db_neat,.,by = NULL)
       
       ## 5.4.1. Regulatory Profit before tax ####
       
       db_neat <- db_raw %>%
              filter(Section %in% "3(i): Regulatory Profit",
                     Category %in% "Regulatory profit / (loss) before tax",
                     Description %in% "Regulatory profit / (loss) before tax") %>%
              select(EDB,
                     Network,
                     Year,
                     rg_pf_tx = Value) %>%
              
              left_join(db_neat,.,by = NULL)
       
       ## 5.4.1. Regulatory Allowance ####
       
       db_neat <- db_raw %>%
              filter(Section %in% "3(i): Regulatory Profit",
                     Description %in% "Regulatory tax allowance") %>%
              select(EDB,
                     Network,
                     Year,
                     rg_tx_al = Value) %>%
              
              left_join(db_neat,.,by = NULL)
       
       # 5.4.1. Regulatory Taxable Income ####
       
       db_neat <- db_raw %>%
              filter(Category %in% "Regulatory taxable income",
                     Description %in% "Regulatory taxable income") %>%
              select(EDB,
                     Network,
                     Year,
                     rg_tax_income = Value) %>%
              
              left_join(db_neat,.,by = NULL)
       
       ### 5.4.1.X. Tax Addition ####
       
       db_neat <- db_raw %>%
              filter(Section %in% "5a(i): Regulatory Tax Allowance",
                     Category %in% "(tax additions)",
                     !Description %in% "(tax additions)") %>%
              
              select(EDB,
                     Network,
                     Year,
                     Description,
                     Value) %>%
              
              group_by(EDB,Network,Year) %>% 
              mutate(rg_tax_add = sum(Value)) %>%
              ungroup() %>%
              select(EDB,Network,Year,rg_tax_add) %>%
              distinct() %>%
              left_join(db_neat,.,by = NULL)
       
       ### 5.4.1.X. Tax Deduction ####
       
       db_neat <- db_raw %>%
              filter(Section %in% "5a(i): Regulatory Tax Allowance",
                     Category %in% "(tax deductions)",
                     !Description %in% "(tax deductions)") %>%
              
              select(EDB,
                     Network,
                     Year,
                     Description,
                     Value) %>%
              
              group_by(EDB,Network,Year) %>% 
              mutate(rg_tax_ded = sum(Value)) %>%
              ungroup() %>%
              select(EDB,Network,Year,rg_tax_ded) %>%
              distinct() %>%
              left_join(db_neat,.,by = NULL)
       
       ### 5.4.1.X. Operating Surplus ####
       
       db_neat <- db_raw %>%
              filter(Section %in% "3(i): Regulatory Profit",
                     Description %in% "Operating surplus / (deficit)") %>%
              
              select(EDB,
                     Network,
                     Year,
                     rg_ope_surp = Value) %>%
              
              left_join(db_neat,.,by = NULL)
       
       ### 5.4.1.X. Depreciation ####
       
       db_neat <- db_raw %>%
              filter(
                     Section %in% "3(i): Regulatory Profit",
                     Description %in% "Total depreciation") %>%
              select(EDB,
                     Network,
                     Year,
                     rg_depr = Value) %>%
              
              left_join(db_neat,.,by = NULL)
       
       ### 5.4.1.X. Depreciation -  Distribution and LV lines####
       
       db_neat <- db_raw %>%
              filter(
                     Section %in% "4(vii): Disclosure by Asset Category",
                     Subcategory %in% "Total depreciation",
                     Description %in% "Distribution and LV lines") %>%
              
              select(EDB,
                     Network,
                     Year,
                     rg_a_dep_li_lv = Value) %>%
              
              left_join(db_neat,.,by = NULL)
       
       ### 5.4.1.X. Depreciation - Distribution and LV cables ####

       
       db_neat <- db_raw %>%
              filter(
                     Section %in% "4(vii): Disclosure by Asset Category",
                     Subcategory %in% "Total depreciation",
                     Description %in% "Distribution and LV cables") %>%
              select(EDB,
                     Network,
                     Year,
                     rg_a_dep_cb_lv = Value) %>%
              
              left_join(db_neat,.,by = NULL)

       ### 5.4.1.X. Depreciation - Distribution substations and transformers ####
       
       db_neat <- db_raw %>%
              filter(
                     Section %in% "4(vii): Disclosure by Asset Category",
                     Subcategory %in% "Total depreciation",
                     Description %in% "Distribution substations and transformers") %>%
              select(EDB,
                     Network,
                     Year,
                     rg_a_dep_sb_tr = Value) %>%
              
              left_join(db_neat,.,by = NULL)

       ### 5.4.1.X. Depreciation - Distribution switchgear ####
       
       db_neat <- db_raw %>%
              filter(
                     Section %in% "4(vii): Disclosure by Asset Category",
                     Subcategory %in% "Total depreciation",
                     Description %in% "Distribution switchgear") %>%
              select(EDB,
                     Network,
                     Year,
                     rg_a_dep_swi = Value) %>%
              
              left_join(db_neat,.,by = NULL)

       ### 5.4.1.X. Depreciation - Subtransmission lines ####
       
       db_neat <- db_raw %>%
              filter(
                     Section %in% "4(vii): Disclosure by Asset Category",
                     Subcategory %in% "Total depreciation",
                     Description %in% "Subtransmission lines") %>%
              select(EDB,
                     Network,
                     Year,
                     rg_a_dep_li_sb = Value) %>%
              
              left_join(db_neat,.,by = NULL)

       ### 5.4.1.X. Depreciation - Subtransmission cables ####
       

       db_neat <- db_raw %>%
              filter(
                     Section %in% "4(vii): Disclosure by Asset Category",
                     Subcategory %in% "Total depreciation",
                     Description %in% "Subtransmission cables") %>%
              select(EDB,
                     Network,
                     Year,
                     rg_a_dep_cb_sb = Value) %>%
              
              left_join(db_neat,.,by = NULL)

       ### 5.4.1.X. Depreciation - Non-network assets ####
       
       db_neat <- db_raw %>%
              filter(
                     Section %in% "4(vii): Disclosure by Asset Category",
                     Subcategory %in% "Total depreciation",
                     Description %in% "Non-network assets") %>%
              select(EDB,
                     Network,
                     Year,
                     rg_a_dep_non_net = Value) %>%
              
              left_join(db_neat,.,by = NULL)

       ### 5.4.1.X. Depreciation - Other network assets ####
       
       db_neat <- db_raw %>%
              filter(
                     Section %in% "4(vii): Disclosure by Asset Category",
                     Subcategory %in% "Total depreciation",
                     Description %in% "Other network assets") %>%
              select(EDB,
                     Network,
                     Year,
                     rg_a_dep_net_ot = Value) %>%
              
              left_join(db_neat,.,by = NULL)

       ### 5.4.1.X. Depreciation - Zone substations ####
       
       db_neat <- db_raw %>%
              filter(
                     Section %in% "4(vii): Disclosure by Asset Category",
                     Subcategory %in% "Total depreciation",
                     Description %in% "Zone substations") %>%
              select(EDB,
                     Network,
                     Year,
                     rg_a_dep_z_sub = Value) %>%
              
              left_join(db_neat,.,by = NULL)

       ### 5.4.1.X. Depreciation - Total ####
       
       db_neat <- db_raw %>%
              filter(
                     Section %in% "4(vii): Disclosure by Asset Category",
                     Subcategory %in% "Total depreciation",
                     Description %in% "Total") %>%
              select(EDB,
                     Network,
                     Year,
                     rg_a_dep_tot = Value) %>%
              
              left_join(db_neat,.,by = NULL)

       ### 5.4.1.X. Revaluation ####
       
       db_neat <- db_raw %>%
              filter(
                     Section %in% "3(i): Regulatory Profit",
                     Description %in% c("Total revaluation",                                                        #
                                        "Total revaluations")) %>%
              select(EDB,
                     Network,
                     Year,
                     rg_reval = Value) %>%
              
              left_join(db_neat,.,by = NULL)

       ### 5.4.1.X. Revaluation - Distribution and LV lines ####

       db_neat <- db_raw %>%                                                                  #
              filter(                                                   #
                     Section %in% "4(vii): Disclosure by Asset Category",                        #
                     Subcategory %in% "Total revaluations",                                      # Selecting and 
                     Description %in% "Distribution and LV lines") %>%                           # Filtering 
              select(EDB,
                     Network,
                     Year,                                                                # variable 1
                     rg_a_rev_li_lv = Value) %>%
              
              left_join(db_neat,.,by = NULL)

       ### 5.4.1.X. Revaluation - Distribution and LV cables ####
       
       db_neat <- db_raw %>%                                                                                #
              filter(                                                   #
                     Section %in% "4(vii): Disclosure by Asset Category",                        #
                     Subcategory %in% "Total revaluations",                                      # Selecting and 
                     Description %in% "Distribution and LV cables") %>%                          # Filtering 
              select(EDB,
                     Network,
                     Year,
                     rg_a_rev_cb_lv = Value) %>%
              
              left_join(db_neat,.,by = NULL)

       ### 5.4.1.X. Revaluation - Distribution substations and transformers ####
       
       db_neat <- db_raw %>%                                                                                #
              filter(                                                   #
                     Section %in% "4(vii): Disclosure by Asset Category",                        #
                     Subcategory %in% "Total revaluations",                                      # Selecting and 
                     Description %in% "Distribution substations and transformers") %>%           # Filtering 
              select(EDB,
                     Network,
                     Year,
                     rg_a_rev_sb_tr = Value) %>%
              
              left_join(db_neat,.,by = NULL)
       
       ### 5.4.1.X. Revaluation - Distribution switchgear ####
       
       db_neat <- db_raw %>%                                                                                #
              filter(                                                   #
                     Section %in% "4(vii): Disclosure by Asset Category",                        #
                     Subcategory %in% "Total revaluations",                                      # Selecting and 
                     Description %in% "Distribution switchgear") %>%                             # Filtering 
              select(EDB,
                     Network,
                     Year,
                     rg_a_rev_swi = Value) %>%
              
              left_join(db_neat,.,by = NULL)

       ### 5.4.1.X. Revaluation - Subtransmission lines ####

       db_neat <- db_raw %>%                                                                                #
              filter(                                                   #
                     Section %in% "4(vii): Disclosure by Asset Category",                        #
                     Subcategory %in% "Total revaluations",                                      # Selecting and 
                     Description %in% "Subtransmission lines") %>%                               # Filtering 
              select(EDB,
                     Network,
                     Year,
                     rg_a_rev_li_sb = Value) %>%
              
              left_join(db_neat,.,by = NULL)

       ### 5.4.1.X. Revaluation - Subtransmission cables ####
       
       db_neat <- db_raw %>%                                                                                #
              filter(                                                   #
                     Section %in% "4(vii): Disclosure by Asset Category",                        #
                     Subcategory %in% "Total revaluations",                                      # Selecting and 
                     Description %in% "Subtransmission cables") %>%                              # Filtering 
              select(EDB,
                     Network,
                     Year,
                     rg_a_rev_cb_sb = Value) %>%
              
              left_join(db_neat,.,by = NULL)

       ### 5.4.1.X. Revaluation - Non-network assets ####
       
       db_neat <- db_raw %>%                                                                                #
              filter(                                                   #
                     Section %in% "4(vii): Disclosure by Asset Category",                        #
                     Subcategory %in% "Total revaluations",                                      # Selecting and 
                     Description %in% "Non-network assets") %>%                                  # Filtering 
              select(EDB,
                     Network,
                     Year,
                     rg_a_rev_non_net = Value) %>%
              
              left_join(db_neat,.,by = NULL)

       ### 5.4.1.X. Revaluation - Other network assets ####
       
       db_neat <- db_raw %>%                                                                                #
              filter(                                                   #
                     Section %in% "4(vii): Disclosure by Asset Category",                        #
                     Subcategory %in% "Total revaluations",                                      # Selecting and 
                     Description %in% "Other network assets") %>%                                # Filtering 
              select(EDB,
                     Network,
                     Year,
                     rg_a_rev_net_ot = Value) %>%
              
              left_join(db_neat,.,by = NULL)

       ### 5.4.1.X. Revaluation - Zone substations ####
       
       db_neat <- db_raw %>%                                                                                #
              filter(                                                   #
                     Section %in% "4(vii): Disclosure by Asset Category",                        #
                     Subcategory %in% "Total revaluations",                                      # Selecting and 
                     Description %in% "Zone substations") %>%                                    # Filtering 
              select(EDB,
                     Network,
                     Year,
                     rg_a_rev_z_sub = Value) %>%
              
              left_join(db_neat,.,by = NULL)

       ### 5.4.1.X. Revaluation - Total ####
       
       db_neat <- db_raw %>%
              filter(
                     Section %in% "4(vii): Disclosure by Asset Category",
                     Subcategory %in% "Total revaluations",
                     Description %in% "Total") %>%
              select(EDB,
                     Network,
                     Year,
                     rg_a_rev_tot = Value) %>%
              
              left_join(db_neat,.,by = NULL)

       ### 5.4.1.X. Pass-through and recoverable costs excluding financial incentives and wash-ups ####
       
       db_neat <- db_raw %>%
              filter(Section %in% "3(i): Regulatory Profit",
                     Description %in% c("Pass-through and recoverable costs",
                                        "Pass-through and recoverable costs excluding financial incentives and wash-ups")) %>%
              select(EDB,
                     Network,
                     Year,
                     rg_pass_recover = Value) %>%
              
              left_join(db_neat,.,by = NULL)

       ### 5.4.1.X. Total regulatory income ####
       
       db_neat <- db_raw %>%
              filter(Section %in% "3(i): Regulatory Profit",
                     Description %in% "Total regulatory income") %>%
              select(EDB,
                     Network,
                     Year,
                     rg_inc_tot = Value) %>%
              
              left_join(db_neat,.,by = NULL)
       
       ### 5.4.1.X. Recoverable costs excluding financial incentives and wash-ups ####
       
       db_neat <- db_raw %>%
              filter(Section %in% c("3(ii): Pass-Through and Recoverable Costs",
                                    "3(ii): Pass-through and Recoverable Costs excluding Financial Incentives and Wash-Ups"),
                     Category %in% c("Recoverable costs",
                                     "Recoverable costs excluding financial incentives and wash-ups")) %>%
                     select(EDB,
                            Network,
                            Year,
                            Description,
                            Value) %>% 
                            group_by(EDB,
                                     Network,
                                     Year) %>% 
                                   mutate(rg_rec_cost = sum(Value)) %>%
                                          ungroup() %>% 
                                                 select(-Description,-Value) %>% 
                                                        distinct() %>%

              left_join(db_neat,.,by = NULL)

       ### 5.4.1.X. Electricity lines service charge payable to Transpower ####
       
       db_neat <- db_raw %>%
              filter(Section %in% c("3(ii): Pass-Through and Recoverable Costs",
                                    "3(ii): Pass-through and Recoverable Costs excluding Financial Incentives and Wash-Ups"),
                     Category %in% c("Recoverable costs",
                                     "Recoverable costs excluding financial incentives and wash-ups"),
                     Description %in% c("Non-exempt EDB electricity lines service charge payable to Transpower",
                                        "Electricity lines service charge payable to Transpower")) %>%
              select(EDB,
                     Network,
                     Year,
                     Description,
                     Value) %>% 
              group_by(EDB,
                       Network,
                       Year) %>% 
              mutate(rg_char_tp = sum(Value)) %>%
              ungroup() %>% 
              select(-Description,-Value) %>% 
              distinct() %>%
              
              left_join(db_neat,.,by = NULL)

       ### 5.4.1.X. Transpower new investment contract charges ####
       
       db_neat <- db_raw %>%
              filter(Section %in% c("3(ii): Pass-Through and Recoverable Costs",
                                    "3(ii): Pass-through and Recoverable Costs excluding Financial Incentives and Wash-Ups"),
                     Description %in% "Transpower new investment contract charges") %>%
              select(EDB,
                     Network,
                     Year,
                     Description,
                     Value) %>% 
              group_by(EDB,
                       Network,
                       Year) %>% 
              mutate(rg_inv_tp = sum(Value)) %>%
              ungroup() %>% 
              select(-Description,-Value) %>% 
              distinct() %>%
              
              left_join(db_neat,.,by = NULL)
       
       ### 5.4.1.X. Electricity lines service charge payable to Transpower ####
       
       db_neat <- db_raw %>%
              filter(Section %in% c("3(ii): Pass-Through and Recoverable Costs",
                                    "3(ii): Pass-through and Recoverable Costs excluding Financial Incentives and Wash-Ups"),
                     Category %in% c("Recoverable costs",
                                     "Recoverable costs excluding financial incentives and wash-ups"),
                     Description %in% c("Avoided transmission charge",                                                 #
                                        "Distributed generation allowance")) %>%
              select(EDB,
                     Network,
                     Year,
                     Description,
                     Value) %>% 
              group_by(EDB,
                       Network,
                       Year) %>% 
              mutate(rg_dg_allo = sum(Value)) %>%
              ungroup() %>% 
              select(-Description,-Value) %>% 
              distinct() %>%
              
              left_join(db_neat,.,by = NULL)
       
       ### 5.4.1.X. Other recoverable costs excluding financial incentives and wash-ups ####
       
       db_neat <- db_raw %>%
              filter(Section %in% c("3(ii): Pass-Through and Recoverable Costs",
                                    "3(ii): Pass-through and Recoverable Costs excluding Financial Incentives and Wash-Ups"),
                     Description %in% c("Other specified pass-through costs",                                          #
                                        "Other recoverable costs excluding financial incentives and wash-ups")) %>%
              select(EDB,
                     Network,
                     Year,
                     Description,
                     Value) %>% 
              group_by(EDB,
                       Network,
                       Year) %>% 
              mutate(rg_rec_oth = sum(Value)) %>%
              ungroup() %>% 
              select(-Description,-Value) %>% 
              distinct() %>%
              
              left_join(db_neat,.,by = NULL)
       
       ### 5.4.1.X. System operator services ####
       
       db_neat <- db_raw %>%
              filter(Section %in% c("3(ii): Pass-Through and Recoverable Costs",
                                    "3(ii): Pass-through and Recoverable Costs excluding Financial Incentives and Wash-Ups"),
                     Description %in% "System operator services") %>%
              select(EDB,
                     Network,
                     Year,
                     Description,
                     Value) %>% 
              group_by(EDB,
                       Network,
                       Year) %>% 
              mutate(rg_sys_op = sum(Value)) %>%
              ungroup() %>% 
              select(-Description,-Value) %>% 
              distinct() %>%
              
              left_join(db_neat,.,by = NULL)

       ### 5.4.1.X. Extended reserves allowance ####
       
       db_neat <- db_raw %>%
              filter(Section %in% c("3(ii): Pass-Through and Recoverable Costs",
                                    "3(ii): Pass-through and Recoverable Costs excluding Financial Incentives and Wash-Ups"),
                     Description %in% c("Extended reserves allowance",
                                        "Recoverable customised price-quality path costs")) %>%
              select(EDB,
                     Network,
                     Year,
                     Description,
                     Value) %>% 
              group_by(EDB,
                       Network,
                       Year) %>% 
              mutate(rg_res_allo = sum(Value)) %>%
              ungroup() %>% 
              select(-Description,-Value) %>% 
              distinct() %>%
              
              left_join(db_neat,.,by = NULL)

       ### 5.4.1.X. Recoverable costs ####
       
       db_neat <- db_raw %>% filter(Section %in% c("3(ii): Pass-Through and Recoverable Costs",
                                        "3(ii): Pass-through and Recoverable Costs excluding Financial Incentives and Wash-Ups"),
                         Category %in% c("Recoverable costs",
                                         "Recoverable costs excluding financial incentives and wash-ups")) %>%
                     
                     select(EDB,
                            Network,
                            Year,
                            Description,
                            Value) %>% 
                            
                            group_by(EDB,Network,Year) %>% 
                                   
                                   mutate(rg_rec_costs_total = sum(Value)) %>% ungroup() %>%
              
                     select(-Description,-Value) %>% 
                            
                            distinct() %>%
              
                                   left_join(db_neat,.,by = NULL)
       
       
       ### 5.4.1.X. Pass-through costs ####

       db_neat <- db_raw %>%
              filter(Section %in% c("3(ii): Pass-Through and Recoverable Costs",
                                    "3(ii): Pass-through and Recoverable Costs excluding Financial Incentives and Wash-Ups"),
                     Category %in% "Pass-through costs") %>%
              select(EDB,
                     Network,
                     Year,
                     Description,
                     Value) %>% 
              group_by(EDB,
                       Network,
                       Year) %>% 
              mutate(rg_pass_cost = sum(Value)) %>%
              ungroup() %>% 
              select(-Description,-Value) %>% 
              distinct() %>%
              
              left_join(db_neat,.,by = NULL)
       
       ### 5.4.1.X. Pass-through costs - Rates ####
       
       db_neat <- db_raw %>% filter(Section %in% c("3(ii): Pass-Through and Recoverable Costs",
                                                   "3(ii): Pass-through and Recoverable Costs excluding Financial Incentives and Wash-Ups"),
                         Category %in% "Pass-through costs",
                         Description %in% "Rates") %>%
                     select(EDB,
                            Network,
                            Year,
                            rg_pass_rates = Value) %>%
              
              left_join(db_neat,.,by = NULL)
       
       
       ### 5.4.1.X. Pass-through costs - Electricity Authority levies ####
       
       db_neat <- db_raw %>% filter(Section %in% c("3(ii): Pass-Through and Recoverable Costs",
                                                   "3(ii): Pass-through and Recoverable Costs excluding Financial Incentives and Wash-Ups"),
                                    Category %in% "Pass-through costs",
                                    Description %in% c("Electricity Authority levies",
                                                       "Industry levies")) %>%
              select(EDB,
                     Network,
                     Year,
                     rg_pass_ea_lev = Value) %>%
              
              left_join(db_neat,.,by = NULL)
       
       
       
       
       ### 5.4.1.X. Pass-through costs - Commerce Act levies ####
       
       db_neat <- db_raw %>% filter(Section %in% c("3(ii): Pass-Through and Recoverable Costs",
                                                   "3(ii): Pass-through and Recoverable Costs excluding Financial Incentives and Wash-Ups"),
                                    Category %in% "Pass-through costs",
                                    Description %in% "Commerce Act levies") %>%
              select(EDB,
                     Network,
                     Year,
                     rg_pass_comcom_lev = Value) %>%
              
              left_join(db_neat,.,by = NULL)
       
       
       
       ### 5.4.1.X. Pass-through costs - Other specified pass-through costs ####

       db_neat <- db_raw %>% filter(Section %in% c("3(ii): Pass-Through and Recoverable Costs",
                                                   "3(ii): Pass-through and Recoverable Costs excluding Financial Incentives and Wash-Ups"),
                                    Category %in% "Pass-through costs",
                                    Description %in% c("Other specified pass-through costs",
                                                       "CPP specified pass through costs")) %>%
              select(EDB,
                     Network,
                     Year,
                     rg_pass_oth = Value) %>%
              
              left_join(db_neat,.,by = NULL)
       
       
       
       ### 5.4.1.X. Revenue - Line Charge ####
       
       db_neat <- db_raw %>%
              filter(Section %in% "3(i): Regulatory Profit",
                     Description %in% "Line charge revenue") %>%
                     select(EDB,
                            Network,
                            Year,
                            rg_rev_line_char = Value) %>%
                     
                                   left_join(db_neat,.,by = NULL)
              
       ### 5.4.1.X. Revenue - Other regulated income (other than gains / (losses) on asset disposals) ####
       
       db_neat <- db_raw %>%                                                                                       #
              filter(                                                                        # Creating table
                     Section %in% "3(i): Regulatory Profit",                                                          #
                     Description %in% "Other regulated income (other than gains / (losses) on asset disposals)") %>%  #
                     select(EDB,
                            Network,
                            Year,
                            rg_rev_oth_inc = Value) %>%
              
                            left_join(db_neat,.,by = NULL)
       
       ### 5.4.1.X. Revenue - Gains / (losses) on asset disposals ####
       
       db_neat <- db_raw %>%                                                                                       #
              filter(                                                                        # Creating table
                     Section %in% "3(i): Regulatory Profit",                                                          #
                     Description %in% "Gains / (losses) on asset disposals") %>%  #
              select(EDB,
                     Network,
                     Year,
                     rg_rev_a_disp = Value) %>%
              
              left_join(db_neat,.,by = NULL)
       
       
       
       
       
       
       ## 5.5. OPEX ####
       
       ### 5.4.1.X. OPEX - Total ####
       
       db_neat <- db_raw %>%
              filter(Section %in% "3(i): Regulatory Profit",
                     Description %in% "Operational expenditure") %>%
              select(EDB,
                     Network,
                     Year,
                     r_opex = Value) %>%
              
              left_join(db_neat,.,by = NULL)
       
       ### 5.4.1.X. OPEX - Network ####
       
       db_neat <- db_raw %>%                                                                      #
              filter(                                                   #
                     Section %in% "6b(i): Operational Expenditure",                                    # Selecting and
                     Category %in% "Network opex",        # Filtering 
                     Description %in% "Network opex") %>%            #
              select(EDB,
                     Network,
                     Year,                                                                # Variable 1 and 2
                     rg_opex_net = Value) %>%
              
              left_join(db_neat,.,by = NULL)
       
       ### 5.4.1.X. OPEX - Network - Service interruptions and emergencies ####
       
       db_neat <- db_raw %>%                                                           #
              filter(Section %in% "6b(i): Operational Expenditure",                   # Selecting and
                     Category %in% "Network opex",                                    # Filtering 
                     Description %in% "Service interruptions and emergencies") %>%    # Variable 1 and 2
              select(EDB,
                     Network,
                     Year,                                                                # Variable 1 and 2
                     rg_opex_serv_int = Value) %>%
              
              left_join(db_neat,.,by = NULL)
       
       ### 5.4.1.X. OPEX - Network - Vegetation management ####

       db_neat <- db_raw %>%                                                                     #
              filter(Section %in% "6b(i): Operational Expenditure",                   # Selecting and
                     Category %in% "Network opex",                                    # Filtering
                     Description %in% "Vegetation management") %>%                    # Variable 3 
              select(EDB,
                     Network,
                     Year,                                                                # Variable 1 and 2
                     rg_opex_veg_mang = Value) %>%
              
              left_join(db_neat,.,by = NULL)
       
       ### 5.4.1.X. OPEX - Network - Routine and corrective maintenance and inspection ####
       
       db_neat <- db_raw %>%                                                                                #
              filter(Section %in% "6b(i): Operational Expenditure",                              # Selecting and
                     Category %in% "Network opex",                                               # Filtering
                     Description %in% "Routine and corrective maintenance and inspection") %>%  # Variable 4
              select(EDB,
                     Network,
                     Year,                                                                # Variable 1 and 2
                     rg_opex_rou_correc = Value) %>%
              
              left_join(db_neat,.,by = NULL)
       
       ### 5.4.1.X. OPEX - Network - Asset replacement and renewal ####
       
       db_neat <- db_raw %>%                                                                     #
              filter(Section %in% "6b(i): Operational Expenditure",                   # Selecting and
                     Category %in% "Network opex",                                    # Filtering
                     Description %in% "Asset replacement and renewal") %>%            # Variable 5
              select(EDB,
                     Network,
                     Year,                                                                # Variable 1 and 2
                     rg_opex_a_rep_ren = Value) %>%
              
              left_join(db_neat,.,by = NULL)
       
       ### 5.4.1.X. OPEX - Non-network ####
       
       db_neat <- db_raw %>%                                                                                #
              filter(                                                   #
                     Section %in% "6b(i): Operational Expenditure",                                # Selecting and
                     Category %in% "Non-network opex",
                     Description %in% "Non-network opex") %>%                      # Filtering 
              select(EDB,
                     Network,
                     Year,                                                                # Variable 1 and 2
                     rg_opex_n_net = Value) %>%
              
              left_join(db_neat,.,by = NULL)
       
       ### 5.4.1.X. OPEX - Non-network - System operations and network support ####
       
       
       db_neat <- db_raw %>%                                                                      #
              filter(Section %in% "6b(i): Operational Expenditure",                              # Selecting and
                     Category %in% "Non-network opex",                                           # Filtering 
                     Description %in% "System operations and network support") %>%               #
              select(EDB,
                     Network,
                     Year,                                                                # Variable 1 and 2
                     rg_opex_n_net_sys_op = Value) %>%
              
              left_join(db_neat,.,by = NULL)
       
       ### 5.4.1.X. OPEX - Non-network - Business support ####
       
       
       
       db_neat <- db_raw %>%                                                                                #
              filter(Section %in% "6b(i): Operational Expenditure",                                # Selecting and
                     Category %in% "Non-network opex",
                     Description %in% "Business support") %>%                      # Filtering 
              select(EDB,
                     Network,
                     Year,                                                                # Variable 1 and 2
                     rg_opex_n_net_bus_sup = Value) %>%
              
              left_join(db_neat,.,by = NULL)

       
       
       
       ### 5.4.1.X. OPEX TOTAL ####
       
       db_neat <- db_raw %>%                                                                                #
              filter(Section %in% "6b(i): Operational Expenditure",                                # Selecting and
                     Category %in% "Operational expenditure",
                     Description %in% "Operational expenditure") %>%
              select(EDB,
                     Network,
                     Year,                                                                # Variable 1 and 2
                     rg_opex_tot = Value) %>%
              
              left_join(db_neat,.,by = NULL)

       ### 5.4.1.X. OPEX - Subcomponents - Energy efficiency and demand side management, reduction of energy losses ####
       
       db_neat <- db_raw %>%                                                                         #
              filter(Section %in% "6b(ii): Subcomponents of Operational Expenditure (where known)", # Selecting and
                     Category %in% "Expenditure on subcomponents",                                  # Filtering 
                     Description %in% "Energy efficiency and demand side management, reduction of energy losses") %>%  #
              select(EDB,
                     Network,
                     Year,                                                                # Variable 1 and 2
                     rg_opex_subcomp_enr_effi = Value)%>%
              
              left_join(db_neat,.,by = NULL)
       
       ### 5.4.1.X. OPEX - Subcomponents - Direct billing* ####
       
       db_neat <- db_raw %>%                                                                                #
              filter(Section %in% "6b(ii): Subcomponents of Operational Expenditure (where known)", # Selecting and
                     Category %in% "Expenditure on subcomponents",                               #
                     Description %in% "Direct billing*") %>%                                     # Filtering 
              select(EDB,
                     Network,
                     Year,                                                                # Variable 1 and 2
                     rg_opex_subcomp_direc_bill = Value)%>%
              
              left_join(db_neat,.,by = NULL)
       
       ### 5.4.1.X. OPEX - Subcomponents - Research and development ####
       
       db_neat <- db_raw %>%                                                                                   #
              filter(Section %in% "6b(ii): Subcomponents of Operational Expenditure (where known)", # Selecting and
                     Category %in% "Expenditure on subcomponents",                                  #
                     Description %in% "Research and development") %>%                               # Filtering 
              select(EDB,
                     Network,
                     Year,                                                                # Variable 1 and 2
                     rg_opex_subcomp_red = Value)%>%
              
              left_join(db_neat,.,by = NULL)
       
       ### 5.4.1.X. OPEX - Subcomponents - Insurance ####
       
       db_neat <- db_raw %>%                                                                                   #
              filter(Section %in% "6b(ii): Subcomponents of Operational Expenditure (where known)", # Selecting and
                     Category %in% "Expenditure on subcomponents",                                  # 
                     Description %in% "Insurance") %>%                                              # Filtering 
              select(EDB,
                     Network,
                     Year,                                                                # Variable 1 and 2
                     rg_opex_subcomp_insur = Value)%>%
              
              left_join(db_neat,.,by = NULL)
       
       ### 5.4.1.X. OPEX - Subcomponents - TOTAL ####
       
       
       db_neat <- mutate(db_neat,rg_opex_subcomp_tot =  rg_opex_subcomp_enr_effi +
                                                        rg_opex_subcomp_direc_bill +
                                                        rg_opex_subcomp_red +
                                                        rg_opex_subcomp_insur)
       
       
       
########################################################################################################################

       ## 5.6. CAPEX - Cost of financing ####
       
       db_neat <- db_raw %>%                                                                      #
              filter(                                                   #
                     Section %in% "6a(i): Expenditure on Assets",                                # Selecting and
                     Description %in% "Cost of financing") %>%                                   # Filtering 
              select(EDB,
                     Network,
                     Year,                                                                # Variable 1 and 2
                     rg_capex_cos_fin = Value) %>%
              
              left_join(db_neat,.,by = NULL)
       
       ## 5.6. CAPEX - Value of capital contributions ####
       
       db_neat <- db_raw %>%                                                                                #
              filter(                                                   #
                     Section %in% "6a(i): Expenditure on Assets",                                # Selecting and
                     Description %in% "Value of capital contributions") %>%                      # Filtering 
              select(EDB,
                     Network,
                     Year,                                                                # Variable 1 and 2
                     rg_capex_cap_cont = Value) %>%
              
              left_join(db_neat,.,by = NULL)
       
       ## 5.6. CAPEX - Value of vested assets ####
       
       db_neat <- db_raw %>%                                                                                #
              filter(                                                   #
                     Section %in% "6a(i): Expenditure on Assets",                                # Selecting and
                     Description %in% "Value of vested assets") %>%                              # Filtering 
              select(EDB,
                     Network,
                     Year,                                                                # Variable 1 and 2
                     rg_capex_a_vest = Value) %>%
              
              left_join(db_neat,.,by = NULL)

       ## 5.6. CAPEX - Expenditure on assets ####
       
       db_neat <- db_raw %>%                                                                                #
              filter(                                                   #
                     Section %in% "6a(i): Expenditure on Assets",                                # Selecting and
                     Category %in% "Expenditure on assets",                                      # Filtering
                     Description %in% "Expenditure on assets") %>%                               # Variable 5
              select(EDB,
                     Network,
                     Year,                                                                # Variable 1 and 2
                     rg_capex_a_expent = Value) %>%
              
              left_join(db_neat,.,by = NULL)
       
       ## 5.6. CAPEX TOTAL ####
       
       db_neat <- db_raw %>%                                                                                #
              filter(                                                   #
                     Section %in% "6a(i): Expenditure on Assets",                                # Selecting and
                     Description %in% "Capital expenditure") %>%                                 # Filtering
              select(EDB,
                     Network,
                     Year,                                                                # Variable 1 and 2
                     rg_capex_total = Value) %>%
              
              left_join(db_neat,.,by = NULL)
       
       ## 5.6. CAPEX - Network assets ####
       
       db_neat <- db_raw %>%                                                                      #
              filter(                                                   #
                     Section %in% "6a(i): Expenditure on Assets",                                # Selecting and
                     Description %in% "Network assets") %>%                                      # Filtering 
              select(EDB,
                     Network,
                     Year,                                                                # Variable 1 and 2
                     rg_capex_a_net = Value) %>%
              
              left_join(db_neat,.,by = NULL)
       
       ## 5.6. CAPEX - Expenditure on non-network assets ####
       
       db_neat <- db_raw %>%                                                                                #
              filter(                                                   #
                     Section %in% "6a(i): Expenditure on Assets",                                # Selecting and
                     Description %in% "Expenditure on non-network assets") %>%                      # Filtering 
              select(EDB,
                     Network,
                     Year,                                                                # Variable 1 and 2
                     rg_capex_n_net = Value) %>%
              
              left_join(db_neat,.,by = NULL)
       
       ## 5.6. CAPEX - Expenditure on assets ####
       
       db_neat <- db_raw %>%                                                                                #
              filter(                                                   #
                     Section %in% "6a(i): Expenditure on Assets",                                # Selecting and
                     Description %in% "Expenditure on assets") %>%                               # Filtering 4
              select(EDB,
                     Network,
                     Year,                                                                # Variable 1 and 2
                     rg_capex_a = Value) %>%
              
              left_join(db_neat,.,by = NULL)
       
       ## 5.6. CAPEX - Expenditure on assets - Total reliability, safety and environment####
       
       db_neat <- db_raw %>%                                                                      #
              filter(                                                   #
                     Section %in% "6a(i): Expenditure on Assets",                                #
                     Subcategory %in% "Reliability, safety and environment",                     # Selecting and 
                     Description %in% "Total reliability, safety and environment") %>%           # Filtering 
              select(EDB,
                     Network,
                     Year,                                                                # Variable 1 and 2
                     rg_capex_a_rse = Value) %>%
              
              left_join(db_neat,.,by = NULL)

       ## 5.6. CAPEX - Expenditure on assets - Consumer connection ####
       
       db_neat <- db_raw %>%                                                                                # Selecting and
              filter(                                                   # Filtering
                     Section %in% "6a(i): Expenditure on Assets",                                # Variable 3
                     Subcategory %in% "Consumer connection") %>%
              select(EDB,
                     Network,
                     Year,                                                                # Variable 1 and 2
                     rg_capex_a_cc = Value) %>%
              
              left_join(db_neat,.,by = NULL)

       ## 5.6. CAPEX - Expenditure on assets - System growth ####

       db_neat <- db_raw %>%                                                                                # Selecting and
              filter(                                                   # Filtering
                     Section %in% "6a(i): Expenditure on Assets",                                # Variable 4
                     Subcategory %in% "System growth") %>%                                       # 
              select(EDB,
                     Network,
                     Year,                                                                # Variable 1 and 2
                     rg_capex_a_sys = Value) %>%
              
              left_join(db_neat,.,by = NULL)
       
       ## 5.6. CAPEX - Expenditure on assets - Asset replacement and renewal ####
       
       db_neat <- db_raw %>%                                                                                # Selecting and
              filter(                                                   # Filtering
                     Section %in% "6a(i): Expenditure on Assets",                                # Variable 5
                     Subcategory %in% "Asset replacement and renewal") %>%                       # 
              select(EDB,
                     Network,
                     Year,                                                                # Variable 1 and 2
                     rg_capex_a_repla = Value) %>%
              
              left_join(db_neat,.,by = NULL)
       
       ## 5.6. CAPEX - Expenditure on assets - Asset relocations ####

       db_neat <- db_raw %>%                                                                                # Selecting and
              filter(Section %in% "6a(i): Expenditure on Assets",                                # Variable 6
                     Subcategory %in% "Asset relocations") %>%                                   # 
              select(EDB,
                     Network,
                     Year,                                                                # Variable 1 and 2
                     rg_capex_a_reloc = Value) %>%
              
              left_join(db_neat,.,by = NULL)
       
       
       ## 5.6. CAPEX - Expenditure on assets - Quality of supply ####
       
       db_neat <- db_raw %>%                                                                  #
              filter(Section %in% "6a(i): Expenditure on Assets",                                #
                     Subcategory %in% "Reliability, safety and environment",                     # Selecting and 
                     Description %in% "Quality of supply") %>%                                   # Filtering 
              select(EDB,
                     Network,
                     Year,                                                                # Variable 1 and 2
                     rg_capex_rse_qual_sup = Value) %>%
              
              left_join(db_neat,.,by = NULL)
       
       ## 5.6. CAPEX - Expenditure on assets - Legislative and regulatory ####
       
       db_neat <- db_raw %>%                                                                                #
              filter(Section %in% "6a(i): Expenditure on Assets",                                #
                     Subcategory %in% "Reliability, safety and environment",                     # Selecting and 
                     Description %in% "Legislative and regulatory") %>%                          # Filtering 
              select(EDB,
                     Network,
                     Year,                                                                # Variable 1 and 2
                     rg_capex_rse_leg_reg = Value) %>%
              
              left_join(db_neat,.,by = NULL)
       
       ## 5.6. CAPEX - Expenditure on assets - Other reliability, safety and environment ####
       
       db_neat <- db_raw %>%                                                                                #
              filter(Section %in% "6a(i): Expenditure on Assets",                                #
                     Subcategory %in% "Reliability, safety and environment",                     # Selecting and 
                     Description %in% "Other reliability, safety and environment") %>%           # Filtering 
              select(EDB,
                     Network,
                     Year,                                                                # Variable 1 and 2
                     rg_capex_rse_oth = Value) %>%
              
              left_join(db_neat,.,by = NULL)
       
       ## 5.6. CAPEX - Expenditure on assets - Total reliability, safety and environment ####
       
       db_neat <- db_raw %>%                                                                                #
              filter(Section %in% "6a(i): Expenditure on Assets",                                #
                     Subcategory %in% "Reliability, safety and environment",                     # Selecting and 
                     Description %in% "Total reliability, safety and environment") %>%           # Filtering 
              select(EDB,
                     Network,
                     Year,                                                                # Variable 1 and 2
                     rg_capex_rse_tot = Value) %>%
              
              left_join(db_neat,.,by = NULL)
       
       ## 5.6. CAPEX - Expenditure on assets - Quality of supply less capital contributions ####
       
       db_neat <- db_raw %>%                                                                      #
              filter(Section %in% "6a(vi): Quality of Supply",                                    # Selecting and
                     Subcategory %in% "Quality of supply less capital contributions",        # Filtering 
                     Description %in% "Quality of supply less capital contributions") %>%            #
              select(EDB,
                     Network,
                     Year,                                                                # Variable 1 and 2
                     rg_capex_qual_less_cc = Value) %>%
              
              left_join(db_neat,.,by = NULL)
       
       ## 5.6. CAPEX - Expenditure on assets - Capital contributions funding quality of supply ####

       db_neat <- db_raw %>%                                                                                #
              filter(Section %in% "6a(vi): Quality of Supply",                                # Selecting and
                     Subcategory %in% "Quality of supply less capital contributions",
                     Description %in% "Capital contributions funding quality of supply") %>%                      # Filtering 
              select(EDB,
                     Network,
                     Year,                                                                # Variable 1 and 2
                     rg_capex_qual_cc = Value) %>%
              
              left_join(db_neat,.,by = NULL)
       
       ## 5.6. CAPEX - Expenditure on assets - Quality of supply expenditure ####
       
       db_neat <- db_raw %>%                                                                                #
              filter(Section %in% "6a(vi): Quality of Supply",                                # Selecting and
                     Subcategory %in% "Quality of supply expenditure",
                     Description %in% "Quality of supply expenditure") %>%
              select(EDB,
                     Network,
                     Year,
                     rg_capex_qual_tot = Value) %>%
              
              left_join(db_neat,.,by = NULL)
       
       ## 5.6. CAPEX - Expenditure on assets - Other reliability, safety and environment less capital contributions ####
       
       db_neat <- db_raw %>%
              filter(Section %in% "6a(viii): Other Reliability, Safety and Environment",
                     Subcategory %in% "Other reliability, safety and environment less capital contributions", 
                     Description %in% "Other reliability, safety and environment less capital contributions") %>%  
              select(EDB,
                     Network,
                     Year,
                     rg_capex_rse_less_cc = Value) %>%
              
              left_join(db_neat,.,by = NULL)
       
       ## 5.6. CAPEX - Expenditure on assets - Capital contributions funding other reliability, safety and environment ####
       
       db_neat <- db_raw %>%
              filter(Section %in% "6a(viii): Other Reliability, Safety and Environment",
                     Subcategory %in% "Other reliability, safety and environment less capital contributions",
                     Description %in% "Capital contributions funding other reliability, safety and environment") %>%                      # Filtering 
              select(EDB,
                     Network,
                     Year,
                     rg_capex_rse_cc = Value) %>%
              
              left_join(db_neat,.,by = NULL)
       
       ## 5.6. CAPEX - Expenditure on assets - Other reliability, safety and environment expenditure ####
       
       db_neat <- db_raw %>%
              filter(Section %in% "6a(viii): Other Reliability, Safety and Environment",
                     Subcategory %in% "Other reliability, safety and environment expenditure",
                     Description %in% "Other reliability, safety and environment expenditure") %>%                              # Filtering 
              select(EDB,
                     Network,
                     Year,
                     rg_capex_rse_other = Value) %>%
              
              left_join(db_neat,.,by = NULL)
       
       ## 5.6. CAPEX - Expenditure on assets - Consumer Connection - Capital contributions funding consumer connection expenditure ####
       
       db_neat <- db_raw %>%
              filter(Section %in% "6a(iii): Consumer Connection",
                     Description %in% "Capital contributions funding consumer connection expenditure") %>%
              select(EDB,
                     Network,
                     Year,
                     rg_capex_conc_cc = Value) %>%
              
              left_join(db_neat,.,by = NULL)
       
       ## 5.6. CAPEX - Expenditure on assets - Consumer Connection - Consumer connection less capital contributions ####
       
       db_neat <- db_raw %>%
              filter(Section %in% "6a(iii): Consumer Connection",
                     Description %in% "Consumer connection less capital contributions") %>%
              select(EDB,
                     Network,
                     Year,
                     rg_capex_conc_less_cc = Value) %>%
              
              left_join(db_neat,.,by = NULL)
       
       ## 5.6. CAPEX - Expenditure on assets - Consumer Connection - Consumer connection expenditure ####
       
       db_neat <- db_raw %>%
              filter(Section %in% "6a(iii): Consumer Connection",
                     Description %in% "Consumer connection expenditure") %>%
              select(EDB,
                     Network,
                     Year,
                     rg_capex_conc_exp = Value) %>%
              
              left_join(db_neat,.,by = NULL)
       
       ## 5.6. CAPEX - System Growth - Distribution and LV lines ####

       db_neat <- db_raw %>%
              filter(Section %in% "6a(iv): System Growth and Asset Replacement and Renewal",
                     Subcategory %in% "System Growth",
                     Description %in% "Distribution and LV lines") %>%
              select(EDB,
                     Network,
                     Year,
                     rg_capex_sys_dis_li= Value) %>%
              
              left_join(db_neat,.,by = NULL)
       
       ## 5.6. CAPEX - System Growth - Distribution and LV cables ####
       
       db_neat <- db_raw %>%
              filter(Section %in% "6a(iv): System Growth and Asset Replacement and Renewal",
                     Subcategory %in% "System Growth",
                     Description %in% "Distribution and LV cables") %>%
              select(EDB,
                     Network,
                     Year,
                     rg_capex_sys_dis_cb = Value) %>%
              
              left_join(db_neat,.,by = NULL)
       
       ## 5.6. CAPEX - System Growth - Distribution substations and transformers ####
       
       
       db_neat <- db_raw %>%
              filter(Section %in% "6a(iv): System Growth and Asset Replacement and Renewal",
                     Subcategory %in% "System Growth",
                     Description %in% "Distribution substations and transformers") %>%
              select(EDB,
                     Network,
                     Year,
                     rg_capex_sys_dis_sub_tran = Value) %>%
              
              left_join(db_neat,.,by = NULL)
       
       ## 5.6. CAPEX - System Growth - Distribution switchgear ####
       
       db_neat <- db_raw %>%
              filter(Section %in% "6a(iv): System Growth and Asset Replacement and Renewal",
                     Subcategory %in% "System Growth",
                     Description %in% "Distribution switchgear") %>%
              select(EDB,
                     Network,
                     Year,
                     rg_capex_sys_dis_swi = Value) %>%
              
              left_join(db_neat,.,by = NULL)
       
       ## 5.6. CAPEX - System Growth - Subtransmission ####
       
       db_neat <- db_raw %>%
              filter(Section %in% "6a(iv): System Growth and Asset Replacement and Renewal",
                     Subcategory %in% "System Growth",
                     Description %in% "Subtransmission") %>%
              select(EDB,
                     Network,
                     Year,
                     rg_capex_sys_sub = Value) %>%
              
              left_join(db_neat,.,by = NULL)
       
       ## 5.6. CAPEX - System Growth - Other network assets ####
       
       db_neat <- db_raw %>%
              filter(Section %in% "6a(iv): System Growth and Asset Replacement and Renewal",
                     Subcategory %in% "System Growth",
                     Description %in% "Other network assets") %>%
              select(EDB,
                     Network,
                     Year,
                     rg_capex_sys_a_oth = Value) %>%
              
              left_join(db_neat,.,by = NULL)
       
       ## 5.6. CAPEX - System Growth - Zone substations ####
       
       db_neat <- db_raw %>%
              filter(Section %in% "6a(iv): System Growth and Asset Replacement and Renewal",
                     Subcategory %in% "System Growth",
                     Description %in% "Zone substations") %>%
              select(EDB,
                     Network,
                     Year,
                     rg_capex_sys_z_sub = Value) %>%
              
              left_join(db_neat,.,by = NULL)
       
       ## 5.6. CAPEX - System Growth - Capital contributions funding system growth ####
       
       db_neat <- db_raw %>%
              filter(Section %in% "6a(iv): System Growth and Asset Replacement and Renewal",
                     Subcategory %in% "System Growth",
                     Description %in% "Capital contributions funding system growth") %>%
              select(EDB,
                     Network,
                     Year,
                     rg_capex_sys_cc = Value) %>%
              
              left_join(db_neat,.,by = NULL)
       
       ## 5.6. CAPEX - System Growth expenditure - TOTAL ####
       
       db_neat <- db_raw %>%
              filter(Section %in% "6a(iv): System Growth and Asset Replacement and Renewal",
                     Subcategory %in% "System Growth",
                     Description %in% "System growth expenditure") %>%
              select(EDB,
                     Network,
                     Year,
                     rg_capex_sys_tot = Value) %>%
              
              left_join(db_neat,.,by = NULL)
       
       ## 5.6. CAPEX - Asset Replacement and Renewal - Distribution and LV lines ####
       
       db_neat <- db_raw %>%
              filter(Section %in% "6a(iv): System Growth and Asset Replacement and Renewal",
                     Subcategory %in% "Asset Replacement and Renewal",
                     Description %in% "Distribution and LV lines") %>%
              select(EDB,
                     Network,
                     Year,
                     rg_capex_rr_dis_li = Value) %>%
              
              left_join(db_neat,.,by = NULL)
       
       ## 5.6. CAPEX - Asset Replacement and Renewal - Distribution and LV cables ####
       
       db_neat <- db_raw %>%
              filter(Section %in% "6a(iv): System Growth and Asset Replacement and Renewal",
                     Subcategory %in% "Asset Replacement and Renewal",
                     Description %in% "Distribution and LV cables") %>%
              select(EDB,
                     Network,
                     Year,
                     rg_capex_rr_dis_cb = Value) %>%
              
              left_join(db_neat,.,by = NULL)
       
       ## 5.6. CAPEX - Asset Replacement and Renewal - Distribution substations and transformers ####
       
       db_neat <- db_raw %>%
              filter(Section %in% "6a(iv): System Growth and Asset Replacement and Renewal",
                     Subcategory %in% "Asset Replacement and Renewal",
                     Description %in% "Distribution substations and transformers") %>%
              select(EDB,
                     Network,
                     Year,
                     rg_capex_rr_dis_sub_tran = Value) %>%
              
              left_join(db_neat,.,by = NULL)
       
       ## 5.6. CAPEX - Asset Replacement and Renewal - Distribution switchgear ####
       
       db_neat <- db_raw %>%
              filter(Section %in% "6a(iv): System Growth and Asset Replacement and Renewal",
                     Subcategory %in% "Asset Replacement and Renewal",
                     Description %in% "Distribution substations and transformers") %>%
              select(EDB,
                     Network,
                     Year,
                     rg_capex_rr_dis_swi = Value) %>%
              
              left_join(db_neat,.,by = NULL)
       
       ## 5.6. CAPEX - Asset Replacement and Renewal - Subtransmission ####
       
       db_neat <- db_raw %>%
              filter(Section %in% "6a(iv): System Growth and Asset Replacement and Renewal",
                     Subcategory %in% "Asset Replacement and Renewal",
                     Description %in% "Subtransmission") %>%
              select(EDB,
                     Network,
                     Year,
                     rg_capex_rr_sub = Value) %>%
              
              left_join(db_neat,.,by = NULL)

       ## 5.6. CAPEX - Asset Replacement and Renewal - Other network assets ####
       
       db_neat <- db_raw %>%
              filter(Section %in% "6a(iv): System Growth and Asset Replacement and Renewal",
                     Subcategory %in% "Asset Replacement and Renewal",
                     Description %in% "Other network assets") %>%
              select(EDB,
                     Network,
                     Year,
                     rg_capex_rr_a_net_oth = Value) %>%
              
              left_join(db_neat,.,by = NULL)
       
       ## 5.6. CAPEX - Asset Replacement and Renewal - Zone substations ####
       
       db_neat <- db_raw %>%
              filter(Section %in% "6a(iv): System Growth and Asset Replacement and Renewal",
                     Subcategory %in% "Asset Replacement and Renewal",
                     Description %in% "Zone substations") %>%
              select(EDB,
                     Network,
                     Year,
                     rg_capex_rr_z_sub = Value) %>%
              
              left_join(db_neat,.,by = NULL)
       
       ## 5.6. CAPEX - Asset Replacement and Renewal - Capital contributions funding asset replacement and renewal ####
       
       db_neat <- db_raw %>%
              filter(Section %in% "6a(iv): System Growth and Asset Replacement and Renewal",
                     Subcategory %in% "Asset Replacement and Renewal",
                     Description %in% "Capital contributions funding asset replacement and renewal") %>%
              select(EDB,
                     Network,
                     Year,
                     rg_capex_rr_cc = Value) %>%
              
              left_join(db_neat,.,by = NULL)
       
       ## 5.6. CAPEX - Asset Replacement and Renewal - Asset replacement and renewal expenditure ####
       
       db_neat <- db_raw %>%
              filter(Section %in% "6a(iv): System Growth and Asset Replacement and Renewal",
                     Subcategory %in% "Asset Replacement and Renewal",
                     Description %in% "Asset replacement and renewal expenditure") %>%
              select(EDB,
                     Network,
                     Year,
                     rg_capex_rr_tot = Value) %>%
              
              left_join(db_neat,.,by = NULL)
       
       ## 5.6. CAPEX - Asset relocations - Asset relocations less capital contributions ####

       db_neat <- db_raw %>%                                                                      #
              filter(Section %in% "6a(v): Asset Relocations",                                    # Selecting and
                     Subcategory %in% "Asset relocations less capital contributions",        # Filtering 
                     Description %in% "Asset relocations less capital contributions") %>%            #
              select(EDB,
                     Network,
                     Year,
                     rg_capex_reloc_less_cc = Value) %>%
              
              left_join(db_neat,.,by = NULL)
       
       ## 5.6. CAPEX - Asset relocations - Capital contributions funding asset relocations ####
       
       db_neat <- db_raw %>%                                                                                #
              filter(Section %in% "6a(v): Asset Relocations",                                # Selecting and
                     Subcategory %in% "Asset relocations less capital contributions",
                     Description %in% "Capital contributions funding asset relocations") %>%                      # Filtering 
              select(EDB,
                     Network,
                     Year,
                     rg_capex_reloc_cc = Value) %>%
              
              left_join(db_neat,.,by = NULL)
       
       ## 5.6. CAPEX - Asset relocations - Asset relocations expenditure ####
       
       db_neat <- db_raw %>%                                                                                #
              filter(Section %in% "6a(v): Asset Relocations",                                # Selecting and
                     Subcategory %in% "Asset relocations expenditure",
                     Description %in% "Asset relocations expenditure") %>%
              select(EDB,
                     Network,
                     Year,
                     rg_capex_reloc_tot = Value) %>%
              
              left_join(db_neat,.,by = NULL)
       
       ## 5.6. CAPEX - Non-network Expenditure - Atypical Expenditure ####
       
       db_neat <- db_raw %>%
              filter(Section %in% "6a(ix): Non-Network Assets",
                     Subcategory %in% "Atypical expenditure",
                     Description %in% "Atypical expenditure") %>%
              
              select(EDB,
                     Network,
                     Year,
                     rg_capex_n_net_atypical = Value)%>%
              
              left_join(db_neat,.,by = NULL)
       
       ## 5.6. CAPEX - Non-network Expenditure - Routine Expenditure ####
       
       db_neat <- db_raw %>%
              filter(Section %in% "6a(ix): Non-Network Assets",
                     Subcategory %in% "Routine expenditure",
                     Description %in% "Routine expenditure") %>%
              
              select(EDB,
                     Network,
                     Year,
                     rg_capex_n_net_routine = Value)%>%
              
              left_join(db_neat,.,by = NULL)
       
       ## 5.6. CAPEX - Non-network Expenditure - TOTAL ####
       
       db_neat <- db_raw %>%
              filter(Section %in% "6a(ix): Non-Network Assets",
                     Subcategory %in% c("Expenditure on non-network assets",
                                        "Non-network assets expenditure"),
                     Description %in% c("Expenditure on non-network assets",
                                        "Non-network assets expenditure")) %>%
              select(EDB,
                     Network,
                     Year,
                     rg_capex_n_net_tot = Value) %>%
              
              left_join(db_neat,.,by = NULL)
       
       ## 5.6. CAPEX - Legislative and Regulator - Capital contributions funding legislative and regulatory ####
       
       db_neat <- db_raw %>%                                                                                #
              filter(Section %in% "6a(vii): Legislative and Regulatory",     # Selecting and
                     Subcategory %in% "Legislative and regulatory less capital contributions",
                     Description %in% "Capital contributions funding legislative and regulatory") %>%      # Filtering 
              select(EDB,
                     Network,
                     Year,
                     rg_capex_leg_reg_cc = Value) %>%
              
              left_join(db_neat,.,by = NULL)
       
       ## 5.6. CAPEX - Capital Contribution - TOTAL ####
       
       db_neat <- mutate(db_neat, rg_capex_cc_tot =     rg_capex_conc_cc + 
                                                        rg_capex_rr_cc + 
                                                        rg_capex_sys_cc + 
                                                        rg_capex_reloc_cc + 
                                                        rg_capex_qual_cc +
                                                        rg_capex_leg_reg_cc + 
                                                        rg_capex_rse_cc)
       
       ## 5.7. Revenue - Consumer totals - Average no. of ICPs in disclosure year ####
       
       db_neat <- db_raw %>% filter(Section %in% "8(i): Billed Quantities by Price Component",
                         Category %in% "Standard",
                         Subcategory %in% "Consumer totals",
                         Description %in% "Average no. of ICPs in disclosure year") %>%
              select(EDB,
                     Year,
                     Network,
                     rev_con_tot_ave_co = Value) %>%
              
              left_join(db_neat,.,by = NULL)
       
       ## 5.7. Revenue - Consumer totals - Standard - Energy delivered to ICPs in disclosure year (MWh) ####
       
       db_neat <- db_raw %>% filter(Section %in% "8(i): Billed Quantities by Price Component",
                         Category %in% "Standard",
                         Subcategory %in% "Consumer totals",
                         Description %in% "Energy delivered to ICPs in disclosure year (MWh)") %>%
              select(EDB,
                     Year,
                     Network,
                     rev_con_stand_tot_mwh = Value) %>%
              
              left_join(db_neat,.,by = NULL)
       
       ## 5.7. Revenue - Consumer totals - Non-Standard - Energy delivered to ICPs in disclosure year (MWh) ####
       
       db_neat <- db_raw %>% filter(Section %in% "8(i): Billed Quantities by Price Component",
                                    Category %in% "Non-standard",
                                    Subcategory %in% "Consumer totals",
                                    Description %in% "Energy delivered to ICPs in disclosure year (MWh)") %>%
              select(EDB,
                     Year,
                     Network,
                     rev_con_n_stand_tot_mwh = Value) %>%
              
              left_join(db_neat,.,by = NULL)
       
       ## 5.7. Revenue - Consumer totals - Non-Standard - Energy delivered to ICPs in disclosure year (MWh) ####
       
       db_neat <- db_raw %>% filter(Section %in% "8(i): Billed Quantities by Price Component",
                                    Category %in% "Total",
                                    Subcategory %in% "Consumer totals",
                                    Description %in% "Energy delivered to ICPs in disclosure year (MWh)") %>%
              select(EDB,
                     Year,
                     Network,
                     rev_con_tot_mwh = Value) %>%
              
              left_join(db_neat,.,by = NULL)

       ## 5.7. Revenue - Actual - Line Charge Revenue ####
       
       
       db_neat <- db_raw %>%    filter(Section %in% "7(i): Revenue",
                                          Subcategory %in% "Actual") %>%
                                   select(EDB,
                                          Network,
                                          Year,
                                          rev_actual = Value)%>%
              
              left_join(db_neat,.,by = NULL)
       
       
       ## 5.7. Revenue - Target - Line Charge Revenue ####
       
       db_neat <- db_raw %>%    filter(Section %in% "7(i): Revenue",
                                          Subcategory %in% "Target") %>%
                                          select(EDB,
                                                 Network,
                                                 Year,
                                                 rev_target = Value)%>%
              
              left_join(db_neat,.,by = NULL)
       
       
       ## 6. RATIOS - Energy delivered to ICPs in disclosure year (MWh) / Average no. of ICPs in disclosure year ####
       
       db_neat <- mutate(db_neat,rt_mwh_icp = rev_con_tot_mwh/rev_con_tot_ave_co)
       
       
       ## 6. RATIOS - R CAIDI ####
       
       db_neat <- mutate(db_neat,r_caidi = r_saidi/r_saifi)
       
       ## 6. RATIOS - RAB/RAB ####
       
       db_neat <- mutate(db_neat,p_rab = rg_rab_closed_tot/rg_rab_open)
       
       

       ## 7. Updating Data
       
       ## Losses 
       
       db_neat <- mutate(db_neat,s_loss = s_ent_enr - rev_con_tot_mwh/1000)

       ## Reliability - Restoring in Less tahn 3 hours.
       
       db_neat <- mutate(db_neat,r_less_3 = r_i_un - r_more_3)
       
       
       ## 8. OTHERS
       
       ## TOTAL ICPS in NZ
       
       db_neat %>% filter(Network %in% "All") %>% select(EDB,Year,rev_con_tot_ave_co) %>% group_by(Year) %>%
              
              summarise(pop = sum(rev_con_tot_ave_co)) %>% ungroup() -> temp
       
       db_neat <- left_join(db_neat,temp,by = NULL)
       

       
              
       # Normalized OPEX in Vegetation Management
       
       db_neat <- db_neat %>% 
              select(EDB,Network,Year,rg_opex_veg_mang) %>% 
                     mutate(norm_rg_opex_veg_mang = rg_opex_veg_mang/max(rg_opex_veg_mang,na.rm = TRUE)) %>%
              
              left_join(db_neat,.,by = NULL)
       
       # Normalized OPEX in Routine and Corrective Inspection
       
       db_neat <- db_neat %>% 
              select(EDB,Network,Year,rg_opex_rou_correc) %>% 
              mutate(norm_rg_opex_rou_correc = rg_opex_rou_correc/max(rg_opex_rou_correc,na.rm = TRUE)) %>%
              
              left_join(db_neat,.,by = NULL)

       # Normalized OPEX in Replacement and Renewal
       
       db_neat <- db_neat %>% 
              select(EDB,Network,Year,rg_opex_a_rep_ren) %>% 
              mutate(norm_rg_opex_a_rep_ren = rg_opex_a_rep_ren/max(rg_opex_a_rep_ren,na.rm = TRUE)) %>%
              
              left_join(db_neat,.,by = NULL)
        
       # Normalized Regulatory - RAB Opening
       
       db_neat <- db_neat %>% 
              select(EDB,Network,Year,rg_rab_open) %>% 
              mutate(norm_rg_rab_open = rg_rab_open/max(rg_rab_open,na.rm = TRUE)) %>%
              
              left_join(db_neat,.,by = NULL)
       
       # Normalized Regulatory - RAB Closing
       
       db_neat <- db_neat %>% 
              select(EDB,Network,Year,rg_rab_closed_tot) %>% 
              mutate(norm_rg_rab_closed_tot = rg_rab_closed_tot/max(rg_rab_closed_tot,na.rm = TRUE)) %>%
              
              left_join(db_neat,.,by = NULL)
       
       
       
       # Normalized Regulatory - Depeciation
       
       db_neat <- db_neat %>% 
              select(EDB,Network,Year,rg_depr) %>% 
              mutate(norm_rg_depr = rg_depr/max(rg_depr,na.rm = TRUE)) %>%
              
              left_join(db_neat,.,by = NULL)
       
       
       # Normalized SAIDI
       
       db_neat %>% 
              select(EDB,Network,Year,r_saidi,rev_con_tot_ave_co)
       
       db_neat %>% 
              filter(Network %in% "All") %>%
                     select(EDB,Network,Year,rev_con_tot_ave_co) %>% 
                            group_by(EDB,Network,Year) %>% 
                                   summarise(nz_con_tot1 = sum(rev_con_tot_ave_co)) %>%
                                          ungroup() -> temp
       
       db_neat %>% 
              filter(Network %in% "All") %>%
                     select(EDB,Network,Year,rev_con_tot_ave_co) %>% 
                            group_by(Network,Year) %>% 
                                   summarise(nz_con_tot = sum(rev_con_tot_ave_co)) %>%
                                          ungroup() -> temp
       

       
       
       
       

       
       
       
       #### Additional data to the EDB ##########################
       
       island <- db_raw %>% select(EDB) %>% filter(!EDB %in% "Aurora Energy (interim)") %>% distinct()
       
       bind_cols     (island,"COD" =   c("AL", # Alpine Energy
                                         "AE", # Aurora Energy				
                                         "BU", # Buller Electricity
                                         "CL", # Centralines
                                         "CO", # Counties Power
                                         "EA", # EA Networks	
                                         "EN", # Eastland Network
                                         "EL", # Electra
                                         "EI", # Electricity Invercargill	
                                         "HO", # Horizon Energy
                                         "MA", # MainPower NZ
                                         "MR", # Marlborough Lines	
                                         "NE", # Nelson Electricity	
                                         "NT", # Network Tasman
                                         "NW", # Network Waitaki
                                         "NO", # Northpower
                                         "OR", # Orion NZ
                                         "ON", # OtagoNet
                                         "PO", # Powerco
                                         "SC", # Scanpower
                                         "LN", # The Lines Company
                                         "PC", # The Power Company
                                         "TP", # Top Energy
                                         "UN", # Unison Networks
                                         "VE", # Vector Lines
                                         "WA", # Waipa Networks
                                         "WL", # WEL Networks
                                         "WE", # Wellington Electricity
                                         "WP") # Westpower
       ) -> island  
       
       bind_cols     (island,"island" =c("South", # Alpine Energy
                                         "South", # Aurora Energy				
                                         "South", # Buller Electricity
                                         "North", # Centralines
                                         "North", # Counties Power
                                         "South", # EA Networks	
                                         "North", # Eastland Network
                                         "North", # Electra
                                         "South", # Electricity Invercargill	
                                         "North", # Horizon Energy
                                         "South", # MainPower NZ
                                         "South", # Marlborough Lines	
                                         "South", # Nelson Electricity	
                                         "South", # Network Tasman
                                         "South", # Network Waitaki
                                         "North", # Northpower
                                         "South", # Orion NZ
                                         "South", # OtagoNet
                                         "North", # Powerco
                                         "North", # Scanpower
                                         "North", # The Lines Company
                                         "South", # The Power Company
                                         "North", # Top Energy
                                         "North", # Unison Networks
                                         "North", # Vector Lines
                                         "North", # Waipa Networks
                                         "North", # WEL Networks
                                         "North", # Wellington Electricity
                                         "South") # Westpower
       ) -> island
       
       db_neat <- left_join(island,db_neat, by = NULL)
       
return(db_neat)
}
