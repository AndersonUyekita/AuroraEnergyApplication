db_consumer_group <- function(x = db_raw)
       {
       # Subsetting the raw data
       db_raw %>% filter(   EDB %in% "Aurora Energy",
                            Section %in% c("8(i): Billed Quantities by Price Component",
                                           "8(ii): Line Charge Revenues ($000) by Price Component")) %>%
              select(Section,
                     Year,
                     Network,
                     Category,
                     Subcategory,
                     Description,
                     Value) -> db_consumer_group
       
       gsub(x = db_consumer_group$Subcategory,
            pattern = "Non-standard domestic / non domestic - ",
            replacement = "") -> db_consumer_group$Subcategory
       
       gsub(x = db_consumer_group$Subcategory,
            pattern = "other - ",
            replacement = "") -> db_consumer_group$Subcategory
       
       gsub(x = db_consumer_group$Subcategory,
            pattern = "General - ",
            replacement = "") -> db_consumer_group$Subcategory
       
       gsub(x = db_consumer_group$Subcategory,
            pattern =  " - Non-standard domestic / non domestic",
            replacement = "") -> db_consumer_group$Subcategory
       
       gsub(x = db_consumer_group$Subcategory,
            pattern =  "Non-domestic - ",
            replacement = "") -> db_consumer_group$Subcategory
       
       gsub(x = db_consumer_group$Subcategory,
            pattern =  " - Non-domestic",
            replacement = "") -> db_consumer_group$Subcategory
       
       gsub(x = db_consumer_group$Subcategory,
            pattern =  "Standard domestic - Standard domestic",
            replacement = "Residential") -> db_consumer_group$Subcategory
       
       gsub(x = db_consumer_group$Subcategory,
            pattern =  "Standard Domestic - Standard domestic",
            replacement = "Residential") -> db_consumer_group$Subcategory
       
       gsub(x = db_consumer_group$Subcategory,
            pattern =  "Standard domestic - Standard Domestic",
            replacement = "Residential") -> db_consumer_group$Subcategory
       
       gsub(x = db_consumer_group$Subcategory,
            pattern =  "Residential - Residential",
            replacement = "Residential") -> db_consumer_group$Subcategory
       
       return(db_consumer_group)
       }