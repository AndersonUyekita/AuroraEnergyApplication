#########################################################
#                                                       #
# Author: AH Uyekita                                    #
# Document: Job Application                             #
# Position: Data and Information Manager                #
# Code: QJ45141                                         #
#                                                       #
#########################################################

# Description

# This function aims to automatized the process of dataset filtering,
# and as output a give a data.frame filtered.

Filtering <- function( x,
                       EDB_,
                       Network_,
                       Disclosure_year_,
                       Schedule_,
                       Schedule_reference_,
                       Section_,
                       Category_,
                       Subcategory_,
                       Description_,
                       Observation_year_,
                       Forecast_year_,
                       Units_,
                       Value_,
                       Text_input_)
       {
       subset_temp <- x
       
       if (EDB_ != "")
              {
              subset_temp <- subset_temp %>%
                     select(everything()) %>%
                     filter(EDB == EDB_)
              }

       if (Network_ != "")
              {
              subset_temp <- subset_temp %>%
                     select(everything()) %>%
                     filter(Network == Network_)
              }         
       
       if (Disclosure_year_ != "")
              {
              subset_temp <- subset_temp %>%
                     select(everything()) %>%
                     filter(Disclosure_year == Disclosure_year_)
              }       
       
       if (Schedule_ != "")
              {
              subset_temp <- subset_temp %>%
                     select(everything()) %>%
                     filter(Schedule == Schedule_)
              }          
       
       if (Schedule_reference_ != "")
              {
              subset_temp <- subset_temp %>%
                     select(everything()) %>%
                     filter(Schedule_reference == Schedule_reference_)
              }       
       
       if (Section_ != "")
              {
              subset_temp <- subset_temp %>%
                     select(everything()) %>%
                     filter(Section == Section_)
              }        
       
       if (Subcategory_ != "")
              {
              subset_temp <- subset_temp %>%
                     select(everything()) %>%
                     filter(Subcategory == Subcategory_)
              }        
       
       if (Description_ != "")
              {
              subset_temp <- subset_temp %>%
                     select(everything()) %>%
                     filter(Description == Description_)
              }        
       
       if (Observation_year_ != "")
              {
              subset_temp <- subset_temp %>%
                     select(everything()) %>%
                     filter(Observation_year == Observation_year_)
              } 
       
       if (Units_ != "")
              {
              subset_temp <- subset_temp %>%
                     select(everything()) %>%
                     filter(Units == Units_)
              }       
       
       if (Value_ != "")
              {
              subset_temp <- subset_temp %>%
                     select(everything()) %>%
                     filter(Value == Value_)
              }    
       
       if (Text_input_ != "")
              {
              subset_temp <- subset_temp %>%
                     select(everything()) %>%
                     filter(Text_input == Text_input_)
              }        

       return(subset_temp)
       }