#



#
# Transform a tidy dataset into a "human-like" table.
# Aims to generate one table to a specific EDB

#
# company  : EDB name (Only ONE EDB)
# variable : variable to find in db_neat
# case     :
#            1) Regular Table
#            2) With Porcentages
#
tablelizator <- function(variable,company,case = 1)
       {
       if (case == 1)
              {
              db_neat %>% filter(EDB %in% company) %>%  # Filtering the EDB company
                     
                     select(Year,                   # 
                            Network,                # Subsetting db_neat
                            Value = variable) %>%   # 
                     
                     spread(Network,Value) %>%  # Verb-Function to transform tidy dataset into "human-table"
                     
                     select(Year,          #
                            CentralOtago,  # Ordering the sequence of columns
                            Dunedin,       #
                            All) %>%       #
                     
                     arrange(desc(Year)) -> temp  # Ordering rows
              
              colnames(temp) <- c("Year",                            #
                                  paste("co_",variable,sep = ""),    # Giving generic names
                                  paste("du_",variable,sep = ""),    # Avoid same names in all tables
                                  paste("all_",variable,sep = ""))   #
              }
       
       if (case == 2)
              {
              db_neat %>% filter(EDB %in% company) %>%  # Filtering the EDB company
                     
                     select(Year,                   # 
                            Network,                # Subsetting db_neat
                            Value = variable) %>%   # 
                     
                            spread(Network,Value) %>%  # Verb-Function to transform tidy dataset into "human-table"
                            
                                   mutate(co_p = 100 * CentralOtago/All,
                                          du_p = 100 * Dunedin/All) %>%

                                          select(Year,          #
                                                 CentralOtago,  # Ordering the sequence of columns
                                                 co_p,          #
                                                 Dunedin,       #
                                                 du_p,          #
                                                 All) %>%       #
                     
                                                 arrange(desc(Year)) -> temp  # Ordering rows
              
              colnames(temp) <- c("Year",                              #
                                  paste("co_",  variable,sep = ""),    # Giving generic names
                                  paste("co_p_",variable,sep = ""),    # Avoid same names in all tables
                                  paste("du_",  variable,sep = ""),    #
                                  paste("du_p_",variable,sep = ""),    #
                                  paste("all_", variable,sep = ""))    #
              }
       return(temp) # return the "human-table"
       
}