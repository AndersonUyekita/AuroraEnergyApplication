# Aurora Energy - Job Application

## Objective

Apply to the position of Data and Information Manager (Job-Description-QJ45141).


```{r echo=FALSE}
workinprogress <- bind_cols(Chapter = c("General Information",
                                        "Descriptive Analysis"),
                            
                            "Description/Status" = c("OK",
                                                     "OK"),
                            
                            Progress = c(100,
                                         100))


workinprogress %>% mutate(Progress = color_bar("lightgreen")(Progress)) %>%

kable(escape = F) %>%
kable_styling("hover", full_width = F) %>%
column_spec(3, width = "3cm")
```