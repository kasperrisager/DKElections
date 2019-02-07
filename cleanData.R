library(tidyverse)

source("parseBerlingske.R")

# This file provides cleaned data
# Consult cleaningAnalyses.R for background

polls_campaign2015 <- function() {
  
  
  # Getting poll data from Berlingske Barometer for 2015
  polls2015 <- berlingskeBarometerPolls(year=2015)
  
  # Set campaign boundaries
  election_date_2015 <- "2015-06-18" 
  campaign_start_2015 <- "2015-05-27"
  
  # Get all polls during campaign
  campaign_polls_2015 <- polls2015 %>% 
    filter(Date < election_date_2015 & Date >= campaign_start_2015)

  # This one is a double, or may have been misattributed to Gallup (Id=1)
  remove_this <- campaign_polls_2015 %>% 
    filter(Id==1, Date =="2015-05-27") %>% 
    summarise(MinUpdated = min(Updated)) %>% 
    pull()
  
  # Include only Ids 1,3,4,7 since they have daily monitoring
  campaign_polls_2015 %>%
    filter(Id %in% c(1,3,4,7) & Updated != remove_this)

}