library(tidyverse)
source("parseBerlingske.R")

# This file provides background for standard dataset cleaning


# -------------------------------
# Dataset polls_campaign2011
# Kasper Risager, 2019-02-021
#--------------------------------


# Getting poll data from Berlingske Barometer for 2011

polls2011 <- berlingskeBarometerPolls(year=2011)

# Figure out who did daily monitoring during 2011 election

# Figure out election date by looking for Id==11 which is known to be election data
election_date_2011 <- polls2011 %>% filter(Id == 11) %>% pull(Date) 

# 17 days back. This was chosen to make sure there was daily monitoring from main institutes
campaign_start_2011 <- election_date_2011 - 17

# Only within campaign
campaign_polls_2011 <- polls2011 %>% 
  filter(Date < election_date_2011 & Date >= campaign_start_2011)

# Check how many polls there are daily. Used for setting length of campaign (22 days)
daily_polls <- campaign_polls_2011 %>% group_by(Date) %>% tally() %>% arrange(Date)

# How many polls did each institute publish in the 22 days?
polls_by_institute <- campaign_polls_2011 %>% group_by(Id, InstituteName) %>% tally()

# Institutes with Id's 1,4,6,7 are doing daily observations (>=17 polls)

# Make sure the four all had one daily observation
print(campaign_polls_2011 %>% filter(Id %in% c(1,4,6,7)) %>% group_by(Date,Id) %>% 
        tally() %>%
        spread(Id,n),
      n=200)

# Id == 4 has a double observation for one date
campaign_polls_2011 %>%
  filter(Id == 4 & Date=="2011-08-31") %>%
  unnest(Results) %>%
  arrange(order_by=Updated) %>%
  arrange(order_by=PartyId)

# Remove the earliest updated occurence
remove_this <- campaign_polls_2011 %>% 
  filter(Id==4, Date =="2011-08-31") %>% 
  summarise(MinUpdated = min(Updated)) %>% 
  pull()


# Make a clean dataset with the four institutes and only one daily observation
# Put in 1200 respondents since none are registered
campaign_polls_2011 %>%
  filter(Id %in% c(1,4,6,7) & Updated != remove_this) %>%
  mutate(Respondents = 1200)






# -------------------------------
# Dataset polls_campaign2015
# Kasper Risager, 2019-02-07
#--------------------------------



# Getting poll data from Berlingske Barometer for 2015

polls2015 <- berlingskeBarometerPolls(year=2015)

# Figure out who did daily monitoring during 2015 election

# Figure out election date by looking for Id==11 which is known to be election data
election_date_2015 <- polls2015 %>% filter(Id == 11) %>% pull(Date) 

# 22 days back. This was chosen to make sure there was daily monitoring from main institutes
campaign_start_2015 <- election_date_2015 - 22

# Only within campaign
# The dataset also includes an exit poll on election night. This is excluded by
# choosing only polls up to but not including the election day
campaign_polls_2015 <- polls2015 %>% 
  filter(Date < election_date_2015 & Date >= campaign_start_2015)

# Check how many polls there are daily. Used for setting length of campaign (22 days)
daily_polls <- campaign_polls_2015 %>% group_by(Date) %>% tally() %>% arrange(Date)

# How many polls did each institute publish in the 22 days?
polls_by_institute <- campaign_polls_2015 %>% group_by(Id, InstituteName) %>% tally()

# Institutes with Id's 1,3,4,7 are doing daily observations (>=22 polls)
# Institute 16 did nearly daily, but we filter them out anyway

# Make sure the four all had one daily observation
print(campaign_polls_2015 %>% filter(Id %in% c(1,3,4,7)) %>% group_by(Date,Id) %>% 
        tally() %>%
        spread(Id,n),
      n=200)

# Id == 1 has an additional observation for one date
# Remove the earliest updated occurence
remove_this <- campaign_polls_2015 %>% 
  filter(Id==1, Date =="2015-05-27") %>% 
  summarise(MinUpdated = min(Updated)) %>% 
  pull()


# Make a clean dataset with the four institutes and only one daily observation
campaign_polls_2015 %>%
  filter(Id %in% c(1,3,4,7) & Updated != remove_this)


# -------------------------------
# Datasets for actual elections taken from Berlingske Barometer
# Kasper Risager, 2019-02-07
#--------------------------------


# Getting poll data from Berlingske Barometer for InstituteId 11
# Filter out irrelevant information
elections <- berlingskeBarometerPolls(instituteId=11) %>% 
  select(Year, Date, Results)
