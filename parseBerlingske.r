library(tidyverse)
library(lubridate)
library(xml2)

berlingskeBarometerURLBase <- "http://www.berlingske.dk/upload/webred/bmsandbox/opinion_poll"

berlingskeBarometerOverviewFilename <- "overview.xml"





parseBerlingskeOverviewInstitute <- function(node) {
  
  years <- node$xmls %>% map(function(x) x$year[[1]]) %>% as.integer()
  id <- node$id
  name <- node$name[[1]]
  
  tibble(Year = years) %>% mutate(Id = as.integer(id), InstituteName = name)
  
}




berlingske_parse_entry <- function(e) {

  party_name <- ""
  party_shortname <- ""
  party_letter <- ""
  
  if(length(e$party$name)>=1) party_name <- e$party$name[[1]]
  if(length(e$party$letter)>=1) party_letter <- e$party$letter[[1]]
  
  tibble(PartyId = as.integer(e$party$id[[1]]), 
         PartyName = party_name, 
         PartyLetter = party_letter, 
         SupportRaw = as.double(e$percent)/100
        )
}

berlingske_parse_poll <- function(p) {
  
    updated <- p$datetime[[1]]
    
    if(length(p$updated)>=1) updated <- p$updated[[1]]
    
    if(updated == "0000-00-00 00:00:00") updated = p$datetime[[1]] 
  
    respondents <- NA
    if(length(p$respondents)>=1) respondents <- p$respondents[[1]]
    if(!is.na(respondents)) {if (respondents == 0) respondents <- NA}
    
    results_raw <- map_dfr(p$entries, berlingske_parse_entry) %>% filter(PartyName != "")
    
    #Rescale, so that it adds up to 100%
    support_raw_sum <- results_raw %>% summarise(sum(SupportRaw)) %>% pull()
    results <- results_raw %>% mutate(Support = SupportRaw/support_raw_sum)
    
    tibble(Date = as_date(p$datetime[[1]]), 
           Updated = as_datetime(updated), 
           Respondents = as.integer(respondents),
           Results = list(results)
           )
}

berlingske_pollsets_available <- function(year = NULL, instituteName = NULL, instituteId = NULL) {
  
  overview_url <- paste(berlingskeBarometerURLBase,berlingskeBarometerOverviewFilename,sep="/")
  
  overview_xmldoc <- read_xml(overview_url)
  
  listdoc <- as_list(overview_xmldoc)
  
  pollfiles <- listdoc %>% map_dfr(parseBerlingskeOverviewInstitute)
  
  if (!is.null(year)) {
    pollfiles <- pollfiles %>% filter(Year == year)
  }
  
  if (!is.null(instituteId)) {
    pollfiles <- pollfiles %>% filter(Id == instituteId)
  }
  
  if (!is.null(instituteName)) {
    pollfiles <- pollfiles %>% filter(InstituteName == instituteName)
  }

  pollfiles  
}

berlingskeSimplify <- function(listdoc) {
  
  listdoc$polls %>% map_dfr(berlingske_parse_poll)
  
}


berlingske_pollset <- function(year, instituteId) {

  paste(paste(berlingskeBarometerURLBase,year,instituteId,sep="/"),".xml",sep="") %>% 
    map(read_xml) %>% 
    map(as_list) %>% 
    map(berlingskeSimplify)

}

berlingskeBarometerPolls <- function(year = NULL, instituteName = NULL, instituteId = NULL) {
  
  berlingske_pollsets_available(year, instituteName, instituteId) %>%  
    mutate(Polls = berlingske_pollset(Year, Id)) %>%
    unnest(Polls)
    
}

