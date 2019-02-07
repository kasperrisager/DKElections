source("sampling.R")


impose_threshold <- function(v, th) {
  vcut = v*(v >= th)
  if (sum(vcut) == 0) {
    rv <- v
  } else {
    rv <- vcut/sum(vcut)
  }
  rv
}


largest_remainders <- function(v, nseats, threshold = NULL) {
  if(is.null(threshold)) vth <- v
  else vth <- impose_threshold(v, threshold)
  floats <- nseats*vth
  wholes <- floor(floats)
  remainders <- floats - wholes
  nwhole <- sum(wholes)
  nremaining <- nseats-nwhole
  remseat <- rank(remainders,ties.method="random")>length(v)-nremaining
  wholes + remseat  
}



# Given a tibble with institute id's, party lettes and support numbers, 
# produce actual support percentages by MC
# Loops over institutes, and produces nrep outcomes for each

rsupport <- function(.data, nrep, seed = NULL) {
  
  # If there are several dates represented, they will be squased into one
  data_resummed <- .data %>% 
    group_by(Id, PartyLetter) %>%
    summarise(Alpha = sum(SupportN)) %>%
    select(Id, PartyLetter, Alpha) %>%
    arrange(Id, PartyLetter) %>%
    ungroup()
  
  ids <- data_resummed %>%
    distinct(Id) %>%
    arrange(Id) %>%
    pull()
  
  party_letters <- data_resummed %>%
    distinct(PartyLetter) %>%
    arrange(PartyLetter) %>%
    pull()
  
  alpha <- array(data_resummed$Alpha,dim=c(length(party_letters),length(ids) ))
  rownames(alpha) <- party_letters
  

  rdirich(alpha, nrep, seed)
}

# Convert to seats using largest remainders with threshold

convert_to_seats <- function(.data, nseats, threshold = NULL) {
  
  if(is.null(dim(.data))) {
    out <- largest_remainders(.data, nseats, threshold)
    names(out) <- names(.data)
  } else {
    out <- apply(.data, 2, largest_remainders, nseats=nseats, threshold=threshold)
  }
  
  out
}
