
##############################################################
### ESTIMATE EFFECTIVENESS SCORES FOR ******* COLORADO *******
##############################################################

##########################################
##### ************************************
###  DONT HAVE 2001 Specials --- Could In theory fill in somewhat from this: http://www.leg.state.co.us/2001s/inetcbill.nsf/Frameset?ReadForm&viewname=1&resultformat=2
#### **********************************
###################################

###################################
## SESSIONS:
## ---- Sessions are ANNUAl -- Bills do NOT carry over from to the next
## ---- Special Sessions folded into annual CSVs
## MEMBER LISTS:
## ---- Directories 1999 - 2015 -- http://www.leg.state.co.us/clics/cslFrontPages.nsf/PrevSessionInfo?OpenForm
## PROCESS:
## ---- VIA CO Gov: https://www.colorado.gov/pacific/sites/default/files/The%20Legislative%20Process_3.pdf
## Sponsorship/Authorship
## -- Multiple primary permitted; All bills must have a house AND senate sponsor
## -- Members can only introduce FIVE bills, excluding appropriations, per session (see process doc; only two bills if waits until Dec.; exceptions granted by a special committee)
###########################
#### OTHER NOTES:
# ---> (1) DATA FORMAT CHANGES FROM BETWEEN 1999, 2000-2015, AND 2016+
# ---> (2) New website appears to remove people who retire as sponsors --- Yields a bunch of senators sponsoring house bills... Filling back in to correct this
###########################

rm(list=ls())
options(stringsAsFactors = FALSE, scipen = 999)

library(tidyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(humaniformat)
library(purrr)
library(readxl)
library(glue)
library(readr)

this_state <- 'CO'
min_year <- 1999 ### Have 1998 but that's only half a session and 1997 doesn't have status info
max_year <- 2018
keep_types <- c("HB", "SB")
spec_elec_codes <- c('s', 'gs')
house_term_length <- 2
sen_term_length <- 4 # Staggered? YES

#### Output Directory
#dir.create(glue("~/Dropbox/Papers/Legislative Effectiveness/State Legislatures/LES_By_State/{this_state}"))
setwd(glue("~/Dropbox/Papers/Legislative Effectiveness/State Legislatures/LES_By_State/{this_state}"))

### Re-Estimate ALL SCORES
# delete <- readline(glue("For {this_state}: Do you want to DELETE ALL SCORES and REESTIMATE??? (y/n)"))
# if(delete == "y"){
#   file.remove(list.files(pattern = paste0('^', this_state, "_LES_\\d{4}_\\d{4}")))
# }

### Data Directory
data_files <- list.files(glue("~/Dropbox/Data/State Legislative Data/States/{this_state}"), full.names = TRUE)
bill_files <- data_files[grepl('Bill_Details', data_files)]

terms <- seq(min_year, max_year, 2)
sessions <- sort(gsub('.+Details_|.csv', '', bill_files))
rm(data_files, bill_files)

#### COMMEMORATIVE BILLS
commem_bills <- read.csv(glue("~/Dropbox/Data/State Legislative Data/Commem Bills/{this_state}_Commem_Bills.csv")) %>% distinct()
commem_bills$session <- gsub("-S", "-SS", commem_bills$session)

#### SUBSTANTIVE AND SIGNIFICANT BILLS
SS_bills <- read.csv("~/Dropbox/Data/State Legislative Data/Significant Bills/SS_Data/SS_Bills.csv")
SS_bills <- SS_bills %>% 
  filter(state == this_state) %>%
  rename(bill_id = bill_num) %>%
  mutate(term = ifelse(year %% 2 == 1, paste0(year, "_", year + 1), paste0(year - 1, "_", year)),
         bill_id = toupper(bill_id),
         SS = 1) %>%   
  filter(!(num_only < 1000 & bill_type == "HB") & !(num_only > 1000 & bill_type == "SB")) %>%# Won't match, but dropping anyway
  select(state, term, year, bill_id, everything()) 

#### Check SS BIll Types 
# table(SS_bills$bill_type)

### Load Klarner Data 
load("~/Dropbox/Data/US Election Data/state_leg/klarner_data/196slers1967to2016_20180908.RData")
klarner <- table; rm(table)
klarner$sab <- toupper(klarner$sab)
klarner <- filter(klarner, year > min_year - 5 & sab == this_state)

#### FIX/DROP PROBLEMATIC KLARNER OBSERVATIONS (Doubles, Multi-race candidates, etc.)
# **** Note: John Buckner 2017+ should be JANET Buckner -- she replaced him early in 2015 term (corrected manually at bottom)

###########################
## LOOP THROUGH TERMS/SESSIONS
#########################
# t <- terms[5]

for(t in terms){
  
  ### Formulate 2-year terms -- Cover both regular and special sessions
  t_yrs <- as.character(glue('{t}_{t+1}'))
  t_sessions <- sessions[grepl(gsub('_', '|', t_yrs), sessions)]
  
  ### Skip Previously Estimated
  if(glue("{this_state}_LES_{t_yrs}.csv") %in% list.files('.')){
    print(glue('. \n ~~~~ SKIPPING {t_yrs} session ---> LES scores already estimated! \n.'))
    next
  }
  
  ###### SESSION IN PROGRESS
  print(glue('. \n . \n ~~~~~~~~~~~~ ESTIMATING SCORES FOR THE {t_yrs} TERM! ~~~~~~~~~~~~~~ '))
  
  ############### Read in data
  bill_path <- glue("~/Dropbox/Data/State Legislative Data/States/{this_state}/{this_state}_Bill_Details_{t_sessions[1]}.csv")
  bills <- read.csv(bill_path)   
  
  ### Need to Fix 2015 Column Names To Match New 2016 Format
  if(t_yrs == "2015_2016"){
    bills <- rename(bills, primary_sponsors = sponsors)
  }
  
  ### If multiple sessions in different files, read in those as well
  if(length(t_sessions) > 1){
    for(s in t_sessions[2:length(t_sessions)]){
      bill_path <- glue("~/Dropbox/Data/State Legislative Data/States/{this_state}/{this_state}_Bill_Details_{s}.csv")
      s_bills <- read.csv(bill_path)
      bills <- bind_rows(bills, s_bills)
    }
    rm(s, s_bills)
  }
  
  ### Clean Term/Session Variables
  bills$term <- t_yrs
  bills$session <- paste(bills$session, bills$session_type, sep = '-')
  bills$session <- gsub("-S", "-SS", bills$session)
  bills <- select(bills, -session_type)
  
  ###### Drop duplicates
  # *** Note: Series of duplicates throughout with different bill_urls, and more/less complete histories 
  # *** Keeping only one -- won't affect history coding as history records pulled from all
  bills <- distinct(bills, bill_number, term, session, title, sponsors, .keep_all = TRUE)
  
  ######## Standardize the Bill ID Var
  bills <- rename(bills, bill_id = bill_number) 
  
  #### Ensure correct padding on Bill IDs
  bills$bill_id <- ifelse(grepl('-[0-9][0-9][0-9]$', bills$bill_id), paste(gsub('-.+', '', bills$bill_id), str_pad(str_trim(gsub('.+-|\n', '', bills$bill_id)), 4, pad = '0'), sep = '-'), bills$bill_id)
  
  ############### Drop Resolutions, Messages, Communications, Reports
  bills <- mutate(bills, bill_type = toupper(gsub('[0-9].+|[0-9]+', '', bill_id)))
  # table(bills$bill_type)
  bills <- filter(bills, bill_type %in% keep_types) %>% select(-bill_type)
  
  ############### Standardize Sponsors
  if(t >= 2015){
    bills$sponsor <- gsub(';.+', '', bills$primary_sponsors)
    bills$sponsor <- gsub('^Rep\\. |^Sen\\. ', '', bills$sponsor)
  }
  
  #### Standardizing Accents in Names --- If left as is, will not match correctly to Klarner Data
  bills$sponsor <- gsub(';.+|/.+', '', tolower(bills$sponsor))
  bills$sponsor <- gsub('á', 'a', bills$sponsor)
  bills$sponsor <- gsub('é', 'e', bills$sponsor)
  bills$sponsor <- gsub('ó', 'o', bills$sponsor)
  bills$sponsor <- gsub('í', 'i', bills$sponsor)
  
  #### Adjusting for Bills By Request -- Need to do BEFORE splitting
  if(any(grepl('\\(br\\)|by request| br$', bills$sponsor))){
    print("-----> BY REQUEST BILLS -- ADAPT SCRIPT --- BREAK"); break
    #print(glue("-----> KEEPING {nrow(filter(bills, grepl('(br)|by request', introducers)))} bill(s) introduced BY REQUEST"))
  }
  
  #### DROP Committee Bills
  if(any(grepl('committee', bills$sponsor))){
    print(glue("-----> DROPPING {nrow(filter(bills, grepl('committee', sponsor)))} bill(s) introduced BY COMMITTEE"))
    break
    # bills <- filter(bills, !grepl('committee', LES_sponsor))
  }
  
  ##### Manual Name Fixes
  if(t_yrs == '1999_2000'){
    bills[bills$sponsor == 'swilliams',]$sponsor <- 'williams s.'
    bills[bills$sponsor == 'twilliams',]$sponsor <- 'williams t.'
  }else if(t_yrs == '2001_2002'){
    # Two Jim Dyers: Jim E. Dyer (D6, durango, https://www.ourcampaigns.com/CandidateDetail.html?CandidateID=136307) 
    # AND Jim F. Dyer (D26, arapahoe, https://www.ourcampaigns.com/CandidateDetail.html?CandidateID=7041)
    # --> Jim E. Dyer resigns in March 2001 -- http://archive.cortezjournal.com/archives/1news1182.htm
    bills[bills$sponsor == 'dyer (durango)',]$sponsor <- 'dyer, jim e.'
    bills[bills$sponsor %in% c('dyer', 'dyer (arapahoe)'),]$sponsor <- 'dyer, jim f.'
  }else if(t_yrs == '2005_2006'){ ## James Kerr appointed in 2005; andrew kerr appointed 2006 ---> 'Kerr' == 'Kerr j.'
    bills[bills$sponsor == 'kerr',]$sponsor <- 'kerr j.'
  }else if(t_yrs == '2007_2008'){# Garza-Hicks recorded as Hicks in 2007-RS
    bills[bills$sponsor == 'hicks',]$sponsor <- 'garza-hicks'
  }else if(t_yrs == '2015_2016'){ ## Fixing a HB with Senate Sponsor
    bills[bills$bill_id == 'HB16-1192',]$sponsor <- 'd. kagan'
  }else if(t_yrs == '2017_2018'){ ## Bunch of sponsor errors -- Must happen when someone resigns???
    bills[bills$bill_id == 'HB17-1277',]$sponsor <- 'd. mitsch bush' # Was Humenik
    bills[bills$bill_id == 'HB17-1166',]$sponsor <- 'c. navarro' # Was Grantham
    bills[bills$bill_id == 'HB17-1104',]$sponsor <- 'c. navarro' # Was priola
    bills[bills$bill_id == 'HB17-1044',]$sponsor <- 'd. mitsch bush' #Was N. Todd
    bills[bills$bill_id == 'HB17-1150',]$sponsor <- 'c. navarro' # Was O. Hill
    bills[bills$bill_id == 'HB18-1133',]$sponsor <- 's. lebsock' # Was Marble
    bills[bills$bill_id == 'SB18-0043',]$sponsor <- 'k. grantham' # Was Lundeen
  }
  
  #### LES Sponsor Variable --- Format = Last first initial. (needed)
  bills$LES_sponsor <- str_trim(bills$sponsor)
  
  ### Need to Flip the First Initials for the Split Term (from beginning to end)
  ### Also need to match those without intials in 2015 to those with initials in 2016
  if(t_yrs == '2015_2016'){
    bills$LES_sponsor <- ifelse(bills$session == '2016-RS', paste0(gsub('^[a-z]\\. ', '', bills$LES_sponsor), ' ', gsub(' .+', '', bills$LES_sponsor)),bills$LES_sponsor)
    for(spon in unique(bills[bills$session == '2015-RS',]$LES_sponsor)){
      matches <- filter(bills, session == '2016-RS' & grepl(paste0(spon, ' '), LES_sponsor)) %>% distinct(LES_sponsor) %>% unlist() %>% unname()
      if(length(matches) == 1 & !is.na(matches[1])){
        bills[bills$LES_sponsor == spon & bills$session == '2015-RS',]$LES_sponsor <- matches
        # print(glue('{spon} ------> {matches}'))
      }
    }
    rm(spon, matches)
  }
  # sort(table(bills$LES_sponsor))
  
  ###### CHeck Missing Sponsors
  # filter(bills, LES_sponsor == "") %>% View()
  if(nrow(filter(bills, LES_sponsor == '')) > 0){
    print(glue("-----> Dropping {nrow(filter(bills, LES_sponsor == ''))} bill(s) without a sponsor"))
    bills <- filter(bills, !(LES_sponsor == ''))     
  }

  
  ###################
  ###### Merge in S&S Bills
  ###################
  # *** For COLORADO: BILLS do NOT Carryover, but bill numbers include year
  # *** Two specials in 2001, One Special Each in 2002, 2006, 2012 (though 2001/2002 = dropped as no history data)
  # ---> Capping Bill numbers at SESSION MAX and assuming bill is from SS with most bills proposed
  SS_term <- SS_bills %>% filter(term == t_yrs) %>% mutate(H_max = NA, S_max = NA, s_spec = NA, any_specials = any(grepl(paste0("-SS"), bills$session)))
  
  ## Adjusting Max Specials by yr
  for(yr in as.numeric(str_split(t_yrs, "_")[[1]]) ){
    if(any(grepl(paste0(yr, "-SS"), bills$session))){
      H_max <- filter(bills, grepl(yr, session) & grepl("^H", bill_id) & grepl("SS", session)) %>% mutate(num = as.numeric(gsub('.+-', '', bill_id))) %>% pull(num)
      H_max <- ifelse(length(H_max) >= 1, max(H_max), NA)
      S_max <- filter(bills, grepl(yr, session) & grepl("^S", bill_id) & grepl("SS", session)) %>% mutate(num = as.numeric(gsub('.+-', '', bill_id))) %>% pull(num)
      S_max <- ifelse(length(S_max) >= 1, max(S_max), NA)
      which_spec <- which(table(bills[grepl(yr, bills$session) & grepl("SS", bills$session),]$session) == max(table(bills[grepl(yr, bills$session) & grepl("SS", bills$session),]$session)))[1]
      which_spec <- names(table(bills[grepl(yr, bills$session) & grepl("SS", bills$session),]$session)[which_spec])
      SS_term[SS_term$year == yr,]$H_max <- H_max
      SS_term[SS_term$year == yr,]$S_max <- S_max
      SS_term[SS_term$year == yr,]$s_spec <- which_spec
      rm(H_max, S_max, which_spec)
    }
  }; rm(yr)
  
  SS_term <- SS_term %>%
    mutate(special = ifelse((grepl("^H", bill_id) & is.na(H_max)) | (grepl("^S", bill_id) & is.na(S_max)), 0, special),
           special = ifelse(!any_specials, 0, special),
           special = ifelse(grepl("^H", bill_id) & !is.na(H_max) & special == 1 & num_only > H_max, 0, special),
           special = ifelse(grepl("^S", bill_id) & !is.na(S_max) & special == 1 & num_only > S_max, 0, special),
           session = ifelse(special == 0, paste0(year, '-RS'), ifelse(is.na(special_num), s_spec, paste0(year, '-SS', special_num)))) %>%
    distinct(term, session, bill_id, SS)
  
  ### Adjust SS Bill Numbers --> HB06S-0123
  # filter(bills, grepl("SS", session)) %>% select(1:5)
  SS_term <- mutate(SS_term, bill_id = ifelse(grepl("SS", session) & !grepl("[0-9][0-9]S-", bill_id), gsub("-", 'S-', bill_id), bill_id))
  
  ### Merge
  bills <- bills %>%
    left_join(SS_term, by = c("bill_id", "term", "session")) %>%
    mutate(SS = ifelse(is.na(SS), 0, SS))
  
  ### Check Missing
  # table(bills$SS)
  # anti_join(SS_term, bills, by = c("bill_id", "term", "session"))
  
  
  ####################################
  ############### Code Commemorative
  #########################################
  bills <- commem_bills %>%
    filter(term == t_yrs) %>%
    select(bill_id, term, session, commem) %>%
    left_join(bills, ., by = c('bill_id', 'term', 'session'))
  # table(bills$commem)
  
  #### Not Commemorative if SS
  bills$commem <- ifelse(bills$SS == 1 & bills$commem == 1, 0, bills$commem)
  
  #############################################################
  ############### Code Bill History
  ##########################################################
  bill_hist_path <- glue("~/Dropbox/Data/State Legislative Data/States/{this_state}/{this_state}_Bill_Histories_{t_sessions[1]}.csv")
  bill_hist <- read.csv(bill_hist_path)
  
  ## If multiple sessions, read in those as well
  if(length(t_sessions) > 1){
    for(s in t_sessions[2:length(t_sessions)]){
      bill_path <- glue("~/Dropbox/Data/State Legislative Data/States/{this_state}/{this_state}_Bill_Histories_{s}.csv")
      s_hist <- read.csv(bill_path)
      bill_hist <- bind_rows(bill_hist, s_hist)
    }
    rm(s, s_hist)
  }
  
  ######## Standardize BillHist Bill IDs
  bill_hist <- rename(bill_hist, bill_id = bill_number) #%>% mutate(bill_id = toupper(bill_id))
  
  ### Clean Term/Session Variables
  bill_hist$term <- t_yrs
  bill_hist$session <- paste(bill_hist$session, bill_hist$session_type, sep = "-")
  bill_hist$session <- gsub("-S", "-SS", bill_hist$session)
  bill_hist <- select(bill_hist, -session_type) %>% 
    arrange(session, bill_id, order)
  
  ### DROPPING SPECIAL SESSION BILLS WITH NO HISTORY INFO
  for(s_id in unique(bills$session)){
    if(!(s_id %in% unique(bill_hist$session))){
      cat('\n'); cat(glue('----> Dropping {nrow(bills[bills$session == s_id,])} bills from {s_id} -- NO ACTIONS!'))
      bills <- filter(bills, session != s_id)
    }
  }
  
  #### Ensure correct padding on Bill IDs
  bill_hist$bill_id <- ifelse(grepl('-[0-9][0-9][0-9]$', bill_hist$bill_id), paste(gsub('-.+', '', bill_hist$bill_id), str_pad(str_trim(gsub('.+-|\n', '', bill_hist$bill_id)), 4, pad = '0'), sep = '-'), bill_hist$bill_id)
  
  ### CODE CHAMBER  --- Coding transitions, then coding specific mentions, then filling in gaps
  # *** This works for 1998-1999 Format AND 2000 - 2015 Format
  # ---> Not need for 2016 plus because we know chamber
  # ---> Need to be careful with 2015_2016 Term!
  if(t <= 2014){
    bill_hist$chamber <- ifelse(grepl('introduced in house|sent to senate|sent back to senate|^house|speaker of the house', tolower(bill_hist$action)), 'House', NA)
    bill_hist$chamber <- ifelse(grepl('introduced in senate|sent to house|sent back to house|^senate|president of the senate', tolower(bill_hist$action)), 'Senate', bill_hist$chamber)
    bill_hist$chamber <- ifelse(grepl('H[A-Z][a-z]|H2nd|H3rd', bill_hist$action), 'House', bill_hist$chamber)
    bill_hist$chamber <- ifelse(grepl('S[A-Z][a-z]|S2nd|S3rd', bill_hist$action), 'Senate', bill_hist$chamber)
    bill_hist$chamber <- ifelse(grepl('bill is signed into law|bill is vetoed|^governor|sent to the governor', tolower(bill_hist$action)), 'Governor', bill_hist$chamber)
    bill_hist$chamber <- ifelse(grepl('consideration.+veto|override', tolower(bill_hist$action)), ifelse(substring(bill_hist$bill_id,1,1) == "S", 'Senate', "House"), bill_hist$chamber)
    bill_hist <- bill_hist %>% group_by(bill_id, session) %>% fill(chamber) %>% ungroup()
  }else if(t == 2015){
    code_rows <- filter(bill_hist, session == '2015-RS')
    code_rows$chamber <- ifelse(grepl('introduced in house|sent to senate|sent back to senate|^house|speaker of the house', tolower(code_rows$action)), 'House', NA)
    code_rows$chamber <- ifelse(grepl('introduced in senate|sent to house|sent back to house|^senate|president of the senate', tolower(code_rows$action)), 'Senate', code_rows$chamber)
    code_rows$chamber <- ifelse(grepl('H[A-Z][a-z]|H2nd|H3rd', code_rows$action), 'House', code_rows$chamber)
    code_rows$chamber <- ifelse(grepl('S[A-Z][a-z]|S2nd|S3rd', code_rows$action), 'Senate', code_rows$chamber)
    code_rows$chamber <- ifelse(grepl('bill is signed into law|bill is vetoed|^governor|sent to the governor', tolower(code_rows$action)), 'Governor', code_rows$chamber)
    code_rows$chamber <- ifelse(grepl('consideration.+veto|override', tolower(code_rows$action)), ifelse(substring(code_rows$bill_id,1,1) == "S", 'Senate', "House"), code_rows$chamber)
    code_rows <- code_rows %>% group_by(bill_id, session) %>% fill(chamber) %>% ungroup()
    bill_hist <- filter(bill_hist, session != '2015-RS') %>% bind_rows(., code_rows) %>% arrange(session, bill_id, order)
    rm(code_rows)
  }

  ### Generalizing Committee Mentions for 1999 (shouldn't affect 2000)
  if(t <= 1999){ 
   bill_hist$action <- gsub('H[A-Z][a-z]+ [A-Z][a-z]+|H[A-Z][a-z]+|^H[A-Z][A-Z]+', 'h_committee', bill_hist$action)   
   bill_hist$action <- gsub('S[A-Z][a-z]+ [A-Z][a-z]+|S[A-Z][a-z]+|^S[A-Z][A-Z]+', 's_committee', bill_hist$action)   
  }
  
  ## Standardize Chamber Variable
  # bill_hist$chamber <- recode(toupper(bill_hist$chamber), "H" = "House", "S" = "Senate")
  
  #### Load function to take a given bill history  and code legislative stages
  # **** Function requires an action column, a chamber column, and that the actions be in the correct order (so may need to reverse in some cases)
  # **** Can skip the chamber sorting with ignore_chamber = True -- Will take all actions from the initiating chamber
  # **** add_chamb = "name of chamber" will subset to intiating chamber and any other addition (e.g., "joint' or "executive")
  source('~/Dropbox/Papers/Legislative Effectiveness/State Legislatures/Estimate LES/code_billhist_fx.R')
  
  ##### Set State-Specific Terms for Identifying Each Stage BY YEAR
  #### Terms: https://leg.colorado.gov/agencies/office-legislative-legal-services/legislative-lingo
  
  #### 1999 or Earlier
  aic_t_99 <- c('_committee amends', '_committee refers', '_committe.+sends to floor', '_committe.+sends bill to floor', 
                '_committee send to [a-z]_committee', '_committee sends bill to [a-z]_committee',
                "_committee pi's this bill")
  # --> PI = Postpones indefinitely --> Implies a hearing -- see terms
  abc_t_99 <- c('_committe.+sends to floor', '_committe.+sends bill to floor', 
                'h2nd', 'h3rd', 's2nd', 's3rd', '^bill is amended', '^bill is lost', '^bill is laid over')
  pc_t_99 <- c('bill passes h3rd', 'sent to senate', 'bill passes s3rd', 'senate to house')
  law_t_99 <- c('signed into law', 'bill becomes law')
  
  #### 2000 to 2015+ ---> FORMAT Stays the same with new website in 2016
  aic_t <- c('committee on.+pass unamended', 'committee on.+amended', 'committee.+postpone indefin',
             'committee.+lay over', 'committee on.+refer.+to', 'witness testimony', 'committee discussion',
             'committee.+to (house|senate) committee of the whole')
  ## Small changes over time, should be accounted for with prhrasing
  abc_t <- c('committee.+to (house|senate) committee of the whole', 'second reading', 'third reading')
  pc_t <- c('^house third reading passed', '^senate third reading passed')
  law_t <- c('governor signed', 'governor became law', 'governor partial veto', 
             'governor action - signed', 'governor action - became law', 'governor action - partial veto')
  # --> Phrasing for gov changes a bit over time, adds 'action -' sometimes
  
  # filter(bill_hist, grepl('governor', tolower(action))) %>% select(bill_id, action)
  # filter(bill_hist, bill_id == 'SB0541') %>% select(chamber, action_date, action, order)
  
  ####################
  ### Output Matrix
  all_bill_stages = tibble(bill_id = character(0),
                           term = character(0),
                           session = character(0),
                           LES_sponsor = character(0),
                           introduced = integer(0),
                           action_in_comm = integer(0),
                           action_beyond_comm = integer(0),
                           passed_chamber = integer(0),
                           law = integer(0),
                           bill_url = character(0))
  
  ### Code History Stages for Each Bill --- Subset Hist File, Code, Save, Repeat
  options(warn = 2)
  for(i in 1:nrow(bills)){
    b_id = bills[i,]$bill_id
    s_id = bills[i,]$session
    b_spon = bills[i,]$LES_sponsor
    hist_sub <- filter(bill_hist, bill_id == b_id, session == s_id) %>% distinct()
    if(as.numeric(substring(s_id, 1, 4)) <= 1999){
      bill_stages <- evaluate_bill_hist(hist_sub, b_id, t_yrs, s_id, b_spon, aic_t_99, abc_t_99, pc_t_99, law_t_99)
    }else{
      bill_stages <- evaluate_bill_hist(hist_sub, b_id, t_yrs, s_id, b_spon, aic_t, abc_t, pc_t, law_t)
      
      ### Add Check for Passage -- Occassionally Introduced in Outchamber listed before passage of intro chamber when happen on same date
      if(bill_stages$passed_chamber == 0 & any(grepl("third reading passed", tolower(hist_sub$action)))){
        bill_stages$action_beyond_comm <- bill_stages$passed_chamber <- 1
      }
      ### Sane as Above but for AIC
      chamb <- ifelse(grepl("^S", b_id), 'senate', 'house')
      if(bill_stages$action_in_comm == 0 & any(grepl(paste0(chamb, ' committee on.+'), tolower(hist_sub$action))) ){
        bill_stages$action_in_comm <- 1
      }
    }
    bill_stages$bill_url <- bills[i,]$bill_url
    all_bill_stages <- bind_rows(all_bill_stages, bill_stages)
    # print(i)
  }
  options(warn = 1)
  
  ### Check Codings
  cat('\n')
  all_bill_stages %>% mutate(chamber = substring(bill_id, 1,1)) %>% group_by(session, chamber) %>% 
    summarize(N = n(), AIC = sum(action_in_comm), ABC = sum(action_beyond_comm), PASS = sum(passed_chamber), LAW = sum(law)) %>% as.data.frame() %>% print()
  # filter(bill_hist, bill_id %in% all_bill_stages[all_bill_stages$action_in_comm == 0,]$bill_id) %>% View()
  # filter(bill_hist, bill_id %in% all_bill_stages[all_bill_stages$action_in_comm == 0 & all_bill_stages$action_beyond_comm == 1,]$bill_id) %>% View()
  
  ### MERGE to BILL DATA
  bills <- left_join(bills, all_bill_stages, by = intersect(colnames(bills), colnames(all_bill_stages))) 
  
  ### MERGE In COMMEMS
  all_bill_stages <- commem_bills %>%
    filter(term == t_yrs) %>%
    select(bill_id, term, session, commem) %>%
    left_join(all_bill_stages, ., by = c('bill_id', 'term', 'session'))
  
  ### MERGE In S&S
  all_bill_stages <- SS_term %>%
    filter(term == t_yrs) %>%
    select(bill_id, term, session, SS) %>%
    left_join(all_bill_stages, ., by = c('bill_id', 'term', 'session')) %>% 
    mutate(SS = ifelse(is.na(SS), 0, SS))
  rm(SS_term)
  
  ### Adjust Commems if SS == 1
  all_bill_stages$commem <- ifelse(all_bill_stages$SS == 1 & all_bill_stages$commem == 1, 0, all_bill_stages$commem)
  
  ### Save Stage Info **** MERGE WITH COMMEM + SS
  if(!dir.exists(glue('~/Dropbox/Data/State Legislative Data/Bill_Stage_Codings/{this_state}'))){
    dir.create(glue('~/Dropbox/Data/State Legislative Data/Bill_Stage_Codings/{this_state}'))
  }
  write.csv(all_bill_stages, glue('~/Dropbox/Data/State Legislative Data/Bill_Stage_Codings/{this_state}/{this_state}_{t_yrs}_Bill_Stage_Codings.csv'), row.names = FALSE )
  
  rm(bill_hist_path, hist_sub, bill_stages, i, all_bill_stages, evaluate_bill_hist, aic_t, abc_t, pc_t, law_t)
  rm(b_id, s_id, b_spon, chamb, bill_hist, aic_t_99, abc_t_99, pc_t_99, law_t_99)
  
  ########################################################################
  ############### Identify Unique Legislators via SLER
  ########################################################
  ## Import and Clean Sponsors Name to Match
  all_sponsors <- bills %>%
    mutate(chamber = ifelse(substring(bill_id, 1, 1) == 'H', "H", "S")) %>%
    select(LES_sponsor, chamber, passed_chamber, law) %>%
    mutate(term = t_yrs) %>%
    group_by(LES_sponsor, chamber, term) %>%
    summarize(num_sponsored_bills = n(), 
              sponsor_pass_rate = sum(passed_chamber) / n(),
              sponsor_law_rate = sum(law) / n()) %>%
    ungroup()
  
  ######## CO --- Formats too Inconsistent for coosponsorship info?????
  all_sponsors$num_cosponsored_bills <- NA
  # for(i in 1:nrow(all_sponsors)){
  #   c_sub <- filter(bills, substring(bill_id, 1, 1) == all_sponsors[i,]$chamber)
  #   all_sponsors$num_cosponsored_bills[i] <- sum(grepl(all_sponsors[i,]$LES_sponsor, tolower(c_sub$sponsor)))
  #   ### ONLY need to adjust this way if sponsored and cosponsored column are the same
  #   all_sponsors$num_cosponsored_bills[i] <- all_sponsors$num_cosponsored_bills[i] - all_sponsors$num_sponsored_bills[i]
  # }
  # all_sponsors <- select(all_sponsors, -cospon_match)
  # View(select(all_sponsors, LES_sponsor, num_sponsored_bills, num_cosponsored_bills))
  
  if(t <= 2015){
    all_sponsors$last_name <- gsub(' [a-z]\\.$', '', all_sponsors$LES_sponsor)
    all_sponsors$first_name <- str_trim(gsub('\\.$', '', str_extract(all_sponsors$LES_sponsor, ' [a-z]\\.$')))
    all_sponsors$first_name <- ifelse(is.na(all_sponsors$first_name), '', all_sponsors$first_name)
  }else{
    all_sponsors$last_name <- gsub('^[a-z]\\. ', '', all_sponsors$LES_sponsor)
    all_sponsors$first_name <- gsub('\\.$', '', str_trim(str_extract(all_sponsors$LES_sponsor, '^[a-z]\\. ')))
    all_sponsors$first_name <- ifelse(is.na(all_sponsors$first_name), '', all_sponsors$first_name)
  }
  all_sponsors <- arrange(all_sponsors, chamber, LES_sponsor)
  
  #### Update Names for Matching 
  if(t_yrs %in% c('2007_2008', '2009_2010', '2011_2012') ){
    all_sponsors[all_sponsors$LES_sponsor == 'gardner b.', ]$first_name <- 'r' # Robert 'Bob'
  }
  if(t_yrs %in% c('2013_2014', '2015_2016', '2017_2018') ){
    all_sponsors[all_sponsors$LES_sponsor %in% c('mitsch bush', 'mitsch bush d.', 'd. mitsch bush'), ]$last_name <- 'bush'
    all_sponsors[all_sponsors$LES_sponsor %in% c('navarro', 'navarro c.', 'c. navarro'), ]$last_name <- 'navarroratzlaff'
  }
  if(t_yrs %in% c('2015_2016', '2017_2018') ){
    all_sponsors[all_sponsors$LES_sponsor %in% c('martinez humenik b.', 'b. martinez humenik'), ]$last_name <- 'humenik'
  }
  if(t_yrs == '2017_2018'){
    all_sponsors[all_sponsors$LES_sponsor == 'j. coleman', ]$last_name <- 'rashadcoleman' 
    all_sponsors[all_sponsors$LES_sponsor == 'b. mclachlan', ]$last_name <- 'hallmclachlan' 
  }
  
  ###############
  ### Subset Klarner State Leg. Election Data ---> Need to Account for Election Timing (Specials and Senate)
  ################

  ### Account for Staggered Senate Terms 
  H_elec_year <- as.numeric(substring(t_yrs, 1, 4)) - 1
  S_elec_year <- H_elec_year - 2 ### Need to get T - 1 and T - 3
  
  ### For Senate: Senate Election Year through House Year + 1 (so if 2000, 2000-2001; if 1998, 1998 to 2001)
  klarner_H <- filter(klarner, sab == this_state & sen == 0 & (year %in% H_elec_year:(H_elec_year + 1) | (year == H_elec_year + 2 & etype %in% spec_elec_codes )))
  klarner_S <- filter(klarner, sab == this_state & sen == 1 & (year %in% S_elec_year:(H_elec_year + 1) | (year == H_elec_year + 2 & etype %in% spec_elec_codes )))
  klarner_sub <- suppressWarnings(bind_rows(klarner_H, klarner_S)); rm(klarner_H, klarner_S)
  
  ### Subset to Winners of Full Terms and Special Elections
  klarner_sub <- filter(klarner_sub, outcome == 'w' & (deter == 1 | etype %in% spec_elec_codes)) %>%
    select(year, sab, sfips, sen, ddez, dname, dno, etype, deter, cand, candid, party, partyz) %>%
    distinct() ### Need to Subset to one result per candidate (later terms are recorded by precint...)
  
  ####################################################
  ###### Match Sponsors Names to Klarner Data, Forgoing Function because mostly only have last name
  ####################################################
  
  ### Create Match Variables
  klarner_sub$last_name <- gsub(',.+', '', klarner_sub$cand)
  all_sponsors$match_name <- tolower(paste0(all_sponsors$last_name, ifelse(all_sponsors$first_name != '', paste0(", ", all_sponsors$first_name), '')))
  klarner_sub$match_name <- tolower(unlist(str_extract(klarner_sub$cand, '[a-zA-z]+, [a-zA-z]')))
  
  #### FIx Match Names
  if(t_yrs == '2001_2002'){
    all_sponsors[all_sponsors$LES_sponsor == 'dyer, jim e.', c('first_name', 'last_name', 'match_name')] <- list('jim', 'dyer', 'dyer, jim e.') 
    all_sponsors[all_sponsors$LES_sponsor == 'dyer, jim f.', c('first_name', 'last_name', 'match_name')] <- list('jim', 'dyer', 'dyer, jim f.') 
    klarner_sub[klarner_sub$cand == 'dyer, jim',]$match_name <- 'dyer, jim e.' 
    klarner_sub[klarner_sub$cand == 'dyer, jim f.',]$match_name <- 'dyer, jim f.' 
  }
  
  ### Subset out General Specials
  klarner_gs <- filter(klarner_sub, etype == 'gs')
  klarner_sub <- filter(klarner_sub, etype != 'gs')
  
  ### Drop Duplicated Klarner Candidats
  klarner_sub <- arrange(klarner_sub, desc(year)) %>% group_by(sen) %>% filter(!duplicated(cand)) %>% ungroup()
  
  all_sponsors$elec_year <- all_sponsors$klarner_id <- all_sponsors$klarner_name <- NA
  all_sponsors$klarner_name <- as.character(all_sponsors$klarner_name)
  all_sponsors$klarner_id <- as.double(all_sponsors$klarner_id)
  all_sponsors$elec_year <- as.double(all_sponsors$elec_year)
  
  ### Loop though and Match
  for(i in 1:nrow(all_sponsors)){
    k_matches <- filter(klarner_sub, last_name == tolower(all_sponsors[i,]$last_name)  & sen == ifelse(all_sponsors[i,]$chamber == "S", 1, 0))
    
    ## Acccount for lack of punctuation or spaces in Klarner Data
    if(nrow(k_matches) == 0 & grepl("-|'| |\\.|`", all_sponsors[i,]$last_name)){
      k_matches <- filter(klarner_sub, last_name == gsub("-|'| ||`", '', tolower(all_sponsors[i,]$last_name)))
    }
    
    ## Check Partial Names (e.g., maiden_name-last_name)
    if(nrow(k_matches) == 0 & grepl("-", all_sponsors[i,]$last_name)){
      k_matches <- filter(klarner_sub, last_name == gsub(".+-", '', tolower(all_sponsors[i,]$last_name)))
    }  
    
    ## Check GS 
    if(nrow(k_matches) == 0 & nrow(klarner_gs) >= 1){
      k_matches <- filter(klarner_gs, last_name == tolower(all_sponsors[i,]$last_name)  & sen == ifelse(all_sponsors[i,]$chamber == "S", 1, 0))
      if(nrow(k_matches) == 0){
        k_matches <- filter(klarner_gs, last_name == gsub("-|'| ||`", '', tolower(all_sponsors[i,]$last_name)))
      }
    }
    
    ### Save and Cross-Check
    if(nrow(k_matches) == 1){
      all_sponsors[i, ]$klarner_name <- k_matches$cand
      all_sponsors[i, ]$klarner_id <- k_matches$candid
      all_sponsors[i, ]$elec_year <- k_matches$year     
    } else if(nrow(k_matches) > 1 & length(unique(k_matches$cand)) != 1){
      ## Check Last, First
      m_sub <- grep(all_sponsors[i,]$match_name, k_matches$match_name)
      ### Check Without Punctuation --- Can't remove spaces unless do it for all_sponsors and k_matches
      if(length(m_sub) == 0){
        m_sub <- grep(gsub("-|'|`", '', all_sponsors[i,]$match_name), k_matches$match_name)        
      }
      ## Check First Initial
      # if(length(m_sub) == 0){
      #   match_name2 <- tolower(paste0(all_sponsors[i,]$last_name, ", ", substring(all_sponsors[i,]$first_name, 1, 1) ))
      #   m_sub <- grep(match_name2, tolower(unlist(str_extract(k_matches$cand, '[a-zA-z]+, [a-zA-z]'))))
      # }
      ### Save if Match
      if(length(m_sub) == 1){
        all_sponsors[i, ]$klarner_name <- k_matches[m_sub,]$cand
        all_sponsors[i, ]$klarner_id <- k_matches[m_sub,]$candid
        all_sponsors[i, ]$elec_year <- k_matches[m_sub,]$year     
      } else{
        print(glue("MULTIPLE MATCHES ::: {all_sponsors[i,]$LES_sponsor} ::: {i} ::: CHAMBER: {all_sponsors[i,]$chamber}"))
      }
    } else if(nrow(k_matches) > 1 & length(unique(k_matches$cand)) == 1){
      all_sponsors[i, ]$klarner_name <- unique(k_matches$cand)
      all_sponsors[i, ]$klarner_id <- unique(k_matches$candid)
      eyear <- as.numeric(str_split(t_yrs, "\\_")[[1]][1]) - 1
      all_sponsors[i, ]$elec_year <- k_matches[which(abs(k_matches$year - eyear) == min(abs(k_matches$year - eyear))),]$year
      rm(eyear)
    } else{
      print(glue("MISSING MATCH ::: {all_sponsors[i,]$LES_sponsor} ::: {i} ::: CHAMBER: {all_sponsors[i,]$chamber}"))
    }
  }
  #select(all_sponsors, LES_sponsor, klarner_name) %>% View()
  
  #### Fix Mismatches
  # if(t_yrs == "2009_2010"){
  #   all_sponsors[all_sponsors$LES_sponsor == "art noonan" & all_sponsors$chamber == "H", c("klarner_name", "klarner_id", "elec_year")] <- NA
  # }
  
  #### Check for Duplicates from Matching
  duplicates <- filter(all_sponsors, !is.na(klarner_name)) %>% group_by(chamber, klarner_name) %>% filter(n() > 1)
  if(nrow(duplicates) > 0){
    cat('-----> Check DUPLICATE Sponsors \n ')
    print(select(duplicates, LES_sponsor, klarner_name, chamber) %>% as.data.frame())
  }; rm(duplicates)
  
  ##### CHECK IF ANY KLARNER CANDIDATES MISSING FROM DATA --- Exclude Senators elected T-1
  km <- filter(klarner_sub, !(candid %in% all_sponsors$klarner_id) )
  km <- filter(km, sen == 0 | (sen == 1 & year == as.numeric(substring(t_yrs, 1, 4)) - 1 ))

  ### Remove candidates who won but were never seated
  if(t_yrs == "2005_2006"){
    km <- filter(km, cand != 'lee, don')
    km <- filter(km, cand != 'tochtrop, lois')
  }else if(t_yrs == "2007_2008"){
    km <- filter(km, cand != "cloer, mark")
  }else if(t_yrs == '2011_2012'){
    km <- filter(km, cand != 'scanlan, christine')
    km <- filter(km, cand != 'romer, chris')
  }
  
  #### Add Candidates who WON but Sponsored NO BILLS into Data
  if(nrow(km) > 0){
    cat(glue("-----> {as.numeric(substring(t_yrs, 1, 4)) - 1} KLARNER CANDIDATES MISSING MATCHES IN DATA ~~> ADDING INTO LIST OF LEGISLATORS: \n . "))
    print(select(km, year, sab, sen, ddez, etype, deter, cand, candid, partyz, match_name) %>% as.data.frame())
    chamb <- ifelse(km$sen == 1, "S", "H")
    for(i in 1:nrow(km)){
      all_sponsors <- add_row(all_sponsors, chamber = chamb[i], term = t_yrs, klarner_name = km$cand[i], klarner_id = km$candid[i], elec_year = km$year[i])
    }
    rm(chamb)
  }
  
  #### Clean
  legis_data <- all_sponsors %>%
    rename(data_name = LES_sponsor) %>%
    mutate(sponsor = ifelse(!is.na(klarner_name), klarner_name, tolower(match_name))) %>%
    select(sponsor, data_name, klarner_name, klarner_id, chamber, term, elec_year, num_sponsored_bills, sponsor_pass_rate, sponsor_law_rate, num_cosponsored_bills) %>%
    arrange(chamber, sponsor)
  
  ###############################################################33
  ##### Estimate Scores + Add in Relatd Variables
  #################################################################
  
  ### Check if bills in data without an ID'd sponsor
  bills <- bills %>% select(-sponsor) %>%
    rename(sponsor = LES_sponsor) %>%
    mutate(chamber = ifelse(substring(bill_id, 1, 1) == 'H', 'H', 'S'))
  
  ### Standard LES: Same as Congressional Measure
  cat('------> Estimating LES Scores ')
  source('~/Dropbox/Papers/Legislative Effectiveness/State Legislatures/Estimate LES/calc_LES_fx.R')
  
  LES <- calc_LES(bills, legis_data, t_yrs, ss_weight = 10, reg_weight = 5, com_weight = 1, stage_weights = c(1,1,1,1,1))
  summ_stats <- LES %>% group_by(chamber) %>% summarize(mean_LES = mean(LES))
  
  ### Need to use this isTRUE business otherwise will sometimes return 1 != 1 -- https://stackoverflow.com/questions/9508518/why-are-these-numbers-not-equal
  if(!isTRUE(all.equal(sum(summ_stats$mean_LES), nrow(summ_stats)))){
    print("----> CHECK LES --- MEAN != 1 ---> BREAK")
    print(summ_stats)
    break
  }
  rm(summ_stats)
  # filter(LES, LES == 0)
  
  #### LES Without Weights + Merge
  LES_noWeights <- calc_LES(bills, legis_data, t_yrs, ss_weight = 5, reg_weight = 5, com_weight = 5, stage_weights = c(1,1,1,1,1))
  LES_noWeights <- rename(LES_noWeights, LES_nw = LES) %>% select(1:6, LES_nw)
  LES <- left_join(LES, LES_noWeights, by = intersect(colnames(LES), colnames(LES_noWeights)))
  rm(LES_noWeights)
  
  ### Fix Term Variable
  LES <- rename(LES, term = session)
  
  #### Merge Agg Stats back in
  LES <- legis_data %>%
    select(sponsor, chamber, num_sponsored_bills, sponsor_pass_rate, sponsor_law_rate, num_cosponsored_bills) %>%
    mutate(chamber = ifelse(chamber == "H", "House", "Senate")) %>%
    left_join(LES, ., by = c("sponsor", "chamber"))
  
  #### If LES == 0 and 
  LES[LES$LES == 0 & is.na(LES$num_sponsored_bills), c('num_sponsored_bills', 'sponsor_pass_rate', 'sponsor_law_rate')] <- 0
  
  ############### Save LES
  write.csv(LES, glue("{this_state}_LES_{t_yrs}.csv"), row.names = FALSE)
  rm(LES)
  
  
  cat(glue(". \n *********************** SESSION {t_yrs} ~~> DONE  ***********************"))
  
}
################# ****** END LOOP

#agg_stats, matches
rm(all_sponsors, bills, legis_data, SS_bills, i, keep_types, H_elec_year, S_elec_year)
rm(k_matches, klarner_sub, m_sub, km, bill_path, t_yrs, calc_LES) # c_sub
rm(t, t_sessions, terms, klarner_gs, commem_bills)

########################################################################################################################################################
############################################################################################################################################################# NAME FIX RECORDS 
########################################################################################################################################################
########################################################################################################################################################
########################################################################################################################################################
# Vacancies filled by APPOINTMENT BY PARTY --- https://ballotpedia.org/How_vacancies_are_filled_in_state_legislatures
########################################################################################################################
#### Full Rosters: 1999 - 2015: http://www.leg.state.co.us/clics/cslFrontPages.nsf/PrevSessionInfo?OpenForm
#### ------------- 2016+: https://leg.colorado.gov/prior-session-information
#######################################


# ~~~~~~~~~~~~ ESTIMATING SCORES FOR THE 1999_2000 TERM! ~~~~~~~~~~~~~~ 
#   session chamber   N AIC ABC PASS LAW
# 1 1999-RS       H 385 380 277  258 222
# 2 1999-RS       S 239 237 173  162 141
# 3 2000-RS       H 493 493 361  341 287
# 4 2000-RS       S 232 232 157  139 125
#### APPOINTED ~ HOUSE:
# -- HOPPE (1/20/1999)
# -- SCOTT (filled sullivants seat)
### APPOINTED ~ SENATE:
# -- ANDREWS (john, appointed 1998)
# -- SULLIVANT (2/17/1999) https://www.ourcampaigns.com/CandidateDetail.html?CandidateID=420472


# ~~~~~~~~~~~~ ESTIMATING SCORES FOR THE 2001_2002 TERM! ~~~~~~~~~~~~~~ 
# ----> Dropping 30 bills from 2001-S1 -- NO ACTIONS!
# ----> Dropping 63 bills from 2001-S2 -- NO ACTIONS!
# ----> Dropping 43 bills from 2002-S1 -- NO ACTIONS!
# 1 2001-RS       H 411 411 282  272 220
# 2 2001-RS       S 244 244 190  176 143
# 3 2002-RS       H 491 491 377  362 304
# 4 2002-RS       S 237 237 191  159 101
### APPOINTED ~ HOUSE:
# -- HARVEY (ted)
### APPOINTED/Won Special ~ SENATE:
# -- ENTZ (March 2001, past H)
# -- ISGAR (May 2001)

# ~~~~~~~~~~~~ ESTIMATING SCORES FOR THE 2003_2004 TERM! ~~~~~~~~~~~~~~ 
#   session chamber   N AIC ABC PASS LAW
# 1 2003-RS       H 383 382 259  244 218
# 2 2003-RS       S 354 353 284  256 231
# 3 2004-RS       H 465 464 345  313 280
# 4 2004-RS       S 261 260 198  170 147
### APPOINTED ~ HOUSE:
# -- CARROLL (terrance)
# -- CERBO
# -- MCGIHON
# -- WELKER
### APPOINTED ~ SENATE:
# -- GROFF
# -- VIEGA
### IN HOUSE:
# -- SANCHEZ -- Through March 2003 -- Per PVS, resigned for health reasons -- https://votesmart.org/candidate/biography/29827/desiree-sanchez

# ~~~~~~~~~~~~ ESTIMATING SCORES FOR THE 2005_2006 TERM! ~~~~~~~~~~~~~~ 
#   session chamber   N AIC ABC PASS LAW
# 1 2005-RS       H 353 350 262  245 192
# 2 2005-RS       S 249 249 209  181 162
# 3 2006-RS       H 412 412 325  311 250
# 4 2006-RS       S 239 239 191  150 144
# 5 2006-S1       H  23  22  12   12   7
# 6 2006-S1       S  13  13   6    4   4
### APPOINTED ~ HOUSE:
# -- GARDNER (cory)
# -- KERR A. (andrew, 2006)
# -- KERR J. (James 'Jim', 2005) -- http://www.leg.state.co.us/clics2005a/directory.nsf
# -- SOPER (john)
### APPOINTED ~ SENATE:
# -- BROPHY (greg, via H, june 2005)
# -- TOCHTROP (lois, via H, Jan 12 2005 = day of convening) 
# -- TRAYLOR -- Kathleen 'KiKi' Traylor -- January 2006 -- https://coloradocommunitymedia.com/stories/traylor-replaces-senator-anderson,35968
### DROP:
# -- lee, don -- resigned after winning, never seated -- https://en.wikipedia.org/wiki/Don_Lee_(politician)
# -- tochtrop, lois -- Never seated in House, appointed to Senate -- https://webcache.googleusercontent.com/search?q=cache:DUkQ14aI6wkJ:https://www.sos.state.co.us/pubs/elections/LawsRules/files/TermLimits012605unsigned.pdf+&cd=17&hl=en&ct=clnk&gl=us


# ~~~~~~~~~~~~ ESTIMATING SCORES FOR THE 2007_2008 TERM! ~~~~~~~~~~~~~~ 
#   session chamber   N AIC ABC PASS LAW
# 1 2007-RS       H 379 379 299  291 264
# 2 2007-RS       S 263 262 220  205 202
# 3 2008-RS       H 415 409 332  327 302
# 4 2008-RS       S 247 247 195  174 171
### APPOINTED ~ HOUSE:
# -- BRUCE (douglas); FERRANDINO; GARZA-HICKS; MIDDLETON; SCANLAN
### APPOINTED ~ SENATE:
# -- CADMAN (bill, sworn in Dec 11, 2007)
# -- WARD (steve, 2006, subsequently ran for Congress)
### DROP:
# -- cloer, mark -- resigned in early 2006 -- seat filled by his aide, garza-hicks

# ~~~~~~~~~~~~ ESTIMATING SCORES FOR THE 2009_2010 TERM! ~~~~~~~~~~~~~~ 
#   session chamber   N AIC ABC PASS LAW
# 1 2009-RS       H 369 369 272  258 239
# 2 2009-RS       S 297 297 255  238 225
# 3 2010-RS       H 432 430 336  320 297
# 4 2010-RS       S 217 217 175  165 156
### APPOINTED ~ HOUSE:
# -- DELGROSSO; KAGAN; NIKKEL (b.j); TYLER (max)
### APPOINTED ~ SENATE:
# -- LUNDBERG (Kevin, Jan 15, 2009, via H, sponsored 2 bills before moving)
# -- WHITEHEAD (Bruce, August 17, 2009)

# ~~~~~~~~~~~~ ESTIMATING SCORES FOR THE 2011_2012 TERM! ~~~~~~~~~~~~~~ 
#   session chamber   N AIC ABC PASS LAW
# 1 2011-RS       H 324 324 229  216 168
# 2 2011-RS       S 273 273 204  193 167
# 3 2012-RS       H 361 360 273  259 221
# 4 2012-RS       S 184 184 140  117  83
# 5 2012-S1       H   7   7   2    2   1
# 6 2012-S1       S   3   3   3    2   2
### APPOINTED ~ HOUSE: 
# -- HAMNER; SINGER; YOUNG (dave)
### APPOINTED ~ SENATE:
# -- NEVILLE
### DROP:
# -- scanlan, christine -- Resigned 12/31/2010 - https://ballotpedia.org/Christine_Scanlan
# -- romer, chris -- resigned to run for mayor of Denver; never seated


# ~~~~~~~~~~~~ ESTIMATING SCORES FOR THE 2013_2014 TERM! ~~~~~~~~~~~~~~ 
#   session chamber   N AIC ABC PASS LAW
# 1 2013-RS       H 325 325 247  241 219
# 2 2013-RS       S 288 288 239  224 222
# 3 2014-RS       H 398 398 310  302 272
# 4 2014-RS       S 223 223 167  162 148
### APPOINTED ~ HOUSE:
# -- BECKER (KC = Kathleen Collins, Nov 2013)
### APPOINTED ~ SENATE:
# -- HERPIN ; RIVERA; ZENZINGER

# ~~~~~~~~~~~~ ESTIMATING SCORES FOR THE 2015_2016 TERM! ~~~~~~~~~~~~~~ 
#   session chamber   N AIC ABC PASS LAW
# 1 2015-RS       H 392 392 279  269 191
# 2 2015-RS       S 290 290 224  214 171
# 3 2016-RS       H 468 468 359  349 260
# 4 2016-RS       S 217 217 168  163 125
### APPOINTED ~ HOUSE:
# -- LEONARD (tim); SIAS (lang); WIST (cole)
### APPOINTED ~ SENATE
# -- TATE (jack, via H, Jan 2016)

# ~~~~~~~~~~~~ ESTIMATING SCORES FOR THE 2017_2018 TERM! ~~~~~~~~~~~~~~ 
# -----> Dropping 3 bill(s) without a sponsor
#   session chamber   N AIC ABC PASS LAW
# 1 2017-RS       H 375 375 305  303 235
# 2 2017-RS       S 306 306 243  229 186
# 3 2017-S1       H   1   1   1    1   0
# 4 2017-S1       S   1   1   0    0   0
# 5 2018-RS       H 438 438 363  358 261
# 6 2018-RS       S 280 280 211  205 162
### APPOINTED ~ HOUSE:
# -- A. WINKLER; D. ROBERTS; J. REYHER; M. CATLIN; S. SANDRIDGE
### APPOINTED ~ SENATE:
# -- D. CORAM (via H)  


# filter(klarner, grepl('humenik', cand)) %>% select(cand, year, sen, etype, outcome, ddez, candid)
# filter(klarner, ddez == 46) %>% select(cand, year, sen, etype, outcome, ddez, candid)


##########################################################################################################
##########################################################################################################
##### MATCHING MISSING (SPECIALS) TO KLARNER USING OTHER TERMS WHERE SPONSOR IS PRESENT
##########################################################################################################

### Re-Load LES Data
LES_paths <- list.files('.', full.names = TRUE)
LES_paths <- LES_paths[!grepl('Archive|_All|Merged', LES_paths)]

LES <- LES_paths %>%
  lapply(read_csv, col_types = cols()) %>%
  bind_rows 

rm(LES_paths)

####### Fill in Missing Data from Candidates Elected in Specials using Subsequent Observations
missing <- unique(LES[is.na(LES$klarner_id),]$sponsor)
for(name in missing){
  name_sub <- filter(LES, grepl(glue("^{name},"), sponsor)); exact = TRUE
  if(nrow(name_sub) == 0){
    name_sub <- filter(LES, grepl(glue("^{name}"), sponsor)) 
    exact <- FALSE
  }
  # If there is only ONE UNIQUE id that matches the name
  if(any(!is.na(name_sub$klarner_id)) & length(unique(na.omit(name_sub$klarner_id))) == 1 ){
    LES[grepl(name, LES$sponsor) & is.na(LES$klarner_name),]$sponsor <- unique(name_sub[!is.na(name_sub$klarner_id),]$sponsor) 
    LES[grepl(name, LES$sponsor) & is.na(LES$klarner_name),]$klarner_name <- unique(na.omit(name_sub$klarner_name))
    LES[grepl(name, LES$sponsor) & is.na(LES$klarner_id),]$klarner_id <- unique(na.omit(name_sub$klarner_id))   
    print(glue(' ~~ {name} ~~ Matched to --> {unique(na.omit(name_sub$klarner_name))}'))
  } else {
    if(exact == TRUE){
      k_sub <- filter(klarner, grepl(paste0('^', name, ','), cand))
    }else{
      k_sub <- filter(klarner, grepl(name, cand))  
    }
    if(length(unique(k_sub$cand)) == 1 & length(unique(k_sub$candid)) == 1 & sum(!is.na(k_sub$candid)) != 0 ){
      LES[grepl(name, LES$sponsor) & is.na(LES$klarner_name),]$sponsor <- unique(k_sub$cand) 
      LES[grepl(name, LES$sponsor) & is.na(LES$klarner_name),]$klarner_name <- unique(k_sub$cand) 
      LES[grepl(name, LES$sponsor) & is.na(LES$klarner_id),]$klarner_id <- unique(k_sub$candid) 
      print(glue(' ~~ {name} ~~ Matched to --> {unique(k_sub$cand) }'))  
    }
  }
}

### Error Fixes -- Mismatches
# LES[LES$data_name %in% "kuhn",]$klarner_id <- NA
# LES[LES$data_name %in% "kuhn",]$klarner_name <- NA
# LES[LES$data_name %in% "kuhn",]$sponsor <- 'kuhn, john r.'

### ****Still missing***** ---> Rest are not in Klarner or 2018+
# still_missing <- LES[is.na(LES$klarner_id),]$sponsor
# name = still_missing[12]; print(name)
# filter(LES, grepl(paste0('^', name), sponsor)) %>% select(sponsor, data_name, klarner_name, klarner_id, term, chamber) %>% arrange(chamber, term) %>% as.data.frame()
# filter(klarner, grepl(paste0('^', name), cand)) %>% select(cand, year, etype, sen, outcome, candid) # %>%  View()
# rm(name, missing, name_sub, still_missing)

### Fix Missing
name_matches <- data.frame(LES_name = 'scott', k_name = 'scott, glenn')
name_matches <- add_row(name_matches, LES_name = 'carroll', k_name = 'carroll, terrance')
name_matches <- add_row(name_matches, LES_name = 'gardner', k_name = 'gardner, cory')
name_matches <- add_row(name_matches, LES_name = 'young', k_name = 'young, dave')
name_matches <- add_row(name_matches, LES_name = 'neville', k_name = 'neville, tim') # Gap betweem 2013 and 2015
name_matches <- add_row(name_matches, LES_name = 'becker', k_name = 'becker, k. c.') # Jon Becker skipped 2013_2014
# name_matches <- add_row(name_matches, LES_name = 'zzzzzzz', k_name = 'zzzzzzzz')
# name_matches <- add_row(name_matches, LES_name = 'zzzzzzz', k_name = 'zzzzzzzz')

for(i in 1:nrow(name_matches)){
  LES[LES$sponsor == name_matches[i,]$LES_name & is.na(LES$klarner_id),]$klarner_id <- unique(klarner[klarner$cand == name_matches[i,]$k_name,]$candid)
  LES[LES$sponsor == name_matches[i,]$LES_name & is.na(LES$klarner_name),]$klarner_name <- name_matches[i,]$k_name
  LES[LES$sponsor == name_matches[i,]$LES_name,]$sponsor <- name_matches[i,]$k_name
}
rm(name_matches, i)

############## Check for Duplicates
### ---> If two people with same last name and one won in a special, will lead to duplicates
### Remaining = Switched from House to Senate
for(t in unique(LES$term)){
  print(glue(' ************ {t} ***************'))
  check_dup <- filter(LES, term == t) %>%
    group_by(chamber) %>%
    mutate(dup = duplicated(klarner_id) & !is.na(klarner_id)) 
  if(any(check_dup$dup)){
    filter(LES, term == t & klarner_id %in% check_dup[check_dup$dup == TRUE,]$klarner_id & !is.na(klarner_id)) %>%
      select(sponsor, data_name, klarner_name, klarner_id, term, chamber) %>%
      print()
  }
}
rm(check_dup, k_sub, exact)


########################################################################################################################################################
########################################################################################################################################################
######## AGGREGATE AND MATCH TO EXTERNAL
########################################################################################################################################################
########################################################################################################################################################

### Klarner State Leg. Election Data --- Using Adjusted File From Top
klarner_sub <- filter(klarner, sab == this_state & year >= min_year - 5 & outcome == 'w')
klarner_sub <- select(klarner_sub, caseid, year, sen, ddez, dno, term, termz, cando, cand, candid, partyz, partyt, exper, outcome, etype)

#### KLARNER IS YEAR OF ELECTION, Not TERM
LES$exper <- LES$party <- LES$district <- NA
LES$district <- as.double(LES$district)
LES$party <- as.character(LES$party)
LES$exper <- as.character(LES$exper)

for(name in unique(LES$sponsor)){
  this_sponsor_LES <- LES[LES$sponsor == name,]
  sponsor_rows <- filter(klarner_sub, candid %in% na.omit(this_sponsor_LES$klarner_id ))
  if(nrow(sponsor_rows) == 0){
    ### Check Losers
    sponsor_rows <- filter(klarner, candid %in% na.omit(this_sponsor_LES$klarner_id ))
    if(nrow(sponsor_rows) >= 1){
      LES[LES$sponsor == name,]$party <- sponsor_rows[1,]$partyz
    }
  } else{
    for(t in this_sponsor_LES$term){
      second_year <- as.numeric(str_split(t, "_")[[1]][2])
      ### Filling in by chamber to account for people who switch chambers mid-term
      for(c in this_sponsor_LES[this_sponsor_LES$term == t,]$chamber){
        sponsor_sub <- filter(sponsor_rows, (etype %in% spec_elec_codes & year == second_year ) | year < second_year )
        sponsor_sub <- filter(sponsor_sub, sen == ifelse(c == "Senate", 1, 0))
        if(nrow(sponsor_sub) > 0 ){
          sponsor_sub <- arrange(sponsor_sub, desc(year))
          LES[LES$sponsor == name & LES$term == t & LES$chamber == c,]$district <- sponsor_sub[1,]$dno
          LES[LES$sponsor == name & LES$term == t & LES$chamber == c,]$party <- sponsor_sub[1,]$partyz
          LES[LES$sponsor == name & LES$term == t & LES$chamber == c,]$exper <- sponsor_sub[1,]$exper
        }else{
          if(nrow(sponsor_rows) > 0){
            sponsor_rows <- arrange(sponsor_rows, year)
            LES[LES$sponsor == name & LES$term == t & LES$chamber == c,]$party <- ifelse(is.logical(na.omit(unique(sponsor_rows$partyz))), NA, na.omit(unique(sponsor_rows$partyz))[1] )
            #LES[LES$sponsor == name & LES$term == s & LES$chamber == c,]$exper <- ifelse(is.logical(na.omit(unique(sponsor_rows$exper))), NA, na.omit(unique(sponsor_rows$exper))[1] )
          }
        }
      }
    }
  }
  # print(name)
}

###### IF MISSING, CHECK ELCTION LOSERS
# filter(LES, is.na(party)) %>% select(sponsor, term, klarner_id, district, party, exper)
# filter(klarner, candid == 246709) %>% select(cand, year, sen, etype, ddez, party, partyz, outcome)

### Manually Fix Those Not in Klarner
LES[LES$sponsor == "traylor",]$party <- 'r'
LES[LES$sponsor == "traylor",]$district <- 22
LES[LES$sponsor == "traylor",]$exper <- 'none'
LES[LES$sponsor == "traylor",]$sponsor <- 'traylor, kathleen'

LES[LES$sponsor == "garza-hicks",]$party <- 'r'
LES[LES$sponsor == "garza-hicks",]$district <- 17
LES[LES$sponsor == "garza-hicks",]$exper <- 'none'
LES[LES$sponsor == "garza-hicks",]$sponsor <- 'garza-hicks, stella'

LES[LES$sponsor == "bruce",]$party <- 'r'
LES[LES$sponsor == "bruce",]$district <- 15
LES[LES$sponsor == "bruce",]$exper <- 'none'
LES[LES$sponsor == "bruce",]$sponsor <- 'bruce, douglas'

LES[LES$sponsor == "ward",]$party <- 'r'
LES[LES$sponsor == "ward",]$district <- 26
LES[LES$sponsor == "ward",]$exper <- 'none'
LES[LES$sponsor == "ward",]$sponsor <- 'ward, steve'


#### *** If any run for reelection, won't be needed once klarner updates ****
LES[LES$sponsor == "roberts, d",]$party <- 'd'
LES[LES$sponsor == "roberts, d",]$sponsor <- 'roberts, dylan'
LES[LES$sponsor == "catlin, m",]$party <- 'r'
LES[LES$sponsor == "catlin, m",]$sponsor <- "catlin, marc"
LES[LES$sponsor == "reyher, j",]$party <- 'r'
LES[LES$sponsor == "reyher, j",]$sponsor <- 'reyher, judy'
LES[LES$sponsor == "sandridge, s",]$party <- 'r'
LES[LES$sponsor == "sandridge, s",]$sponsor <- 'sandridge, shane'

rm(klarner_sub, this_sponsor_LES, c, name, sponsor_rows, sponsor_sub, second_year, t)

#############################
#### NAME STANDARDIZATION
##############################
## ** Sponsor name variants that are the same person **

### REMOVE NICKNAMES
LES$sponsor <- str_trim(gsub('  +', ' ', gsub('\\(.+\\)', '', LES$sponsor)))
# table(LES$sponsor)

### Manual Fixes -- Collapsing to 1 OR Updating Name
LES[LES$klarner_id %in% 17990,]$sponsor <- 'dyer, jim e.'
LES[LES$sponsor == 'fitzgerald, joan',]$sponsor <- 'fitz-gerald, joan'
LES[LES$sponsor == 'bush, diane e. mitsch',]$sponsor <- 'mitsch bush, diane e.'
# **** Janet Buckner replaced John, but Klarner is wrong
LES[LES$sponsor == 'buckner, john w.' & LES$term %in% c("2015_2016", "2017_2018"),]$sponsor <- 'buckner, janet'



#########################################################
############ Match to Hall/Fouirnaies
########################################################

library(readstata13)

### Hall and Fournaies 2018, Covers 1988 - 2014
### --> https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/PGCVDP
hf_data <- read.dta13("~/Dropbox/Data/Hall_Fouirnaies_2018/How_Do_interest_Groups_Seek_Access_to_Committees.dta")
hf_data <- filter(hf_data, state == this_state)
hf_data <- hf_data[,c(colnames(hf_data)[c(1:3, 7:14, 204, 208, 217)], colnames(hf_data)[grepl('cmt_', colnames(hf_data))])]
hf_data$term <- paste0(hf_data$year + 1, "_", hf_data$year + 2)
hf_data$chamber <- ifelse(hf_data$chamber == 'senate', 'Senate', "House")

#### Doubling the Senate Rows + Adding back in
# **** ALL data should be right (assuming still in chamber) EXCEPT Majority Member IN STAGGERED STATES ********
senate <- filter(hf_data, chamber == "Senate")
senate$year <- senate$year + 2
senate$term <- paste0(senate$year + 1, "_", senate$year + 2)
senate$MajorityMember <- NA 
hf_data <- bind_rows(hf_data, senate) %>% arrange(year, chamber)
rm(senate)

### Subset
hf_data <- filter(hf_data, year > min_year - 4) %>% distinct() %>% select(-MajorityMember)

### Merge
LES <- left_join(LES, select(hf_data, -year), by = c('term' = 'term', 'chamber' = 'chamber', 'klarner_id' = 'CandId'))

### Check Duplicates
mutate(LES, check_dup = paste(sponsor, term, chamber, sep = "--")) %>%
  mutate(dup = duplicated(check_dup)) %>%
  filter(dup == TRUE) #%>% View()

### Set Committees to NA for Years without Data -- May have matched candids in year range
set_NA <- colnames(hf_data)
set_NA <- set_NA[!(set_NA %in% c("CandId", 'chamber', "year", "term")) ]
LES[LES$term == '2017_2018', set_NA] <- NA
rm(hf_data, set_NA)

#################################
### Shor and McCarty Data, 1993 - 2016
####################################

ideo <- readstata13::read.dta13("~/Dropbox/Data/Shor_McCarty_Data/shor_mccarty_1993_2016_individual_data_May_2018.dta")
ideo <- filter(ideo, st == this_state) %>% select(-st_id)
ideo$match_name <- str_extract(tolower(ideo$name), '[a-z]+, [a-z]+')
ideo$match_name <- ifelse(is.na(ideo$match_name), tolower(ideo$name), ideo$match_name)
ideo$last_name <- gsub(',.+', '', tolower(ideo$name))

#### Doing this row by row to more easily account for party, unique data_names, etc.
#### Starting with MT (May 7, 2019) this now cross-checks to make sure it doesn't match on last name if multiple smiths, for example.
LES$SM_name <- LES$SM_party <- LES$np_score <- NA
LES$SM_name <- as.character(LES$SM_name)
LES$SM_party <- as.character(LES$SM_party)
LES$np_score <- as.double(LES$np_score)

for(i in 1:nrow(LES)){
  ####### **** CHECK LAST NAME + ACCOUNT FOR VARIATIONS IF NO MATCH *******
  check_last <- which(gsub(",.+|\\'", '', LES[i,]$sponsor) == tolower(ideo$last_name) )
  ### if none, adjust name
  if(length(check_last) == 0){
    check_last <- which(gsub(",.+|\\'| |-", '', LES[i,]$sponsor) == gsub(" |\\'|-", '', tolower(ideo$last_name) ))
  }
  ## If Still None, Try Data Name
  if(length(check_last) == 0){
    d_name <- str_split(LES[i,]$data_name, " ")[[1]]
    check_last <- which(d_name[length(d_name)] == tolower(ideo$last_name) )
  }
  
  ####### ***** IF MORE THAN ONE MATCH *******
  if(length(check_last) > 1){
    ## Check First initial if more than 1 last name match
    ideo_match <- ideo[check_last,][substring(gsub('.+, ', '', tolower(ideo[check_last,]$name)), 1, 1) == substring(gsub(".+, ", '', LES[i,]$sponsor), 1, 1),]
    ### Check Party if Still Too Long
    if(nrow(ideo_match) > 1){
      ideo_match <- filter(ideo_match, party == toupper(LES[i,]$party))  
    }
    ### Check Last + First Name
    if(nrow(ideo_match) > 1){
      ideo_match <- filter(ideo_match, match_name == gsub(' [a-z]\\.$', '', LES[i,]$sponsor) )    
    }
    ##### ****** IF ONE LAST NAME MATCH - VERIFY NOT IDENTICAL LAST NAMES *********
  } else if(length(check_last) == 1){
    #### CHECK IF ANY OTHER LEGISLATORS WITH SAME LAST NAME
    lastname <- gsub(",.+|\\'", '', LES[i,]$sponsor)
    num_with_same_last <- filter(LES, grepl(glue('^{lastname},'), sponsor)) %>% select(sponsor) %>% unlist() %>% unique()
    if(length(num_with_same_last) > 1){
      ### Check FUll Name
      if(grepl(ideo[check_last,]$match_name, LES[i,]$sponsor)){
        ideo_match <- ideo[check_last,]   
      } else{
        ## Set to 0 rows
        ideo_match <- filter(ideo, match_name == 'zzzz')
      }
    } else {
      ideo_match <- ideo[check_last,]  
    }
  } else{
    # Set to 0 rows if no match
    ideo_match <- filter(ideo, match_name == 'zzzz')
  }
  ### Save if ONE MATCH After whole process
  if(nrow(ideo_match) == 1){
    LES[LES$sponsor == LES[i,]$sponsor,]$SM_name <- ideo_match$name
    LES[LES$sponsor == LES[i,]$sponsor,]$SM_party <- ideo_match$party
    LES[LES$sponsor == LES[i,]$sponsor,]$np_score <- ideo_match$np_score
    #cat(" \n Manually matched ", toupper(LES[i,]$sponsor), " to ", toupper(ideo_match$name), "\n .")
  }
  rm(ideo_match)
}
# select(LES, sponsor, SM_name) %>% distinct() %>% View()

### SM Names Matched to Multiple LES Sponsors
# ---> so this will catch all errors except those where Sponsor A is Matched to Voter Score B, when Sponsor B and Voter A are missing
filter(LES, !is.na(SM_name)) %>%
  group_by(SM_name) %>% 
  summarize(name_matches = paste(unique(sponsor), collapse = "----")) %>% 
  filter(grepl("----", name_matches))

#### FIX MISMATCHES
LES[LES$sponsor %in% c('rashadcoleman, james', 'hallmclachlan, barbara'), c('SM_name', 'SM_party', 'np_score')] <- NA

### Matches In Which LES First Name != Shor-McCarty First Name
# ** Notable Non-Mismatches:
mutate(LES, first_name_LES = gsub(', ', '', str_extract(sponsor, ', [a-z]+')),first_name_SM = gsub(', ', '', str_extract(tolower(SM_name), ', [a-z]+'))) %>% 
  filter(first_name_LES != first_name_SM) %>% select(sponsor, SM_name) %>% distinct() %>% as.data.frame()

### MANUAL FIXES ------- NO DATA FOR 2017-2018 YET!
# filter(LES, is.na(np_score)) %>% filter( !(term %in% c('2017_2018')) ) %>% group_by(sponsor) %>% summarize(terms = paste0(term, collapse = "|")) %>% as.data.frame()
# filter(ideo, grepl('buck', tolower(name))) %>% as.data.frame()  %>% select(name, party, st, np_score)
# filter(ideo, !(ideo$name %in% LES$SM_name) ) %>% select(name, party, st, np_score)

name_matches <- data.frame(LES_name = 'dyer, jim e.', SM_name = 'Dyer, E. Jim')
# name_matches <- add_row(name_matches, LES_name = 'buckner, janet', SM_name = 'zzzzzzz')
name_matches <- add_row(name_matches, LES_name = 'dyer, jim f.', SM_name = 'Dyer, F. Jim')
name_matches <- add_row(name_matches, LES_name = 'gardner, robert', SM_name = 'Gardner, Bob')
name_matches <- add_row(name_matches, LES_name = 'lee, pete', SM_name = 'Lee, Sanford') # Sanford 'Pete' Lee -- Can't confirm but represents D18 2011+ + https://www.coloradocapitolwatch.com/legislator/1/2018/243/1/
# name_matches <- add_row(name_matches, LES_name = 'sandoval, paula e.', SM_name = 'zzzzzzz')
# name_matches <- add_row(name_matches, LES_name = 'whitehead, bruce', SM_name = 'zzzzzzz')
# name_matches <- add_row(name_matches, LES_name = 'zzzzz', SM_name = 'zzzzzzz')


for(i in 1:nrow(name_matches)){
  LES[LES$sponsor == name_matches[i,]$LES_name,]$SM_name <- ideo[ideo$name == name_matches[i,]$SM_name,]$name
  LES[LES$sponsor == name_matches[i,]$LES_name,]$SM_party <- ideo[ideo$name == name_matches[i,]$SM_name,]$party
  LES[LES$sponsor == name_matches[i,]$LES_name,]$np_score <- ideo[ideo$name == name_matches[i,]$SM_name,]$np_score
}

####### Manual Edits (Needs more precision...)
# Switches Chambers, both recorded as John Tate
LES[LES$sponsor == 'tate, jack' & LES$chamber == "House",]$SM_name <- ideo[ideo$name == 'Tate, John A.' & ideo$house2015 %in% 1,]$name
LES[LES$sponsor == 'tate, jack' & LES$chamber == "House",]$SM_party <- ideo[ideo$name == 'Tate, John A.' & ideo$house2015 %in% 1,]$party
LES[LES$sponsor == 'tate, jack' & LES$chamber == "House",]$np_score <- ideo[ideo$name == 'Tate, John A.' & ideo$house2015 %in% 1,]$np_score
LES[LES$sponsor == 'tate, jack' & LES$chamber == "Senate",]$SM_name <- ideo[ideo$name == 'Tate, John A.' & ideo$senate2016 %in% 1,]$name
LES[LES$sponsor == 'tate, jack' & LES$chamber == "Senate",]$SM_party <- ideo[ideo$name == 'Tate, John A.' & ideo$senate2016 %in% 1,]$party
LES[LES$sponsor == 'tate, jack' & LES$chamber == "Senate",]$np_score <- ideo[ideo$name == 'Tate, John A.' & ideo$senate2016 %in% 1,]$np_score

rm(ideo, check_last, name_matches, d_name, i, lastname, num_with_same_last)

############################################
######## MORE NAME STANDARDIZATION
###########################################

### Manual Fixes
LES[LES$sponsor == 'michaelsonjenet, dafna',]$sponsor <- 'jenet, dafna michaelson'
LES[LES$sponsor == 'nikkel, b. j.',]$sponsor <- 'nikkel, betty june'
LES[LES$sponsor == "stengel, joe",]$sponsor <- 'stengel, joseph jr.'
LES[LES$sponsor == "blickensderfer, tom",]$sponsor <- 'blickensderfer, charles thomas'
LES[LES$sponsor == "becker, k. c.",]$sponsor <- 'becker, kathleen collins'
LES[LES$sponsor == "massey, tom",]$sponsor <- 'massey, thomas jr.'
# LES[LES$sponsor == "zzzzzzzzzz",]$sponsor <- 'zzzzzzzz'
 

##############################################
########### HAND CODING MAJORITY MEMBER VARIABLE
###############################################
### CODE ALL AS 0, THEN CODE MAJORITY --> Assumes Indeps NOT in Majority
###############################
# table(LES$term)

LES$in_majority <- 0

### House -- 1995 - 2020
LES[as.numeric(substring(LES$term,1,4)) %in% c(2005:2010, 2013:2020) & LES$chamber == 'House'  & LES$party %in% 'd',]$in_majority <- 1
LES[as.numeric(substring(LES$term,1,4)) %in% c(1999:2004, 2011:2012) & LES$chamber == 'House' & LES$party %in% 'r',]$in_majority <- 1

### Senate -- 1995 - 2020
LES[as.numeric(substring(LES$term,1,4)) %in% c(2005:2014, 2019:2020) & LES$chamber == 'Senate' & LES$party %in% 'd',]$in_majority <- 1
LES[as.numeric(substring(LES$term,1,4)) %in% c(1999:2000, 2003:2004, 2015:2018) & LES$chamber == 'Senate' & LES$party %in% 'r',]$in_majority <- 1

##############################################
###  Save
##############################################

### Check Missingnes
LES %>%
  group_by(chamber, term) %>%
  summarize(
    Election_Data_Missing = round(sum(is.na(klarner_id))/n(), 2),
    CM_Leadership_Missing = round(sum(is.na(SpeakerHouse))/n(), 2),
    CM_Totals_Missing = round(sum(is.na(cmt_number))/n(), 2),
    Ideology_Missing = round(sum(is.na(np_score))/n(), 2)) 

###### SAVE Merged File
colnames(LES)
if(!dir.exists("Merged")){dir.create("Merged")}
write.csv(LES, glue("Merged/{this_state}_LES_All_M.csv"), row.names = FALSE)

### Save by Session
for(t in unique(LES$term)){
  LES_sub <- filter(LES, term == t)
  write.csv(LES_sub, glue("Merged/{this_state}_LES_{t}_M.csv"), row.names = FALSE)  
}; rm(LES_sub)


##############################################
###  ******* EXPLORE ***********
##############################################

library(ggplot2)
library(ggridges)
library(forcats)

LES %>%
  group_by(term, party) %>%
  summarize(mean_LES = mean(LES),
            max_LES = max(LES)) 

#### Should split this by chamber
LES %>%
  mutate(t_factor = fct_rev(as.factor(gsub('_', '-', term) ))) %>% 
  filter(party %in% c("d", "r")) %>%
  ggplot(aes(y = t_factor)) +
  geom_density_ridges(aes(x = LES, fill = party), alpha = .8, color = "white", from = 0, to = 5) +
  xlab("LES") + 
  ylab("Term") + 
  ggtitle(glue("Legislative Effectiveness in {this_state}")) +
  scale_fill_cyclical(
    breaks = c("d", "r"),
    labels = c('d' = "Democrat", 'r' = "Republican"),
    values = c("dodgerblue2", "red2"),
    name = "Party", guide = "legend") +
  theme_ridges(grid = FALSE) + 
  facet_wrap(~ chamber) + 
  theme(axis.title.x = element_text(hjust = 0.5),axis.title.y = element_text(hjust = 0.5), legend.position = "bottom")

ggsave(glue("~/Dropbox/Papers/Legislative Effectiveness/State Legislatures/Figures/{this_state}_LES_ridgeplot.pdf"), height = 8, width = 7)

#########
ggplot(LES, aes(x = np_score, y = LES, color = party)) + 
  geom_point() + 
  geom_smooth(formula = "y ~ x", method = 'lm', se = FALSE) + 
  facet_wrap(~ term) + 
  scale_color_manual(values=c("dodgerblue2", "red2", 'gray50'))

####### CHECK OUTLIERS
# filter(LES, party == 'd' & np_score > 0) %>% select(sponsor, term, chamber, np_score, party, SM_party)
# filter(LES, party == 'r' & np_score < 0) %>% select(sponsor, term, chamber, np_score, party, SM_party)

### Within Legislator Correlations over time
stargazer::stargazer(lm(LES ~ lag(LES) + factor(sponsor) + factor(term) + factor(chamber), data = LES), omit = 'factor', type = 'text')
stargazer::stargazer(lm(LES_rank ~ lag(LES_rank) + factor(sponsor) + factor(term) + factor(chamber), data = LES), omit = 'factor', type = 'text')

### Majority Member...
# stargazer::stargazer(lm(LES ~ lag(LES) + MajorityMember + factor(sponsor) + factor(chamber), data = LES), omit = 'factor', type = 'text')
