#***************************************************Script - 1**********************************************************************************************
#Downloading clinical trial records from the EU-CTR registry


libraries = c( "XML","robotstxt", "tidyft","data.table", "DBI", "httr", "RSQLite","tidyverse","rvest","stringr","robotstxt","selectr","xml2","dplyr","forcats","magrittr","tidyr","ggplot2","lubridate","tibble","purrr","googleLanguageR","cld2")
lapply(libraries, require, character.only = TRUE)
counter=0
ids <- c(1:112032)

for (page_result in seq(from=1, to= 2142)) {
  link <- url(paste0("https://www.clinicaltrialsregister.eu/ctr-search/search?query=&page=",i)) 
  page <- read_html(link)
  ad <- page %>% html_nodes(".even+ tr a") %>% html_attr("href") %>% paste("https://www.clinicaltrialsregister.eu",., sep="")
  ad <- data.frame(ad)
  write.table(ad, "eur_links.csv", sep = ",",row.names = FALSE, col.names = !file.exists("eur_links.csv"), append = T)
  
  
  counter = counter + 1
  print(paste("Count = ", counter,"ID = ",page_result))
  
  
}

eur_page_links <- data.frame()
eur_page_links <- read.csv("eur_links.csv")
for (i in seq_along(ids)) {
  official_url = eur_page_links[ids[i],"ad"] 
  output_file=paste0("european_page",ids[i],".html")
  download.file(official_url, destfile = output_file, quiet = TRUE)
  time_of_download = as.character(timestamp())
  time_stamp = data.frame(Trial_ID = as.character(ids[i]),
                          downloaded_time = time_of_download,
                          URL = as.character(official_url))
  write.table(time_stamp, "time_stamp_european.csv", sep = ",",row.names = FALSE, col.names = !file.exists("time_stamp_european.csv"), append = T)
  
  
  
  counter = counter + 1
  print(paste("Count = ", counter,"ID = ",ids[i]))
  
}


#*****************************************************Script - 2********************************************************************************************
#Web scraped all the downloaded records for the keyword 'India' or 'CTRI'

libraries = c( "XML", "tidyft","data.table", "DBI", "httr", "RSQLite","tidyverse","rvest","stringr","robotstxt","selectr","xml2","dplyr","forcats","magrittr","tidyr","ggplot2","lubridate","tibble","purrr","googleLanguageR","cld2")
lapply(libraries, require, character.only = TRUE)
ids <- c(1:112032)

counter = 0
for (i in seq_along(ids)) { 
  
  myfile = paste0("european_page_crosscheck",ids[i],".html")
  
  if (file.exists(myfile)) {
    
    eur_page <- read_html(myfile)
    
    if(is_empty(eur_page)) {
      
      next 
    }
    
    new_function <- function(a) {
      
      if (length(a) == 0) {
        a <- "NA"
      } else if (a == "") {
        a <- "NA"
      } else {
        return(a)
      }
    }
    
    serial_no <- ids[i]
    Header <- eur_page %>% html_nodes(".title") %>% html_text() %>% str_squish() %>% str_trim()
    length_header <- length(Header) 
    if (length_header == 0) {
      next
    }
    
    EudraCT_number <- eur_page %>% html_nodes("tr:nth-child(1) .cellLighterGrey") %>% html_text() %>% str_squish() %>% str_trim()
    EudraCT_number <- new_function(EudraCT_number)
    summary_labels <- eur_page %>% html_nodes(".cellGrey") %>% html_text() %>% str_squish() %>% str_trim()
    length_summary <- length(summary_labels)
    if (length_summary == 7) {
      Date_first_entered_in_db <- eur_page %>% html_nodes("tr:nth-child(6) .cellLighterGrey") %>% html_text %>% str_squish() %>% str_trim()
      Date_first_entered_in_db <- new_function(Date_first_entered_in_db)
      Trial_results <- eur_page %>% html_nodes(".cellLighterGrey a") %>% html_text %>% str_squish() %>% str_trim()
      Trial_results <- new_function(Trial_results)
    } else {
      Date_first_entered_in_db <- eur_page %>% html_nodes("tr:nth-child(4) .cellLighterGrey") %>% html_text %>% str_squish() %>% str_trim()
      Date_first_entered_in_db <- new_function(Date_first_entered_in_db)
      Trial_results <- eur_page %>% html_nodes(".cellLighterGrey a") %>% html_text %>% str_squish() %>% str_trim()
      Trial_results <- new_function(Trial_results)
    }
    
    ##Scrapping from the first summary header field
    Summary <- eur_page %>% html_nodes(".cellLighterGrey") %>% html_text() %>% str_squish() %>% str_trim()
    Summary_india <- toString(Summary) %>% str_extract(., regex("\\bindia\\b", ignore_case = TRUE))
    Summary_india <- toString(Summary_india)
    Summary_ctri <- toString(Summary) %>% str_extract(., regex("\\bctri\\b", ignore_case = TRUE))
    Summary_ctri <- toString(Summary_ctri)
    
    
    
    ##Scrapping from the second protocol information header field
    Protocol_fields <- eur_page %>% html_nodes("#section-a .second") %>% html_text() %>% str_squish() %>% str_trim()
    Protocol_fields <- toString(Protocol_fields) %>% str_extract(., regex("\\bMember State Concerned\\b", ignore_case = TRUE))
    Protocol_fields <- toString(Protocol_fields)
    if (Protocol_fields == "Member State Concerned") {
      Member_state_concerned <- eur_page %>% html_nodes("#section-a .tricell:nth-child(1) .third") %>% html_text() %>% str_squish() %>% str_trim()
      Full_title_of_trial <- eur_page %>% html_nodes("#section-a .tricell:nth-child(3) tr:nth-child(1) td") %>% html_text() %>% str_replace_all('\\"',"") %>% str_squish() %>% str_trim()
    } else {
      Member_state_concerned <- "NA"
      Full_title_of_trial <- eur_page %>% html_nodes("#section-a .tricell:nth-child(2) .third td") %>% html_text() %>% str_squish() %>% str_trim()
    }
    
    Protocol <- eur_page %>% html_nodes("#section-a .third") %>% html_text() %>% str_squish() %>% str_trim()
    Protocol_india <- toString(Protocol) %>% str_extract(., regex("\\bindia\\b", ignore_case = TRUE))
    Protocol_ctri <- toString(Protocol) %>% str_extract(., regex("\\bctri\\b", ignore_case = TRUE))
    
    
    
    ##Scrapping from the third sponsor information field
    ##The sponsor field is a very scattered one with maximum no. of labels 73 and minimum no. of labels 7
    Sponsor <- eur_page %>% html_nodes("#section-b .third") %>% html_text() %>% str_squish() %>% str_trim()
    Sponsor_india <- toString(Sponsor) %>% str_extract(., regex("\\bindia\\b", ignore_case = TRUE))
    Sponsor_ctri <- toString(Sponsor) %>% str_extract(., regex("\\bctri\\b", ignore_case = TRUE))
    
    ##Scrapping from the applicant identification field
    Applicant_identification <- eur_page %>% html_nodes("#section-c .third") %>% html_text() %>% str_squish() %>% str_trim()
    AI_india <- toString(Applicant_identification) %>% str_extract(., regex("\\bindia\\b", ignore_case = TRUE))
    AI_ctri <- toString(Applicant_identification) %>% str_extract(., regex("\\bctri\\b", ignore_case = TRUE))
    
    
    ##Scrapping from the fourth IMP information field
    ##Scattered one - make the length table for the IMP field as well
    IMP <- eur_page %>% html_nodes("#section-d .third") %>% html_text() %>% str_squish() %>% str_trim()
    IMP_india <- toString(IMP) %>% str_extract(., regex("\\bindia\\b", ignore_case = TRUE))
    IMP_ctri <- toString(IMP) %>% str_extract(., regex("\\bctri\\b", ignore_case = TRUE))
    
    
    ##Scrapping from the fifth table "Information on placebo"
    
    Information_on_placebo <- eur_page %>% html_nodes("#section-d8 .third") %>% html_text() %>% str_squish() %>% str_trim()
    Information_on_placebo_india <- toString(Information_on_placebo) %>% str_extract(., regex("\\bindia\\b", ignore_case = TRUE))
    Information_on_placebo_ctri <- toString(Information_on_placebo) %>% str_extract(., regex("\\bctri\\b", ignore_case = TRUE))
    
    
    ##Scrapping from the sixth table "General Information"
    ##Scattered-data-- can be confirmed from the length-table
    ##both countries of recruitment and phase information is included in the general information table
    ##phase and countries of recruitment information are in the general information table
    General_information <- eur_page %>% html_nodes("#section-e .third") %>% html_text() %>% str_squish() %>% str_trim()
    General_information_india <- toString(General_information) %>% str_extract(., regex("\\bindia\\b", ignore_case = TRUE))
    General_information_ctri <- toString(General_information) %>% str_extract(., regex("\\bctri\\b", ignore_case = TRUE))
    
    
    ##Scrapping from the seventh table "population of trial subjects"
    Population_of_subjects <- eur_page %>% html_nodes("#section-f .third") %>% html_text() %>% str_squish() %>% str_trim()
    Pos_india <- toString(Population_of_subjects) %>% str_extract(., regex("\\bindia\\b", ignore_case = TRUE))
    Pos_ctri <- toString(Population_of_subjects) %>% str_extract(., regex("\\bctri\\b", ignore_case = TRUE))
    
    
    ##Scrapping from the eighth table "investigatory networks to be involved in the trial"
    Investigatory_networks <- eur_page %>% html_nodes("#section-g .third") %>% html_text() %>% str_squish() %>% str_trim()
    INetwork_india <- toString(Investigatory_networks) %>% str_extract(., regex("\\bindia\\b", ignore_case = TRUE))
    INetwork_ctri <- toString(Investigatory_networks) %>% str_extract(., regex("\\bctri\\b", ignore_case = TRUE))
    
    
    ##Scrapping from the ninth table "Ethics-committee"
    Ethics_committee <- eur_page %>% html_nodes("#section-n .third") %>% html_text() %>% str_squish() %>% str_trim()
    Ethics_india <- toString(Ethics_committee) %>% str_extract(., regex("\\bindia\\b", ignore_case = TRUE))
    Ethics_ctri <- toString(Ethics_committee) %>% str_extract(., regex("\\bctri\\b", ignore_case = TRUE))
    
    
    ##Scrapping from the tenth table "third country where the trial was authorized"
    Third_country <- eur_page %>% html_nodes("#section-h .third") %>% html_text() %>% str_squish() %>% str_trim()
    Third_country_india <- toString(Third_country) %>% str_extract(., regex("\\bindia\\b", ignore_case = TRUE))
    Third_country_ctri <- toString(Third_country) %>% str_extract(., regex("\\bctri\\b", ignore_case = TRUE))
    
    
    ##Scrapping from the eleventh table "end of trial"
    
    End_of_trial <- eur_page %>% html_nodes("#section-p .third") %>% html_text() %>% str_squish() %>% str_trim()
    End_of_trial_india <- toString(End_of_trial) %>% str_extract(., regex("\\bindia\\b", ignore_case = TRUE))
    End_of_trial_ctri <- toString(End_of_trial) %>% str_extract(., regex("\\bctri\\b", ignore_case = TRUE))
    
    
    #Scrapping for "India" and "ctri" in the whole page
    Whole_page <- eur_page %>% html_nodes(".third") %>% html_text() %>% str_squish() %>% str_trim()
    WP_india <- toString(Whole_page) %>% str_extract(., regex("\\bindia\\b", ignore_case = TRUE))
    WP_india_no_bound <- toString(Whole_page) %>% str_extract(., regex("india", ignore_case = TRUE))
    
    WP_ctri <- toString(Whole_page) %>% str_extract(., regex("\\bctri\\b", ignore_case = TRUE))
    WP_ctri_no_bound <- toString(Whole_page) %>% str_extract(., regex("ctri", ignore_case = TRUE))
    
    
    Scraped_for_searching_india <- data.frame(serial_no,EudraCT_number,Date_first_entered_in_db, Trial_results,
                                              Summary_india,Summary_ctri,Member_state_concerned,Full_title_of_trial,
                                              Protocol_india,Protocol_ctri,Sponsor_india,Sponsor_ctri,AI_india,AI_ctri,
                                              IMP_india,IMP_ctri,Information_on_placebo_india,Information_on_placebo_ctri,
                                              General_information_india,General_information_ctri,Pos_india,Pos_ctri,INetwork_india,
                                              INetwork_ctri,Ethics_india,Ethics_ctri,Third_country_ctri,Third_country_india,
                                              End_of_trial_india,End_of_trial_ctri,WP_india,WP_ctri,WP_india_no_bound,WP_ctri_no_bound)
    
    
    write.table(Scraped_for_searching_india,"Scraped_for_searching_india.csv", sep = ",",row.names = FALSE, col.names = !file.exists("Scraped_for_searching_india.csv"), append = T)
    
    
    
    counter = counter + 1
    print(paste("Count = ", counter,"ID = ",ids[i]))
    
    
    
  }
}


#********************************************************Script - 3***************************************************************************************
#Web-scraped from 7315 records which have "India" keyword in "General Information on trial" field

libraries = c( "XML", "tidyft","data.table", "DBI", "httr", "RSQLite","tidyverse","rvest","stringr","robotstxt","selectr","xml2","dplyr","forcats","magrittr","tidyr","ggplot2","lubridate","tibble","purrr","googleLanguageR","cld2")
lapply(libraries, require, character.only = TRUE)
ids <- c(1:7315)
counter = 0
for (i in seq_along(ids)) { 
  
  myfile = paste0("european_page",ids[i],".html")
  
  if (file.exists(myfile)) {
    
    eur_page <- read_html(myfile)
    
    if(is_empty(eur_page)) {
      
      next 
    }
    
    new_function <- function(a) {
      
      if (length(a) == 0) {
        a <- "NA"
      } else if (a == "") {
        a <- "NA"
      } else {
        return(a)
      }
    }
    
    new_function2 <- function(e) {
      serial_no <- ids[i]
      EudraCT_number <- eur_page %>% html_nodes("tr:nth-child(1) .cellLighterGrey") %>% html_text() %>% str_squish() %>% str_trim()
      EudraCT_number <- new_function(EudraCT_number)
      clinical_trial_type <- eur_page %>% html_nodes(".cellLighterGrey") %>% html_text() %>% toString(.) %>% str_extract(.,regex("Outside EU/EEA",ignore_case = TRUE)) %>% str_squish() %>% str_trim()
      clinical_trial_type <- toString(clinical_trial_type)
      clinical_trial_type <- new_function(clinical_trial_type)
      countries_of_recruitment <- eur_page %>% html_nodes(e) %>% html_text() %>% toString(.) %>% str_extract(.,regex("\\bindia\\b",ignore_case = TRUE)) %>% str_squish() %>% str_trim()
      countries_of_recruitment <- toString(countries_of_recruitment)
      countries_of_recruitment <- new_function(countries_of_recruitment) 
      COR_fields <- data.frame(serial_no,EudraCT_number,clinical_trial_type,countries_of_recruitment)
      write.table(COR_fields,"7315_India.csv", sep = ",",row.names = FALSE, col.names = !file.exists("7315_India.csv"), append = T)
      return(COR_fields)
    }
    
    General_info_labels <- eur_page %>% html_nodes("#section-e .second") %>% html_text()
    length_info_labels <- length(General_info_labels)
    
    if (length_info_labels == 69) {
      result <- new_function2(".tricell:nth-child(77) .third , .tricell:nth-child(76) .third , .tricell:nth-child(75) .third , .tricell:nth-child(74) .third , .tricell:nth-child(73) .third , .tricell:nth-child(72) .third , .tricell:nth-child(70) .third , .tricell:nth-child(69) .third , .tricell:nth-child(68) .third , .tricell:nth-child(67) .third , .tricell:nth-child(66) .third , .tricell:nth-child(64) .third , .tricell:nth-child(63) .third , .tricell:nth-child(62) .third , .tricell:nth-child(61) .third , .tricell:nth-child(60) .third , .tricell:nth-child(59) .third , .tricell:nth-child(58) .third , .tricell:nth-child(57) .third , .tricell:nth-child(56) .third")
    } else if (length_info_labels == 72) {
      result <- new_function2(".tricell:nth-child(77) .third , .tricell:nth-child(76) .third , .tricell:nth-child(75) .third , .tricell:nth-child(74) .third , .tricell:nth-child(73) .third , .tricell:nth-child(72) .third , .tricell:nth-child(69) .third , .tricell:nth-child(68) .third , .tricell:nth-child(70) .third , .tricell:nth-child(67) .third , .tricell:nth-child(66) .third , .tricell:nth-child(63) .third , .tricell:nth-child(64) .third , .tricell:nth-child(62) .third , .tricell:nth-child(61) .third , .tricell:nth-child(60) .third , .tricell:nth-child(58) .third , .tricell:nth-child(59) .third , .tricell:nth-child(57) .third , .tricell:nth-child(56) .third")
    } else if (length_info_labels == 79) {
      result <- new_function2(".tricell:nth-child(81) .third , .tricell:nth-child(80) .third , .tricell:nth-child(79) .third , .tricell:nth-child(78) .third , .tricell:nth-child(77) .third , .tricell:nth-child(76) .third , .tricell:nth-child(72) .third , .tricell:nth-child(73) .third , .tricell:nth-child(74) .third , .tricell:nth-child(71) .third , .tricell:nth-child(70) .third , .tricell:nth-child(68) .third , .tricell:nth-child(67) .third , .tricell:nth-child(66) .third , .tricell:nth-child(65) .third , .tricell:nth-child(64) .third , .tricell:nth-child(63) .third , .tricell:nth-child(62) .third , .tricell:nth-child(61) .third , .tricell:nth-child(60) .third , .tricell:nth-child(59) .third")
    } else if (length_info_labels == 52) {
      result <- new_function2(".tricell:nth-child(61) .third , .tricell:nth-child(60) .third , .tricell:nth-child(59) .third , .tricell:nth-child(58) .third , .tricell:nth-child(57) .third , .tricell:nth-child(56) .third , .tricell:nth-child(55) .third , .tricell:nth-child(54) .third , .tricell:nth-child(53) .third , .tricell:nth-child(52) .third , .tricell:nth-child(63) .third , .tricell:nth-child(62) .third")
    } else if (length_info_labels == 63) {
      result <- new_function2(".tricell:nth-child(71) .third , .tricell:nth-child(70) .third , .tricell:nth-child(69) .third , .tricell:nth-child(68) .third , .tricell:nth-child(67) .third , .tricell:nth-child(66) .third , .tricell:nth-child(65) .third , .tricell:nth-child(64) .third , .tricell:nth-child(63) .third , .tricell:nth-child(62) .third , .tricell:nth-child(61) .third , .tricell:nth-child(60) .third , .tricell:nth-child(59) .third , .tricell:nth-child(58) .third , .tricell:nth-child(57) .third , .tricell:nth-child(56) .third , .tricell:nth-child(55) .third , .tricell:nth-child(54) .third , .tricell:nth-child(53) .third , .tricell:nth-child(52) .third , .tricell:nth-child(51) .third")
    } else if (length_info_labels == 65) {
      result <- new_function2(".tricell:nth-child(76) .third , .tricell:nth-child(75) .third , .tricell:nth-child(74) .third , .tricell:nth-child(73) .third , .tricell:nth-child(72) .third , .tricell:nth-child(71) .third , .tricell:nth-child(70) .third , .tricell:nth-child(69) .third , .tricell:nth-child(68) .third , .tricell:nth-child(67) .third , .tricell:nth-child(66) .third , .tricell:nth-child(65) .third , .tricell:nth-child(64) .third , .tricell:nth-child(63) .third , .tricell:nth-child(62) .third , .tricell:nth-child(61) .third , .tricell:nth-child(60) .third , .tricell:nth-child(59) .third , .tricell:nth-child(58) .third , .tricell:nth-child(57) .third , .tricell:nth-child(56) .third , .tricell:nth-child(55) .third , .tricell:nth-child(54) .third")
    } else if (length_info_labels == 70) {
      result <- new_function2(".tricell:nth-child(72) .third , .tricell:nth-child(71) .third , .tricell:nth-child(70) .third , .tricell:nth-child(69) .third , .tricell:nth-child(68) .third , .tricell:nth-child(67) .third , .tricell:nth-child(66) .third , .tricell:nth-child(65) .third , .tricell:nth-child(64) .third , .tricell:nth-child(63) .third , .tricell:nth-child(62) .third , .tricell:nth-child(61) .third , .tricell:nth-child(60) .third , .tricell:nth-child(59) .third")
    } else if(length_info_labels == 76){
      result <- new_function2(".tricell:nth-child(81) .third , .tricell:nth-child(80) .third , .tricell:nth-child(79) .third , .tricell:nth-child(78) .third , .tricell:nth-child(77) .third , .tricell:nth-child(76) .third , .tricell:nth-child(75) .third , .tricell:nth-child(74) .third , .tricell:nth-child(73) .third , .tricell:nth-child(72) .third , .tricell:nth-child(71) .third , .tricell:nth-child(70) .third , .tricell:nth-child(69) .third , .tricell:nth-child(68) .third , .tricell:nth-child(67) .third , .tricell:nth-child(66) .third , .tricell:nth-child(65) .third , .tricell:nth-child(64) .third , .tricell:nth-child(63) .third , .tricell:nth-child(62) .third , .tricell:nth-child(61) .third , .tricell:nth-child(60) .third , .tricell:nth-child(59) .third")
    } else if(length_info_labels == 88){
      result <- new_function2(".tricell:nth-child(81) .third , .tricell:nth-child(80) .third , .tricell:nth-child(79) .third , .tricell:nth-child(78) .third , .tricell:nth-child(77) .third , .tricell:nth-child(76) .third , .tricell:nth-child(75) .third , .tricell:nth-child(74) .third , .tricell:nth-child(73) .third , .tricell:nth-child(72) .third , .tricell:nth-child(71) .third , .tricell:nth-child(70) .third , .tricell:nth-child(69) .third , .tricell:nth-child(68) .third , .tricell:nth-child(67) .third , .tricell:nth-child(66) .third , .tricell:nth-child(65) .third , .tricell:nth-child(64) .third , .tricell:nth-child(63) .third , .tricell:nth-child(62) .third , .tricell:nth-child(61) .third , .tricell:nth-child(60) .third")
    } else if(length_info_labels == 89){
      result <- new_function2(".tricell:nth-child(85) .third , .tricell:nth-child(84) .third , .tricell:nth-child(83) .third , .tricell:nth-child(82) .third , .tricell:nth-child(81) .third , .tricell:nth-child(80) .third , .tricell:nth-child(79) .third , .tricell:nth-child(78) .third , .tricell:nth-child(77) .third , .tricell:nth-child(76) .third , .tricell:nth-child(75) .third , .tricell:nth-child(74) .third , .tricell:nth-child(73) .third , .tricell:nth-child(72) .third , .tricell:nth-child(71) .third , .tricell:nth-child(70) .third , .tricell:nth-child(69) .third , .tricell:nth-child(68) .third , .tricell:nth-child(67) .third , .tricell:nth-child(66) .third , .tricell:nth-child(65) .third , .tricell:nth-child(64) .third")
    } else if (length_info_labels == 53) {
      result <- new_function2(".tricell:nth-child(59) .third , .tricell:nth-child(64) .third , .tricell:nth-child(63) .third , .tricell:nth-child(62) .third , .tricell:nth-child(60) .third , .tricell:nth-child(58) .third , .tricell:nth-child(57) .third , .tricell:nth-child(54) .third , .tricell:nth-child(55) .third , .tricell:nth-child(56) .third , .tricell:nth-child(53) .third , .tricell:nth-child(52) .third , .tricell:nth-child(61) .third")
    } else if (length_info_labels == 55) {
      result <- new_function2("#section-e .tricell:nth-child(52) .third , #section-e .tricell:nth-child(58) .third ,  #section-e .tricell:nth-child(63) .third , #section-e .tricell:nth-child(53) .third , #section-e .tricell:nth-child(54) .third , #section-e .tricell:nth-child(64) .third , #section-e .tricell:nth-child(65) .third , #section-e .tricell:nth-child(66) .third , #section-e .tricell:nth-child(62) .third , #section-e .tricell:nth-child(61) .third , #section-e .tricell:nth-child(59) .third , #section-e .tricell:nth-child(55) .third , #section-e .tricell:nth-child(56) .third , #section-e .tricell:nth-child(57) .third , #section-e .tricell:nth-child(60) .third")
    } else if (length_info_labels == 56) {
      result <- new_function2(".tricell:nth-child(56) .third , .tricell:nth-child(55) .third , .tricell:nth-child(54) .third , .tricell:nth-child(53) .third , .tricell:nth-child(52) .third , .tricell:nth-child(51) .third , .tricell:nth-child(50) .third , .tricell:nth-child(49) .third , .tricell:nth-child(48) .third , .tricell:nth-child(67) .third , .tricell:nth-child(66) .third , .tricell:nth-child(65) .third , .tricell:nth-child(64) .third , .tricell:nth-child(63) .third , .tricell:nth-child(62) .third , .tricell:nth-child(61) .third , .tricell:nth-child(60) .third , .tricell:nth-child(59) .third , .tricell:nth-child(58) .third , .tricell:nth-child(57) .third , .tricell:nth-child(47) .third")
    } else if (length_info_labels == 58) {
      result <- new_function2(".tricell:nth-child(69) .third , .tricell:nth-child(68) .third , .tricell:nth-child(67) .third , .tricell:nth-child(66) .third , .tricell:nth-child(65) .third , .tricell:nth-child(63) .third , .tricell:nth-child(64) .third , .tricell:nth-child(62) .third , .tricell:nth-child(61) .third , .tricell:nth-child(60) .third , .tricell:nth-child(59) .third , .tricell:nth-child(58) .third , .tricell:nth-child(57) .third , .tricell:nth-child(56) .third , .tricell:nth-child(55) .third , .tricell:nth-child(54) .third , .tricell:nth-child(53) .third , .tricell:nth-child(52) .third")
    } else if (length_info_labels == 59) {
      result <- new_function2("#section-e .tricell:nth-child(61) .third , #section-e .tricell:nth-child(60) .third , #section-e .tricell:nth-child(62) .third , #section-e .tricell:nth-child(63) .third , .tricell:nth-child(66) .third ,  .tricell:nth-child(65) .third , .tricell:nth-child(64) .third , #section-e .tricell:nth-child(55) .third , #section-e .tricell:nth-child(56) .third , #section-e .tricell:nth-child(57) .third , #section-e .tricell:nth-child(58) .third , #section-e .tricell:nth-child(59) .third , #section-e .tricell:nth-child(54) .third , .tricell:nth-child(67) .third")
    } else if (length_info_labels == 60) {
      result <- new_function2(".tricell:nth-child(67) .third , .tricell:nth-child(68) .third , .tricell:nth-child(64) .third , .tricell:nth-child(65) .third , .tricell:nth-child(66) .third , .tricell:nth-child(63) .third , .tricell:nth-child(55) .third , .tricell:nth-child(56) .third , .tricell:nth-child(57) .third , .tricell:nth-child(58) .third , .tricell:nth-child(59) .third , .tricell:nth-child(60) .third , .tricell:nth-child(61) .third , .tricell:nth-child(62) .third")
    } else if (length_info_labels == 61) {
      result <- new_function2(".tricell:nth-child(69) .third , .tricell:nth-child(70) .third , .tricell:nth-child(72) .third , .tricell:nth-child(68) .third , .tricell:nth-child(71) .third , .tricell:nth-child(64) .third , .tricell:nth-child(65) .third , .tricell:nth-child(66) .third , .tricell:nth-child(63) .third , .tricell:nth-child(52) .third , .tricell:nth-child(53) .third , .tricell:nth-child(54) .third , .tricell:nth-child(55) .third , .tricell:nth-child(56) .third , .tricell:nth-child(57) .third , .tricell:nth-child(58) .third , .tricell:nth-child(59) .third , .tricell:nth-child(60) .third , .tricell:nth-child(62) .third , .tricell:nth-child(61) .third , .tricell:nth-child(67) .third")
    } else if (length_info_labels == 66) {
      result <- new_function2(".tricell:nth-child(69) .third , .tricell:nth-child(70) .third , .tricell:nth-child(71) .third , .tricell:nth-child(72) .third , .tricell:nth-child(73) .third , .tricell:nth-child(74) .third , .tricell:nth-child(65) .third , .tricell:nth-child(66) .third , .tricell:nth-child(67) .third , .tricell:nth-child(64) .third , .tricell:nth-child(63) .third , .tricell:nth-child(61) .third , .tricell:nth-child(60) .third , .tricell:nth-child(59) .third , .tricell:nth-child(58) .third , .tricell:nth-child(57) .third , .tricell:nth-child(56) .third , .tricell:nth-child(55) .third , .tricell:nth-child(54) .third , .tricell:nth-child(62) .third , .tricell:nth-child(68) .third")
    } else if (length_info_labels == 67) {
      result <- new_function2(".tricell:nth-child(70) .third , .tricell:nth-child(71) .third , .tricell:nth-child(72) .third , .tricell:nth-child(73) .third , .tricell:nth-child(74) .third , .tricell:nth-child(75) .third , .tricell:nth-child(66) .third , .tricell:nth-child(67) .third , .tricell:nth-child(68) .third , .tricell:nth-child(65) .third , .tricell:nth-child(64) .third , .tricell:nth-child(62) .third , .tricell:nth-child(61) .third , .tricell:nth-child(60) .third , .tricell:nth-child(59) .third , .tricell:nth-child(58) .third , .tricell:nth-child(57) .third , .tricell:nth-child(56) .third , .tricell:nth-child(55) .third , .tricell:nth-child(54) .third , .tricell:nth-child(63) .third , .tricell:nth-child(69) .third")
    } else if (length_info_labels == 68) {
      result <- new_function2(".tricell:nth-child(71) .third , .tricell:nth-child(72) .third , .tricell:nth-child(73) .third , .tricell:nth-child(74) .third , .tricell:nth-child(75) .third , .tricell:nth-child(65) .third , .tricell:nth-child(66) .third , .tricell:nth-child(67) .third , .tricell:nth-child(68) .third , .tricell:nth-child(69) .third , .tricell:nth-child(63) .third , .tricell:nth-child(62) .third , .tricell:nth-child(61) .third , .tricell:nth-child(60) .third , .tricell:nth-child(59) .third , .tricell:nth-child(58) .third , .tricell:nth-child(56) .third , .tricell:nth-child(57) .third , .tricell:nth-child(55) .third , .tricell:nth-child(76) .third , .tricell:nth-child(64) .third , .tricell:nth-child(70) .third")
    } else if (length_info_labels == 101) {
      result <- new_function2(".tricell:nth-child(88) .third , .tricell:nth-child(87) .third , .tricell:nth-child(86) .third , .tricell:nth-child(85) .third , .tricell:nth-child(84) .third , .tricell:nth-child(83) .third , .tricell:nth-child(81) .third , .tricell:nth-child(80) .third , .tricell:nth-child(79) .third , .tricell:nth-child(78) .third , .tricell:nth-child(77) .third , .tricell:nth-child(75) .third , .tricell:nth-child(74) .third , .tricell:nth-child(73) .third , .tricell:nth-child(72) .third , .tricell:nth-child(71) .third , .tricell:nth-child(70) .third , .tricell:nth-child(69) .third , .tricell:nth-child(68) .third")
    } else if (length_info_labels == 102) {
      result <- new_function2(".tricell:nth-child(89) .third , .tricell:nth-child(88) .third , .tricell:nth-child(87) .third , .tricell:nth-child(86) .third , .tricell:nth-child(85) .third , .tricell:nth-child(84) .third , .tricell:nth-child(82) .third , .tricell:nth-child(81) .third , .tricell:nth-child(80) .third , .tricell:nth-child(79) .third , .tricell:nth-child(78) .third , .tricell:nth-child(76) .third , .tricell:nth-child(75) .third , .tricell:nth-child(74) .third , .tricell:nth-child(73) .third , .tricell:nth-child(72) .third , .tricell:nth-child(71) .third , .tricell:nth-child(70) .third , .tricell:nth-child(69) .third , .tricell:nth-child(68) .third")
    } else if (length_info_labels == 108) {
      result <- new_function2(".tricell:nth-child(92) .third , .tricell:nth-child(91) .third , .tricell:nth-child(90) .third , .tricell:nth-child(89) .third , .tricell:nth-child(88) .third , .tricell:nth-child(87) .third , .tricell:nth-child(85) .third , .tricell:nth-child(84) .third , .tricell:nth-child(83) .third , .tricell:nth-child(82) .third , .tricell:nth-child(81) .third , .tricell:nth-child(79) .third , .tricell:nth-child(78) .third , .tricell:nth-child(77) .third , .tricell:nth-child(76) .third , .tricell:nth-child(75) .third , .tricell:nth-child(74) .third , .tricell:nth-child(73) .third , .tricell:nth-child(72) .third , .tricell:nth-child(71) .third")
    } else if (length_info_labels == 109) {
      result <- new_function2(".tricell:nth-child(93) .third , .tricell:nth-child(92) .third , .tricell:nth-child(91) .third , .tricell:nth-child(90) .third , .tricell:nth-child(89) .third , .tricell:nth-child(88) .third , .tricell:nth-child(86) .third , .tricell:nth-child(85) .third , .tricell:nth-child(84) .third , .tricell:nth-child(83) .third , .tricell:nth-child(82) .third , .tricell:nth-child(80) .third , .tricell:nth-child(79) .third , .tricell:nth-child(78) .third , .tricell:nth-child(77) .third , .tricell:nth-child(76) .third , .tricell:nth-child(75) .third , .tricell:nth-child(74) .third , .tricell:nth-child(73) .third , .tricell:nth-child(72) .third")
    } else if (length_info_labels == 131) {
      result <- new_function2(".tricell:nth-child(100) .third , .tricell:nth-child(99) .third , .tricell:nth-child(98) .third , .tricell:nth-child(97) .third , .tricell:nth-child(96) .third , .tricell:nth-child(95) .third , .tricell:nth-child(93) .third , .tricell:nth-child(92) .third , .tricell:nth-child(91) .third , .tricell:nth-child(90) .third , .tricell:nth-child(89) .third , .tricell:nth-child(87) .third , .tricell:nth-child(86) .third , .tricell:nth-child(85) .third , .tricell:nth-child(84) .third , .tricell:nth-child(83) .third , .tricell:nth-child(82) .third , .tricell:nth-child(81) .third")
    } else if (length_info_labels == 132) {
      result <- new_function2(".tricell:nth-child(101) .third , .tricell:nth-child(100) .third , .tricell:nth-child(99) .third , .tricell:nth-child(98) .third , .tricell:nth-child(97) .third , .tricell:nth-child(96) .third , .tricell:nth-child(94) .third , .tricell:nth-child(93) .third , .tricell:nth-child(92) .third , .tricell:nth-child(91) .third , .tricell:nth-child(90) .third , .tricell:nth-child(88) .third , .tricell:nth-child(87) .third , .tricell:nth-child(86) .third , .tricell:nth-child(85) .third , .tricell:nth-child(84) .third , .tricell:nth-child(83) .third , .tricell:nth-child(82) .third , .tricell:nth-child(81) .third")
    } else if (length_info_labels == 143) {
      result <- new_function2(".tricell:nth-child(106) .third , .tricell:nth-child(105) .third , .tricell:nth-child(104) .third , .tricell:nth-child(103) .third , .tricell:nth-child(102) .third , .tricell:nth-child(101) .third , .tricell:nth-child(99) .third , .tricell:nth-child(98) .third , .tricell:nth-child(97) .third , .tricell:nth-child(96) .third , .tricell:nth-child(95) .third , .tricell:nth-child(93) .third , .tricell:nth-child(92) .third , .tricell:nth-child(91) .third , .tricell:nth-child(90) .third , .tricell:nth-child(89) .third , .tricell:nth-child(88) .third , .tricell:nth-child(87) .third , .tricell:nth-child(86) .third , .tricell:nth-child(85) .third")
    } else if (length_info_labels == 54) {
      result <- new_function2(".tricell:nth-child(52) .third , .tricell:nth-child(53) .third , .tricell:nth-child(54) .third , .tricell:nth-child(55) .third , .tricell:nth-child(56) .third , .tricell:nth-child(59) .third , .tricell:nth-child(58) .third , .tricell:nth-child(63) .third , .tricell:nth-child(64) .third , .tricell:nth-child(65) .third , .tricell:nth-child(60) .third , .tricell:nth-child(61) .third")
    } else if (length_info_labels == 57) {
      result <- new_function2("#section-e .tricell:nth-child(62) .third , #section-e .tricell:nth-child(63) .third , #section-e .tricell:nth-child(61) .third , #section-e .tricell:nth-child(64) .third , #section-e .tricell:nth-child(55) .third , .tricell:nth-child(56) .third , #section-e .tricell:nth-child(57) .third , #section-e .tricell:nth-child(58) .third , .tricell:nth-child(59) .third , #section-e .tricell:nth-child(54) .third")
    } else if (length_info_labels == 62) {
      result <- new_function2(".tricell:nth-child(60) .third , .tricell:nth-child(61) .third , .tricell:nth-child(59) .third , .tricell:nth-child(58) .third , .tricell:nth-child(57) .third , .tricell:nth-child(56) .third , .tricell:nth-child(55) .third , .tricell:nth-child(54) .third , .tricell:nth-child(63) .third , .tricell:nth-child(64) .third , .tricell:nth-child(65) .third , .tricell:nth-child(73) .third , .tricell:nth-child(72) .third , .tricell:nth-child(71) .third , .tricell:nth-child(69) .third , .tricell:nth-child(70) .third , .tricell:nth-child(66) .third , .tricell:nth-child(67) .third , .tricell:nth-child(53) .third")
    } else if (length_info_labels == 64) {
      result <- new_function2(".tricell:nth-child(69) .third , .tricell:nth-child(70) .third , .tricell:nth-child(68) .third , .tricell:nth-child(71) .third , .tricell:nth-child(72) .third , .tricell:nth-child(64) .third , .tricell:nth-child(63) .third , .tricell:nth-child(62) .third , .tricell:nth-child(65) .third , .tricell:nth-child(66) .third , .tricell:nth-child(53) .third , .tricell:nth-child(54) .third , .tricell:nth-child(55) .third , .tricell:nth-child(56) .third , .tricell:nth-child(57) .third , .tricell:nth-child(58) .third , .tricell:nth-child(59) .third , .tricell:nth-child(60) .third , .tricell:nth-child(52) .third")
    } else if (length_info_labels == 71) {
      result <- new_function2(".tricell:nth-child(76) .third , .tricell:nth-child(75) .third , .tricell:nth-child(74) .third , .tricell:nth-child(73) .third , .tricell:nth-child(72) .third , .tricell:nth-child(68) .third , .tricell:nth-child(69) .third , .tricell:nth-child(67) .third , .tricell:nth-child(66) .third , .tricell:nth-child(64) .third , .tricell:nth-child(63) .third , .tricell:nth-child(62) .third , .tricell:nth-child(61) .third , .tricell:nth-child(60) .third , .tricell:nth-child(58) .third , .tricell:nth-child(59) .third , .tricell:nth-child(57) .third , .tricell:nth-child(56) .third , .tricell:nth-child(70) .third")
    } else if (length_info_labels == 73) {
      result <- new_function2(".tricell:nth-child(73) .third , .tricell:nth-child(74) .third , .tricell:nth-child(75) .third , .tricell:nth-child(76) .third , .tricell:nth-child(77) .third , .tricell:nth-child(71) .third , .tricell:nth-child(70) .third , .tricell:nth-child(69) .third , .tricell:nth-child(68) .third , .tricell:nth-child(67) .third , .tricell:nth-child(65) .third , .tricell:nth-child(64) .third , .tricell:nth-child(63) .third , .tricell:nth-child(62) .third , .tricell:nth-child(61) .third , .tricell:nth-child(60) .third , .tricell:nth-child(59) .third , .tricell:nth-child(58) .third , .tricell:nth-child(57) .third , .tricell:nth-child(56) .third , .tricell:nth-child(78) .third")
    } else if (length_info_labels == 74) {
      result <- new_function2(".tricell:nth-child(69) .third , .tricell:nth-child(68) .third , .tricell:nth-child(67) .third , .tricell:nth-child(70) .third , .tricell:nth-child(71) .third , .tricell:nth-child(73) .third , .tricell:nth-child(74) .third , .tricell:nth-child(75) .third , .tricell:nth-child(76) .third , .tricell:nth-child(65) .third , .tricell:nth-child(64) .third , .tricell:nth-child(63) .third , .tricell:nth-child(62) .third , .tricell:nth-child(61) .third , .tricell:nth-child(60) .third , .tricell:nth-child(59) .third , .tricell:nth-child(58) .third")
    } else if (length_info_labels == 75) {
      result <- new_function2(".tricell:nth-child(70) .third , .tricell:nth-child(77) .third , .tricell:nth-child(76) .third , .tricell:nth-child(75) .third , .tricell:nth-child(74) .third , .tricell:nth-child(71) .third , .tricell:nth-child(72) .third , .tricell:nth-child(69) .third , .tricell:nth-child(68) .third , .tricell:nth-child(66) .third , .tricell:nth-child(65) .third , .tricell:nth-child(64) .third , .tricell:nth-child(63) .third , .tricell:nth-child(62) .third , .tricell:nth-child(61) .third , .tricell:nth-child(60) .third , .tricell:nth-child(59) .third , .tricell:nth-child(58) .third")
    } else if (length_info_labels == 77) {
      result <- new_function2(".tricell:nth-child(66) .third , .tricell:nth-child(65) .third , .tricell:nth-child(64) .third , .tricell:nth-child(63) .third , .tricell:nth-child(62) .third , .tricell:nth-child(61) .third , .tricell:nth-child(60) .third , .tricell:nth-child(59) .third , .tricell:nth-child(70) .third , .tricell:nth-child(71) .third , .tricell:nth-child(72) .third , .tricell:nth-child(74) .third , .tricell:nth-child(75) .third , .tricell:nth-child(76) .third , .tricell:nth-child(77) .third , .tricell:nth-child(78) .third , .tricell:nth-child(79) .third , .tricell:nth-child(69) .third , .tricell:nth-child(68) .third")
    } else if (length_info_labels == 78) {
      result <- new_function2(".tricell:nth-child(80) .third , .tricell:nth-child(79) .third , .tricell:nth-child(78) .third , .tricell:nth-child(77) .third , .tricell:nth-child(76) .third , .tricell:nth-child(75) .third , .tricell:nth-child(73) .third , .tricell:nth-child(71) .third , .tricell:nth-child(72) .third , .tricell:nth-child(70) .third , .tricell:nth-child(69) .third , .tricell:nth-child(67) .third , .tricell:nth-child(66) .third , .tricell:nth-child(65) .third , .tricell:nth-child(64) .third , .tricell:nth-child(63) .third , .tricell:nth-child(62) .third , .tricell:nth-child(61) .third , .tricell:nth-child(60) .third , .tricell:nth-child(59) .third")
    } else if (length_info_labels == 80) {
      result <- new_function2(".tricell:nth-child(76) .third , .tricell:nth-child(77) .third , .tricell:nth-child(78) .third , .tricell:nth-child(74) .third , .tricell:nth-child(73) .third , .tricell:nth-child(72) .third , .tricell:nth-child(71) .third , .tricell:nth-child(70) .third , .tricell:nth-child(68) .third , .tricell:nth-child(67) .third , .tricell:nth-child(66) .third , .tricell:nth-child(65) .third , .tricell:nth-child(64) .third , .tricell:nth-child(63) .third , .tricell:nth-child(62) .third , .tricell:nth-child(61) .third , .tricell:nth-child(60) .third , .tricell:nth-child(79) .third")
    } else if (length_info_labels == 81) {
      result <- new_function2(".tricell:nth-child(80) .third , .tricell:nth-child(79) .third , .tricell:nth-child(78) .third , .tricell:nth-child(77) .third , .tricell:nth-child(76) .third , .tricell:nth-child(75) .third , .tricell:nth-child(72) .third , .tricell:nth-child(71) .third , .tricell:nth-child(70) .third , .tricell:nth-child(69) .third , .tricell:nth-child(66) .third , .tricell:nth-child(67) .third , .tricell:nth-child(65) .third , .tricell:nth-child(64) .third , .tricell:nth-child(63) .third , .tricell:nth-child(62) .third , .tricell:nth-child(61) .third , .tricell:nth-child(60) .third , .tricell:nth-child(73) .third")
    } else if (length_info_labels == 82) {
      result <- new_function2(".tricell:nth-child(81) .third , .tricell:nth-child(80) .third , .tricell:nth-child(79) .third , .tricell:nth-child(78) .third , .tricell:nth-child(77) .third , .tricell:nth-child(76) .third , .tricell:nth-child(72) .third , .tricell:nth-child(73) .third , .tricell:nth-child(74) .third , .tricell:nth-child(71) .third , .tricell:nth-child(68) .third , .tricell:nth-child(67) .third , .tricell:nth-child(66) .third , .tricell:nth-child(65) .third , .tricell:nth-child(64) .third , .tricell:nth-child(63) .third , .tricell:nth-child(62) .third , .tricell:nth-child(61) .third , .tricell:nth-child(60) .third , .tricell:nth-child(70) .third")
    } else if (length_info_labels == 83) {
      result <- new_function2(".tricell:nth-child(77) .third , .tricell:nth-child(78) .third , .tricell:nth-child(79) .third , .tricell:nth-child(80) .third , .tricell:nth-child(81) .third , .tricell:nth-child(73) .third , .tricell:nth-child(74) .third , .tricell:nth-child(75) .third , .tricell:nth-child(72) .third , .tricell:nth-child(71) .third , .tricell:nth-child(69) .third , .tricell:nth-child(68) .third , .tricell:nth-child(67) .third , .tricell:nth-child(66) .third , .tricell:nth-child(65) .third , .tricell:nth-child(64) .third , .tricell:nth-child(63) .third , .tricell:nth-child(62) .third , .tricell:nth-child(61) .third , .tricell:nth-child(82) .third")
    } else if (length_info_labels == 84) {
      result <- new_function2(".tricell:nth-child(83) .third , .tricell:nth-child(82) .third , .tricell:nth-child(81) .third , .tricell:nth-child(80) .third , .tricell:nth-child(79) .third , .tricell:nth-child(78) .third , .tricell:nth-child(74) .third , .tricell:nth-child(75) .third , .tricell:nth-child(76) .third , .tricell:nth-child(73) .third , .tricell:nth-child(72) .third , .tricell:nth-child(70) .third , .tricell:nth-child(69) .third , .tricell:nth-child(68) .third , .tricell:nth-child(67) .third , .tricell:nth-child(66) .third , .tricell:nth-child(65) .third , .tricell:nth-child(64) .third , .tricell:nth-child(63) .third , .tricell:nth-child(62) .third")
    } else if (length_info_labels == 85) {
      result <- new_function2(".tricell:nth-child(81) .third , .tricell:nth-child(80) .third , .tricell:nth-child(79) .third , .tricell:nth-child(78) .third , .tricell:nth-child(74) .third , .tricell:nth-child(75) .third , .tricell:nth-child(76) .third , .tricell:nth-child(73) .third , .tricell:nth-child(72) .third , .tricell:nth-child(70) .third , .tricell:nth-child(69) .third , .tricell:nth-child(68) .third , .tricell:nth-child(67) .third , .tricell:nth-child(66) .third , .tricell:nth-child(65) .third , .tricell:nth-child(64) .third , .tricell:nth-child(63) .third , .tricell:nth-child(62) .third")
    } else if (length_info_labels == 86) {
      result <- new_function2(".tricell:nth-child(72) .third , .tricell:nth-child(73) .third , .tricell:nth-child(74) .third , .tricell:nth-child(75) .third , .tricell:nth-child(76) .third , .tricell:nth-child(82) .third , .tricell:nth-child(81) .third , .tricell:nth-child(80) .third , .tricell:nth-child(79) .third , .tricell:nth-child(78) .third , .tricell:nth-child(70) .third , .tricell:nth-child(69) .third , .tricell:nth-child(68) .third , .tricell:nth-child(67) .third , .tricell:nth-child(66) .third , .tricell:nth-child(65) .third , .tricell:nth-child(64) .third , .tricell:nth-child(63) .third , .tricell:nth-child(62) .third")
    } else if (length_info_labels == 87) {
      result <- new_function2(".tricell:nth-child(69) .third , .tricell:nth-child(68) .third , .tricell:nth-child(67) .third , .tricell:nth-child(66) .third , .tricell:nth-child(65) .third , .tricell:nth-child(64) .third , .tricell:nth-child(62) .third , .tricell:nth-child(63) .third , .tricell:nth-child(70) .third , .tricell:nth-child(79) .third , .tricell:nth-child(78) .third , .tricell:nth-child(80) .third , .tricell:nth-child(81) .third , .tricell:nth-child(82) .third , .tricell:nth-child(83) .third , .tricell:nth-child(75) .third , .tricell:nth-child(74) .third , .tricell:nth-child(76) .third , .tricell:nth-child(73) .third , .tricell:nth-child(72) .third")
    } else if (length_info_labels == 91) {
      result <- new_function2(".tricell:nth-child(84) .third , .tricell:nth-child(83) .third , .tricell:nth-child(82) .third , .tricell:nth-child(81) .third , .tricell:nth-child(80) .third , .tricell:nth-child(79) .third , .tricell:nth-child(75) .third , .tricell:nth-child(76) .third , .tricell:nth-child(77) .third , .tricell:nth-child(74) .third , .tricell:nth-child(73) .third , .tricell:nth-child(71) .third , .tricell:nth-child(70) .third , .tricell:nth-child(69) .third , .tricell:nth-child(68) .third , .tricell:nth-child(67) .third , .tricell:nth-child(66) .third , .tricell:nth-child(65) .third , .tricell:nth-child(64) .third , .tricell:nth-child(63) .third")
    } else if (length_info_labels == 92) {
      result <- new_function2(".tricell:nth-child(85) .third , .tricell:nth-child(84) .third , .tricell:nth-child(83) .third , .tricell:nth-child(82) .third , .tricell:nth-child(81) .third , .tricell:nth-child(80) .third , .tricell:nth-child(76) .third , .tricell:nth-child(77) .third , .tricell:nth-child(78) .third , .tricell:nth-child(75) .third , .tricell:nth-child(74) .third , .tricell:nth-child(72) .third , .tricell:nth-child(71) .third , .tricell:nth-child(70) .third , .tricell:nth-child(69) .third , .tricell:nth-child(68) .third , .tricell:nth-child(67) .third , .tricell:nth-child(65) .third , .tricell:nth-child(66) .third , .tricell:nth-child(64) .third")
    } else if (length_info_labels == 93) {
      result <- new_function2(".tricell:nth-child(65) .third , .tricell:nth-child(66) .third , .tricell:nth-child(67) .third , .tricell:nth-child(68) .third , .tricell:nth-child(69) .third , .tricell:nth-child(70) .third , .tricell:nth-child(71) .third , .tricell:nth-child(72) .third , .tricell:nth-child(73) .third , .tricell:nth-child(85) .third , .tricell:nth-child(84) .third , .tricell:nth-child(83) .third , .tricell:nth-child(82) .third , .tricell:nth-child(81) .third , .tricell:nth-child(79) .third , .tricell:nth-child(78) .third , .tricell:nth-child(77) .third , .tricell:nth-child(76) .third , .tricell:nth-child(75) .third , .tricell:nth-child(86) .third")
    } else if (length_info_labels == 94) {
      result <- new_function2(".tricell:nth-child(87) .third , .tricell:nth-child(86) .third , .tricell:nth-child(85) .third , .tricell:nth-child(84) .third , .tricell:nth-child(83) .third , .tricell:nth-child(82) .third , .tricell:nth-child(74) .third , .tricell:nth-child(73) .third , .tricell:nth-child(72) .third , .tricell:nth-child(71) .third , .tricell:nth-child(70) .third , .tricell:nth-child(69) .third , .tricell:nth-child(67) .third , .tricell:nth-child(68) .third , .tricell:nth-child(66) .third , .tricell:nth-child(65) .third , .tricell:nth-child(80) .third , .tricell:nth-child(79) .third , .tricell:nth-child(78) .third , .tricell:nth-child(77) .third , .tricell:nth-child(76) .third")
    } else if (length_info_labels == 98) {
      result <- new_function2(".tricell:nth-child(88) .third , .tricell:nth-child(87) .third , .tricell:nth-child(86) .third , .tricell:nth-child(85) .third , .tricell:nth-child(84) .third , .tricell:nth-child(83) .third , .tricell:nth-child(81) .third , .tricell:nth-child(80) .third , .tricell:nth-child(79) .third , .tricell:nth-child(78) .third , .tricell:nth-child(77) .third , .tricell:nth-child(75) .third , .tricell:nth-child(74) .third , .tricell:nth-child(73) .third , .tricell:nth-child(72) .third , .tricell:nth-child(71) .third , .tricell:nth-child(70) .third , .tricell:nth-child(68) .third , .tricell:nth-child(69) .third , .tricell:nth-child(67) .third")
    } else if (length_info_labels == 99) {
      result <- new_function2(".tricell:nth-child(89) .third , .tricell:nth-child(88) .third , .tricell:nth-child(87) .third , .tricell:nth-child(86) .third , .tricell:nth-child(85) .third , .tricell:nth-child(84) .third , .tricell:nth-child(82) .third , .tricell:nth-child(81) .third , .tricell:nth-child(80) .third , .tricell:nth-child(79) .third , .tricell:nth-child(78) .third , .tricell:nth-child(76) .third , .tricell:nth-child(75) .third , .tricell:nth-child(74) .third , .tricell:nth-child(73) .third , .tricell:nth-child(71) .third , .tricell:nth-child(72) .third , .tricell:nth-child(70) .third , .tricell:nth-child(69) .third , .tricell:nth-child(68) .third")
    }
    
    
    
    
    
    
    
    
    counter = counter + 1
    print(paste("Count = ", counter,"ID = ",ids[i]))
    
    
  }}


#**************************************************Script - 4*********************************************************************************************
#For cross-checking with Methodology - A and Methodology - B


libraries = c( "XML","robotstxt", "tidyft","data.table", "DBI", "httr", "RSQLite","tidyverse","rvest","stringr","robotstxt","selectr","xml2","dplyr","forcats","magrittr","tidyr","ggplot2","lubridate","tibble","purrr","googleLanguageR","cld2")
lapply(libraries, require, character.only = TRUE)
counter=0

for (page_result in seq(from=1, to= 70)) {
  link <- url(paste0("https://www.clinicaltrialsregister.eu/ctr-search/search?query=india&page=",page_result)) 
  page <- read_html(link)
  ad <- page %>% html_nodes(".even+ tr a") %>% html_attr("href") 
  ad <- paste0("https://www.clinicaltrialsregister.eu",ad)
  
  time <- as.character(timestamp())
  link <- data.frame(ad,time)
  
  write.table(link, "eur_links_india_search_1394.csv", sep = ",",row.names = FALSE, col.names = !file.exists("eur_links_india_search_1394.csv"), append = T)
  
  
  counter = counter + 1
  print(paste("Count = ", counter,"ID = ",page_result))
  
}
