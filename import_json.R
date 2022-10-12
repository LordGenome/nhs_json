## R ----

setwd("/Users/graham/Dropbox/01_gitR/nhs_json")
## load packages ----
library(tidyverse)
library(lubridate)
library(qcc)
library(jsonlite)
library(httr)

# list of valid metrics for filters
# 
# areaType: Area type as string
# areaName: Area name as string
# areaCode: Area Code as string
# date: Date as string [YYYY-MM-DD]

# list of valid values for the areaType metric
#
# Please note that the values of the areaType metric are case-sensitive.
# 
# overview: Overview data for the United Kingdom
# nation: Nation data (England, Northern Ireland, Scotland, and Wales)
# region: Region data
# nhsRegion: NHS Region data
# utla: Upper-tier local authority data
# ltla: Lower-tier local authority data

## Example ----

# endpoint <- 'https://api.coronavirus.data.gov.uk/v1/data?filters=areaType=nation;areaName=england&structure={"date":"date","newCases":"newCasesByPublishDate","newDeaths":"newDeathsByDeathDate"}'

endpoint <- 'https://api.coronavirus.data.gov.uk/v1/data?filters=areaType=nation;areaName=england&structure={"date":"date","newCases":"newCasesByPublishDate"}'


httr::GET(
  url = endpoint,
  timeout(10)
) -> response


if (response$status_code >= 400) {
  err_msg = httr::http_status(response)
  stop(err_msg)
}

# Convert response from binary to JSON:
json_text <- httr::content(response, "text")
data      <- jsonlite::fromJSON(json_text)

covid_data <- as_tibble(data$data)

covid_data_by_date <- covid_data %>% arrange(date)

covid_data <- covid_data_by_date %>% mutate(totalCases = cumsum(newCases))

covid_data <- mutate(covid_data, log2newCases = log2(newCases))

covid_data <- mutate(covid_data, log2totalCases = log2(totalCases))

## preliminary plot ----

# filtering by date

covid_data$date <- as_date(covid_data$date)
covid_data$newCases <- as.numeric(covid_data$newCases)
sys_date <- Sys.Date()
start_date <- as.Date("2020-03-01")
label_start_date <- format(start_date, "%d %b")
label_sys_date <- format(sys_date, "%d %b")
label_dates <- paste0(label_start_date," to ",label_sys_date, " https://github.com/LordGenome/nhs_json")


date_range <- seq(as.Date("2020-03-01"), as.Date(sys_date), by = "days") %>% as.character() #need as.charactor %in% to work
date_range <- as.Date(date_range)

covid_data <- filter(covid_data, date %in% date_range)

## new cases ----

ggplot(covid_data) +
  stat_smooth(mapping = aes(x = date, y = newCases), span= 0.1, show.legend = TRUE) +
  # geom_point(colour = "black", alpha = 0.5, size = 1, mapping =  aes(x = date, y = newCases)) +
  theme_bw() +
  scale_x_date(NULL,
               breaks = scales::breaks_width("1 week"),
               labels = scales::label_date_short()) +
  scale_y_continuous(breaks = seq(0, 400000, by = 10000 ))  +
  ylab("new cases") +
  labs (title = "Covid-19 new cases in England",
        subtitle = "Source: https://api.coronavirus.data.gov.uk/v1/data",
        caption = label_dates)

## total cases ----

ggplot(covid_data) +
  stat_smooth(mapping = aes(x = date, y = totalCases), span= 0.07, show.legend = TRUE) +
  geom_point(colour = "black", alpha = 0.5, size = 1, mapping =  aes(x = date, y = totalCases)) +
  theme_bw() +
  scale_x_date(NULL,
               breaks = scales::breaks_width("1 week"),
               labels = scales::label_date_short()) +
  scale_y_continuous(breaks = seq(0, 40000000, by = 1000000 ))  +
  ylab("total cases") +
  labs (title = "Covid-19 total cases in England",
        subtitle = "Source: https://api.coronavirus.data.gov.uk/v1/data",
        caption = label_dates)

  
  

## log2 of new cases ----
ggplot(covid_data) +
  stat_smooth(mapping = aes(x = date, y = log2newCases), span= 0.2, show.legend = TRUE) +
  geom_point(colour = "black", alpha = 0.5, size = 1, mapping =  aes(x = date, y = log2newCases)) +
  theme_bw() +
  scale_x_date(NULL,
               breaks = scales::breaks_width("1 week"),
               labels = scales::label_date_short()) +
  scale_y_continuous(breaks = seq(0, 15, by = 1)) +
  ylab("new cases (log2)") +
  labs (title = "Covid-19 new cases in UK",
        subtitle = "Source: https://api.coronavirus.data.gov.uk/v1/data",
        caption = label_dates)


## log2 of total cases ----
ggplot(covid_data) +
  stat_smooth(mapping = aes(x = date, y = log2totalCases), span= 0.2, show.legend = TRUE) +
  #geom_point(mapping =  aes(x = date, y = log2totalCases)) +
  geom_point(colour = "black", alpha = 0.5, size = 1, aes(x = date, y = log2totalCases)) +
  theme_bw() +
  scale_x_date(NULL,
               breaks = scales::breaks_width("1 week"),
               labels = scales::label_date_short()) +
  scale_y_continuous(breaks = seq(0, 20, by = 1)) +
  ylab("total cases (log2)") +
  labs (title = "Covid-19 cases in UK",
        subtitle = "Source: https://api.coronavirus.data.gov.uk/v1/data",
        caption = label_dates)



# 
# $length
# [1] 132
# 
# $maxPageLimit
# [1] 1000
# 
# $data






#' Extracts paginated data by requesting all of the pages
#' and combining the results.
#'
#' @param filters    API filters. See the API documentations for 
#'                   additional information.
#'                   
#' @param structure  Structure parameter. See the API documentations 
#'                   for additional information.
#'                   
#' @return list      Comprehensive list of dictionaries containing all 
#'                   the data for the given ``filter`` and ``structure`.`
get_paginated_data <- function (filters, structure) {
  
  endpoint     <- "https://api.coronavirus.data.gov.uk/v1/data"
  results      <- list()
  current_page <- 1
  
  repeat {
    
    httr::GET(
      url   = endpoint,
      query = list(
        filters   = paste(filters, collapse = ";"),
        structure = jsonlite::toJSON(structure, auto_unbox = TRUE),
        page      = current_page
      ),
      timeout(10)
    ) -> response
    
    # Handle errors:
    if ( response$status_code >= 400 ) {
      err_msg = httr::http_status(response)
      stop(err_msg)
    } else if ( response$status_code == 204 ) {
      break
    }
    
    # Convert response from binary to JSON:
    json_text <- content(response, "text")
    dt        <- jsonlite::fromJSON(json_text)
    results   <- rbind(results, dt$data)
    
    if ( is.null( dt$pagination$`next` ) ){
      break
    }
    
    current_page <- current_page + 1;
    
  }
  
  return(results)
  
}


# Create filters:
query_filters <- c(
  "areaType=region"
)

# Create the structure as a list or a list of lists:
query_structure <- list(
  date       = "date", 
  name       = "areaName", 
  code       = "areaCode", 
  daily      = "newCasesBySpecimenDate",
  cumulative = "cumCasesBySpecimenDate"
)

result <- get_paginated_data(query_filters, query_structure)

list(
  "Shape"                = dim(result),
  "Data (first 3 items)" = result[0:3, 0:-1]
) -> report

print(report)































