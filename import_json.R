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

print(data)


$length
[1] 132

$maxPageLimit
[1] 1000

$data