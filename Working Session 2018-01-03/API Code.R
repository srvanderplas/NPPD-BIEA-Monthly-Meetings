# --- Setup --------------------------------------------------------------------
pkgs <- installed.packages()

require_pkgs <- c("dplyr", "tidyr", "magrittr", "lubridate", "stringr", 
                  "ggplot2", "jsonlite", "httr", "Quandl", "httpuv")

pkgs_to_install <- require_pkgs[!require_pkgs %in% pkgs]

if (length(pkgs_to_install) > 0) {
  install.packages(pkgs_to_install)
}

# Clean up
rm(pkgs, require_pkgs, pkgs_to_install)

# Load packages

# General data libraries
library(dplyr) # Working with data
library(tidyr) # Reshaping data
library(magrittr) # %>% pipe
library(lubridate) # dates and times
library(stringr) # string manipulation

# JSON/API libraries
library(jsonlite) # fromJSON function
library(httpuv)
library(httr) # GET, content functions

# --- Famous Quotes ------------------------------------------------------------
url <- "https://andruxnet-random-famous-quotes.p.mashape.com/"

# https://market.mashape.com/andruxnet/random-famous-quotes

key <- "fhfr95fNmbmshLOdzucukM4rsSnGp1ZbUVEjsn2dtrWFykYGGn" # From demo code

# Create a list of headers
header_list <- c("X-Mashape-Key" = key, 
                 "Content-Type" = "application/x-www-form-urlencoded",
                 "Accept" = "application/json")

GET(url)
# Status 401 = unauthorized. Need to include a key for id purposes

GET(url, add_headers(header_list))
# Status 200 = it worked!

# Store in an object this time...
req <- GET(url, add_headers(header_list))

# Get response from API using content() function
res <- content(req, as = "text")
# Convert from JSON to an R list object
fromJSON(res)


## For some reason, I can't make this return multiple values...?
# Add additional parameters
params <- c("cat" = "famous", 
            "count" = "10")


GET(url = url, add_headers(header_list), 
    query = as.list(params))
# Note the response URL has changed

req <- GET(url = url, add_headers(header_list), 
            query = as.list(params))
# Get response from API using content() function
res <- content(req, as = "text")
# Convert from JSON to an R list object
fromJSON(res)


# --- ProPublica ---------------------------------------------------------------
# Search ProPublica for public power nonprofits
search_query <- "%2BPUBLIC%20%2BPOWER&state%5Bid%5D=NE"

url <- paste0(
  "https://projects.propublica.org/nonprofits/api/v2/search.json?q=",
  search_query)

# Method 1: Get/content()
req <- GET(url = url)
res <- content(req, type = "text")
# Ugly list-data

# Method 2: fromJSON
res <- fromJSON(url)

# --- Quandl -------------------------------------------------------------------
# Get a login and API key: https://www.quandl.com/?modal=register
key <- "1hHPXBgH3x_ZA72dC65w"

# Install the Quandl R package (done above):
# install.packages("Quandl")

library(Quandl)

# URL to explore: https://www.quandl.com/data/LME-London-Metal-Exchange

Quandl.api_key(key)
# Get 1 year's worth of data from the London Metal Exchange on Cobalt prices
cobalt_data <- Quandl('LME/PR_CO', start_date = '2016-12-29', 
                      end_date = '2017-12-29')

library(ggplot2)
qplot(x = Date, y = `Cash Buyer`, geom = "line", data = cobalt_data) + 
  ylab("Cobalt Price ($, Cash)")

cobalt_long <- cobalt_data %>%
  gather(key = "Type", value = value, -Date)
qplot(x = Date, y = value, color = Type, geom = "line", data = cobalt_long) + 
  ylab("Cobalt Price ($)")


# --- Weather ------------------------------------------------------------------
lat <- 41.4303
lon <- -97.3594

# Get information about the location: 
baseurl <- "https://api.weather.gov/points/"
url <- paste0(baseurl, lat, ",", lon)
columbusInfo <- fromJSON(url)

str(columbusInfo)

# Hourly forecasts
# Get URL from information object
hourlyForecast <- fromJSON(columbusInfo$properties$forecastHourly)


# Plot the forecasted temperature
hourlyForecastData <- hourlyForecast$properties$periods

# First, get date/time data into a reasonable form...
hourlyForecastData <- hourlyForecastData %>%
  mutate(
    startTime = ymd_hms(startTime, tz = "America/Chicago"),
    endTime = ymd_hms(endTime, tz = "America/Chicago"),
    midTime = startTime + minutes(30)
  ) %>%
  # Get a numeric estimate for wind speed - take the number closest to the mph
  mutate(
    windSpeedNum = str_extract(windSpeed, "\\d{1,} mph") %>%
      str_replace(" mph", "") %>%
      as.numeric()
  )

# Plot temperature using midTime
qplot(x = midTime, y = temperature, data = hourlyForecastData, geom = "line")

# Plot temperature using start and end time
ggplot(data = hourlyForecastData) + 
  geom_segment(aes(x = startTime, y = temperature, xend = endTime, yend = temperature))

# Plot windSpeedNum using midTime
qplot(x = midTime, y = windSpeedNum, data = hourlyForecastData, geom = "line")

# Correlate temperature and windSpeedNum
qplot(x = temperature, y = windSpeedNum, geom = "jitter", data = hourlyForecastData) + 
  geom_smooth(method = "lm")


# Get grid-based forecast

gridForecast <- fromJSON(columbusInfo$properties$forecastGridData)

## Would need to write a function to coerce this into some sort of data frame
## What follows is incomplete but is a start at how I'd approach it.

# Create a function to make variables into reasonable data
format_grid_forecast <- function(x, name) {
  
  # Handle possibly NULL values - allowed in JSON but not in R objects
  if ("values" %in% names(x)) {
    if (length(x$values) > 0) {
      z <- as_data_frame(x$values)
    } else {
      z <- data_frame(values = NA)
    }
  } else {
    z <- as_data_frame(x)
  }
  
  df <- cbind(
    variable = name, 
    z
  )
  
  df
}

library(purrr)
# This is more advanced than you need to worry about...
res <- gridForecast$properties %>%
  data_frame(x = ., name = names(.)) %>%
  pmap(format_grid_forecast)


# --- Github -------------------------------------------------------------------
# Stolen from https://gist.github.com/mGalarnyk/533ccfd1bb46680925448774bcb71647

# Can be github, linkedin etc depending on application
oauth_endpoints("github")

# Get from https://github.com/settings/applications/
myapp <- oauth_app(appname = "Access via R",
                   key = "67a9c8b9b10cc3ca91ab",
                   secret = "677b83c6b61a222ef0d8ba0ae34ea936197b061d")

# Get a token
github_token <- oauth2.0_token(oauth_endpoints("github"), myapp)

# Actually start using the API
gtoken <- config(token = github_token)

# Get all of my repositories
req <- GET("https://api.github.com/users/srvanderplas/repos", gtoken)

# Check to make sure no errors occurred with the request. 
# stop_for_status will convert any HTTP errors into R errors
stop_for_status(req)

# Extract content from the request
res <- content(req, as = "text")
github_repos <- fromJSON(res, simplifyDataFrame = T, flatten = T)

# Get only github repositories with associated webpages
filter(github_repos, has_pages)

# Get the most recently updated repository
github_repos %>%
  # Convert updated time to a datetime object
  mutate(updated_at = ymd_hms(updated_at)) %>%
  arrange(desc(updated_at)) %>%
  filter(row_number() == 1)

# See what the most recent commits from NPPD-BIEA-Monthly-Meetings are
req2 <- GET("https://api.github.com/repos/srvanderplas/NPPD-BIEA-Monthly-Meetings/commits", gtoken)
stop_for_status(req2)

res2 <- content(req2, as = "text") %>%
  fromJSON(simplifyDataFrame = T, flatten = T)

# Look at the data frame, but ignore any variable with 
# "url", "committer", "payload", "signature", or "parents in the name - 
# too many variables
select(res2, -matches("url|committer|signature|payload|parents"))

# Look at all commit messages for the repo
res2$commit.message
