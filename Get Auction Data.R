library(rvest)
library(tidyverse)

#url for webscraping

#last date of auction date
date <- as.Date("2020-04-04")
#base url for webscraping
base_url <- "https://www.domain.com.au/auction-results/sydney/"
data <- data.frame()
i <- 110

#repeat 100 times
while (i > 0 ){ 
  #url = base_url + date
  url <- paste(base_url, date, sep="")
  #read auction sales dta from the url
  flag <- TRUE
  tryCatch(
    expr = {temps <- read_html(url) %>%
    html_nodes(xpath ='//*/article')},
    error = function(e) flag <<- FALSE)
  if (!flag){
    i <- i - 1
    date <- date - 7
    #sleep to prevent possible data scraping interruption
    Sys.sleep(runif(1, 0, 1))
    next
  }
  
  for (temp in temps){
    #get suburb data
    suburb <- temp %>%
      html_nodes(xpath = 'header/h3') %>%
      html_text()
    
    property_address <- temp %>%
      html_nodes(xpath = 'ul/li[1]/a') %>%
      html_text()
    
    property_link <- temp %>%
      html_nodes(xpath = 'ul/li[1]/a') %>%
      html_attr("href")
    
    property_type <- temp %>%
      html_nodes(xpath = 'ul/li[2]/span[1]') %>%
      html_text()
    
    property_bedrooms <- c()
    bedrooms <- temp %>%
      html_nodes(xpath = 'ul/li[2]')
    for (bedroom in bedrooms){
      property_bedroom <- bedroom %>%
        html_node(xpath = 'span[2]') %>%
        html_text()
      property_bedrooms <- append(property_bedrooms, property_bedroom)
    }
    
    auction_result <- temp %>%
      html_nodes(xpath = 'ul/li[3]/span[1]') %>%
      html_text()
    
    property_prices <- c() 
    prices <- temp %>%
      html_nodes(xpath = 'ul/li[3]')
    for (price in prices){
      property_price <- price %>%
        html_node(xpath = 'span[2]') %>%
        html_text()
      property_prices <- append(property_prices, property_price)
    }
    
    
    data <-  data %>%
      rbind(data.frame(rep(date, length(property_address)),
                       property_address,
                       rep(suburb, length(property_address)),
                       property_link,
                       property_type,
                       property_bedrooms,
                       auction_result,
                       property_prices))
  }
  i <- i - 1
  date <- date - 7 #Saturday is the auction day
  #sleep to prevent possible data scraping interruption
  Sys.sleep(runif(1, 0, 1))
}
colnames(data) = c('date',
                   'property_address', 
                   'suburb',
                   'property_link',
                   'property_type',
                   'property_bedrooms',
                   'auction_result',
                   'property_prices')


strsplit(as.character(data$property_link[3159]), split = "-", fixed = TRUE)

postcode <- sapply(
  strsplit(as.character(data$property_link), split = "-", fixed = TRUE), function(x){
    if (length(x) == 1){
      NA
    } else if (nchar('[['(x, length(x))) == 4){
      '[['(x, length(x))
    } else {
      '[['(x, length(x)-1)
    }
  }
)

data$post_code <- postcode
write.csv(data, "property.csv", row.names = FALSE)

