library(rvest)
library(tidyverse)

# This is all to do with trying to authenticate to Medium via twitter
# but it doesn't work so ignore :-)

library(httr)
library(twitteR)

# from developer.twitter.com
# Access token & access token secret
# 415588776-zoQLxDiid6jYhTq9IYHlMAtcqw6kwDk6SAGie8bU (Access token)
# m9FXaMx6HLWgwrECJC5yLgDsPWlvDXRgn1AeVdWc0 (Access token secret)
# 
# Consumer API keys
# klkqha8pNbHCqClyKjXCg (API key)
# 09udFckXuVXId0CkMym9dojdmvgBbf8K3eDjpw964 (API secret key)
medium_url1 <- "https://medium.com/me/stats"
medium_url2 <- "https://medium.com/citizensonline/stats/stories"
twitter_url <- "https://api.twitter.com/oauth/authenticate"
consumer_key    <- 'klkqha8pNbHCqClyKjXCg'
consumer_secret <- '09udFckXuVXId0CkMym9dojdmvgBbf8K3eDjpw964'
access_token    <- '415588776-zoQLxDiid6jYhTq9IYHlMAtcqw6kwDk6SAGie8bU'
access_secret   <- 'm9FXaMx6HLWgwrECJC5yLgDsPWlvDXRgn1AeVdWc0'

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

twitauth <- oauth_app("twitter",
              # redirect_uri = medium_url,
              key = access_token,
              secret = access_secret
)

twitter_token <- oauth1.0_token(oauth_endpoints("twitter"), app = twitauth)
twitter_token <- "415588776-zoQLxDiid6jYhTq9IYHlMAtcqw6kwDk6SAGie8bU"
result <- GET(paste0(twitter_url, "?oauth_token=", access_token))
page <- httr::GET(url = twitter_url, config(token = twitter_token))


# This is where the actual useful bit starts
# I found that doing "save page as HTML" or even "View source" in Firefox did not,
# for some reason, capture all the entries that were visible on the page. Something something JavaScript.
# However, when I selected just the whole table and did "View selection source", this captured all the rows.
# I could paste this into a <html><head></head><body> template.

# https://medium.com/citizensonline/stats/stories

posts <- read_html("medium_stats_page2.html") %>%
  html_node(".sortableTable") %>% 
    html_nodes("tbody tr:not(.sortableTable-row--dateBucket)")
post_titles <- posts %>%
  html_nodes(".sortableTable-rowTitle div a") %>%
    html_text() %>% 
      enc2utf8()
post_dates <- posts %>% 
  html_nodes("td:nth-of-type(1) .sortableTable-value") %>% 
    html_text() %>% 
      as.numeric() %>%
        as_tibble() %>% 
          mutate(date = round(value/1000)) %>% 
            select(date) %>%
              unlist() %>% 
                lubridate::as_datetime()
post_views <- posts %>% 
  html_nodes("td:nth-of-type(2) .sortableTable-value") %>% 
    html_text() %>% 
      as.numeric()
post_reads <- posts %>% 
  html_nodes("td:nth-of-type(3) .sortableTable-value") %>% 
    html_text() %>% 
      as.numeric()
post_fans <- posts %>% 
  html_nodes("td:nth-of-type(5) .sortableTable-value") %>% 
    html_text() %>% 
      as.numeric()

stats <- tibble(Title = post_titles, Date = post_dates, Views = post_views, Reads = post_reads, Fans = post_fans)
write_csv(stats, "medium_stats2.csv")
