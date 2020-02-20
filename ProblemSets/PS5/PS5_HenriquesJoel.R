library(rvest)
library(tidyquant)
library(fredr)
library(tidyverse)
library(ggthemes)
library(httr)

Wins <- read_html("https://en.wikipedia.org/wiki/Ballon_d%27Or") 
listoutput <- Wins %>% html_nodes("#mw-content-text > div > table:nth-child(20)") %>% 
  html_table(fill=TRUE)
table <- listoutput[[1]]

MSFT <- tq_get("MSFT", get = "stock.prices", from = "2010-01-01", to = "2020-01-01")
ggplot(data = MSFT, aes(x = date, y = close)) +
  geom_line() + 
  labs(title = "MSFT Line Chart", y = "Closing Price", x = "") +
  theme_wsj() 

