library(rvest)
library(tidyverse)
url <- "https://en.wikipedia.org/w/index.php?title=Opinion_polling_for_the_United_Kingdom_European_Union_membership_referendum&oldid=896735054"
h <- read_html(url)
nodes <- html_nodes(h, "table")
html_table(nodes[[6]], fill = TRUE)
sapply(nodes[6], html_table, fill=TRUE)
