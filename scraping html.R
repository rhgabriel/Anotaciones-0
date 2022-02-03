library(rvest)
library(xml2)
library(dplyr)
url <- "https://web.archive.org/web/20181024132313/http://www.stevetheump.com/Payrolls.htm"
h <- read_html(url)
nodes <- html_nodes(h, "table")
html_text(nodes[[8]])
html_table(nodes[[2]])
html_table(nodes[[3]])
html_table(nodes[[4]])
html_table(nodes[[5]])
html_table(nodes[[6]])
html_table(nodes[[7]])
html_table(nodes[[8]])
sapply(nodes[1:4], html_table, fill=TRUE)

html_table(nodes[[length(nodes)-2]])
html_table(nodes[[length(nodes)-1]])
html_table(nodes[[length(nodes)]])

tab_1 <- html_table(nodes[[10]])
tab_1<- subset(tab_1, select = -X1) %>%
  slice(-c(1))%>%
  setNames(c("team", "Payroll", "Average"))
tab_1
tab_2 <- html_table(nodes[[19]]) %>%
  slice(-c(1))%>%
  setNames(c("team", "Payroll", "Average"))
tab_2
full_join (tab_2, tab_1, by = "team")

