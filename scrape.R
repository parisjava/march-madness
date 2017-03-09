library(magrittr) # to use %>% operator
library(rvest)    # for web scraping

###
# scrape is a function that creates a data frame corresponding to
# the table on a webpage. Has been tested only with pages from teamrankings.com
# which have very regular tables (i.e. always 8 columns, one row per team)
# "td" = table data in HTML parlance
# "th" = table heading in HTML parlance
# Different seasons have different numbers of teams, thus number of rows not known in
# advance. But 8 columns, 1 row per team means length(data)/8 rows.
# @param url a web address containing a table
# @return M data frame containing the information from table on url
# Credit: http://r-exercises.com/2016/12/20/web-scraping-solutions/
###

scrape <- function(url) {
  TAB=read_html(url)%>%html_nodes('td')%>%html_text()
  NAMES=read_html(url)%>%html_nodes('th')%>%html_text()
  M=data.frame(matrix(TAB,ncol=8,nrow=length(TAB)/8,byrow=T))
  colnames(M) = NAMES
  return(M)
}

# read from the "home page" of Stats on this website, and get the urls of all
# pages pertaining to a certain stat
home <- "https://www.teamrankings.com/ncb/stats/"
links <- read_html(home)%>%html_nodes('a')%>%html_attrs()
links <- unlist(links)
links <- links[grep("ncaa-basketball/stat",links,perl=TRUE,value=FALSE)]
links <- paste0("https://www.teamrankings.com",links)

years = c("2017","2016","2015","2014","2013")
combinations <- as.vector(outer(links,years,paste,sep="?date="))
urls <- paste0(combinations,"-05-01") #May 1 is after season end, so gets full season stats
vnames <- gsub("-","_",combinations) # replace all '-' in combinations with '_'

get.filename.to.write <- function (url) {
  start <- unlist(gregexpr(pattern ='/stat/',url)) + 6 # +6 for / s t a t /
  question.mark <- unlist(gregexpr(pattern = '\\?',url))
  stat <- substr(url,start,question.mark - 1) 
  stat <- gsub("-","_",stat) # replace '-' with '_'
  year <- substr(url,question.mark + 6, question.mark + 9)
  return(paste0(stat,"_",year,".csv"))
}

for (i in 1:length(urls)) {
  file.name <- get.filename.to.write(urls[i])
  print(file.name)
  M <- scrape(urls[i])
  write.csv(M, file.name)
}