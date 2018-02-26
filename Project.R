library(rvest)
library(dplyr)
library(tidyverse)
library(stringr)
library(tibble)

#In function

product<-function(keyword,page)
{
url<-"https://www.amazon.com/product-reviews/%s/ref=cm_cr_arp_d_paging_btm_%i?ie=UTF8&reviewerType=all_reviews&pageNumber=%i"
amazon<-sprintf(url,URLencode(keyword),page,page)
reviewpage<-read_html(amazon)
  


profile_name <- reviewpage%>%
  html_nodes(".review-byline .author")%>%
  html_text(trim=T)%>%
  gsub("By","",.)


personal_rating <- reviewpage%>%
  html_nodes("#cm_cr-review_list  .review-rating")%>%
  html_text()%>%
  str_extract("\\d")%>%
  as.numeric()


description <- reviewpage%>%
  html_nodes("#cm_cr-review_list .a-color-base")%>%
  html_text(trim=T)


review <- reviewpage%>%
  html_nodes(".review-text")%>%
  html_text(trim=T)


verification <- reviewpage%>%
  html_nodes(".review-data.a-spacing-mini")%>%
  html_text(trim=T)%>%
  grepl("Verified Purchase",.)


df <- data.frame(profile_name,personal_rating,description,review,verification)


}
product("B06XCM9LJ4",3)

multiple_page <- function(keyword,page)
{
  map(1:page,function(n)
  {
    product(keyword,n)
  })%>%
    bind_rows()%>%
    mutate(revid=1:n())%>%
    View()
}

multiple_page("B003EM8008",6)


#main page function

main_page <- function(keyword)
{
main <- "https://www.amazon.com/gp/product/%s"
item <- sprintf(main,URLencode(keyword))
items <- read_html(item)
name <- items%>%
  html_node("#productTitle")%>%
  html_text(trim=T)

ratings <- items%>%
  html_node(".arp-rating-out-of-text")%>%
  html_text()%>%
  str_extract("\\d{1}.\\d{1}")%>%
  as.numeric()

price <- items%>%
  html_node("#priceblock_ourprice")%>%
  html_text(trim=T)%>%
  gsub("\\$","",.)%>%
  as.numeric()

no_of_reviews <- items%>%
  html_node("#acrCustomerReviewText")%>%
  html_text(trim=T)%>%
  gsub("[A-z]","",.)%>%
  gsub(",","",.)%>%
  as.numeric()

product_id <- keyword
  
df <- data_frame(product_id,name,price,ratings,no_of_reviews)
View(df)

}
main_page("B003EM8008")

#creating a database
main1 <- "https://www.amazon.com/gp/product/B003EM8008"
items1 <- read_html(main1)
name1 <- items1%>%
  html_node("#productTitle")%>%
  html_text(trim=T)

ratings1 <- items1%>%
  html_node(".arp-rating-out-of-text")%>%
  html_text()%>%
  str_extract("\\d{1}.\\d{1}")%>%
  as.numeric()

price1 <- items1%>%
  html_node("#priceblock_ourprice")%>%
  html_text(trim=T)%>%
  gsub("\\$","",.)%>%
  as.numeric()

no_of_reviews1 <- items1%>%
  html_node("#acrCustomerReviewText")%>%
  html_text(trim=T)%>%
  gsub("[A-z]","",.)%>%
  gsub(",","",.)%>%
  as.numeric()

product_id1 <- "B003EM8008"

df1 <- data_frame(product_id1,name1,price1,ratings1,no_of_reviews1)
View(df1)

#creating database for maintable

str(no_of_reviews1)
library(tidyverse)
library(sqldf)

maintable<-dbConnect(SQLite(),dbname="maintable.sqlite")
dbSendQuery(conn=maintable,"create table maintable(
            product_id1 text primary key,
            name1 text,
            price1 num,
            ratings1 num,
            no_of_reviews1 num)
            without rowid")
dbWriteTable(conn=maintable,name="maintable",value=df1,row.names=F,header=T,overwrite=T)
dbGetQuery(conn=maintable,"select * from maintable")


