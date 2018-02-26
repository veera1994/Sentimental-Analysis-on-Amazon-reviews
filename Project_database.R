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


library(tidyverse)
library(sqldf)

dbSendQuery(conn=da5020,"drop table maintable" )
dbSendQuery(conn=da5020,"drop table reviewtable" )

dbListTables(da5020)

da5020<-dbConnect(SQLite(),dbname="da5020.sqlite")
dbSendQuery(conn=da5020,"create table maintable(
            product_id1 text primary key,
            name1 text,
            price1 num,
            ratings1 num,
            no_of_reviews1 num)
            without rowid")
dbWriteTable(conn=da5020,name="maintable",value=df1,row.names=F,header=T,overwrite=T)

dbGetQuery(conn=da5020,"select * from maintable")



# creating database for review table

url <- "https://www.amazon.com/Panasonic-RP-HJE120-K-In-Ear-Headphone-Black/product-reviews/B003EM8008/ref=cm_cr_dp_d_show_all_btm?ie=UTF8&reviewerType=all_reviews"
content <- read_html(url)
profile_name <- content%>%
  html_nodes(".review-byline .author")%>%
  html_text(trim=T)%>%
  gsub("By","",.)


personal_rating <- content%>%
  html_nodes("#cm_cr-review_list  .review-rating")%>%
  html_text()%>%
  str_extract("\\d")%>%
  as.numeric()


description <- content%>%
  html_nodes("#cm_cr-review_list .a-color-base")%>%
  html_text(trim=T)


review <- content%>%
  html_nodes(".review-text")%>%
  html_text(trim=T)


verification <- content%>%
  html_nodes(".review-data.a-spacing-mini")%>%
  html_text(trim=T)%>%
  grepl("Verified Purchase",.)
product_id1 <- "B003EM8008"


df <- data.frame(profile_name,personal_rating,description,review,verification,product_id1)
View(df)

dbSendQuery(conn=da5020,"create table reviewtable(
            profile_name text,
            personal_rating num,
            description text,
            review text,
            verification logical,
            product_id1 text,
            foreign key(product_id1) references maintable(product_id1))")

dbWriteTable(conn=da5020,name="reviewtable",value=df,row.names=F,header=T,overwrite=T)

dbGetQuery(conn=da5020,"select * from reviewtable r
           join maintable m
           on r.product_id1=m.product_id1")
dbGetQuery(conn=da5020,"select * from reviewtable")
dbGetQuery(conn=da5020,"select * from maintable")


