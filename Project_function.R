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


multiple_page <- function(keyword,page)
{
  x <- map(1:page,function(n)
  {
    product(keyword,n)
  })%>%
    bind_rows()%>%
    mutate(revid=1:n())
  
dbWriteTable(conn=da5020,name="reviewtable",value=x,row.names=F,header=T,overwrite=T)

}

multiple_page("B00JDA2ODY",6)


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

  dbWriteTable(conn=da5020,name="maintable",value=df,row.names=F,header=T,overwrite=T)
  
}
main_page("B00JDA2ODY")

dbGetQuery(conn=da5020,"select * from maintable")
dbGetQuery(conn=da5020,"select * from reviewtable")
