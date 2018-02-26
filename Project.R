library(rvest)
library(dplyr)
library(tidyverse)
library(stringr)
library(tibble)
library(RSQLite)
library(sentimentr)
#Reviews data function

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

product_id<-keyword
df <- data.frame(profile_name,personal_rating,description,review,verification,product_id)


}
ma<-dbConnect(SQLite(),dbname="x.sqlite")
dbSendQuery(conn=ma,"create table maintable(
            product_id1 text primary key,
            name1 text,
            price1 num,
            ratings1 num,
            no_of_reviews1 num)
            without rowid")
dbSendQuery(conn=ma,"create table reviewtable(
            profile_name text,
            personal_rating num,
            description text,
            review text,
            verification logical,
            product_id1 text,
            foreign key(product_id1) references maintable(product_id1))")



multiple_page <- function(keyword,page)
{
  x <- map(1:page,function(n)
  {
    product(keyword,n)
  })%>%
    bind_rows()
dbWriteTable(conn=ma,name="reviewtable",value=x,row.names=F,header=T,overwrite=T)

}

multiple_page("B0756CYWWD",4)


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

  dbWriteTable(conn=ma,name="maintable",value=df,row.names=F,header=T,overwrite=T)
  
}
main_page("B0756CYWWD")

a<-dbGetQuery(conn=ma,"select * from maintable m
           join reviewtable r
           on m.product_id=r.product_id")
head(a)
dbGetQuery(conn=ma,"select * from maintable")

sent_agg <- with(a, sentiment_by(review))
head(sent_agg)
par(mfrow=c(1,2))
with(a, hist(personal_rating))
with(sent_agg, hist(ave_sentiment))
mean(a$personal_rating)
mean(sent_agg$ave_sentiment)


best_reviews <- slice(a, top_n(sent_agg, 10, ave_sentiment)$element_id)
with(best_reviews, sentiment_by(review)) %>% highlight()

worst_reviews <- slice(a, top_n(sent_agg, 10, -ave_sentiment)$element_id)
with(worst_reviews, sentiment_by(review)) %>% highlight()

library(dplyr)
library(stringr)
library(tidytext)

get_sentiments("afinn")
get_sentiments("bing")
get_sentiments("nrc")

b<-a$review
text_tb <- tibble(c = seq_along(b),
                  text = b)
head(text_tb)
x<-text_tb %>%
  unnest_tokens(word, text)
x
x%>%
  count(word,sort=T)
x %>%
  anti_join(stop_words) %>%
  count(word, sort = TRUE)
x %>%
  right_join(get_sentiments("nrc")) %>%
  filter(!is.na(sentiment)) %>%
  count(sentiment, sort = TRUE)

bing_word_counts <- x %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ggplot(aes(reorder(word, n), n, fill = sentiment)) +
  geom_bar(alpha = 0.8, stat = "identity", show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment", x = NULL) +
  coord_flip()

library(wordcloud)

x %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))

library(reshape2)

x %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("#F8766D", "#00BFC4"),
                   max.words = 100)

afinn <- x %>%
  mutate(word_count = 1:n(),
         index = word_count %/% 500 + 1) %>% 
  inner_join(get_sentiments("afinn")) %>%
  group_by(index) %>%
  summarise(sentiment = sum(score)) %>%
  mutate(method = "AFINN")
afinn



