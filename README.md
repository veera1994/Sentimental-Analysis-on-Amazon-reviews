# Sentimental-Analysis-on-Amazon-reviews

Abstract:
Sentiment analysis or opinion mining is one of the major tasks of NLP (Natural Language Processing). Sentiment analysis has gain much attention in recent years. In this paper, we aim to tackle the problem of sentiment polarity categorization, which is one of the fundamental problems of sentiment analysis. A general process for sentiment polarity categorization is proposed with detailed process descriptions. Data used in this study are online product reviews collected from Amazon.com. Experiments for both sentencelevel categorization and reviewlevel categorization are performed with promising outcomes.

Methodology:
1. Introduction to the website
2. Scrape the reviews data and main page data from respective amazon website using rvest in r.
3. Read the data in r and create a dataframe.
4. Clean the dataset to remove inaccurate, irrelevant data and then replacing, modifying, or deleting the dirty or coarse data.
5. Store the tidy data in SQLite database.
6. Retrieve the required data by giving queries.
7. Perform a Sentimental Analysis on the reviews data to get the emotions of people.

Conclusions from the Project:
Q1. Will this work only for one product?
Answer. We have created a function to generalise the analysis. Product id is used as a parameter to do analysis on the reviews.
Q2. Difference between Ratings and sentiment score?
Answer. Total Average Ratings in this case is 4.5 but Average Sentiment score is 0.2 Ratings are biased since it does not show the real emotions of the people
Q3. Most used Negative Word or Fault in the product?
Answer. Noise
Q4. Most used Positive Word or Gain in the product?
Answer. Great and Comfortable

Learning Outcomes:
● Web Scraping using Rvest.
● Cleaning and formatting data using general expressions.
● Creating Database model with relationships.
● Storing data in a database using SQLLite.
● Retrieving data from the database using queries.
● Text mining and finding various outcomes
