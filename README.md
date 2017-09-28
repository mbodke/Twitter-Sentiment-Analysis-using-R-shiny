# Projects 

The objective of this project is to create a user interface using Shiny to enable users to evaluate the 
sentiments of the tweets based on either a given twitter username or a trending Twitter hashtag. The 
interface also allows the user to give the number of tweets that they would like to analyze along with the 
radius of the area in the United States that they would like to cover. Our code extracts data from Twitter, 
performs text mining to build word clouds for positive and negative sentiments and understand the overall 
sentiments for the given search criteria. 
The purpose is to investigate whether removing stopwords helps or hampers the effectiveness of twitter 
sentiment classification methods. To this end, we applied two different stopword identification methods 
to Twitter data. We then compared and contrasted the various approaches to perform text mining and 
evaluate them on different criteria such as reduction in feature space, data sparsity and the classification 
performance. One of our major understanding from this project was to identify the advantages of dynamic 
stopword lists against traditional lists. Traditional lists such as Van’s and Brown’s stoplists are outdated 
and not domain specific. In order to create dynamic stoplists, we used Zipf’s Law using Inverse Document 
Frequency to eliminate High Frequency terms and then compared the results obtained by traditional 
stoplists. 
This report briefly explains the features of the user interface built by the team and how it integrates with 
the code to make it more interactive and easy to use. Some of the challenges we faced were difficulty to 
analyze the Twitter data since it is limited to 140 characters and contains lot of noisy data and excessive 
use of abbreviations, irregular expressions, and infrequent words. We have also identified areas of future 
work that can be done to include more enhancements and improve the performance of the user interface. 

