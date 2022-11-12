# DMO_TWEETS
 
This project analyses tweets posted by 113 destination marketing organisations (DMOs) worldwide from January 2017 to September 2022

<h1> Scripts </h1>
<ol>
<li> 0_dmo_tweet_collection.ipynb - collect tweets separated into tweets and interactions (quotes and replies). <br>
    * It loads DMOs twitter handles from dmo_twitter.csv <br><br>
<li> 1_FYR_dmo_data_cleaning_preparation.R - clean and prepare the data. It's for your reference (FYR) only because the datasets were not available because of Twitter's copyright <br><br>
<li> 2_dmo_visualise_usage_pattern.R - analyse and visualise DMOs' usage patterns. <br>
    * It loads dmo_cleaned.feather and reply_cleaned.feather <br><br>
<li> 3_FYR_dmo_lda_nrc.R - perform topic modelling (LDA) and sentiment analysis using NRC EmoLex. This script is for your reference only (Twitter copyrighted data)<br><br>
<li> 4_dmo_pca.R - perform principal component analysis to reveal 4 general content strategies <br>
    * It loads dmo_final.feather
</ol>
