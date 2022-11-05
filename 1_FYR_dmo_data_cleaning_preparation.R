library(tidyverse)
library(qdap)
library(zoo)
library(stringi)
library(tm)
library(cld2)
library(arrow)

############################################################################
### IMPORTANT - datasets are not available due to the copyright reason   ###
### This script is for your reference only                               ###
############################################################################

# 1. Import feather file of tweets and replies ----
## Load initial datasets. The original datasets were not provided (copyrighted)
dmo_tweepy <- read_feather("dmo_tweets.feather") |>
        dplyr::select(-index)
reply_tweepy <- read_feather("dmo_replies.feather") |>
        dplyr::select(-index)

# 2. Identify original tweets and replies ----
## The purpose of this step is to separate self-reply tweets from other replies
## Self-reply tweets and quoted tweets will be included as original tweets
## reply_self_notna indicates the replies to the valid accounts
reply_self_notna <- reply_tweepy[!is.na(reply_tweepy$referenced_tweets),] |>
        rowwise() |>
        mutate(type = unlist(referenced_tweets)[2],
               interaction_id = unlist(referenced_tweets)[1]) |>
        ungroup() |>
        dplyr::select(-referenced_tweets)

## reply_self_na indicates the replies to the invalid accounts
reply_self_na <- reply_tweepy[is.na(reply_tweepy$referenced_tweets),] |>
        mutate(type = case_when(is.na(in_reply_to_user_id) ~ "quoted",
                                TRUE ~ "replied_to"),
               interaction_id = "99") |>
        dplyr::select(-referenced_tweets)

## Combine the two dataframes
## The "self" column determines whether the tweet is "quoted" or "self-reply". These will be included as the original tweets
reply_self <- reply_self_notna |> bind_rows(reply_self_na) |>
        mutate(self = case_when(type == "quoted" ~ "yes",
                                in_reply_to_user_id == author_id ~ "yes",
                                TRUE ~ "no"))

# 3 Revise the tweet and reply dataframes ----
dmo_revise <- dmo_tweepy |>
        bind_rows(reply_self[reply_self$self == "yes",])
reply_revise <- reply_self[reply_self$self == "no",]

dmo_cleaned_original <- dmo_revise |>
        dplyr::filter(is.na(withheld[1])) |>
        dplyr::select(-withheld) |>
        mutate(cleaned_full_text = str_replace_all(text,"[\r\n]"," ")) |> # Remove new line punc for sentiment analysis
        mutate(cleaned_text = gsub(","," ", text)) |> #Remove commas
        mutate(cleaned_text = str_remove_all(cleaned_text,"(@[A-Za-z0-9]+)|([^0-9A-Za-z \t])|(\\w+:\\/\\/\\S+)")) |> # Remove all punctuations and mentions
        mutate(pre_text = qdap::replace_abbreviation(cleaned_text)) |>
        mutate(pre_text = qdap::replace_contraction(pre_text)) |>
        mutate(pre_text = gsub("[[:punct:]]", " ", x = pre_text)) |>
        mutate(pre_text = stripWhitespace(removeNumbers(tolower(pre_text)))) |>
        filter(!str_detect(pre_text,"^rt")) |>
        filter(!str_detect(pre_text,"^my week")) |>
        filter(nchar(pre_text) > 3) |>
        mutate(language = detect_language(pre_text)) |>
        mutate(created_at = as.Date(created_at)) |>
        filter(language == "en") |>
        mutate(ascii = stringi::stri_enc_mark(pre_text)) |>
        filter(ascii == "ASCII") |>
        dplyr::select(-language,-ascii) 

dmo_cleaned <- dmo_cleaned_original |>
        dplyr::select(id,conversation_id,text,created_at,
                      author_id,retweet_count,reply_count,like_count,
                      quote_count,country, cleaned_full_text,
                      cleaned_text, pre_text)

reply_cleaned_original <- reply_revise |>
        mutate(cleaned_full_text = str_replace_all(text,"[\r\n]"," ")) |> # Remove new line punc for sentiment analysis
        mutate(cleaned_text = gsub(","," ", text)) |> #Remove commas
        mutate(cleaned_text = str_remove_all(cleaned_text,"(@[A-Za-z0-9]+)|([^0-9A-Za-z \t])|(\\w+:\\/\\/\\S+)")) |> # Remove all punctuations and mentions
        mutate(pre_text = qdap::replace_abbreviation(cleaned_text)) |>
        mutate(pre_text = qdap::replace_contraction(pre_text)) |>
        mutate(pre_text = gsub("[[:punct:]]", " ", x = pre_text)) |>
        mutate(pre_text = stripWhitespace(removeNumbers(tolower(pre_text)))) |>
        filter(!str_detect(pre_text,"^rt")) |>
        filter(nchar(pre_text) > 3) |>
        mutate(language = detect_language(pre_text)) |>
        mutate(created_at = as.Date(created_at)) |>
        filter(language == "en") |>
        mutate(ascii = stringi::stri_enc_mark(pre_text)) |>
        filter(ascii == "ASCII") |>
        dplyr::select(-language,-ascii) 

reply_cleaned <- reply_cleaned_original |>
        dplyr::select(id,conversation_id,
                      in_reply_to_user_id,
                      text,created_at,
                      author_id,retweet_count,reply_count,like_count,
                      quote_count,country, cleaned_full_text,
                      cleaned_text, pre_text)