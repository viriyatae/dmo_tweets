library(tidyverse)
library(zoo)
library(ggrepel)
library(arrow)
library(ggpubr)
library(scales)
library(xts)

dmo_cleaned <- read_feather("dmo_cleaned.feather")
reply_cleaned <- read_feather("reply_cleaned.feather")

# 1. Plot overall tweets and replies timeline ----
## Create tweet 
tweet_xts <- xts(dmo_cleaned[,c("country")], order.by = dmo_cleaned$created_at)
tweet_per_month <- apply.monthly(tweet_xts, FUN = nrow)
time(tweet_per_month) <- lubridate::floor_date(time(tweet_per_month), unit = "month")
overall_tweet_df <- tweet_per_month |> as.data.frame() %>% mutate(date = rownames(.)) |>
        mutate(date = ymd(date)) |>
        set_colnames(c("freq","date"))

reply_xts <- xts(reply_cleaned[,c("country")], order.by = reply_cleaned$created_at)
reply_per_month <- apply.monthly(reply_xts, FUN = nrow)
time(reply_per_month) <- lubridate::floor_date(time(reply_per_month), unit = "month")
reply_df <- reply_per_month |> as.data.frame() %>% mutate(date = rownames(.)) |>
        mutate(date = ymd(date)) |>
        set_colnames(c("freq","date"))

## Plot the monthly tweets and replies timeline
ggplot(mapping = aes(x = date, y = freq)) +
        geom_point(data = overall_tweet_df, aes(color = "tweet")) +
        geom_line(data = overall_tweet_df, aes(color = "tweet")) +
        ggrepel::geom_text_repel(data = overall_tweet_df, aes(label = freq), size = 3, direction = "y", segment.color = NA) +
        geom_point(data = reply_df, aes(color = "reply")) +
        geom_line(data = reply_df, aes(color = "reply")) +
        ggrepel::geom_text_repel(data = reply_df, aes(label = freq), size = 3, direction = "y", segment.color = NA) +
        theme_light() +
        ylab("Tweets / replies per month") +
        scale_x_date(expand = expansion(mult = c(0,0),
                                        add = c(0,60))) +
        theme(axis.title.x = element_blank(),
              legend.title = element_blank(),
              legend.position = "right",
              strip.background = element_rect(fill = "#777777"),
              strip.text = element_text(colour = 'white')) +
        scale_colour_manual(values = c("tweet" = "seagreen", "reply" = "salmon")) +
        scale_x_date(breaks = scales::pretty_breaks(n = 12),
                     minor_breaks = scales::pretty_breaks(n = 36)) +
        geom_vline(xintercept = as.numeric(as.Date("2020-01-01")), colour = "red", linetype = "dotted") +
        ylim(0, 5000)

# 2. Tweet usage patterns before and during the pandemic ----
## 2.1 Tweets by country ----
tweet_countries <- dmo_cleaned |>
        group_by(created_at, country) |>
        summarise(freq = n()) |>
        ungroup() |>
        pivot_wider(names_from = "country",
                    values_from = "freq") %>%
        mutate(across(2:length(.), ~replace_na(.x, 0)))

tweet_xts_country <- xts(tweet_countries[,2:length(tweet_countries)], order.by = tweet_countries$created_at)

## 2.2 Monthly tweets by country ----
xts_tweet_df <- apply.monthly(tweet_xts_country, FUN = colSums)
time(xts_tweet_df) <- lubridate::floor_date(time(xts_tweet_df), unit = "month")

## 2.3 Average monthly tweet before and during the pandemic  ----
tweet_df <- xts_tweet_df |> as.data.frame() %>% mutate(date = rownames(.)) |>
        pivot_longer(cols = 1:(length(tweet_countries)-1), names_to = "country", values_to = "freq") |>
        mutate(date = ymd(date)) |>
        mutate(period = case_when(date < "2020-01-01" ~ "before",
                                  date >= "2020-01-01" ~ "during")) |>
        group_by(country,period) |>
        summarise(avg = mean(freq))

## Country order
tweet_order <- (tweet_df |> filter(period == "during") |> arrange(desc(avg)))$country %>%
        str_replace_all("_"," ") %>%
        str_to_title() %>%
        str_replace_all("And", "and")

## Revise the tweet_df for visualisation
tweet_df <- tweet_df |>
        mutate(country_full = str_to_title(str_replace_all(country, "_"," "))) |>
        mutate(country_full = str_replace_all(country_full, "And", "and")) |>
        mutate(country_full = factor(country_full, levels = tweet_order))

ggplot(data = tweet_df, mapping = aes(x = country_full, y = avg, color = period)) +
        geom_line(aes(group = country_full),
                  color = "grey",
                  size = 0.5,
                  arrow = arrow(ends = "last", type = "open", length = unit(0.01,"npc"))) +
        geom_point() +
        xlab("") +
        ylab("Average no. of tweets per month") +
        coord_flip() +
        theme_light() +
        scale_color_viridis_d(option = "magma", direction = 1, end = 0.8) +
        theme(legend.position = "top", legend.title = element_blank())

# 3. Reply pattern before and during the pandemic
## 3.1 Reply by country ----
reply_countries <- reply_cleaned |>
        group_by(created_at, country) |>
        summarise(freq = n()) |>
        ungroup() |>
        pivot_wider(names_from = "country",
                    values_from = "freq") %>%
        mutate(across(2:length(.), ~replace_na(.x, 0)))

## 3.2 Monthly replies by country ----
reply_xts_country <- xts(reply_countries[,2:113], order.by = reply_countries$created_at)
xts_reply_df <- apply.monthly(reply_xts_country, FUN = colSums)
time(xts_reply_df) <- lubridate::floor_date(time(xts_reply_df), unit = "month")

## 2.3 Average monthly replies before and during the pandemic  ----
country_reply_df <- xts_reply_df |> as.data.frame() %>% mutate(date = rownames(.)) |>
        pivot_longer(cols = 1:112, names_to = "country", values_to = "freq") |>
        mutate(date = ymd(date)) |>
        mutate(period = case_when(date < "2020-01-01" ~ "before",
                                  date >= "2020-01-01" ~ "during")) |>
        group_by(country,period) |>
        summarise(avg = mean(freq))

## Country order
country_reply_order <- (country_reply_df |>
                                filter(period == "during") |>
                                arrange(desc(avg)))$country

## REvise country_reply_df
country_reply_df <- country_reply_df |>
        mutate(country = factor(country, levels = country_reply_order))

# 4. Tweet and reply usage pattern ----
## Create a dataframe combining average tweets/replies per month by DMO
## Names of patterns were included
country_pattern <- tweet_df[,1:3] |>
        inner_join(country_reply_df,
                   by = c("country","period")) |>
        set_colnames(c("country","period","avg_tweets","avg_replies")) |>
        inner_join(tweet_df[,1:3] |>
                           inner_join(country_reply_df,
                                      by = c("country","period")) |>
                           set_colnames(c("country","period","avg_tweets","avg_replies")) |>
                           pivot_wider(names_from = "period", values_from = c("avg_tweets","avg_replies")) |>
                           mutate(strategy = case_when((avg_tweets_before > avg_tweets_during) & (avg_replies_before > avg_replies_during) ~ "diminish",
                                                       (avg_tweets_before < avg_tweets_during) & (avg_replies_before > avg_replies_during) ~ "broadcast",
                                                       (avg_tweets_before > avg_tweets_during) & (avg_replies_before < avg_replies_during) ~ "converse",
                                                       (avg_tweets_before < avg_tweets_during) & (avg_replies_before < avg_replies_during) ~ "enhance")) |>
                           dplyr::select(c("country","strategy")), by = "country") |>
        mutate(country_full = str_to_title(str_replace_all(country, "_"," "))) |>
        mutate(country_full = str_replace_all(country_full, "And", "and")) |>
        mutate(country_full = factor(country_full, levels = tweet_order))

## Diminish
plot_diminish <- ggplot(country_pattern, aes(x = avg_tweets, y = avg_replies)) +
        geom_point(alpha = 0) +
        geom_path(data = country_pattern |> filter(strategy == "diminish"), aes(group = country_full, color = strategy),
                  size = 1,
                  alpha = 0.6,
                  arrow = arrow(type = "closed",
                                end = "last",
                                length = unit(0.01, "npc"))) +
        geom_path(data = country_pattern |> filter(strategy != "diminish"), aes(group = country_full, color = strategy),
                  size = 1,
                  alpha = 0.1,
                  arrow = arrow(type = "closed",
                                end = "last",
                                length = unit(0.01, "npc"))) +
        ggrepel::geom_text_repel(data = country_pattern |> filter(strategy == "diminish") |> filter(period == "during"), aes(label = country_full), size = 4, alpha = 1, color = "black") +
        ggrepel::geom_text_repel(data = country_pattern |> filter(strategy != "diminish") |> filter(period == "during"), aes(label = country_full), size = 4, alpha = 1, color = NA) +
        theme_light() +
        scale_y_continuous(trans = log_trans(), breaks = pretty_breaks()) +
        scale_x_continuous(trans = log_trans(), breaks = pretty_breaks()) +
        xlab("Average no. of tweets per month (log scale)") +
        ylab("Average no. of replies per month (log scale)")

## Disseminate
plot_broadcast <- ggplot(country_pattern, aes(x = avg_tweets, y = avg_replies)) +
        geom_point(alpha = 0) +
        geom_path(data = country_pattern |> filter(strategy == "broadcast"), aes(group = country_full, color = strategy),
                  size = 1,
                  alpha = 0.6,
                  arrow = arrow(type = "closed",
                                end = "last",
                                length = unit(0.01, "npc"))) +
        geom_path(data = country_pattern |> filter(strategy != "broadcast"), aes(group = country_full, color = strategy),
                  size = 1,
                  alpha = 0.1,
                  arrow = arrow(type = "closed",
                                end = "last",
                                length = unit(0.01, "npc"))) +
        ggrepel::geom_text_repel(data = country_pattern |> filter(strategy == "broadcast") |> filter(period == "during"), aes(label = country_full), size = 4, alpha = 1, color = "black") +
        ggrepel::geom_text_repel(data = country_pattern |> filter(strategy != "broadcast") |> filter(period == "during"), aes(label = country_full), size = 4, alpha = 1, color = NA) +
        theme_light() +
        scale_y_continuous(trans = log_trans(), breaks = pretty_breaks()) +
        scale_x_continuous(trans = log_trans(), breaks = pretty_breaks()) +
        xlab("Average no. of tweets per month (log scale)") +
        ylab("Average no. of replies per month (log scale)")

## Converse
plot_converse <- ggplot(country_pattern, aes(x = avg_tweets, y = avg_replies)) +
        geom_point(alpha = 0) +
        geom_path(data = country_pattern |> filter(strategy == "converse"), aes(group = country_full, color = strategy),
                  size = 1,
                  alpha = 0.6,
                  arrow = arrow(type = "closed",
                                end = "last",
                                length = unit(0.01, "npc"))) +
        geom_path(data = country_pattern |> filter(strategy != "converse"), aes(group = country_full, color = strategy),
                  size = 1,
                  alpha = 0.1,
                  arrow = arrow(type = "closed",
                                end = "last",
                                length = unit(0.01, "npc"))) +
        ggrepel::geom_text_repel(data = country_pattern |> filter(strategy == "converse") |> filter(period == "during"), aes(label = country_full), size = 4, alpha = 1, color = "black") +
        ggrepel::geom_text_repel(data = country_pattern |> filter(strategy != "converse") |> filter(period == "during"), aes(label = country_full), size = 4, alpha = 1, color = NA) +
        theme_light() +
        scale_y_continuous(trans = log_trans(), breaks = pretty_breaks()) +
        scale_x_continuous(trans = log_trans(), breaks = pretty_breaks()) +
        xlab("Average no. of tweets per month (log scale)") +
        ylab("Average no. of replies per month (log scale)")

## Enhance
plot_enhance <- ggplot(country_pattern, aes(x = avg_tweets, y = avg_replies)) +
        geom_point(alpha = 0) +
        geom_path(data = country_pattern |> filter(strategy == "enhance"), aes(group = country_full, color = strategy),
                  size = 1,
                  alpha = 0.6,
                  arrow = arrow(type = "closed",
                                end = "last",
                                length = unit(0.01, "npc"))) +
        geom_path(data = country_pattern |> filter(strategy != "enhance"), aes(group = country_full, color = strategy),
                  size = 1,
                  alpha = 0.1,
                  arrow = arrow(type = "closed",
                                end = "last",
                                length = unit(0.01, "npc"))) +
        ggrepel::geom_text_repel(data = country_pattern |> filter(strategy == "enhance") |> filter(period == "during"), aes(label = country_full), size = 4, alpha = 1, color = "black") +
        ggrepel::geom_text_repel(data = country_pattern |> filter(strategy != "enhance") |> filter(period == "during"), aes(label = country_full), size = 4, alpha = 1, color = NA) +
        theme_light() +
        scale_y_continuous(trans = log_trans(), breaks = pretty_breaks()) +
        scale_x_continuous(trans = log_trans(), breaks = pretty_breaks()) +
        xlab("Average no. of tweets per month (log scale)") +
        ylab("Average no. of replies per month (log scale)")

## Combine plots and visualise
ggarrange(plot_converse,plot_enhance,plot_diminish,plot_broadcast,
          ncol = 2, nrow=2, common.legend = TRUE, legend="bottom")

# 5. Tweets/replies by DMO for appendix ----
create_country_tweets <- function(dmo) {
        country_xts <- xts(dmo_cleaned[dmo_cleaned$country == dmo,c("country")], order.by = dmo_cleaned[dmo_cleaned$country == dmo,]$created_at)
        country_per_month <- apply.monthly(country_xts, FUN = nrow)
        time(country_per_month) <- lubridate::floor_date(time(country_per_month), unit = "month")
        country_raw_df <- country_per_month |> as.data.frame() %>% mutate(date = rownames(.)) |>
                mutate(date = ymd(date))
        return(country_raw_df)
}
create_country_replies <- function(dmo) {
        country_xts <- xts(reply_cleaned[reply_cleaned$country == dmo,c("country")], order.by = reply_cleaned[reply_cleaned$country == dmo,]$created_at)
        country_per_month <- apply.monthly(country_xts, FUN = nrow)
        time(country_per_month) <- lubridate::floor_date(time(country_per_month), unit = "month")
        country_raw_df <- country_per_month |> as.data.frame() %>% mutate(date = rownames(.)) |>
                mutate(date = ymd(date))
        return(country_raw_df)
}

dmo = "norway"
ggplot(mapping = aes(x = date, y = country)) +
        geom_point(data = create_country_tweets(dmo), aes(color = "tweet")) +
        geom_line(data = create_country_tweets(dmo), aes(color = "tweet")) +
        geom_point(data = create_country_replies(dmo), aes(color = "reply")) +
        geom_line(data = create_country_replies(dmo), aes(color = "reply")) +
        theme_light() +
        ylab("Tweets / replies per month") +
        scale_x_date(expand = expansion(mult = c(0,0),
                                        add = c(0,60))) +
        theme(axis.title.x = element_blank(),
              legend.title = element_blank(),
              legend.position = "right",
              strip.background = element_rect(fill = "#777777"),
              strip.text = element_text(colour = 'white')) +
        scale_colour_manual(values = c("tweet" = "seagreen", "reply" = "salmon")) +
        scale_x_date(breaks = scales::pretty_breaks(n = 12),
                     minor_breaks = scales::pretty_breaks(n = 36)) +
        geom_vline(xintercept = as.numeric(as.Date("2020-01-01")), colour = "red", linetype = "dotted") +
        ggtitle(str_to_title(dmo))
