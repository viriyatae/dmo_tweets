library(tidyverse)
library(arrow)
library(ggrepel)
library(factoextra)

dmo_final <- read_feather("dmo_final.feather")

nrc_sentiment <- c("anger", "anticipation", "disgust", "fear", "joy", "sadness", "surprise", "trust", "negative", "positive")
lda_topic <- c("protocol", "information", "industry", "award", "celebration", "participation", "hotel", "festival", "invitation", "greeting", "view", "park", "mountain", "beach", "town", "architecture", "culture", "food", "sports")

# 1. PCA Split to before (2018-2019) and during the pandemic ----
dmo_before <- dmo_final |> 
        filter(created_at < "2020-01-01" & created_at >= "2018-01-01") |>
        mutate(country = paste0(country,"_b")) |>
        group_by(country) |>
        summarise(across(c(all_of(nrc_sentiment), all_of(lda_topic)), mean)) |>
        column_to_rownames(var = "country")

dmo_after <- dmo_final |> 
        filter(created_at >= "2020-01-01") |>
        mutate(country = paste0(country,"_a")) |>
        group_by(country) |>
        summarise(across(c(all_of(nrc_sentiment), all_of(lda_topic)), mean)) |>
        column_to_rownames(var = "country")

dmo_before_after <- dmo_before |>
        bind_rows(dmo_after)

dmo_before_after_pca <- prcomp(~.,data = dmo_before_after,
                               center = TRUE,  # Centers means to 0 (optional)
                               scale = TRUE)   # Sets unit variance (helpful)
summary(dmo_before_after_pca)
fviz_eig(dmo_before_after_pca)
sqrt(dmo_before_after_pca$sdev)

# 2 Visualising DMO content strategies ----
## Scaling PC1 and PC2 for visualisation 
dmo_pc12_scaled <- dmo_before_after_pca[["x"]] |>
        as.data.frame() |>
        mutate(across(everything(), ~scale(.)/6))
colnames(dmo_pc12_scaled) <- colnames(dmo_before_after_pca[["x"]] |> as.data.frame())

## Binding PCs to the original variables
dmo_before_after_full <- dmo_before_after |>
        bind_cols(dmo_pc12_scaled) |>
        rownames_to_column(var = "country_bd") |>
        mutate(period = case_when(str_detect(country_bd, "\\w*_b$") ~ "before",
                                  str_detect(country_bd, "\\w*_a$") ~ "during")) |>
        mutate(country = str_sub(country_bd, end=-3)) |>
        mutate(country_full = str_to_title(str_replace_all(country, "_"," "))) |>
        mutate(country_full = str_replace_all(country_full, "And", "and"))

## Extract loadings of PC1 and PC2
rotation_PC1_2 <- dmo_before_after_pca[["rotation"]][,1:2] |>
        as.data.frame() |>
        rownames_to_column(var = "factor") |>
        mutate(type = case_when(factor %in% lda_topic ~ "topic",
                                factor %in% nrc_sentiment ~ "nrc"))

## 2.1 Visualise top 30 ----
top30 <- as.character((table(dmo_final$country) |> sort(decreasing = TRUE) |> as.data.frame() |> slice_head(n = 30))$Var1)

ggplot(data = dmo_before_after_full |> filter(country %in% top30), mapping = aes(x = PC1, y = PC2)) +
        geom_segment(data = rotation_PC1_2, size = 0.6, aes(x = 0, y = 0, xend = PC1, yend = PC2, color = type), arrow = arrow(length = unit(0.005, "npc"), type = "closed"), alpha = 0.3, linetype = 3) +
        ggrepel::geom_text_repel(data = rotation_PC1_2, aes(label = factor, color = type), size = 4, alpha = 0.6) +
        geom_point(alpha = 0) +
        geom_path(aes(group = country),
                  color = "black",
                  arrow = arrow(type = "closed",
                                end = "last",
                                length = unit(0.01, "npc"))) +
        ggrepel::geom_text_repel(data = dmo_before_after_full |> filter(country %in% top30) |> filter(period == "during"), aes(label = country_full), size = 4, alpha = 1, color = "black") +
        theme_light() +
        geom_hline(yintercept = 0, colour = "red", linetype = "dotted") +
        geom_vline(xintercept = 0, colour = "red", linetype = "dotted") +
        ggtitle("Principal Component Analysis: Changes of DMO Communications on Twitter before/during the pandemic") +
        theme(legend.position = "none", legend.title = element_blank()) + 
        guides(colour = guide_legend(nrow = 1)) +
        scale_color_manual(values = c("nrc"="violet","topic"="seagreen")) +
        xlab("PC1 (21.82%)") +
        ylab("PC2 (11.09%)")

## 2.2 Visualise next 30 ----
top60 <- as.character((table(dmo_final$country) |> sort(decreasing = TRUE) |> as.data.frame())[31:60,]$Var1)

ggplot(data = dmo_before_after_full |> filter(country %in% top60), mapping = aes(x = PC1, y = PC2)) +
        geom_segment(data = rotation_PC1_2, size = 0.6, aes(x = 0, y = 0, xend = PC1, yend = PC2, color = type), arrow = arrow(length = unit(0.005, "npc"), type = "closed"), alpha = 0.3, linetype = 3) +
        ggrepel::geom_text_repel(data = rotation_PC1_2, aes(label = factor, color = type), size = 4, alpha = 0.6) +
        geom_point(alpha = 0) +
        geom_path(aes(group = country),
                  color = "black",
                  arrow = arrow(type = "closed",
                                end = "last",
                                length = unit(0.01, "npc"))) +
        ggrepel::geom_text_repel(data = dmo_before_after_full |> filter(country %in% top60) |> filter(period == "during"), aes(label = country_full), size = 4, alpha = 1, color = "black") +
        theme_light() +
        geom_hline(yintercept = 0, colour = "red", linetype = "dotted") +
        geom_vline(xintercept = 0, colour = "red", linetype = "dotted") +
        ggtitle("Principal Component Analysis: Changes of DMO Communications on Twitter before/during the pandemic") +
        theme(legend.position = "none", legend.title = element_blank()) + 
        guides(colour = guide_legend(nrow = 1))+
        scale_color_manual(values = c("nrc"="violet","topic"="seagreen")) +
        xlab("PC1 (21.82%)") +
        ylab("PC2 (11.09%)")

# 3. Country - topic/emotion changes ----
## Create functions to extract data
create_country_topic <- function(dmo) {
        country_topic <- dmo_final[dmo_final$country == dmo,] |>
                filter(created_at >= "2018-01-01") |>
                mutate(period = case_when(created_at < "2020-01-01" ~ "before (2018-2019)",
                                          TRUE ~ "during")) |>
                group_by(period) |>
                dplyr::summarise(across(all_of(lda_topic), mean)) |>
                pivot_longer(cols = 2:20, names_to = "topic", values_to = "gamma")
        topic_order <- (country_topic |> filter(period == "during") |>
                                arrange(desc(gamma)))$topic
        country_topic_reordered <- country_topic |>
                mutate(topic = factor(topic, level = topic_order))
        return(country_topic_reordered)
}
create_country_emotion <- function(dmo) {
        country_emotion <- dmo_final[dmo_final$country == dmo,] |>
                filter(created_at >= "2018-01-01") |>
                mutate(period = case_when(created_at < "2020-01-01" ~ "before (2018-2019)",
                                          TRUE ~ "during")) |>
                group_by(period) |>
                dplyr::summarise(across(all_of(nrc_sentiment), mean)) |>
                pivot_longer(cols = 2:11, names_to = "emotion", values_to = "avg")
        emotion_order <- (country_emotion |> filter(period == "during") |>
                                  arrange(desc(avg)))$emotion
        country_emotion_reordered <- country_emotion |>
                mutate(emotion = factor(emotion, level = emotion_order))
        return(country_emotion_reordered)
}

## Specify DMO
dmo = "norway"

## Plot topic
plot_topic <- ggplot(create_country_topic(dmo), aes(x = topic, y = gamma, color = period)) +
        geom_point() +
        geom_line(aes(group = topic),
                  color = "black",
                  size = 0.25,
                  arrow = arrow(ends = "last", type = "open", length = unit(0.03,"npc"))) +
        xlab("") +
        ylab("Average probability (gamma)") +
        scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
        coord_flip() +
        theme_light() +
        scale_color_viridis_d(option = "magma", direction = 1, end = 0.8) +
        theme(legend.position = "right", legend.title = element_blank())

## Plot emotion
plot_emotion <- ggplot(create_country_emotion(dmo), aes(x = emotion, y = avg, color = period)) +
        geom_point() +
        geom_line(aes(group = emotion),
                  color = "black",
                  size = 0.25,
                  arrow = arrow(ends = "last", type = "open", length = unit(0.03,"npc"))) +
        xlab("") +
        ylab("Average emotion counts per tweet") +
        scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
        coord_flip() +
        theme_light() +
        scale_color_viridis_d(option = "magma", direction = 1, end = 0.8) +
        theme(legend.position = "right", legend.title = element_blank())

## Combine plots
annotate_figure(ggarrange(plot_topic,plot_emotion,ncol = 2, nrow=1, common.legend = TRUE, legend="bottom"),
                top = text_grob(str_to_title(str_replace_all(dmo,"_"," ")), just = c(0,1), face = "plain", size = 14))


