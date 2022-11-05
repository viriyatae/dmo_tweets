library(tidyverse)
library(topicmodels)
library(forcats)
library(ldatuning)
library(SnowballC)
library(textstem)
library(tidytext)
library(tm)

############################################################################
### IMPORTANT - datasets are not available due to the copyright reason   ###
### This script is for your reference only                               ###
############################################################################

# 1. Prepare stop words ----
## 1.1 Create a stop word list ----
## First, create a list of words in the country names
more_stop <- tibble(str_split(string = unique(dmo_cleaned$country), 
                              pattern = "[[:space:]]", 
                              simplify = TRUE) |>
                            as.data.frame() |>
                            pivot_longer(cols = 1:4, names_to = "order", values_to = "word") |>
                            filter(word != "") |>
                            dplyr::select(-order),lexicon = "proper") |>
        mutate(word = str_remove_all(word,"(@[A-Za-z0-9]+)|([^0-9A-Za-z \t])|(\\w+:\\/\\/\\S+)")) |>
        distinct() |>
        filter(word %nin% c("Islands","St","and","new"))

## Next, add a list of stop words from proper names and words specific to the dataset
## Words from this list were created and selected from the words after setting the sparsity in Step 5
more_stop <- more_stop |>
        bind_rows(tribble(
                ~word, ~lexicon,
                "amp", "custom",
                "dubai", "proper")) |>
        bind_rows(tibble(word = c("prague","norwegian","turkey","london","madrid","soviet","greek",
                                  "tokyo","moscow","copenhagen","britain","istanbul","lisbon",
                                  "oslo","german","spanish","berlin","chinese","porto","irish",
                                  "italian","portuguese","barcelona","swaziland","athens","york",
                                  "thai","canadian","victoria","seoul","swiss","tahiti",
                                  "dublin","delhi","bangkok","danish","sydney","amman","usa",
                                  "freetown","valencia","bali","kingston","holland","belfast",
                                  "sicily","pradesh","ocho","tuscany","gujarat"),
                         lexicon = "proper")) |>
        bind_rows(tibble(word = c("de","youll","visitcz","la","gt","san","al","weve","youve","bvi",
                                  "onehappyisland","beautyhasanaddress","dekhoapnadesh","brandusa",
                                  "uk","del","letherinspireyou","alentejo","isnt","el", "wat",
                                  "doesnt","malediven","maldiva","globalgreen","igturkey", "mara",
                                  "youd","feelitforyourself","lovegreatbritain","wont","rt",
                                  "worldsleadingdestin","rajasthan","thisisqueensland","santa",
                                  "giro","algarve","rio","negril","dutch","aworldofwond","en",
                                  "natureslittlesecret","salonetwitt","caribbeandream","montego",
                                  "yearofreturn","montecarlo","pc","myway","omgb","valenciana",
                                  "paryatanparv","stpatricksdai","wouldnt","john","turkish",
                                  "onecaribbean","madeira","le","azores","asturias",
                                  "feeltherhythm","visittheusa","ourheartsarealwaysopen",
                                  "theislandsoftah","ourbvi","regga","cartagena","di","juan","stayinspired",
                                  "theyr","wadi","tur","rediscoverthemag","beherenow",
                                  "usvi","gvb","ntb","james","itstimetobook","ii","reise","freetown",
                                  "mmprc","igworldsleadingdestin","karnataka","couldnt",
                                  "takeanotherlook","peter","ipw","petra","happinessisaplace",
                                  "acuriousplace","da","havent","stanbul","euskadi",
                                  "wadirum","viaje","greeksummerfeel","antonio","lovejo","antalya",
                                  "los","viaggio","alexander","goodmorningworldnz","bviparadise",
                                  "timetobeenlighten","seyahat","ti","visitbrasil","im","bvisecret",
                                  "george","paul","punta","hfrresort","didnt","ussr",
                                  "thisiswa","italianvillage","th","five","onelove",
                                  "via","nd","rd","six","co","freetowonder"),
                         lexicon = "custom"))

## The final stop word list is created from the pre-defined libraries and added stop words
## Words were lemmatised and stemmed
tweet_stop_words <- stop_words |>
        filter(lexicon %in% c("snowball","onix")) |>
        bind_rows(more_stop) |>
        mutate(word = tolower(word)) |>
        mutate(word = gsub("[[:punct:]]", " ", x = word)) |>
        mutate(word = gsub(" ", "", x = word)) |>
        mutate(word_lemmatised = lemmatize_words(word)) |>
        mutate(word_stemmed = SnowballC::wordStem(word_lemmatised))

# 2. Unnest the data and remove stop words ----
dmo_unnested <- dmo_cleaned |> 
        unnest_tokens(word, pre_text) |>
        mutate(word_lemmatised = lemmatize_words(word)) |>
        mutate(word_stemmed = wordStem(word_lemmatised))

dmo_stop_removed <- dmo_unnested |>
        filter(nchar(word_stemmed) > 1) |>
        anti_join(tweet_stop_words, by = "word_stemmed") |>
        filter(!str_detect(word_lemmatised, paste(tweet_stop_words[tweet_stop_words$lexicon == "proper",]$word_lemmatised, collapse = '|')))

# 3. Transform the df into DocumentTermMatrix ----
dtm_dmo <- dmo_stop_removed |>
        dplyr::count(id, word_stemmed) |>
        cast_dtm(document = id, term = word_stemmed, value = n)
inspect(dtm_dmo)

## Remove sparse terms to reduce words and improve performance
dtm_dmo_trimmed <- removeSparseTerms(dtm_dmo,0.999)
inspect(dtm_dmo_trimmed)
dmo_stop_removed_trimmed <- dmo_stop_removed |>
        dplyr::filter(word_stemmed %in% dtm_dmo_trimmed[["dimnames"]][["Terms"]])

## Check frequenyly found words (reiterate stop words as necessary)
freq_words_trimmed <- dmo_stop_removed_trimmed |> group_by(word_stemmed) |> summarise(freq = n()) |> arrange(desc(freq))

## Final dtm ready
dtm_dmo_final <- dmo_stop_removed_trimmed |>
        dplyr::count(id, word_stemmed) |>
        cast_dtm(document = id, term = word_stemmed, value = n)

# 4. LDA Tuning ----
## 4.1 Tuning the number of topics ----
result_tuning <- FindTopicsNumber(
        dtm_dmo_final,
        topics = seq(from = 2, to = 40, by = 1),
        metrics = c("CaoJuan2009",
                    "Deveaud2014",
                    "Arun2010",
                    "Griffiths2004"),
        method = "Gibbs",
        control = list(seed = 20221101), 
        mc.cores = NA,
        verbose = TRUE
)
FindTopicsNumber_plot(result_tuning[,1:3]) # The results show that 15 and 19 are the most suitable

## 4.2 Hyperparameter tuning ----
### Create a grid ----
k_topic <- c(15,19)
alpha <- c(1, 0.1, 0.01)
delta <- c(0.1, 0.01, 0.001)
hypergrid <- expand.grid(k_topic = k_topic, alpha = alpha, delta = delta)

### Create lists ----
lda_output_list <- list()
perplexity <- list()

### Run hyperparameter tuning ----
for (i in 1:nrow(hypergrid)) {
        lda_output_list[[i]] <- LDA(
                dtm_dmo_final,
                k = hypergrid[i,"k_topic"],
                method = "Gibbs",
                control = list(seed = 22551,
                               alpha = hypergrid[i,"alpha"],
                               delta = hypergrid[i,"delta"])
        )
        perplexity[[i]] <- perplexity(lda_output_list[[i]], dtm_dmo_final)
        print(paste0("Finished ",i," out of ",nrow(hypergrid)))
}

## The lowest perplexity is...
hypergrid[which.min(unlist(perplexity)),]

# 5. Test LDA -----
## 5.1 Specify the number of topics ----
k <- 19

## 5.2 LDA algorithm ----
lda_output <- LDA(
        dtm_dmo_final,
        k = k,
        method = "Gibbs", 
        control = list(seed = c(2022,11,1,10,19), 
                       alpha = 0.1, 
                       delta = 0.001,
                       burnin = 4000,
                       iter = 2000,
                       thin = 500,
                       nstart = 5,
                       best = TRUE)
)

## 5.3 How many words per topic? ----
n_words <- 15

## See most representative words of each topic
terms(lda_output, n_words)

## 5.4 Specify topic names ----
lda_topic <- c("protocol","greeting","mountain","hotel",
               "culture","festival","industry","award",
               "view","park","beach","participation",
               "information","architecture","food","town",
               "invitation","celebration","sports")
lda_topic_reordered <- c("protocol","information","industry","award","celebration", # Info
                         "participation","hotel","festival","invitation", # Participation
                         "greeting","view", # Emotion
                         "park","mountain","beach","town", # Image - natural
                         "architecture","culture","food","sports") # Image - cultural

## 5.5 Create a dataframe ----
lda <- as_tibble(as.numeric(lda_output@documents)) |>
        bind_cols(as_tibble(lda_output@gamma)) |>
        bind_cols(topic = topics(lda_output)) |>
        mutate(topic_n = factor(topic, level = 1:k, labels = lda_topic)) |>
        mutate(value = as.numeric(value))

## 5.6 [Beta] Extract  (the distribution of terms (words) by topic) ----
lda_beta <- lda_output |>
        broom::tidy(matrix = "beta")

## 5.7 [Beta] Produce a top 20 words and beta for each topic ----
lda_beta_words <- lda_beta |>
        group_by(topic) |>
        arrange(desc(beta)) |>
        slice_head(n = n_words) |>
        ungroup() |>
        mutate(term2 = fct_reorder(term, beta, mean)) |>
        mutate(topic = factor(topic, level = 1:k, labels = lda_topic))

## 5.8 Change to original words for better understanding ----
mode <- function(x) {
        ux <- unique(x)
        ux[which.max(tabulate(match(x, ux)))]
}

dmo_word_original <- dmo_stop_removed |>
        group_by(word_stemmed) |>
        dplyr::summarise(word_orig = mode(word))

lda_beta_orig <- lda_beta_words |>
        inner_join(dmo_word_original, by = c("term" = "word_stemmed")) |>
        #mutate(topic_name = factor(topic, levels = 1:ncol(lda_terms_11), labels = topic_name_list)) |>
        mutate(topic_name = factor(topic, levels = lda_topic)) |>
        mutate(word_orig = fct_reorder(word_orig, beta, mean)) |>
        mutate(topic_name = factor(topic_name, level = lda_topic_reordered))

# 6. Combine DF after LDA ----
dmo_lda <- dmo_cleaned |>
        mutate(id = as.numeric(id)) |>
        inner_join(lda, by = c("id"="value"))

# 7. NRC sentiment analysis ----
dmo_nrc <- do.call(rbind,apply(dmo_cleaned['cleaned_text'], MARGIN = 1, FUN = get_nrc_sentiment))

## 7.1 Specify the types of sentiment ----
nrc_sentiment <- c("anger","anticipation","disgust","fear","joy","sadness","surprise","trust","negative","positive")

## 7.2 Bind the final df ----
dmo_final <- dmo_cleaned |> mutate(id = as.numeric(id)) |>
        bind_cols(dmo_nrc) |>
        inner_join(lda, by = c("id"="value"))
mutate(url = paste0("http://www.twitter.com/anyuser/status/",id))

colnames(dmo_final) <- c(colnames(dmo_final[,1:23]),lda_topic,"topic","topic_n","url") 
