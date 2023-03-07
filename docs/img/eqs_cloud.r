# Loading packages
library(tidyverse)
library(readtext)
library(quanteda)
library(quanteda.textstats)
library(ggwordcloud)

# Loading EqualStrength proposal text and tranforming to corpus
txt_proposal <- readtext("proposal.txt")
cp_proposal  <- corpus(txt_proposal$text)

# Tokenise and process tokens
tk_proposal <- 
    tokens(cp_proposal, 
        remove_numbers = TRUE, remove_punct = TRUE,
        remove_symbols = TRUE, remove_separators = TRUE)

tk_proposal <- tokens_remove(tk_proposal, c(stopwords("english"), "will"))
tk_proposal <- tokens_select(tk_proposal, min_nchar=3L)

# Creating data frame with frequencies
dfm_proposal <- dfm(tk_proposal)
df_final <- textstat_frequency(dfm_proposal)

# Filtering out words with low frequency
df_final <- df_final |> filter(frequency > 15)

# Generating the wordcloud

gg_wordcloud  <- 
    ggplot(df_final, aes(label = feature, size = frequency,
        color = factor(sample.int(6, nrow(df_final), replace = TRUE)))) +
        geom_text_wordcloud_area(eccentricity = 0.3, seed = 2) +
        scale_size_area(max_size = 10) +
        scale_color_manual(values = c("#502c3e", "#2C3E50", "#00a0d9",
                                        "#13967D", "#587b1c", "#9c3d05")) +
        theme_minimal()

ggsave("eqs_cloud.svg", gg_wordcloud, dpi = 300)
