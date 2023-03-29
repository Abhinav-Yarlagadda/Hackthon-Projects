library(tidyverse)
library(tidytext)
library(readr)

dat <- read_csv("df.csv")

review_words <- dat %>% 
  unnest_tokens(word, reviewText, token = "words") 
review_words

review_words %>% 
  group_by(word) %>% 
  summarize(n = n()) %>% 
  arrange(desc(n))

stoplist <- stop_words %>% 
  filter(lexicon == "onix") %>% 
  distinct()

sentiments

test <- review_words %>% 
  anti_join(stoplist, by = "word") %>% 
  inner_join(sentiments, by = "word") %>% 
  group_by(review_num, sentiment) %>% 
  summarize(n = n(), 
            .groups = "drop") %>% 
  pivot_wider(names_from = "sentiment",
              values_from = "n")

test <- test |>
  mutate_at(c('positive', 'negative'), ~replace_na(.,0)) |>
  mutate(prop_positive = positive / (positive + negative)) |>
  mutate(sentiment = ifelse(prop_positive == 0.5, "neutral", ifelse(prop_positive > 0.5, "positive", "negative")))

dat2 <- dat |>
  full_join(test, by = "review_num")

xtabs(~help_prop, data = dat2)

dat2 <- dat2 |>
  mutate(category = ifelse(help_prop >= 0.7, "genuine", ifelse(help_prop <= 0.3, "fraudulent", "unsure")))

dat3 <- dat2 |>
  filter(llm == "FALSE", category != "unsure")

dat3 <- dat3 |>
  filter(!is.na(category))

dat3 <- dat3 %>% mutate(fraud = ifelse(category=="fraudulent", 1, 0))

dat4 <- dat3 |>
  filter(!is.na(sentiment)) |>
  filter(!is.na(overall))

glm1 <- glm(fraud~unixReviewTime+sentiment+overall, family = binomial(link = 'logit'),dat = dat4)
summary(glm1)

predicted_y <- predict(glm1, type = "response") >= 0.5
actual_y <- dat4$fraud == 1
xtabs(~predicted_y + actual_y) |> prop.table()
