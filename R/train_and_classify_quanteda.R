library(tidyverse)
library(quanteda)


set.seed(324)
iis_rnd <- hand_checked2 %>% 
    sample_n(., size=nrow(.), replace=FALSE) # randomize order

iis_train <- iis_rnd %>% 
    filter(row_number() <= floor(.8*nrow(iis_rnd)))
iis_test <- iis_rnd %>% 
    filter(row_number() > floor(.8*nrow(iis_rnd)))

iis_corpus <- corpus(iis_rnd$resumen %>% tolower()) # create quanteda corpus
docvars(iis_corpus) <- iis_rnd$mass   # attach class labels to the corpus text

# includes stopwords in ngrams . . .
# iis_dfm <- dfm(iis_corpus, 
#                tolower = TRUE, 
#                remove = stopwords("es"),
#                remove_punct = TRUE,
#                ngrams = 1:3) %>% 
#     dfm_trim(min_count = 2, min_docfreq = 2)

# removing stopwords before constructing ngrams
iis_dfm <- tokens(iis_corpus, 
                     what = "word",
                     remove_punct = TRUE,
                     remove_numbers = TRUE) %>% 
    tokens_remove(stopwords("es")) %>% 
    tokens_ngrams(n = 1:3) %>% 
    dfm(verbose = FALSE) %>% 
    dfm_trim(min_count = 2, min_docfreq = 2)

iis_dfm_train <- iis_dfm[1:nrow(iis_train), ]
iis_dfm_test<- iis_dfm[(nrow(iis_train)+1):nrow(iis_rnd), ] 

nb_classifier <- textmodel_nb(x = iis_dfm_train, 
                              y = iis_dfm_train@docvars$docvar1,
                              prior = "docfreq",
                              distribution = "Bernoulli")

nb_coefs <- coef(nb_classifier) %>% 
    as_tibble(rownames = "ngram") %>% 
    arrange(-`1`)


pred <- predict(nb_classifier, iis_dfm_test)

# use pred$nb.predicted to extract the class labels
table(predicted = pred$nb.predicted, actual = iis_test %>% pull("mass"))
mean(pred$nb.predicted == iis_test %>% pull("mass"))*100
