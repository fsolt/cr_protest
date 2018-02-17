library(tidyverse)
library(quanteda)

load("data/training_data.rda")

# create quanteda corpus of IIS data
set.seed(324)
iis_rnd <- iis %>% 
    select(resumen, mass) %>% 
    sample_n(., size=nrow(.), replace=FALSE) # randomize order

iis_corpus <- corpus(iis_rnd$resumen %>% tolower()) # create quanteda corpus
docvars(iis_corpus) <- iis_rnd$mass   # attach class labels to the corpus text

# remove stopwords, construct ngrams, and build document-feature matrix
iis_dfm <- tokens(iis_corpus, 
                  what = "word",
                  remove_punct = TRUE,
                  remove_numbers = TRUE) %>% 
    tokens_remove(stopwords("es")) %>% 
    tokens_ngrams(n = 1:3) %>% 
    dfm(verbose = FALSE) %>% 
    dfm_trim(min_docfreq = 10)

# k-fold cross validation
k <- 10
folds <- cut(seq(1, nrow(iis_rnd)), breaks = k, labels = FALSE)

correctly_predicted <- vector("double", k)
for (i in 1:k) {
    test_rows <- which(folds == i)
    iis_dfm_test <- iis_dfm[test_rows, ] 
    iis_dfm_train <- iis_dfm[-test_rows, ]
    
    nb_classifier <- textmodel_nb(x = iis_dfm_train, 
                                  y = iis_dfm_train@docvars$docvar1,
                                  prior = "docfreq",
                                  distribution = "Bernoulli")
    
    # nb_coefs <- coef(nb_classifier) %>% 
    #     as_tibble(rownames = "ngram") %>% 
    #     arrange(-`1`)
    
    pred <- predict(nb_classifier, iis_dfm_test)
    
    # use pred$nb.predicted to extract the class labels
    # table(predicted = pred$nb.predicted, actual = iis_test %>% pull("mass")) # confusion matrix
    correctly_predicted[i] <- mean(pred$nb.predicted == iis_dfm_test@docvars$docvar1)*100
}

mean(correctly_predicted)


# Train on IIS data, test on FS hand-coded sample

all_cr_corpus <- corpus(all_coded_cr$resumen %>% tolower()) # create quanteda corpus
docvars(all_cr_corpus) <- all_coded_cr$mass   # attach class labels to the corpus text

# remove stopwords, construct ngrams, and build document-feature matrix
all_cr_dfm <- tokens(all_cr_corpus, 
                  what = "word",
                  remove_punct = TRUE,
                  remove_numbers = TRUE) %>% 
    tokens_remove(stopwords("es")) %>% 
    tokens_ngrams(n = 1:3) %>% 
    dfm(verbose = FALSE) %>% 
    dfm_trim(min_docfreq = 10)

test_rows <- seq(nrow(iis) + 1, nrow(all_coded_cr))
all_cr_dfm_test <- all_cr_dfm[test_rows, ] 
all_cr_dfm_train <- all_cr_dfm[-test_rows, ]

nb_classifier <- textmodel_nb(x = all_cr_dfm_train, 
                              y = all_cr_dfm_train@docvars$docvar1,
                              prior = "docfreq",
                              distribution = "Bernoulli")
    
nb_coefs <- coef(nb_classifier) %>%
    as_tibble(rownames = "ngram") %>%
    arrange(-`1`)
    
pred <- predict(nb_classifier, all_cr_dfm_test)
    
# use pred$nb.predicted to extract the class labels
table(predicted = pred$nb.predicted, actual = all_cr_dfm_test@docvars$docvar1) # confusion matrix
mean(pred$nb.predicted == all_cr_dfm_test@docvars$docvar1)*100

hand_checked$pred <- pred$nb.predicted



