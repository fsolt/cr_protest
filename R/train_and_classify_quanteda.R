library(tidyverse)
library(glmnet)
library(e1071)
library(maxent)
library(randomForest)
library(caTools)
library(ipred)
library(quanteda)

load("data/training_data.rda")

# create quanteda corpus of IIS data
set.seed(324)
iis_rnd <- iis_cr %>% 
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

dichotomize <- function(x) as.numeric(x > .5)
correct <- function(x) as.numeric(x == k_fold_classified0$mass)

k_fold_classified0 <- map_dfr(1:k, function(i) {
    test_rows <- which(folds == i)
    iis_dfm_test <- iis_dfm[test_rows, ] 
    iis_dfm_train <- iis_dfm[-test_rows, ]
    
    # Naive Bayes, via quanteda
    nb_classifier <- textmodel_nb(x = iis_dfm_train, 
                                  y = iis_dfm_train@docvars$docvar1,
                                  prior = "uniform",
                                  distribution = "Bernoulli")
    
    pred_nb <- predict(nb_classifier, iis_dfm_test) %>% 
        nth(2) %>% # posterior.prob
        as_tibble() %>% 
        transmute(pred_nb = `1`)

    # Lasso, via glmnet
    glmnet_classifier <- glmnet(x = iis_dfm_train, 
                                y = iis_dfm_train@docvars$docvar1,
                                family = "binomial")
    
    pred_glmnet <- predict(glmnet_classifier, 
                           newx = iis_dfm_test, 
                           type = "response",
                           s = .01) %>% 
        as_tibble() %>% 
        transmute(pred_glmnet = `1`)

    # Support vector machine, via e1071
    svm_classifier <- svm(x = iis_dfm_train, 
                          y = iis_dfm_train@docvars$docvar1,
                          type = "C-classification",
                          cross = 0, 
                          cost = 100, 
                          kernel = "radial",
                          probability = TRUE)

    pred_svm <- predict(svm_classifier, 
                        newdata = iis_dfm_test, 
                        probability = TRUE) %>% 
        attr("probabilities") %>% 
        as_tibble() %>% 
        transmute(pred_svm = `1`)
    
    # Maximum entropy, via maxent
    maxent_classifier <- maxent(iis_dfm_train,
                                iis_dfm_train@docvars$docvar1)
    
    pred_maxent <- predict(maxent_classifier, 
                           feature_matrix = iis_dfm_test) %>% 
        as_tibble() %>% 
        transmute(pred_maxent = as.numeric(`1`))
    
    # Random forests, via randomForest
    rf_classifier <- randomForest(x = as.matrix(iis_dfm_train),
                            y = as.factor(iis_dfm_train@docvars$docvar1), 
                            ntree = 100) 
    pred_rf <- predict(rf_classifier,
                       as.matrix(iis_dfm_test),
                       type = "prob") %>% 
        as.data.frame() %>% 
        as_tibble() %>% 
        transmute(pred_rf = `1`)
    
    # Logit boosting, via caTools
    lb_classifier <- LogitBoost(xlearn = as.matrix(iis_dfm_train), 
                            ylearn = iis_dfm_train@docvars$docvar1,
                            nIter = 100)
    
    pred_lb <- predict(lb_classifier,
                       xtest = data.frame(as.matrix(iis_dfm_test)), 
                       type = c("raw")) %>% 
        as_tibble() %>% 
        transmute(pred_lb = `1`)
   
    # Stabilised linear discriminant analysis, via ipred
    slda_classifier <- slda(as.factor.iis_dfm_train.docvars.docvar1. ~ .,
                            data = data.frame(as.matrix(iis_dfm_train), 
                                              as.factor(iis_dfm_train@docvars$docvar1)))
    
    pred_slda <- predict(slda_classifier,
                         newdata = data.frame(as.matrix(iis_dfm_test))) %>% 
        nth(2) %>% 
        as_tibble() %>% 
        transmute(pred_slda = `1`)
    
    results_i <- bind_cols(mass = iis_dfm_test@docvars$docvar1,
                           fold = rep(i, length(iis_dfm_test@docvars$docvar1)),
                           pred_nb,
                           pred_glmnet,
                           pred_svm,
                           pred_maxent,
                           pred_rf,
                           pred_lb,
                           pred_slda)
    
    return(results_i)
}) 

k_fold_classified <- k_fold_classified0 %>% 
    mutate_at(vars(starts_with("pred_")), funs(dichotomize)) %>% 
    mutate(pred_ensemble = as.numeric((pred_nb + 
                                           pred_glmnet +
                                           pred_svm + 
                                           pred_maxent + 
                                           pred_rf +
                                           pred_lb +
                                           pred_slda) >= 4),
           correct_ensemble = as.numeric(mass==pred_ensemble),
           correct_ensemble3 = as.numeric(as.numeric((pred_nb + pred_glmnet + pred_svm) >= 2) == mass)) %>% 
    mutate_at(vars(starts_with("pred_")), funs(correct = correct))

save(k_fold_classified, file = "data/k_fold_classified.rda")

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

test_rows <- seq(nrow(iis_cr) + 1, nrow(all_coded_cr))
all_cr_dfm_test <- all_cr_dfm[test_rows, ] 
all_cr_dfm_train <- all_cr_dfm[-test_rows, ]

nb_classifier <- textmodel_nb(x = all_cr_dfm_train, 
                              y = all_cr_dfm_train@docvars$docvar1,
                              prior = "uniform",
                              distribution = "Bernoulli")

pred_nb <- predict(nb_classifier, all_cr_dfm_test) %>% 
    nth(2) %>% # posterior.prob
    as_tibble() %>% 
    transmute(pred_nb = `1`)

# Lasso, via glmnet
glmnet_classifier <- glmnet(x = all_cr_dfm_train, 
                            y = all_cr_dfm_train@docvars$docvar1,
                            family = "binomial")

pred_glmnet <- predict(glmnet_classifier, 
                       newx = all_cr_dfm_test, 
                       type = "response",
                       s = .01) %>% 
    as_tibble() %>% 
    transmute(pred_glmnet = `1`)

# Support vector machine, via e1071
svm_classifier <- svm(x = all_cr_dfm_train, 
                      y = all_cr_dfm_train@docvars$docvar1,
                      type = "C-classification",
                      cross = 0, 
                      cost = 100, 
                      kernel = "radial",
                      probability = TRUE)

pred_svm <- predict(svm_classifier, 
                    newdata = all_cr_dfm_test, 
                    probability = TRUE) %>% 
    attr("probabilities") %>% 
    as_tibble() %>% 
    transmute(pred_svm = `1`)

# Maximum entropy, via maxent
maxent_classifier <- maxent(all_cr_dfm_train,
                            all_cr_dfm_train@docvars$docvar1)

pred_maxent <- predict(maxent_classifier, 
                       feature_matrix = all_cr_dfm_test) %>% 
    as_tibble() %>% 
    transmute(pred_maxent = as.numeric(`1`))

# Random forests, via randomForest
rf_classifier <- randomForest(x = as.matrix(all_cr_dfm_train),
                              y = as.factor(all_cr_dfm_train@docvars$docvar1), 
                              ntree = 100) 
pred_rf <- predict(rf_classifier,
                   as.matrix(all_cr_dfm_test),
                   type = "prob") %>% 
    as.data.frame() %>% 
    as_tibble() %>% 
    transmute(pred_rf = `1`)

# Logit boosting, via caTools
lb_classifier <- LogitBoost(xlearn = as.matrix(all_cr_dfm_train), 
                            ylearn = all_cr_dfm_train@docvars$docvar1,
                            nIter = 100)

pred_lb <- predict(lb_classifier,
                   xtest = data.frame(as.matrix(all_cr_dfm_test)), 
                   type = c("raw")) %>% 
    as_tibble() %>% 
    transmute(pred_lb = `1`)

# Stabilised linear discriminant analysis, via ipred
slda_classifier <- slda(as.factor.all_cr_dfm_train.docvars.docvar1. ~ .,
                        data = data.frame(as.matrix(all_cr_dfm_train), 
                                          as.factor(all_cr_dfm_train@docvars$docvar1)))

pred_slda <- predict(slda_classifier,
                     newdata = data.frame(as.matrix(all_cr_dfm_test))) %>% 
    nth(2) %>% 
    as_tibble() %>% 
    transmute(pred_slda = `1`) 

correct <- function(x) as.numeric(x == all_cr_dfm_test@docvars$docvar1)

split_test <- bind_cols(mass = all_cr_dfm_test@docvars$docvar1,
                        pred_nb,
                        pred_glmnet,
                        pred_svm,
                        pred_maxent,
                        pred_rf,
                        pred_lb,
                        pred_slda) %>% 
    mutate_at(vars(starts_with("pred_")), funs(dichotomize)) %>% 
    mutate_at(vars(starts_with("pred_")), funs(correct = correct)) %>% 
    mutate(pred_ensemble = as.numeric((pred_nb + 
                                           pred_glmnet +
                                           pred_svm + 
                                           pred_maxent + 
                                           pred_rf +
                                           pred_lb +
                                           pred_slda) >= 4),
           correct_ensemble = as.numeric(mass==pred_ensemble),
           correct_ensemble3 = as.numeric(as.numeric((pred_nb + pred_glmnet + pred_svm) >= 2) == mass))





