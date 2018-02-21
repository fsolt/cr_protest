library(tidyverse)
library(RTextTools) # Per https://github.com/timjurka/RTextTools/issues/4, run `trace("create_matrix", edit=T)`, go to line 42 and change `Acronym`` to `acronym`
library(beepr)


# Test--------
set.seed(324)
prot_rnd <- sample_n(hand_checked2, size=nrow(hand_checked2), replace=FALSE)


ptm <- proc.time()
ptm

resort_dtm <- function(working.dtm) { # via https://groups.google.com/forum/#!topic/rtexttools-help/VILrGoRpRrU
    # sorts a sparse matrix in triplet format (i,j,v) first by i, then by j.
    # Args:
    #   working.dtm: a sparse matrix in i,j,v format using $i $j and $v respectively. Any other variables that may exist in the sparse matrix are not operated on, and will be returned as-is.
    # Returns:
    #   A sparse matrix sorted by i, then by j.
    working.df <- data.frame(i = working.dtm$i, j = working.dtm$j, v = working.dtm$v)  # create a data frame comprised of i,j,v values from the sparse matrix passed in.
    working.df <- working.df[order(working.df$i, working.df$j), ] # sort the data frame first by i, then by j.
    working.dtm$i <- working.df$i  # reassign the sparse matrix' i values with the i values from the sorted data frame.
    working.dtm$j <- working.df$j  # ditto for j values.
    working.dtm$v <- working.df$v  # ditto for v values.
    return(working.dtm) # pass back the (now sorted) data frame.
}  # end function


f <- .9
#frac <- seq(.1, 1, by = .1)
#analytics_by_size <- map(frac, function(f) {
    matrix <- create_matrix(prot_rnd$resumen,
                              language="spanish",
                              removeNumbers=TRUE, 
                              stemWords=FALSE, 
                              weighting=tm::weightTfIdf) %>% 
        resort_dtm()
    container <- create_container(matrix, 
                                  prot_rnd$mass, 
                                  trainSize=1:round(f*dim(prot_rnd)[1]),
                                  testSize=(round(f*dim(prot_rnd)[1])+1):dim(prot_rnd)[1],
                                  virgin=FALSE)
    #container <- create_container(matrix, data$protest, trainSize=1:dim(data)[1], virgin=FALSE) #train using all data (no reserved test set)
    models <- train_models(container, algorithms=c("SVM", "GLMNET", "MAXENT", "SLDA", "BOOSTING", "BAGGING", "RF", "TREE")) #Also NNET
    #models <- train_models(container, algorithms=c("SVM", "GLMNET", "MAXENT", "SLDA"))
    results <- classify_models(container, models)
    analytics <- create_analytics(container, results)
    summary(analytics)
#})
    

tested <- prot_rnd[(round(f*dim(prot_rnd)[1])+1):dim(prot_rnd)[1], ] %>% 
    cbind(analytics@document_summary)

fp <- tested %>% filter(CONSENSUS_CODE==1 & mass==0)
fn <- tested %>% filter(CONSENSUS_CODE==0 & mass==1)
    
proc.time()
(proc.time() - ptm)/60
beep()

# write.csv(analytics@label_summary, "label_summary.csv")
# write.csv(analytics@algorithm_summary, "algorithm_summary.csv")
# write.csv(analytics@ensemble_summary, "ensemble_summary.csv")

# save(matrix, file="originalMatrix.Rd")
# save(models1, file="trainedModels.Rd")

# load("originalMatrix.Rd")
# load("trainedModels.Rd")

# Train--------

matrix <- create_matrix(hand_checked2$resumen,
                        language="spanish",
                        removeNumbers=TRUE, 
                        stemWords=FALSE, 
                        weighting=tm::weightTfIdf)
#train using all data (no reserved test set)
container <- create_container(matrix,
                              hand_checked2$mass,
                              trainSize=1:dim(prot1)[1],
                              virgin=FALSE) 
models <- train_models(container, algorithms=c("SVM", "GLMNET", "MAXENT"))


# Classify---------

ptm <- proc.time()


new_matrix <- create_matrix(cleaned_texts$resumen,
                            language="spanish",
                            removeNumbers=TRUE,
                            stemWords=FALSE,
                            weighting=tm::weightTfIdf, 
                            originalMatrix=matrix)

new_container <- create_container(new_matrix,
                                  cleaned_texts$mass, 
                                  testSize=seq(dim(cleaned_texts)[1]),
                                  virgin=TRUE)

new_results <- classify_models(new_container, models)

label <- data.frame(sapply(dplyr::select(new_results, contains("LABEL")), function(j) as.numeric(levels(j))[j]))
prob <- dplyr::select(new_results, contains("PROB"))
new_results <- cbind(label, prob)
new_results$sum <- rowSums(label)
cleaned_texts["consensus"] <- as.numeric(new_results$sum>=2)


    d$consensus_agree[new_results$sum>4] <- new_results$sum[new_results$sum>4]
    d$consensus_agree[new_results$sum<=4] <- 8-new_results$sum[new_results$sum<=4]
    d
}
(proc.time() - ptm)/60

for (i in 1:length(uncoded)) {
    write.csv(c.d[[i]], paste0("../Classified/", uncoded[i], ".csv"))
    if (i==1) class.data <- c.d[[i]] else class.data <- rbind(class.data, c.d[[i]])
}
class.data <- class.data[, c(1, 7:8, 2:6)]

class.data1 <- class.data
class.data1 %<>% group_by(file) %>% mutate(line = row_number(file))
class.data1$protest <- class.data1$consensus

for (i in 1:length(coded)) {
    dat <- read.csv(paste0("Training/",coded[i],".csv"), stringsAsFactors=F)
    dat <- dat[, c("text", "protest")]
    dat$file <- coded[i]
    if (i==1) data1 <- dat else data1 <- rbind(data1, dat)
} 
data1 %<>% group_by(file) %>% mutate(line = row_number(file))
data1 <- left_join(data1, meta)

class.data2 <- bind_rows(class.data1, data1)
class.data2 <- arrange(class.data2, file, line)

cy.protests <- group_by(class.data2, country, year) %>% summarise(protests = sum(protest), lines = max(line))

write.csv(cy.protests, "cy_protests.csv")
