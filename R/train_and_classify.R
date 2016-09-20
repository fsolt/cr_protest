library(RTextTools)
library(readr)
library(stringr)
library(dplyr)
library(beepr)

prot <- read_csv("data-raw/protestas.csv")
names(prot) <- names(prot) %>%
    tolower() %>% 
    make.names() %>% 
    str_replace_all("\\.", "_")

mass_protests <- c("Actos sobre la propiedad",
                   "Bloqueo",
                   "Huelga",
                   "Marcha",
                   "Mitin o concentración",
                   "Paro",
                   "Toma de propiedad")

prot <- prot %>%
    mutate(mass = if_else(tipo_de_protesta %in% mass_protests,
                          1, 0))


set.seed(324)
prot1 <- sample_n(prot, size=nrow(prot), replace=FALSE)


ptm <- proc.time()
ptm
matrix <- create_matrix(prot1$resumen, language="spanish",
                        removeNumbers=TRUE, stemWords=FALSE, weighting=tm::weightTfIdf)
container <- create_container(matrix, prot1$mass, trainSize=1:round(.75*dim(prot1)[1]), testSize=(round(.75*dim(prot1)[1])+1):dim(prot1)[1], virgin=FALSE)
#container <- create_container(matrix, data$protest, trainSize=1:dim(data)[1], virgin=FALSE) #train using all data (no reserved test set)
models <- train_models(container, algorithms=c("SVM","GLMNET","MAXENT", "SLDA","BOOSTING","BAGGING","RF","TREE", "NNET")) #Also NNET
#models <- train_models(container, algorithms=c("SVM","GLMNET","MAXENT"))
results <- classify_models(container, models)
analytics <- create_analytics(container, results)

summary(analytics)
proc.time()
(proc.time() - ptm)/60
beep()

# write.csv(analytics@label_summary, "label_summary.csv")
# write.csv(analytics@algorithm_summary, "algorithm_summary.csv")
# write.csv(analytics@ensemble_summary, "ensemble_summary.csv")



# save(matrix,file="originalMatrix.Rd")
# save(models1,file="trainedModels.Rd")

# load("originalMatrix.Rd")
# load("trainedModels.Rd")

all.texts <- meta$file[meta$country!="Brasil"]
uncoded <- all.texts[!all.texts %in% coded]

ptm <- proc.time()
dir.create("../Classified", showWarnings = FALSE) # Make Classified directory, if it doesn't already exist

library(doParallel)
registerDoParallel(cores=8)

c.d <- foreach(i = 1:length(uncoded), .packages='RTextTools') %dopar% {
    d <- paste0("../Clean_Texts/", uncoded[i], ".txt") %>% 
        readLines %>% 
        data.frame(file = uncoded[i], text = ., stringsAsFactors=F)
    
    d <- left_join(d, meta)
    d["protest"] <- NA
    
    new_matrix <- create_matrix(cbind(d["text"], d["country"]), language="spanish",
                                removeNumbers=TRUE, stemWords=FALSE, weighting=tm::weightTfIdf, originalMatrix=matrix)
    
    new_container <- create_container(new_matrix, d$protest, testSize=seq(dim(d)[1]), virgin=T)
    
    new_results <- classify_models(new_container, models)
    
    label <- data.frame(sapply(dplyr::select(new_results, contains("LABEL")), function(j) as.numeric(levels(j))[j]))
    prob <- dplyr::select(new_results, contains("PROB"))
    new_results <- cbind(label, prob)
    new_results$sum <- rowSums(label)
    d["consensus"] <- as.numeric(new_results$sum>4)
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
