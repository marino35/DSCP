# Creating RDS Data Files
write.csv(ngram1,"unigrams.csv",row.names=F)
unigrams <- read.csv("unigrams.csv",stringsAsFactors = F)
saveRDS(unigrams,"unigrams.RData")

write.csv(ngram2,"bigrams.csv",row.names=F)
bigrams <- read.csv("bigrams.csv",stringsAsFactors = F)
saveRDS(bigrams,"bigrams.RData")

write.csv(ngram3,"trigrams.csv",row.names=F)
trigrams <- read.csv("trigrams.csv",stringsAsFactors = F)
saveRDS(trigrams,"trigrams.RData")