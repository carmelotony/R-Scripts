library(Biostrings)


#read in fasta file
ToxoTransciprts <-  readDNAStringSet(file.choose())
seq_names = names(ToxoTransciprts)
#create df from fasta header
fastahead <- data.frame(seq_names)
head(fastahead)
#split header on | (cancel special character with \\)
fastaheadsplt <- do.call(rbind, strsplit(as.character(fastahead$seq_names), '\\|'))
head(fastaheadsplt)
#remove all but first 2 columns
fastaheadsplt <- subset(fastaheadsplt, select = c(1,2))
head(fastaheadsplt)
# convert to df
fastaheadsplt <- as.data.frame(fastaheadsplt)
#remove " from entries (cancel special character with \\)
fastaheadEdit <- as.data.frame(sapply(fastaheadsplt,gsub,pattern='\\"',replacement=""))
head(fastaheadEdit)
#remove gene= from gene entries
fastaheadEdit <- as.data.frame(sapply(fastaheadsplt,gsub,pattern='gene=',replacement=""))
head(fastaheadEdit)
#write tab delimited file
write.table(fastaheadEdit, quote = FALSE, file = "SALMON_Transcript_MAP", sep = "\t", row.names = FALSE, col.names = FALSE)
