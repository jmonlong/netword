day.words = scan('random.words.3000.txt', '', quiet=TRUE)
any(grepl(' ', day.words))
day.words = day.words[which(nchar(day.words)>3)]
set.seed = (123456)
day.words = matrix(sample(day.words), ncol=2, byrow=2)

write.table(day.words, file='word_pairs.tsv', sep='\t',
            row.names=FALSE, col.names=FALSE, quote=FALSE)
