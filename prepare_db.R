library(DBI)
library(RSQLite)
library(rlang)

## read embedding matrix
emb = read.table('googlenews.simple_words.tsv.gz', sep='\t',
                 row.names=1, as.is=TRUE)

## stringify the embeddings in a vector of character
emb.v = apply(emb, 1, paste, collapse='_')
## prepare a master data.frame to save in the SQLite database
emb.df = data.frame(word=rownames(emb), emb=emb.v, stringsAsFactors=FALSE)

## to speed up queries bin words in 100 buckets
n.buckets = 100
w.hashes = sapply(emb.df$word, hash)
w.bucket = strtoi(substr(w.hashes, 1, 5), 16)
w.bucket = w.bucket %% n.buckets
## check that there are about the same number of words in each bucket
summary(as.numeric(table(w.bucket)))

## open database
con <- dbConnect(SQLite(), dbname = "googlenews.simple_words.opt.db")

## add each bucket
for(bucket in 0:n.buckets){
  emb.b.df = emb.df[which(w.bucket == bucket),]
  dbWriteTable(con, paste0('words_', bucket), emb.b.df)
}

## add a table to save highscores/leaderboard
highscores = data.frame(name='Jean', date='init', step=2, difficulty=0.1, path='')
dbWriteTable(con, 'highscores', highscores, overwrite=TRUE)

## disconnect
dbDisconnect(con)
