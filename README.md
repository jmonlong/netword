# Netword 

## Shiny app

See [`app.R`](app.R).
More explanation soon.

## Preparing the database with the word embeddings

The pre-trained model was downloaded from https://code.google.com/archive/p/word2vec/, specifically the [ GoogleNews-vectors-negative300.bin.gz](https://drive.google.com/file/d/0B7XkCwpI5KDYNlNUTTlSS21pQmM/edit?usp=sharing) file.

This file is then read and filtered with [`prepare_db.py`](prepare_db.py). 
It uses the [gensim](https://pypi.org/project/gensim/) library to load the model.
An new text-based output is created.

Then the [`prepare_db.R`](prepare_db.R) script reads this embedding matrix and converts it into a SQLite database for fast query.

### Next

1. Look for a better/recent pre-trained model, maybe from Gensim.
2. Filter out mispelled words, maybe running a spell-checker.
3. Look for a French pre-trained model.

## Finding the best solution

*Xian's part*

- [Tutorial on how to load a pre-trained model and find nearby words](https://radimrehurek.com/gensim/auto_examples/howtos/run_downloader_api.html#sphx-glr-auto-examples-howtos-run-downloader-api-py)
- The similarity currently used by the game in the R app is the *'dot': as the square root of the average inner product of the vector elements (sqrt(sum(x . y) / ncol(x))) capped to zero*.

```py
from gensim.models.keyedvectors import KeyedVectors

# load model
model = KeyedVectors.load_word2vec_format('GoogleNews-vectors-negative300.bin',
                                          binary=True)

model.wv.most_similar('tree')
```
