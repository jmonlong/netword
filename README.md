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

Use the [`find_best_path.py`](find_best_path.py).

```
usage: find_best_path.py [-h] [--similarity_limit SIMILARITY_LIMIT] [--similarity_band SIMILARITY_BAND] [--greedy] start_word end_word

Find the shortest path between two words. Outputs one word per line, including input words, to stdout

positional arguments:
  start_word
  end_word

options:
  -h, --help            show this help message and exit
  --similarity_limit SIMILARITY_LIMIT
  --similarity_band SIMILARITY_BAND
                        Similarity band to filter out some candidates. 1 means no filtering (optimal). 0.05-0.1 could provide some speed up but might miss some solutions.
  --greedy              Use a greedy approach
```

Note: by default, running the script used all available cores on my laptop. 
To change that, I set the `OPENBLAS_NUM_THREADS` variable before running the script: 

```sh
export OPENBLAS_NUM_THREADS=1
```

Another Python script, [`prepare_best_paths.py`](prepare_best_paths.py) loads the functions defined in `find_best_path.py` and pre-compute the optimal paths for a list of word pairs and different difficulty levels.
The random word pairs had first been selected by [`prepare_word_pairs.R`](prepare_word_pairs.R) from the list of 3000 common English words.
