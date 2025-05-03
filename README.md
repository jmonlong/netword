# Netword 

## Shiny app

See [`app.R`](app.R).
More explanation soon.

## Preparing the database with the word embeddings

The pre-trained model was downloaded from [https://nlp.stanford.edu/projects/glove/](https://nlp.stanford.edu/projects/glove/), specifically the [Wikipedia 2014 + Gigaword 5 model](https://nlp.stanford.edu/data/glove.6B.zip).

The words are then spell-checked using aspell to remove some unwanted words.

```sh
wget https://nlp.stanford.edu/data/glove.6B.zip
unzip -p glove.6B.zip glove.6B.300d.txt | gzip > glove.6B.300d.txt.gz
zcat glove.6B.300d.txt.gz | cut -d ' ' -f1 | aspell clean -l en > glove.6B.300d.spellchecked.txt
```

Then the [`prepare_db.R`](prepare_db.R) script reads this embedding matrix and converts it into a SQLite database for fast query.

## Finding the best solution

Use the [`find_best_path.py`](find_best_path.py) to find the solution for two words, or the [`prepare_best_paths.py`](prepare_best_paths.py) to processes a list of word pairs.
This second script loads the functions defined in `find_best_path.py` and pre-compute the optimal paths for a list of word pairs and different difficulty levels.
The random word pairs had first been selected by [`prepare_word_pairs.R`](prepare_word_pairs.R) from the list of 3000 common English words.

Note: by default, running the script used all available cores on my laptop. 
To change that, I set the `OPENBLAS_NUM_THREADS` variable before running the script: 

```sh
export OPENBLAS_NUM_THREADS=4
```

