# French version - Nerwordix

## Preparing the database with the word embeddings

1. Downloaded pre-trained model from https://fauconnier.github.io/#data
1. Filtered and converted to text with `prepare_db.fr.py`
1. Prepared database with `prepare_db.fr.R`

```sh
wget https://embeddings.net/embeddings/frWac_no_postag_no_phrase_500_cbow_cut100.bin
python3 prepare_db.fr.py
Rscript prepare_db.fr.R
```

## Finding the best solution and preparing words of the day

First, find a list of frequent words used in French.
I'm using a list from https://eduscol.education.fr/186/liste-de-frequence-lexicale.
They are listed in `mots-frequents.1500.txt`.

Then use the [`prepare_best_paths.py`](prepare_best_paths.py) (as for the English words) to create a list of word pairs (and potentially solutions).

Note: by default, running the script used all available cores on my laptop. 
To change that, I set the `OPENBLAS_NUM_THREADS` variable before running the script: 

```sh
export OPENBLAS_NUM_THREADS=4

python3 prepare_best_paths.py -m frWac_no_postag_no_phrase_500_cbow_cut100.filtered.tsv.gz -w mots-frequents.1500.txt -o word_pairs.tsv
```

