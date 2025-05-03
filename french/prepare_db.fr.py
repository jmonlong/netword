from gensim.models.keyedvectors import KeyedVectors
import random
import gzip

# load model
model = KeyedVectors.load_word2vec_format('frWac_no_postag_no_phrase_500_cbow_cut100.bin',
                                          binary=True)

# the big matrix (119227 x 500)
model.vectors.shape
model.vectors

# check for non-alphabetic characters and capitalized letters
nonalpha_words = []
cap_words = []
for word in model.index_to_key:
    if not word.isalpha():
        nonalpha_words.append(word)
    if word.lower() != word:
        cap_words.append(word)

# the ~8000 words with non-alphabetic characters look mostly like words we don't use/want
len(nonalpha_words)
for xx in range(100):
    print(nonalpha_words[random.randint(0, len(nonalpha_words)-1)])
# maybe keep the words with hiphens? to remove weird ones and encoding problems
for xx in range(100):
    word = nonalpha_words[random.randint(0, len(nonalpha_words)-1)]
    if word.replace('-', '').isalpha():
        print(word)

# the 99 words with capitalized letters look like encoding problems
len(cap_words)

# keep words with no capitalized letters and only letters or hiphens
words_to_keep = []
for word in model.index_to_key:
    if word.lower() == word and word.replace('-', '').isalpha():
        words_to_keep.append(word)
len(words_to_keep)
# 117876

# write a matrix file with the embeddings
out_fn = 'frWac_no_postag_no_phrase_500_cbow_cut100.filtered.tsv.gz'
outf = gzip.open(out_fn, 'wt')
for word in words_to_keep:
    vec = model.vectors[model.key_to_index[word]]
    tow = [word.lower()]
    for val in vec:
        tow.append(str(val))
    outf.write(' '.join(tow) + '\n')
outf.close()
