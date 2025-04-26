from gensim.models.keyedvectors import KeyedVectors

# load model
model = KeyedVectors.load_word2vec_format('GoogleNews-vectors-negative300.bin',
                                          binary=True)

# the big matrix (3M x 300)
model.vectors

# keep words with only alphabetic characters
simple_words = set()
simple_words_lower = set()
cap_words = []
for word in model.index_to_key:
    if word.isalpha():
        if word.lower() == word:
            simple_words.add(word)
            simple_words_lower.add(word)
        else:
            cap_words.append(word)
len(simple_words)
len(cap_words)

# add alphabetic words with upper case if not present
for word in cap_words:
    if word.lower() not in simple_words_lower:
        simple_words.add(word)
        simple_words_lower.add(word.lower())
len(simple_words)

# write a matrix file with the embeddings
out_fn = 'googlenews.simple_words.tsv'
outf = open(out_fn, 'wt')
for word in simple_words:
    vec = model.vectors[model.key_to_index[word]]
    tow = [word.lower()]
    for val in vec:
        tow.append(str(val))
    outf.write('\t'.join(tow) + '\n')
outf.close()
