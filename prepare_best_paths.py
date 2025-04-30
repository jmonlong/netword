import find_best_path
from gensim.models.keyedvectors import KeyedVectors


# read word pairs
pairs = []
inf = open('word_pairs.tsv', 'rt')
for line in inf:
    line = line.rstrip().split('\t')
    pairs.append(line)
inf.close()

# read list of 3k common words
accepted_words = set()
with open('random.words.3000.txt', 'rt') as inf:
    for line in inf:
        accepted_words.add(line.rstrip().lower())

# load model
model = KeyedVectors.load_word2vec_format('GoogleNews-vectors-negative300.bin.gz',
                                          binary=True)

# compute optimal path for the first pairs
outf = open('word_pairs_computer.tsv', 'wt')
for idx in range(30):
    # level: easy
    print('{}-{} easy'.format(pairs[idx][0], pairs[idx][1]))
    path_list = find_best_path.find_path(model, pairs[idx][0], pairs[idx][1],
                                         similarity_limit=.3,
                                         accepted_words=accepted_words,
                                         max_words=6,
                                         quiet=True)
    solution = 'NA'
    if len(path_list) > 0:
        solution = '-'.join(path_list)
    outf.write('{}\t{}\t{}\t{}\t{}\n'.format(pairs[idx][0], pairs[idx][1],
                                             len(path_list), solution, 'Easy'))
    # level: medium
    print('{}-{} medium'.format(pairs[idx][0], pairs[idx][1]))
    path_list = find_best_path.find_path(model, pairs[idx][0], pairs[idx][1],
                                         similarity_limit=.4,
                                         accepted_words=accepted_words,
                                         max_words=6,
                                         quiet=True)
    solution = 'NA'
    if len(path_list) > 0:
        solution = '-'.join(path_list)
    outf.write('{}\t{}\t{}\t{}\t{}\n'.format(pairs[idx][0], pairs[idx][1],
                                             len(path_list), solution, 'Medium'))
    # level: difficult
    print('{}-{} hard'.format(pairs[idx][0], pairs[idx][1]))
    path_list = find_best_path.find_path(model, pairs[idx][0], pairs[idx][1],
                                         similarity_limit=.5,
                                         accepted_words=accepted_words,
                                         max_words=6,
                                         quiet=True)
    solution = 'NA'
    if len(path_list) > 0:
        solution = '-'.join(path_list)
    outf.write('{}\t{}\t{}\t{}\t{}\n'.format(pairs[idx][0], pairs[idx][1],
                                             len(path_list), solution, 'Hard'))
    # level: easy
    print('{}-{} easy'.format(pairs[idx][0], pairs[idx][1]))
    path_list = find_best_path.find_path(model, pairs[idx][0], pairs[idx][1],
                                         similarity_limit=.3,
                                         accepted_words=accepted_words,
                                         greedy=True,
                                         quiet=True)
    solution = 'NA'
    if len(path_list) > 0:
        solution = '-'.join(path_list)
    outf.write('{}\t{}\t{}\t{}\t{}\n'.format(pairs[idx][0], pairs[idx][1],
                                             len(path_list), solution, 'Easy'))
    # level: medium
    print('{}-{} medium'.format(pairs[idx][0], pairs[idx][1]))
    path_list = find_best_path.find_path(model, pairs[idx][0], pairs[idx][1],
                                         similarity_limit=.4,
                                         accepted_words=accepted_words,
                                         greedy=True,
                                         quiet=True)
    solution = 'NA'
    if len(path_list) > 0:
        solution = '-'.join(path_list)
    outf.write('{}\t{}\t{}\t{}\t{}\n'.format(pairs[idx][0], pairs[idx][1],
                                             len(path_list), solution, 'Medium'))
    # level: difficult
    print('{}-{} hard'.format(pairs[idx][0], pairs[idx][1]))
    path_list = find_best_path.find_path(model, pairs[idx][0], pairs[idx][1],
                                         similarity_limit=.5,
                                         accepted_words=accepted_words,
                                         greedy=True,
                                         quiet=True)
    solution = 'NA'
    if len(path_list) > 0:
        solution = '-'.join(path_list)
    outf.write('{}\t{}\t{}\t{}\t{}\n'.format(pairs[idx][0], pairs[idx][1],
                                             len(path_list), solution, 'Hard'))
outf.close()
