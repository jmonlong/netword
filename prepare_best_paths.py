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
# model = KeyedVectors.load_word2vec_format('GoogleNews-vectors-negative300.bin.gz',
#                                           binary=True)
model = KeyedVectors.load_word2vec_format('glove.6B.300d.txt.gz', binary=False, no_header=True)

# difficulty level definition
levels = [.3, .4, .5, .6]

outf = open('word_pairs_computer.new.tsv', 'wt')

# read already computed pairs
inf = open('word_pairs_computer.tsv', 'rt')
idx = 0
pairs_done = set()
for line in inf:
    outf.write(line)
    line = line.rstrip().split('\t')
    if line[0] + '_' + line[1] not in pairs_done:
        pairs_done.add(line[0] + '_' + line[1])
        idx += 1
inf.close()

# compute optimal path for the other pairs
while idx < len(pairs):
    pair_res = []
    for lvl in levels:
        print('{} {}-{} {}'.format(idx, pairs[idx][0], pairs[idx][1], lvl))
        # greedy first
        print('\tgreedy')
        path_list = find_best_path.find_path(model,
                                             pairs[idx][0], pairs[idx][1],
                                             similarity_limit=lvl,
                                             accepted_words=accepted_words,
                                             greedy=True,
                                             quiet=True)
        solution = 'NA'
        if len(path_list) > 0:
            solution = '-'.join(path_list)
        pair_res.append('{}\t{}\t{}\t{}\t{}'.format(pairs[idx][0],
                                                    pairs[idx][1],
                                                    len(path_list),
                                                    solution, lvl))
        # skip looking for more optimal solution if greedy failed
        if len(path_list) == 0:
            continue
        # more optimal solution
        print('\tmore optimal')
        path_list = find_best_path.find_path(model,
                                             pairs[idx][0], pairs[idx][1],
                                             similarity_limit=lvl,
                                             accepted_words=accepted_words,
                                             max_words=10,
                                             quiet=True)
        solution = 'NA'
        if len(path_list) > 0:
            solution = '-'.join(path_list)
        pair_res.append('{}\t{}\t{}\t{}\t{}'.format(pairs[idx][0],
                                                    pairs[idx][1],
                                                    len(path_list),
                                                    solution, lvl))
    # write results
    outf.write('\n'.join(pair_res) + '\n')
    idx += 1
outf.close()
