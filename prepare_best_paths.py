import find_best_path
from gensim.models.keyedvectors import KeyedVectors
import argparse
import random


parser = argparse.ArgumentParser('Prepare solution for pairs of words')
parser.add_argument('-m', required=True, help='model file')
parser.add_argument('-w', required=True, help='words to use file')
parser.add_argument('-o', required=True,
                    help='output file (will append new lines to it)')
parser.add_argument('-n', type=int, default=100,
                    help='how many pairs to prepare (default 100)')
args = parser.parse_args()

# read list of 3k common words
accepted_words = []
with open(args.w, 'rt') as inf:
    for line in inf:
        accepted_words.append(line.rstrip().lower())

# load model
model = KeyedVectors.load_word2vec_format(args.m, binary=False, no_header=True)

# difficulty level definition
levels = [.3, .4, .5, .6]

# open output file in "append" mode
outf = open(args.o, 'at')

# compute optimal path for the other pairs
for idx in range(args.n):
    pair_res = []
    words = random.sample(accepted_words, 2)
    while not model.has_index_for(words[0]) or \
          not model.has_index_for(words[1]):
        words = random.sample(accepted_words, 2)
    for lvl in levels:
        print('{} {}-{} {}'.format(idx, words[0], words[1], lvl))
        # greedy first
        print('\tgreedy')
        path_list = find_best_path.find_path(model,
                                             words[0], words[1],
                                             similarity_limit=lvl,
                                             accepted_words=accepted_words,
                                             greedy=True,
                                             quiet=True)
        solution = 'NA'
        if len(path_list) > 0:
            solution = '-'.join(path_list)
        pair_res.append('{}\t{}\t{}\t{}\t{}'.format(words[0],
                                                    words[1],
                                                    len(path_list),
                                                    solution, lvl))
        # skip looking for more optimal solution if greedy failed
        if len(path_list) == 0:
            continue
        # more optimal solution
        print('\tmore optimal')
        path_list = find_best_path.find_path(model,
                                             words[0], words[1],
                                             similarity_limit=lvl,
                                             accepted_words=accepted_words,
                                             max_words=10,
                                             quiet=True)
        solution = 'NA'
        if len(path_list) > 0:
            solution = '-'.join(path_list)
        pair_res.append('{}\t{}\t{}\t{}\t{}'.format(words[0],
                                                    words[1],
                                                    len(path_list),
                                                    solution, lvl))
    # write results
    outf.write('\n'.join(pair_res) + '\n')
    idx += 1
outf.close()
