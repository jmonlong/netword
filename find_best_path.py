import argparse
from gensim.models.keyedvectors import KeyedVectors
import heapq
import math


def is_valid(word, accepted_words):
    return word.isalpha() and (word.lower() == word) and word in accepted_words

# Use A* algorithm to find the shortest path between start_word and end_word
# Words must have a similarity greater than or equal to the similarity_limit
# Returns a list of words between start_word and end_word, not including them
def find_path(model, start_word, end_word, similarity_limit,
              accepted_words, greedy=False, sim_band=1, max_words=0,
              quiet=False):

    ######################### Set up A*

    # Keep track of words that we've already seen, and map to the previous
    seen_words = dict()
    seen_words[start_word] = ""

    # The priority queue of words to visit 
    # This uses heapq.heappush(to_visit, (distance, object)) and heapq.heappop(to_visit)
    to_visit = []
    # Initialize to_visit with the start word
    heapq.heappush(to_visit, (0, start_word))

    ######################## A* loop

    # The path between the words. Idk if return value optimization is a thing in python but it doesn't hurt
    path = []
    # Helper function to fill in the path when we're done
    def get_path(curr_word):
        prev_word = curr_word
        while prev_word != start_word:
            path.insert(0, curr_word)
            prev_word = seen_words[curr_word]
            curr_word = prev_word
        path.insert(0, start_word)
        path.append(end_word)

    while len(to_visit) > 0:
        (curr_dist, curr_word) = heapq.heappop(to_visit)

        # If the next word is the word we're looking for
        # I'm going to try to stop this from happening by catching it earlier but just in case
        if curr_word == end_word:
            get_path(curr_word)
            if not quiet:
                print('unexpectedly found endword')
            return path

        # Try to get all words that are within the similarity limit
        # Since we can only get n at a time, progressively increase n
        n = 10
        # print( "Loading " + str(n) + " words")
        next_words = model.most_similar(curr_word, topn = n)
        # print( "Got words")
        while next_words[-1][1] >= similarity_limit:
            # If the last word is still within the similarity limit, look for more
            # print("Similarity was " + str(next_words[-1][1]) + " look for " + str(n) + " more")
            n *= 2
            next_words = model.most_similar(curr_word, topn = n)

        if not quiet:
            print("{} ({}). Heap size: {}.".format(curr_word,
                                                   round(curr_dist, 2),
                                                   len(to_visit)))

        sim_curr_end = model.similarity(curr_word, end_word)

        # Go through the words connected from curr_word
        n_added = 0
        n_skipped = 0
        to_add = []
        for next_word in next_words:

            # If this word is too far away, then we can stop 
            if next_word[1] < similarity_limit:
                break

            # If this word is the word we're looking for, then return
            elif next_word[0] == end_word:
                # Get and return the path to the start word
                get_path(curr_word)
                if not quiet:
                    print('found endword where expected')
                return path

            # If this isn't a valid word, then skip it
            elif not is_valid(next_word[0], accepted_words):
                continue

            # If we've already seen this word, then skip it
            elif next_word[0] in seen_words:
                continue

            # If the next word seems further from the goal word than the current word
            # Note that this means that we aren't guaranteed to get the optimal answer
            # no need to do that when using the greedy approach
            elif model.similarity(next_word[0], end_word) < sim_curr_end - sim_band and not greedy:
                n_skipped += 1
                continue

            # If we got here, then this is a new word that we can use
            else:
                # we want to add this word
                to_add.append([next_word[0],
                               model.similarity(next_word[0], end_word)])

        if len(to_add) > 0:
            # sort the words to add by decreasing similarity with the end word
            to_add_sorted = sorted(to_add, key=lambda ws: -ws[1])
            # how many words to actually add
            to_add_max = len(to_add)
            if max_words > 0:
                to_add_max = max(2, max_words - math.floor(curr_dist))
            for word_score in to_add_sorted:
                if n_added >= to_add_max:
                    n_skipped += 1
                    continue
                # Add it to our seen words, pointing to the word it came from
                seen_words[word_score[0]] = curr_word

                # Add it to the priority queue, including the predicted distance (really 1 - similarity) to the end word
                if greedy:
                    # trying a greedy approach, priorising by the distance to end word only
                    heapq.heappush(to_visit, (1-word_score[1], word_score[0]))
                else:
                    heapq.heappush(to_visit, (math.floor(curr_dist)+1+(1-word_score[1]), 
                                              word_score[0]))
                # print("\tAdding ", next_word[0], ' after ', curr_word)
                n_added += 1
        if not quiet:
            print("\tAdded {}, skipped {}".format(n_added, n_skipped))


    # If we got here, then we found nothing
    if not quiet:
        print('found nothing')
        print(len(to_visit))
    return path


def main():
    parser = argparse.ArgumentParser(description="Find the shortest path between two words. Outputs one word per line, including input words, to stdout")
    parser.add_argument('start_word')
    parser.add_argument('end_word')
    parser.add_argument('--similarity_limit', default=0.4, type=float)
    parser.add_argument('--similarity_band', default=1, type=float,
                        help='Similarity band to filter out some candidates. '
                        '1 means no filtering (optimal). 0.05-0.1 could '
                        'provide some speed up but might miss some solutions.')
    parser.add_argument('--greedy', action='store_true',
                        help='Use a greedy approach')

    args = parser.parse_args()
    print("Finding shortest path from \"" + args.start_word + "\" to \"" + args.end_word + " with similarity limit " + str(args.similarity_limit))
    if args.greedy:
        print("Using a greedy approach")

    # read list of 3k common words
    accepted_words = set()
    with open('random.words.3000.txt', 'rt') as inf:
        for line in inf:
            accepted_words.add(line.rstrip().lower())

    # load model
    model = KeyedVectors.load_word2vec_format('GoogleNews-vectors-negative300.bin.gz',
                                              binary=True)
    print("Loaded model")

    path_list = find_path(model, args.start_word, args.end_word,
                          similarity_limit=args.similarity_limit,
                          accepted_words=accepted_words,
                          greedy=args.greedy,
                          sim_band=args.similarity_band)

    if len(path_list) == 0:
        return

    print("Found a solution in {} steps:".format(len(path_list) - 2))
    for word in path_list:
        print(word)

    return

if __name__ == "__main__":
    main() 
