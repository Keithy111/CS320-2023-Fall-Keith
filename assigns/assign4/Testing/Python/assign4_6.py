# //
# Please implement the following function
# that enumerates all the pairs (i, j) of natural
# numbers satisfying $i <= j$; a pair (i1, j1) must
# be enumerated ahead of another pair (i2, j2) if the
# following condition holds:
#   i1*i1*i1 + j1*j1*j1 < i2*i2*i2 + j2*j2*j2
# //
# let
# theNatPairs_cubesum(): (int * int) stream = fn () =>
# //
# *)
#
# Your implementation of theNatPairs_cubesum should
# return a generator corresponding to the OCaml stream
# If you have already implement a stream in Python, you
# turn it into a generator by calling the following fun:
#
# def generator_of_stream(fxs):
#     while True:
#         cxs = fxs()
#         if cxs.ctag == 0:
#             break
#         else:
#             fxs = cxs.cons2
#             yield cxs.cons1
#     raise StopIteration
#
# def theNatPairs_cubesum(): # please give your implementation
#
################################################

import sys
sys.path.append("./../../../../classlib/Python")

def generator_of_stream(fxs):
    while True:
        cxs = fxs()
        if cxs.ctag == 0:
            break
        else:
            fxs = cxs.cons2
            yield cxs.cons1
    raise StopIteration

# Define a class for a stream cell
class StreamCell:
    def __init__(self, value, next_stream):
        self.value = value
        self.next_stream = next_stream

# Define a function to create an empty stream
def empty_stream():
    raise StopIteration

# Define a function to merge two streams based on a given condition
def merge_streams(cmp, s1, s2):
    try:
        x1, s1 = next(s1)
        x2, s2 = next(s2)
        while True:
            if cmp(x1, x2):
                yield x1
                x1, s1 = next(s1)
            else:
                yield x2
                x2, s2 = next(s2)
    except StopIteration:
        pass

# Define a helper function to generate natural number pairs with a condition
def pairs_with_condition(i, j, cmp):
    while i <= j:
        sum1 = i ** 3 + j ** 3
        if sum1 > 0:
            yield (i, j)
        i, j = i, j - 1

# Define theNatPairs_cubesum function
def theNatPairs_cubesum():
    cmp = lambda pair1, pair2: pair1[0] ** 3 + pair1[1] ** 3 < pair2[0] ** 3 + pair2[1] ** 3
    all_pairs = merge_streams(cmp, pairs_with_condition(1, 1, True), pairs_with_condition(1, 1, True))
    for pair in all_pairs:
        yield pair
