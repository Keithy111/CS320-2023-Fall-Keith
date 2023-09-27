
# Please translate the following code
# into Python and then implement all the
# functions called in the code to make the
# translation (written in Python) works correctly
#
# let
# string_merge
# (cs1: string)(cs2: string): string =
# let n1 = string_length(cs1)
# and n2 = string_length(cs2) in
# let rec
# foreach(i1: int)(i2:int)(work) =
# if
# i1 < n1
# then (
# if
# i2 < n2
# then
#   let c1 = string_get_at(cs1)(i1)
#   and c2 = string_get_at(cs2)(i2) in
#   if c1 <= c2
#   then (work(c1); foreach(i1+1)(i2+0)(work))
#   else (work(c2); foreach(i1+0)(i2+1)(work))
# else
#   int1_foreach(n1-i1)
#     (fun i -> work(string_get_at(cs1)(i1+i)))
# ) else (
#   int1_foreach(n2-i2)
#     (fun i -> work(string_get_at(cs2)(i2+i)))
# )
# in
#   string_make_fwork(foreach(0)(0))
# ;;
#
import sys
sys.path.append("./../../../../classlib/Python")

def int1_foreach(n0, work_func):
    i0 = 0
    while(i0 < n0):
        work_func(i0)
        i0 = (i0 + 1)
    return None # work_func(i0) is done for all 0 <= i0 < n0

def string_merge(cs1, cs2):
    def foreach(i1, i2, work):
        if i1 < len(cs1):
            if i2 < len(cs2):
                c1 = cs1[i1]
                c2 = cs2[i2]
                if c1 <= c2:
                    work(c1)
                    foreach(i1 + 1, i2, work)
                else:
                    work(c2)
                    foreach(i1, i2 + 1, work)
            else:
                int1_foreach(len(cs1) - i1, lambda i: work(cs1[i1 + i]))
        else:
            int1_foreach(len(cs2) - i2, lambda i: work(cs2[i2 + i]))

    def string_make_fwork(f):
        result = []
        foreach(0, 0, lambda c: result.append(c))
        return ''.join(result)

    return string_make_fwork(foreach)