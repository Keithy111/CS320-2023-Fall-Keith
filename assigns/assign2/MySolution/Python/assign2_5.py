# Assign2-5: 20 points
# Please implement in Python a function
# of the name fnlist_make_fwork that corresponds
# to the function list_make_fwork in the library
# MyOCaml.ml

#let list_make_fwork(fwork: ('x0 -> unit) -> unit): 'x0 list =
#  let res = ref([]) in
#    let work(x0) = (res := (x0 :: !res))
#    in(*let*)(fwork(work); list_reverse(!res) )
#;;

#use "./../../../../classlib/Python/MyPython.py";;

import sys
sys.path.append("./../../../../classlib/Python")

class fnlist:
    ctag = -1
    def get_ctag(self):
        return self.ctag
    def __iter__(self):
        return fnlist_iter(self)
    def __reversed__(self):
        return fnlist_reverse(self)
    
class fnlist_iter:
    def __iter__(self):
        return self
    def __init__(self, itms):
        self.itms = itms
    def __next__(self):
        if (self.itms.ctag==0):
            raise StopIteration
        else:
            itm1 = self.itms.cons1
            self.itms = self.itms.cons2
            return itm1

class fnlist_nil(fnlist):
    def __init__(self):
        self.ctag = 0
        # return None
# end-of-class(fnlist_nil)

class fnlist_cons(fnlist):
    def __init__(self, cons1, cons2):
        self.ctag = 1
        self.cons1 = cons1
        self.cons2 = cons2
        # return None
    def get_cons1(self):
        return self.cons1
    def get_cons2(self):
        return self.cons2
# end-of-class(fnlist_cons)

def fnlist_reverse(xs):
    res = fnlist_nil()
    for x1 in xs:
        res = fnlist_cons(x1, res)
    return res

def fnlist_make_fwork(fwork):
    res = fnlist_nil()  # Initialize an empty fnlist
    def work(x0):
        nonlocal res
        res = fnlist_cons(x0, res)
    fwork(work)  # Call the provided function with the work function
    return fnlist_reverse(res)  # Reverse the fnlist and return it