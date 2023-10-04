# Assign3-6: 30 points
# Please translate the datatype mylist (given in Assign2) into
# type classes (by following the example of fnlist in MyPython.py).
# Then please translate mylist_foreach and mylist_rforeach into Python
#

#type 'a mylist =
#  | MyNil
#  | MyCons of 'a * 'a mylist
#  | MySnoc of 'a mylist * 'a
#  | MyReverse of 'a mylist
#  | MyAppend2 of 'a mylist * 'a mylist

#class fnlist:
#    ctag = -1
#    def get_ctag(self):
#        return self.ctag
#    def __iter__(self):
#        return fnlist_iter(self)
#    def __reversed__(self):
#        return fnlist_reverse(self)
# end-of-class(fnlist)

#let rec
#mylist_foreach
#(xs: 'a mylist)
#(work: 'a -> unit): unit =
#match xs with
#| MyNil -> ()
#| MyCons(x1, xs) ->
#  (work(x1); mylist_foreach(xs)(work))
#| MySnoc(xs, x1) ->
#  (mylist_foreach(xs)(work); work(x1))
#| MyReverse(xs) -> mylist_rforeach(xs)(work)
#| MyAppend2(xs1, xs2) ->
#  (mylist_foreach(xs1)(work); mylist_foreach(xs2)(work))

#and
#mylist_rforeach
#(xs: 'a mylist)
#(work: 'a -> unit): unit =
#match xs with
#| MyNil -> ()
#| MyCons(x1, xs) ->
#  (mylist_rforeach(xs)(work); work(x1))
#| MySnoc(xs, x1) ->
#  (work(x1); mylist_rforeach(xs)(work))
#| MyReverse(xs) -> mylist_foreach(xs)(work)
#| MyAppend2(xs1, xs2) ->
#  (mylist_rforeach(xs2)(work); mylist_rforeach(xs1)(work))
#;;

import sys
sys.path.append("./../../../../classlib/Python")

class MyList:
    def __init__(self, ctag):
        self.ctag = ctag

class MyNil(MyList):
    def __init__(self):
        super().__init__(0)

class MyCons(MyList):
    def __init__(self, x1, xs):
        super().__init__(1)
        self.x1 = x1
        self.xs = xs

class MySnoc(MyList):
    def __init__(self, xs, x1):
        super().__init__(2)
        self.xs = xs
        self.x1 = x1

class MyReverse(MyList):
    def __init__(self, xs):
        super().__init__(3)
        self.xs = xs

class MyAppend2(MyList):
    def __init__(self, xs1, xs2):
        super().__init__(4)
        self.xs1 = xs1
        self.xs2 = xs2

def mylist_nil():
    return MyNil()

def mylist_cons(x1, xs):
    return MyCons(x1, xs)

def mylist_snoc(xs, x1):
    return MySnoc(xs, x1)

def mylist_append2(xs1, xs2):
    return MyAppend2(xs1, xs2)

def mylist_reverse(xs):
    return MyReverse(xs)

def mylist_foreach(xs, work):
    ctag = xs.ctag
    if ctag == 0:
        pass
    elif ctag == 1:
        work(xs.x1)
        mylist_foreach(xs.xs, work)
    elif ctag == 2:
        mylist_foreach(xs.xs, work)
        work(xs.x1)
    elif ctag == 3:
        mylist_rforeach(xs.xs, work)
    elif ctag == 4:
        mylist_foreach(xs.xs1, work)
        mylist_foreach(xs.xs2, work)

def mylist_rforeach(xs, work):
    ctag = xs.ctag
    if ctag == 0:
        pass
    elif ctag == 1:
        mylist_rforeach(xs.xs, work)
        work(xs.x1)
    elif ctag == 2:
        work(xs.x1)
        mylist_rforeach(xs.xs, work)
    elif ctag == 3:
        mylist_foreach(xs.xs, work)
    elif ctag == 4:
        mylist_rforeach(xs.xs2, work)
        mylist_rforeach(xs.xs1, work)
