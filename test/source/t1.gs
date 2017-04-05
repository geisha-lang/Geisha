def one = c -> 1

def id = c -> c + 1

def fun2 = c -> id(c)

def fun = c -> one(c)

def fun1 = a -> b -> add(a)(b)