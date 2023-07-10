myList = []

myList += 'a'
assert myList == ['a']

myList += ['b']
// - is overloded with the kind of remove method for a lists
assert myList - ['b'] == ['a']

def expr = ''
for (i in [1, '*', 5]) {
    expr += i
}
assert expr == '1*5'

assert [1, [2, 3]].flatten() == [1, 2, 3]
assert [1, 2, 3].intersect([4, 3, 1]) == [1, 3]

def list = [1, 2, 3]
poped = list.pop()
assert poped == 1
assert list == [2, 3]

list = [ [1, 0], [0, 1, 2] ]
list = list.sort { a, b -> a[0] <=> b[0] }

list = list.sort { item -> item.size() }
assert list == [[1, 0], [0, 1, 2]]

def doubled = [1, 2, 3].collect { item ->
    item * 2
}
assert doubled == [2, 4, 6]

def odd = [1, 2, 3].findAll { item ->
    item % 2 == 1
}
assert odd == [1, 3]

def quickSort(list) {
    if (list.size() < 2) {
        return list
    }

    def pivot = list[list.size().intdiv(2)]
    def left = list.findAll { item -> item < pivot }
    def right = list.findAll { item -> item > pivot }
    // Use pivot could lose repeated pivot elements
    def middle = list.findAll { item -> item == pivot }

    return quickSort(left) + middle + quickSort(right)
}

assert quickSort([])                 == []
assert quickSort([1])                == [1]
assert quickSort([1,2])              == [1,2]
assert quickSort([2,1])              == [1,2]
assert quickSort([3,1,2])            == [1,2,3]
assert quickSort([3,1,2,2])          == [1,2,2,3]
assert quickSort([1.0f,'a',10,null]) == [null,1.0f,10,'a']
assert quickSort('bca')              == 'abc'.toList()
