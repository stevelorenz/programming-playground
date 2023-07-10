def store = ''

10.times {
    store += 'x'
}

assert store == 'xxxxxxxxxx'

store = ''
// Generate a range that takes a closure as its argument
1.upto(5) { number ->
    store += number
}
assert store == '12345'

store = ''
0.step(0.5, 0.1) { number ->
    store += number + ' '
}
assert store == '0 0.1 0.2 0.3 0.4 '
