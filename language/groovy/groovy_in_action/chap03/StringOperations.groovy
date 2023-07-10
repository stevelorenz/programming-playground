String greeting = 'Hello Groovy!'

assert greeting.startsWith('Hello')

assert greeting.getAt(0) == 'H'
assert greeting[0] == 'H'

assert greeting.indexOf('Groovy') > 0
assert greeting.contains('Groovy')

assert greeting[6..11] == 'Groovy'

assert greeting.count('o') == 3

assert 'x'.padLeft(3) == '  x'
assert 'x'.center(3)       == ' x '
