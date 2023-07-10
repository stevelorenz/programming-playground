def result = ''

(5..9).each { element ->
    result += element
}
assert result == '56789'

def age = 36
switch (age) {
    case 16..20: insurenceRate = 0.05; break
    case 21..50: insurenceRate = 0.06; break
    case 51..65: insurenceRate = 0.07; break
    default: throw new IllegalArgumentException()
}
assert insurenceRate == 0.06

def ages = [20, 36, 42, 56]
def midage = 21..50
assert ages.grep(midage) == [36, 42]
