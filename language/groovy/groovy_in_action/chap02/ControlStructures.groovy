if (false) assert false

if (null) {
    asser false
}
else {
    assert true
}

def i = 0
while (i < 10) {
    i++
}
assert i == 10

def clinks = 0
for (remainingGuests in 0..9) {
    clinks += remainingGuests
}
assert clinks == (10 * 9) / 2

def list = [0, 1, 2 ,3]
for (j in list) {
    assert j == list[j]
}

list.each() { item ->
    assert item == list[item]
}

switch (3) {
    case 1: assert false; break
    case 3: assert true; break
    default: assert false
}
