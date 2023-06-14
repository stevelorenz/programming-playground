def i = 0
while (i < 10) {
	i++
}

assert i == 10

def list = [0, 1, 2 ,3]
for (j in list) {
	assert j == list[j]
}

list.each() { item ->
	assert item == list[item]
}
