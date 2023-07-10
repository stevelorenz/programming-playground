assert (0..10).contains(10)

assert(0..<10).contains(9)
assert(0..<10).contains(10) == false

def a = 0..10
assert a instanceof Range
assert a.contains(5)

def today = new Date()
def yesterday = today - 1
assert (yesterday..today).size() == 2

def log = ''
(9..<5).each { element ->
    log += element
}
assert log == '9876'
