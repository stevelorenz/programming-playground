// Groovy list is instance of java.util.List
def roman = ['', 'I', 'II', 'III', 'IV', 'V', 'VI', 'VII']
assert roman[4] == 'IV'

// List size is extended when accessing an index out-of current bound
roman[8] = 'VIII'
assert roman.size() == 9
