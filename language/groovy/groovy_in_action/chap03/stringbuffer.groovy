def greeting = 'Hello' // Java String

// Use assign at the same time when converting from String to StringBuffer
greeting <<= ' Groovy'
assert greeting instanceof java.lang.StringBuffer

greeting << '!'

// Modify StringBuffer in place
greeting[1..4] = 'i'
assert greeting.toString() == 'Hi Groovy'
