def myMap = [a:1, b:2, c: 3]

assert !myMap.isEmpty()
assert myMap.size() == 3
assert myMap.containsKey('a')
assert myMap.entrySet() instanceof Collection
assert myMap.keySet() == ['a', 'b', 'c'] as Set
assert myMap.values().toList() == [1, 2 ,3]

def store = ''
myMap.each { entry ->
    store += entry.key
    store += entry.value
}
assert store == 'a1b2c3'

store = ''
myMap.each { key, value ->
    store += key
    store += value
}
assert store == 'a1b2c3'

def textCorpus =
"""
Look for the bare necessities
The simple bare necessities
Forget about your worries and your strife
I mean the bare necessities
Old Mother Nature's recipes
That bring the bare necessities of life
"""

def words = textCorpus.tokenize()
assert words instanceof List
def wordFrequency = [:]

words.each { word ->
    wordFrequency[word] = wordFrequency.get(word, 0) + 1
}
def wordList = wordFrequency.keySet().toList()
wordList.sort { item -> wordFrequency[item] }

def statistic = "\n"
// The least 5 words
wordList[-1..-5].each { word ->
  statistic += word.padLeft(12)    + ': '
  statistic += wordFrequency[word] + "\n"
}
assert statistic == """
 necessities: 4
        bare: 4
         the: 3
        your: 2
        life: 1
"""
