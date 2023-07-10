class BookBeanClass {

    String title

}

def groovyBook = new BookBeanClass()

groovyBook.setTitle('Groovy in Action')
assert groovyBook.getTitle() == 'Groovy in Action'
groovyBook.title = 'Groovy conquers the world'
assert groovyBook.title == 'Groovy conquers the world'
