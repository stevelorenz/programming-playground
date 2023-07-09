import groovy.transform.Immutable

@Immutable class FixedBook {
	String title
}

def gina = new FixedBook('Groovy in Action')

// FixedBook is an Immutable class!
try {
	gina.title = "Oops!"
	assert false, "should not reach here!"

} catch (ReadOnlyPropertyException expected) {
	println("Expected Error (Immutable class): '$expected.message'")
}
