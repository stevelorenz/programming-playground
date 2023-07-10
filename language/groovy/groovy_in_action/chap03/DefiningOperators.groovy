import groovy.transform.Immutable

@Immutable
class Monkey {

    int amount
    String currency

    Monkey plus (Monkey other) {
        if (other == null) return this

        if (other.currency != currency) {
            throw new IllegalArgumentException("Cannot add $other.currency to $currency")
        }
        return new Monkey(amount + other.amount, currency)
    }

}

Monkey buck = new Monkey(1, 'USD')
assert buck
// In Groovy == operator is overloaded and calls the equals method of the type
// If you need the Java-style == to compare e.g. addresses, use the is() operator
assert buck + buck == new Monkey(2, 'USD')
assert (buck + buck).equals(new Monkey(2, 'USD'))
