#! /usr/bin/env python
# -*- coding: utf-8 -*-
# vim:fenc=utf-8

import collections
import random

Card = collections.namedtuple("Card", ["rank", "suit"])


class FrenchDeck(object):

    ranks = [str(n) for n in range(2, 11)] + list("AJQK")
    suits = "spades diamonds clubs hearts".split()

    def __init__(self):
        self._cards = [Card(rank, suit) for rank in self.ranks for suit in self.suits]

    def __len__(self):
        return len(self._cards)

    def __getitem__(self, position):
        return self._cards[position]


suit_values = dict(spades=3, hearts=2, diamonds=1, clubs=0)


def spades_high(card):
    """Get the key for the sorting algorithm."""
    rank_value = FrenchDeck.ranks.index(card.rank)
    return rank_value * len(suit_values) + suit_values[card.suit]


if __name__ == "__main__":
    deck = FrenchDeck()
    print(f"The number of cards in a French deck: {len(deck)}")
    rand_card = random.choice(deck)
    print(f"Get a random card with rank:{rand_card.rank}, suit:{rand_card.suit}")

    print("First 3 card objects in the deck:")
    print(deck[:3])

    deck_sorted = sorted(deck, key=spades_high)
    print("The highest card after sorting:")
    print(deck_sorted[-1])
