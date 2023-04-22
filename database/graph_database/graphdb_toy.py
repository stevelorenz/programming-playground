#!/bin/python3

"""
A very very simple toy to understand the basics of a Graph DB
"""

import networkx as nx
import matplotlib.pyplot as plt


def main():
    graph = nx.Graph()

    #########################
    #  Add nodes and edges  #
    #########################

    graph.add_node("Alice")
    graph.add_node("Bob")
    graph.add_node("Charlie")

    graph.add_edge("Alice", "Bob")
    graph.add_edge("Bob", "Charlie")

    ####################
    #  Plot the graph  #
    ####################

    # nx.draw(graph, with_labels=True)
    # plt.show()

    #####################
    #  Query the graph  #
    #####################

    neighbors = list(graph.neighbors("Bob"))
    print(neighbors)


if __name__ == "__main__":
    main()
