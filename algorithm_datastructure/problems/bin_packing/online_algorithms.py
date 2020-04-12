def nextfit(weights, capability):
    bin_num = 0
    # The rest capability in current used bin.
    rest_cap = capability

    for w in weights:
        if rest_cap >= w:
            rest_cap = rest_cap - w
        else:
            # Add a new bin and use the new bin.
            bin_num += 1
            rest_cap = capability - w

    return bin_num


# Test Data
weights = [2, 5, 4, 7, 1, 3, 8]
capability = 10

print("Number of bins required in Next Fit :", nextfit(weights, capability))
