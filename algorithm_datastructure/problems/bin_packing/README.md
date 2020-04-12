# Bin Packing Problem

Given n items of different weights and bins each of capacity c, assign each item to a bin such that number of total used
bins is minimized. It may be assumed that all items have weights smaller than bin capacity.

```
Input:  wieght[]       = {4, 8, 1, 4, 2, 1}
        Bin Capacity c = 10
Output: 2
We need minimum 2 bins to accommodate all items
First bin contains {4, 4, 2} and second bin {8, 2}

Input:  wieght[]       = {9, 8, 2, 2, 5, 4}
        Bin Capacity c = 10
Output: 4
We need minimum 4 bins to accommodate all items.  

Input:  wieght[]       = {2, 5, 4, 7, 1, 3, 8}; 
        Bin Capacity c = 10
Output: 3
```

## Applications:

- Loading of containers like trucks.
- Placing data on multiple disks.
- Job scheduling.
- Packing advertisements in fixed length radio/TV station breaks.
- Storing a large collection of music onto tapes/CD’s, etc.
 

## Online Algorithms

These algorithms are for Bin Packing problems where items arrive one at a time (in unknown order), each must be put in a
bin, before considering the next item.
