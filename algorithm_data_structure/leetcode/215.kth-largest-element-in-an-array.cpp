/*
 * @lc app=leetcode id=215 lang=cpp
 *
 * [215] Kth Largest Element in an Array
 */

// @lc code=start

#include <cstdlib>
#include <ctime>

#include <algorithm>
#include <functional>
#include <vector>
#include <iostream>
#include <string_view>

using namespace std;

// Simple sort solution. Need O(logN) complexity to sort the nums.
int useSimpleSort(const vector<int> &nums, int k)
{
    vector<int> nums_copy = nums;
    sort(nums_copy.begin(), nums_copy.end());
    return nums_copy[nums_copy.size() - k];
}

void print(string_view text, vector<int> const &v = {})
{
    cout << text << ": ";
    for (const auto &e : v)
        cout << e << ' ';
    cout << '\n';
}

// Use the idea of heap
int useHeap(const vector<int> &nums, int k)
{
    vector<int> h = nums; // Should be deep copy
    make_heap(h.begin(), h.end());

    for (size_t i = 0; i < k - 1; i++)
    {
        // The pop_heap is not actually really pop_heap...
        // According to the documentaiton, it actually just
        // makes the subrange [first, last-1] into a new heap.
        // The actual element is not poped yet, a calling to pop_back on the vector
        // is required to actually pop_back() the element
        pop_heap(h.begin(), h.end());
        h.pop_back(); // Actually remove the element...
    }
    return h[0]; // The largest element is the first element of the heap
}

int quickParition(vector<int> &nums, int left, int right)
{
    int pivot = nums[left];
    // Two pointers approach !!! Double-close.
    int l = left + 1, r = right;

    while (l <= r)
    {
        if (nums[l] < pivot && nums[r] > pivot)
        { // Move the larger element to the beginning
            swap(nums[l++], nums[r--]);
        }
        if (nums[l] >= pivot)
            l++;
        if (nums[r] <= pivot)
            r--;
    }
    // Move the pivot element to its position.
    swap(nums[left], nums[r]);
    return r;
}

// Use quick selection.
int useQuickSelect(vector<int> &nums, int k)
{
    int left = 0, right = nums.size() - 1;

    while (true)
    {
        // The position is [0, nums.size() - 1]
        int pos = quickParition(nums, left, right);

        // I get this part!
        if (pos == k - 1)
        {
            return nums[pos];
        }
        if (pos > k - 1)
        {
            right = pos - 1;
        }
        else
        {
            left = pos + 1;
        }
    }
}

int useSTLAlgorithm(vector<int> &nums, int k)
{
    int n = nums.size();
    // This algorithm seems to be not very intuitive...
    // This sort the nums in the small-big format!
    // It makes sure that all element < nums.end() - k are smaller than the nums[n-k]
    nth_element(nums.begin(), nums.end() - k, nums.end());
    return nums[n - k];
}

class Solution
{
public:
    int findKthLargest(vector<int> &nums, int k)
    {
        // int ret = useSimpleSort(nums, k);
        // int ret = useHeap(nums, k);
        // int ret = useSTLAlgorithm(nums, k);
        int ret = useQuickSelect(nums, k);
        return ret;
    }
};
// @lc code=end
