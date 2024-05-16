---
name: Ordering
---

TinyAPL has a total array ordering, which means all arrays can be compared, for functions like Grade.

The ordering is as follows:

* of two arrays with different shape, the smallest is the one with the lexicographically smallest shape;
* of two arrays with the same shape, the elements are compared in ravel order:
	* numbers are smaller than characters or boxes;
	* characters are larger than numbers and smaller than boxes;
	* boxes are larger than numbers and characters;
	* two numbers are compared first by their real part and then their imaginary part;
	* two characters are compared by Unicode codepoint;
	* two boxes are compared by comparing their contained arrays, as defined here.