
### 1.0.3.0 

* Reverted Marcus' patch.
* Heuristic testing of `dtwMemo` against `fastDTW` is kind of working now.

### 1.0.2.0 

* There was a zero indexing bug that was discovered by *Marcus Dean Gabriel*.

### 1.0.1.0 

* Switched from `MemoTrie` to memoization in a vector. Less memory footprint.
* Bumped version of vector dependency from `>= 0.10 && < 0.11` to `>= 0.10 && < 0.12`.
