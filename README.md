
This module implements dynamic time warping as described on [Wikipedia](http://en.wikipedia.org/w/index.php?title=Dynamic_time_warping&oldid=643501828)

Additionally 'fastDtw' is implemented as described in the paper:
"FastDTW: Toward Accurate Dynamic Time Warping in Linear Time and
Space" by Stan Salvador and Philip Chan.

For further information see the documentation of the `Data.DTW` module.

### Example

    >>> -- create two sample datasets
    >>> let as = [ sin x | x <- [0,0.1..pi] ]
    >>> let bs = [ sin (x+0.1) | x <- [0,0.1..pi] ]
    >>> -- define a cost function between two datapoints
    >>> let dist x y = abs (x-y)
    >>> -- define a function that will half the size of a dataset (see below)
    >>> let shrink xs = case xs of (a:b:cs) -> (a+b)/2 : shrink cs; a:[] -> [a]; [] -> []
    >>> -- calculate the cost with fastDtw and dtwMemo for comparison
    >>> cost $ fastDtw dist shrink 2 as bs :: Float
    >>> 0.19879311
    >>> cost $ dtwMemo (\x y -> abs (x-y)) as bs :: Float
    >>> 0.19879311

