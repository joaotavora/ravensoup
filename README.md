# Ravensoup

A programming exercise in text processing.

## TL;DR

Open the `ravensoup.lisp` file. Compile and load it.  It is written in
portable Common Lisp, with a few SBCL-specific optimizations.

Now run `(ravensoup:benchmark-all)` in the file's directory.

## Problem statement

> Everyone loves alphabet soup. And of course, you want to know if you
> can construct a message from the letters found in your bowl.
>
> Your task is to write function that takes as input two strings, the
> `MESSAGE` you want to write, and all the letters found in your `BOWL`
> of alphabet soup.
>
> Assumptions: (1) It may be a very large bowl of soup containing many
> letters. (2) There is no guarantee that each letter occurs a similar
> number of times - indeed some letters might be missing entirely.  (3)
> The letters are ordered randomly.
>
> The function should determine if you can write your message with the
> letters found in your bowl of soup. The function should return True or
> False accordingly.

## Pseudo-code solution

Before we get to the Common Lisp solution(s), let's analyse a generic
solution to this problem in pseudo-pseudocode:

1. To solve the alphabet soup, we use an associative container, call
   it `NEEDED`, holding the number of occurances of each character in
   the `MESSAGE` string, indexed by character.  We also store the
   total length of the `MESSAGE` string in a variable called `LEN`;

2. We fill this container by iterating the `MESSAGE` and counting the
   number of "needed" characters;

3. We iterate the `BOWL` argument and, for each one of its characters,
   decrement the corresponding number in `NEEDED`.  If the resulting
   number is _not_ negative, we still "needed" that letter, so we also
   decrement the `LEN` variable.  If it is negative or if the
   character isn't found in `NEEDED`, we didn't really use that
   letter, so we keep `LEN` untouched;

4. If at any moment `LEN` reaches 0, we don't need any more letters
   and so report success ("True") .  If we reach the end of `BOWL`
   with non-zero `LEN`, we report failure ("False").

The Big-O time complexity is `O(m+s)`, where `m` is the number of
characters in message and `s` is the number of characters in
`BOWL`.  If we assume `s >> m` this is `O(s)`.

## Common Lisp solutions

Because strings aren't all born equal, we expect some solutions to be
faster for unibyte strings, like ASCII-encoded strings, and some to be
more versatile, but slower.

To evaluate various solutions we have collected three datasets:

- `big-ascii.txt` - a 6MB ASCII text file taken from Peter Norvig's
  site;
- `das-kapital-utf-8.txt` - UTF-8 German version of Marx's "Das
  Kapital";
- `big-chinese-utf-8.txt` - A large file of UTF-8 Chinese text, no
  idea what it contains.

(To see how we turn a regular text file into a dataset of phrases and
bowls, see the function `RAVENSOUP::PHRASES-AND-BOWL`).

Four different solutions are presented:

- `RAVENSOUP:SPELLABLE-P-HASH` - Uses a hash table for the `NEEDED`
  container.  This is the slowest, for the three datasets.

- `RAVENSOUP:SPELLABLE-P-ASCII` - Uses a (stack-allocated) array as
  the `NEEDED` container.  This is indexed with `CL:CHAR-CODE` and is
  very fast for the ASCII dataset.  Unfortunately, it breaks on
  multibyte-character strings, i.e. it doesn't work at all for the
  remaining two datasets.

- `RAVENSOUP:SPELLABLE-P-MIXED` - Uses a mix of the two previous
  techniques: uses `CL:CHAR-CODE` to try to address a local array and
  falls back to the hash table if the code is too high.  Works for all
  three datasets.  Fast for `big-ascii.txt` (though not as fast as the
  ASCII-specialized version), and also quite fast for UTF-8 german
  text, since most characters used in that language have small codes.
  Since the same is definitely not true for chinese, this is just
  about as slow as `RAVENSOUP:SPELLABLE-P-HASH` for that UTF-8
  dataset.

- `RAVENSOUP:SPELLABLE-P-MIXED-TRAINED` - Like the previous
  alternative, uses a mix of the array and hash-table techniques.
  However instead of trying to index the local array via
  `CL:CHAR-CODE`, accepts a function specially trained on the dataset
  to get an optimized code that assigns lower numbers to characters
  occuring more frequently in `BOWL`.  This speeds up the performance
  for the chinese dataset considerably, making it the fastest for that
  situation, while still performing reasonably well for the other
  datasets.

## Results

The function `RAVENSOUP:BENCHMARK-ALL` tests all functions in all
combinations.  Here is a summary of results for SBCL 1.4.1.

```
big-ascii.txt
  SPELLABLE-P-HASH              26.229s (slowest)
  SPELLABLE-P-ASCII             2.423s  (fastest)
  SPELLABLE-P-MIXED             4.977s
  SPELLABLE-P-MIXED-TRAINED     6.310s

das-kapital-utf-8.txt
  SPELLABLE-P-HASH              0.875s  (slowest)
  SPELLABLE-P-ASCII             FAIL
  SPELLABLE-P-MIXED             0.124s  (fastest)
  SPELLABLE-P-MIXED-TRAINED     0.196s

big-chinese-utf-8.txt
  SPELLABLE-P-HASH              14.571s (slowest)
  SPELLABLE-P-ASCII             FAIL
  SPELLABLE-P-MIXED             14.470s
  SPELLABLE-P-MIXED-TRAINED     5.061s  (fastest)
```

CCL 1.11.5 gives the same kind of results:

```
big-ascii.txt
  SPELLABLE-P-HASH              65.644s (slowest)
  SPELLABLE-P-ASCII             4.070s  (fastest)
  SPELLABLE-P-MIXED             6.904s
  SPELLABLE-P-MIXED-TRAINED     11.394s

das-kapital-utf-8.txt
  SPELLABLE-P-HASH              1.957s  (slowest)
  SPELLABLE-P-ASCII             FAIL
  SPELLABLE-P-MIXED             0.225s  (fastest)
  SPELLABLE-P-MIXED-TRAINED     0.447s

big-chinese-utf-8.txt
  SPELLABLE-P-HASH              39.587s (slowest)
  SPELLABLE-P-ASCII             FAIL
  SPELLABLE-P-MIXED             37.87s
  SPELLABLE-P-MIXED-TRAINED     10.431s (fastest)
```

## Conclusion

The absolute time values aren't very meaningful, but it's interesting
to compare the performance of different algorithms across the same
dataset.  The conclusion seems to be, as usual, that the more
assumptions we can safely make about our data the better.  But even if
we can't make many assumptions it pays to study the characteristics of
data beforehand.

## Copyright

Copyright (C) 2019 João Távora

<!-- Local Variables: -->
<!-- coding: utf-8 -->
<!-- End: -->
