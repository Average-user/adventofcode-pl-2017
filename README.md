## AdventOfCode 2017 in Prolog. (SWI-Prolog 7.2.3)

#### Project structure:
For every ```src/dayX.pl``` will be a ```src/Inputs/dayX.txt``` in where the
puzzle input would be stored. Also, some commonly
used code will be stored in ```src/tools.pl```.

#### Usage
In order to see what are the solutions for one puzzle follow this example:

Make sure you are inside the ```src``` folder, and:

``` text
src$ prolog
...
?-  [day01].
true.

?- day01a(A), day01b(B).
A = 1216,
B = 1072.
```

If you want to see how it works with your input, just replace the
correspondent ```Inputs/dayX.txt``` with your input, following the given format.

#### Missing Stuff
-   [Day 05, part B](https://github.com/Average-user/adventofcode-pl-2017/blob/master/src/day05.pl).
-   [Day 22, part B](https://github.com/Average-user/adventofcode-pl-2017/blob/master/src/day22.pl).

#### [License](https://github.com/Average-user/adventofcode-pl-2017/blob/master/LICENSE)
``` text
MIT License

Copyright (c) 2017 Lucas Polymeris

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
```
