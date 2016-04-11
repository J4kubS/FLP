# RV-2-RKA
Convert regular expression (in postfix notation) to an extended finite automaton. Written under GHC version 7.6.3.

## How to build
```
$ gmake
```

## How to run
```
$ rv-2-rka [options] [file]
```

Run with `-h` to list all available options. If no file is specified, the stdin is assumed.
Program will read a single line (*nix line endings) from file, and will try to perform action(s) according to options.

## Examples
This section shows few examples of program's output for different inputs. For more examples see `test` folder ([link](test/)).

#### Empty
Program's output for empty input ([link](test/empty.in)):
```
$ rv-2-rka -t test/empty.in
0,1
0
1
```

#### Single symbol
Program's output for a single symbol ([link](test/single.in)):
```
$ rv-2-rka -t test/single.in
0,1
0
1
0,a,1
```

#### Multiple symbols with operators
Program's output for multiple symbols and operators ([link](test/all.in)):
```
$ rv-2-rka -t test/all.in
0,1,2,3,4,5,6,7,8
0
8
0,,1
0,,8
1,,2
1,,4
2,a,3
3,,6
4,b,5
5,,6
6,c,7
7,,1
7,,8
```
