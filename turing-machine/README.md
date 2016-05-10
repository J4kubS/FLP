# Turing machine
Non-deterministic Turing machine simulator in Prolog.

## How to build
```
$ make
```

## How to run
```
$ ./turing-machine
```
## Configuration
See [tests](test/) for input and output examples. Application uses `stdin` and `stdout`.

## Examples
| File                                        | Description                               | Execution time |
|:--------------------------------------------|:------------------------------------------|---------------:|
| [a-star-ab-star.in](test/a-star-ab-star.in) | `a*+(ab)*` acceptor                       | 20 ms          |
| [a-plus.in](test/a-plus.in)                 | `(a)+` acceptor                           | 19 ms          |
| [alphabet.in](test/alphabet.in)             | Increment every character until it is `z` | 30 ms          |
