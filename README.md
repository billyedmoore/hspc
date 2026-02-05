# Haskell Pascal Compiler

Toy Pascal compiler written in Haskell using an adapted version of
the incremental approach outlined in 
[this paper](http://scheme2006.cs.uchicago.edu/11-ghuloum.pdf).

Currently implements a very small (but slowly growing) subset of
the language.

## Progress

Increments:

- [X] Integers
- [X] Binary Operators on Ints
- [X] Unary Operators on Ints
- [X] Local Variables and Assignment
- [X] Booleans 
- [X] Branching (if statements)
- [X] While Loops
- [X] Comparisons
- [X] Blocks as statements
- [X] For Loops
- [ ] Strings (WriteLn("Hello World"))
- [ ] Type Checking
- [ ] Procedures
- [ ] Functions
- [ ] Arrays
- [ ] Floats
- [ ] Records
- [ ] Pointers 

## Testing

HSPC's approach to testing is to test parity with [fpc](https://www.freepascal.org/).
This is tested with integration tests comparing the `STDOUT` and
`EXIT_CODE` of compiled versions of the same pascal code
(one by `hspc` and one by `fpc`).

To rerun the programs against `fpc`.

```bash
# from project root
cd test/sample_programs
bash generate_goldens.bash
```

To run the tests.

```bash
# from project root
cabal test
```
