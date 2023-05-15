# Plutus smart contract: Bet on euclid's algorithm 

The program checks wether the result of the extended euclid algorithm is correct given two initial values `a` and 
`b`. The validator runs `True` if the redeemer (the bet) is a result of type `(r,s,t)`  such that satifies
`(r,s,t) == gdc(a,b)` and otherwise `False` The values `a` and `b` are the input values of the function `gdc` which according to the requirement of the alogrithm both must follow the conditions of `a > b` and `b > 0`. The `r` value is the result (greatest common divisor) and `s,t` are the quofitients.

# Directories

* app: Plutus smart contract folder.
* contractdata: Compilation, datum and redeemer files.
* scripts: bash scripts to deploy and spend the contract.
* test: testing files.
* utilities: module to convert and serialice plutus data.

# Build, execution and tests.

```bash
# Build
cabal update
cabal build

# Compile the smart contract
cabal exec euclid-contract

# Testing
cabal test euclid-testing
```
