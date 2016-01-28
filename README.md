# cluzzy
Basic fuzzy logic hardware design in CλaSH.

### Premise.

CλaSH is an open-source extension to the Glasgow Haskell Compiler offering
hardware simulation capabilities and code synthesis into other modeling mediums
like VHDL and Verilog, all within the comfort of Haskell purity and amazing
type system.

This library aims to showcase CλaSH's capabilities by using it to design and
test a number of the components of a fuzzy logic controller.

### Compiling and running.

First off, please follow CλaSH's setup instructions off [their
homepage](http://www.clash-lang.org/).

Afterwards, clone and build the project as such:

```sh
git clone https://github.com/aznashwan/cluzzy
cd cluzzy && cabal build
```

### Documentation.

In the Docs folder, you may find all the associated PDFs generated from the
haddock documentation. In case you wish to refreshten the documentation, please
run `make docs` in the root directory of the project.

The documentation is meant to provide a quick overview into the basic
functionings of CλaSH by detailing the full development cycle of the most basic
component of the design, the `fuzzifier`.

Details on writing both a combinational and sequential modeling of a component
are given in the [fuzzifier module's
documentation](https://github.com/aznashwan/cluzzy/blob/master/Docs/Fuzzifier.pdf),
and details on writing a _testbench_ for a component are given in its [testbench
module](https://github.com/aznashwan/cluzzy/blob/master/Docs/FuzzifierTestbench.pdf)

The rest of the components and testbenches generally follow the exact
same approach as the _fuzzifier_, and are thus more sparsely documented.
