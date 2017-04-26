# Configuring 

All configuration happens in the `_oasis` file in the root of this source distribution. If you modify this, you must reconfigure the project as follows:

```shell
$ oasis setup
```

# Building

The file `Makefile` provides all the usual build support. To build, type `make`. 

# Running the Compiler

The compiler is split into 4 different pieces. Each piece has a corresponding `.byte` file that will be produced by a build in the root directory. 

- `cam` - The full compiler. Does lexing, parsing, code generation, and runs the program. 
- `camc` - Lexer, parser, and code generator
- `camlex` - Lexer only
- `camparse` - Lexer and parser

To run any part of the compiler, execute one of the `.byte` files in the root directory. For example:

```shell
$ ./camc.byte <filename>
```

If you want to use the canonical name, you can execute it like this:

```shell
$ opam config exec -- camparse
```

# Executing Commands

In general, any command you wish to execute with the build environment of the compiler should be prefixed with:

```
$ opam config exec -- <your command>
```


# Running a Toplevel

First, make sure to install utop (a very nice toplevel):

```shell
$ opam install merlin utop ocp-indent
```

To start the toplevel:

```
$ opam config exec -- utop
```