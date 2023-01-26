Bunched Fuzz: Sensitivity for Vector Metrics
===

## Install

Made with ocaml 4.13.1, dune, and why3

install dependencies:
```
$ opam install --deps-only -d -t .
```

then run
```
$ why3 config detect
$ dune build
```

to compile the tool

## How to typecheck programs

To typecheck a program type:

```
$ dune exec -- dfuzz examples/bfuzz/kmeans.fz
```


# troubleshooting installation process
```
$ why3 config detect
$ why3 config show
```

and then copy-paste the result into `~/.why3.conf` (maybe the first command is unnecessary)

the issue I had is the `detect` subcommand makes a `[partial_prover]` entry and no `[strategy]` entries and this seems to not get picked up by why3 current version for some reason. not sure what's up there.

after that:
```
$ dune build
$ dune exec -- dfuzz examples/bfuzz/kmeans.fz
```

# forked from

dfuzz: Linear Dependent Types for Differential Privacy

http://arxiv.org/abs/1503.04522

[1] Marco Gaboardi, Andreas Haeberlen, Justin Hsu, Arjun Narayan, and Benjamin C. Pierce. 2013. Linear dependent types for differential privacy. In Proceedings of the 40th annual ACM SIGPLAN-SIGACT symposium on Principles of programming languages (POPL '13). ACM, New York, NY, USA, 357-370. DOI=10.1145/2429069.2429113 http://doi.acm.org/10.1145/2429069.2429113

