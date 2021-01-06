#### This is an implementation of the paper : ~~~

## Getting started

### Prerequisite

- SWI-Prolog newer than 8.0.2
- Python 3  ( tested with 3.7.6 )
- Python package : pycairo

### Installation

```console
$ git clone https://github.com/wenchiyang/pCTL-REBEL
$ cd pCTL-REBEL
```

## Configuration

This implementation is based on SWI-Prolog. All *.pl files are prolog files. There are two existing domains (1) blocks world and (2) load unload

### Run a customized property

```console
$ cd pCTL_refactoring/blocksworld
```

You can find existing properties in properties.pl. To add a new formula in properties.pl, use the following template. Remember to replace name_of_property and Phi.

```prolog
name_of_property :-
    statistics(runtime, [Start|_]), % start measuring runtime
    Phi = until(_, 3, states([[]]), states([[on(a,b)]]), >=, 0.6), % specify a pCTL formula
    evaluate(Phi), !, % evaluate formula
    statistics(runtime, [Stop|_]), % stop measuring runtime
    print_message(informational, exetime(Start, Stop)).
```

Here's a cheetsheet for specifying Phi in the prolog language. For more details, please refer to the paper.

| pCTL formula                                       | Prolog representation                                        |
| -------------------------------------------------- | ------------------------------------------------------------ |
|  $\mathtt{\{on(a, b)\}}$                           | $\mathtt{states([[on(a,b)]])}$                               |
| $\mathtt{\{cl(a), on(a, b)\} \vee \{cl(c)\}}$      | $\mathtt{states([[cl(a), on(a,b)], [cl(c)]])}$               |
| $\mathtt{P_{\bowtie p} [\phi_1 U^{\leq n}\phi_2]}$ | $\mathtt{until(Output, n, \phi_1, \phi_2, \bowtie, p)}$  // for unbounded $\mathtt{U}$, just set a large $\mathtt{n}$ |
| $\mathtt{P_{\bowtie p} [X\; \phi]}$                | $\mathtt{next(Output, \phi, \bowtie, p)}$                    |
| $\mathtt{\phi_1 \wedge \phi_2}$                    | $\mathtt{and(Output, \phi_1, \phi_2)}$                       |
| $\mathtt{\phi_1 \vee \phi_2}$                      | To be determined                                             |

After adding a new property in properties.pl, do not forget to change run.py

```python
tasks = [
    ...
    ["name_of_task", ['swipl','-g','name_of_property','-g','halt','properties.pl']]
]
```

To run pCTL-REBEL on the specified task lists in the blocks world domain, do:

```console
$ python run.py
```

Then you can find the output files here:

```console
$ cd experiment
$ ls
name_of_task.jpg          name_of_task.txt
```

## Add a new domain

To do this, create a domain folder and add the the five required files:

```console
$ mkdir newdomain
$ cd newdomain
$ touch setting.pl sorting.pl precond.pl properties.pl run.py
```

- setting.pl  # specify the abstract transitions
- sorting.pl  # specify the state relations, action atoms and their order. This requires the most effort.
- precond.pl # uses the CHR library to remove illegal states
- properties.pl # specify properties to be model checked
- run.py # pCTL-REBEL runner
