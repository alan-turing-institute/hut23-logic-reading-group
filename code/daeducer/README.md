# Daeducer

A simple TFL proof constructor that follows the approach in Chapter 17 of the Forall x: Calgary book on formal logic.

Daeducer is an interactive terminal application.
Once executed you can enter commands to build up valid proofs in the style of those from the Forall x: Calgary book.

## Build

These steps have been tested on Linux (Ubuntu 22.04) and macOS (Sequoia 15.7.4).
You'll need to have build-essentials (make, gcc, etc.) and git installed for this to work.

To build Daeducer you must first clone the repository and build the sympbolic library.

```sh
git clone https://github.com/alan-turing-institute/hut23-logic-reading-group.git
pushd hut23-logic-reading-group/sudoku/symbolic/
./configure
make
popd
```

Having build this you can now build Daeducer.

```sh
cd hut23-logic-reading-group/code/daeducer/
make
```

This will generate a `daeducer` executable.

## Use

To use Daeducer, simply run the command:

```sh
./daeducer
```

You will be presented with a command prompt where you can enter Daeducer commands.
Enter `help` to show a list of commands that you can use.
Enter `<ctrl>-d` to exit the programme.

Here's a sequence of example commands to try out, taken from Exercise B, Chapter 17 of Forall x: Calgary.

```sh
premise A -> D
assumption A ^ B
and_elim_left 2
imp_elim 1 3
or_intro 4 E
discharge
imp_intro 2 5
qed
```

## Licence

Daeducer is released under the AGPL-3.
See the LICENSE file for the full text of the licence.

