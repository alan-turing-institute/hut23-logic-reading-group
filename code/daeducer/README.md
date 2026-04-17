# Daeducer

A simple TFL proof constructor that follows the approach in Chapter 17 of the Forall x: Calgary book on formal logic.

Daeducer is an interactive terminal application.
Once executed you can enter commands to build up valid proofs in the style of those from the Forall x: Calgary book.

The tool can check proofs you enter or use machine learning models to automatically generate valid proofs.

## Build

These steps have been tested on Linux (Ubuntu 22.04) and macOS (Sequoia 15.7.4).
You'll need to have some prerequisites (build-essentials, git) installed for this to work.

Install the prerequisites:

```sh
sudo apt install build-essentials git
```

Clone the repository:

```sh
git clone --recursive \
     https://github.com/alan-turing-institute/hut23-logic-reading-group.git
cd hut23-logic-reading-group
```

All subsequent steps assume you're starting from a working directory in root of the working tree.

Build the symbolic library:

```sh
pushd sudoku/symbolic/
./configure
make
popd
```

Build the llama.cpp libraries.
Different options can be used when building to optimise for your particular hardware.
For example if you want to build for CPU-only but optimised to use OpenBLAS:

```sh
pushd code/daeducer/llama.cpp
sudo apt install libopenblas-dev
cmake -DCMAKE_BUILD_TYPE=Release -B build \
    -DGGML_BLAS=ON -DGGML_BLAS_VENDOR=OpenBLAS -DGGML_VULKAN=ON
cmake --build build --config Release
popd
```

See the section below on building llama.cpp in case you want to use other accelerated compute options.

In order for the Daeducer executable to be able to find the llama.cpp libraries you should either install the libraries on your system or add their path to the `LD_LIBRARY_PATH` environment variable like so:

```sh
export LD_LIBRARY_PATH=$PWD/llama.cpp/build/bin${LD_LIBRARY_PATH:+:${LD_LIBRARY_PATH}}
```

Having built the libraries you can now build Daeducer.
The Makefile is already set up to look for the libraries relative to the build directory.

```sh
pushd code/daeducer/
make
popd
```

This will generate a `daeducer` executable.

## Download a machine learning model

If you want Daeducer to support automatic proof generation, I recommend using [Qwen3.5-35B-A3B-UD-Q8_K_XL](https://huggingface.co/unsloth/Qwen3.5-35B-A3B-GGUF/blob/main/Qwen3.5-35B-A3B-UD-Q8_K_XL.gguf).
This is quite large (larger than the average home-GPU can support) but Daeducer is quite heavily tailored to its use and I've found it gives excellent results.

You can download and install it to the correct directory directly from HuggingFace:

```sh
pushd code/daeducer/models
wget https://huggingface.co/unsloth/Qwen3.5-35B-A3B-GGUF/resolve/main/Qwen3.5-35B-A3B-UD-Q8_K_XL.gguf
popd
```

## Use

To use Daeducer, simply run the command:

```sh
cd code/daeducer/
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

If you've started a proof but are not sure how to finish it, you can ask the machine learning model to complete it for you.
To do this use the `prove` command, which should be followed by the expression you want to prove.
For example:

```sh
premise ((A -> B) -> A)
prove A
```

## Building llama.cpp

If you have an NVIDIA GPU:

Install the [CUDA toolkit](https://developer.nvidia.com/cuda-downloads):

```sh
wget https://developer.download.nvidia.com/compute/cuda/repos/ubuntu2404/x86_64/cuda-keyring_1.1-1_all.deb
sudo dpkg -i cuda-keyring_1.1-1_all.deb
sudo apt-get update
sudo apt-get -y install cuda-toolkit-13-2
export PATH=/usr/local/cuda/bin${PATH:+:${PATH}}
export LD_LIBRARY_PATH=/usr/local/cuda/lib64${LD_LIBRARY_PATH:+:${LD_LIBRARY_PATH}}
```

Now you can build llama.cpp with CUDA acceleration enabled:

```sh
pushd code/daeducer/llama.cpp
cmake -DCMAKE_BUILD_TYPE=Release -B build -DGGML_CUDA=ON
cmake --build build --config Release
popd
```

For the full suite of options available for building llama.cpp, see the [llama.cpp docs](https://github.com/ggml-org/llama.cpp/blob/master/docs/build.md#using-the-lunarg-vulkan-sdk).

## Licence

Daeducer is released under the AGPL-3.
See the LICENSE file for the full text of the licence.

