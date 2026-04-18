# Daeducer Models

Any AI models you want to use with Daeducer to generate proofs should be stored in this directory.
At least one will be needed in order to perform automatic proof generation.

## Models

I recommend [Qwen3.5-35B-A3B-UD-Q8_K_XL](https://huggingface.co/unsloth/Qwen3.5-35B-A3B-GGUF/blob/main/Qwen3.5-35B-A3B-UD-Q8_K_XL.gguf). At nearly 50 GiB this is a relatively large model for a home setup, but it's a solid reasoning model that Daeducer has been carefully configured to work with.
In my tests I found it to consistently give the best results for proof generation.

In practice you can use any model in GGUF format. Models I tried and which all technically work (although many of them not very well) include the following:

| Model | Size (GiB) | Notes |
|:------|-----:|:------|
| [Qwen3.5-35B-A3B-UD-Q8_K_XL](https://huggingface.co/unsloth/Qwen3.5-35B-A3B-GGUF/blob/main/Qwen3.5-35B-A3B-UD-Q8_K_XL.gguf) | 48.7 | Default; recommended |
| [NVIDIA-Nemotron-3-Nano-4B-UD-Q2_K_XL](https://huggingface.co/unsloth/NVIDIA-Nemotron-3-Nano-4B-GGUF/blob/main/NVIDIA-Nemotron-3-Nano-4B-UD-Q2_K_XL.gguf) | 2.5 | Best for 8 GiB GPU |
[Qwen3.5-35B-A3B-UD-IQ2_M](https://huggingface.co/unsloth/Qwen3.5-35B-A3B-GGUF/blob/main/Qwen3.5-35B-A3B-UD-IQ2_M.gguf) | 11.4 | |
| [mistral-7b-instruct-v0.2.Q4_K_M](https://huggingface.co/jonahhenry/mistral-7b-instruct-v0.2.Q4_K_M-GGUF/blob/main/mistral-7b-instruct-v0.2.Q4_K_M.gguf) | 4.37 | |
| [DeepSeek-Prover-V2-7B-Q2_K](https://huggingface.co/unsloth/DeepSeek-Prover-V2-7B-GGUF/blob/main/DeepSeek-Prover-V2-7B-Q2_K.gguf) | 2.72 | |
| [DeepSeek-R1-Distill-Qwen-1.5B-Q4_K_M](https://huggingface.co/bartowski/DeepSeek-R1-Distill-Qwen-1.5B-GGUF/blob/main/DeepSeek-R1-Distill-Qwen-1.5B-Q4_K_M.gguf) | 1.12 | |
| [DeepSeek-R1-Distill-Qwen-7B.Q2_K](https://huggingface.co/roleplaiapp/DeepSeek-R1-Distill-Qwen-7B-Q2_K-GGUF/blob/main/deepseek-r1-distill-qwen-7b-q2_k.gguf) | 3.02 | |
| [Huihui-Qwen3.5-9B-abliterated-Grimoire-ORPO.i1-IQ1_M](https://huggingface.co/mradermacher/Huihui-Qwen3.5-9B-abliterated-Grimoire-ORPO-i1-GGUF/blob/main/Huihui-Qwen3.5-9B-abliterated-Grimoire-ORPO.i1-IQ1_M.gguf) | 2.88 | |
| [Qwen3.5-4B.Q2_K](https://huggingface.co/AaryanK/Qwen3.5-4B-GGUF/blob/main/Qwen3.5-4B.q2_k.gguf) | 1.8 | |
| [Qwen3.5-9B-Harmonic-IQ1_M.gguf](https://huggingface.co/DJLougen/Qwen3.5-9B-Harmonic-GGUF/blob/main/Qwen3.5-9B-Harmonic-IQ1_M.gguf) | 2.88 | |

The default Qwen3.5-35B-A3B-UD-Q8_K_XL model was tested and worked well running on an A100 with 80 GiB.

The small NVIDIA-Nemotron-3-Nano-4B-UD-Q2_K_XL model was tested on an RTX Pro 2000 with 8 GiB.

The other models, including the smaller Qwen3.5 variants, tended to give quite poor results in comparison.

## Installation

Use the download link on the model card to download the GGUF file into the `models` directory.
For example, for Qwen3.5-35B-A3B-UD-Q8_K_XL you'd do the following:

```sh
pushd models
wget https://huggingface.co/unsloth/Qwen3.5-35B-A3B-GGUF/resolve/main/Qwen3.5-35B-A3B-UD-Q8_K_XL.gguf
popd
```

If you're using something other than the default you'll also need to update the "MODEL_FILE" preprocessor variable in the `src/model.c` file:

```c
#define MODEL_FILE "Qwen3.5-35B-A3B-UD-Q8_K_XL.gguf"
```

Then rebuild the executable to cement the change:

```sh
make
```
