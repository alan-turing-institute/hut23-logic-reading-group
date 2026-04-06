// vim: noet:ts=2:sts=2:sw=2

// SPDX-License-Identifier: AGPL-3.0-or-later
// Copyright © 2026 David Llewellyn-Jones

#ifndef _SAMPLER_H_
#define _SAMPLER_H_

#include "vector.h"
#include "config.h"

#define INFINITY (float)(1e+300 * 1e+300)

typedef struct _Sampler Sampler;

Sampler* sampler_new();
void sampler_delete(Sampler* psSampler);
void sampler_set_grammar(Sampler* psSampler, char const* const szGrammarFile, struct llama_vocab const* psVocab);
llama_token sampler_sample(Sampler* psSampler, struct llama_context * ctx, int nIdx, struct llama_vocab const* vocab);
void sampler_accept(Config* psConfig, Sampler* psSampler, llama_token nToken);
bool sampler_apply_grammar(Sampler* psSampler);
void sampler_set_apply_grammar(Sampler* psSampler, bool boApplyGrammar);
void sampler_set_reasoning_budget(Sampler* psSampler, Config* psConfig, int32_t nBudget);
void sampler_reset(Sampler* psSampler);
void sampler_neuralize(Sampler* psSampler);
void sampler_output_progress(Sampler* psSampler);
void sampler_clear_progress();

#endif /* _SAMPLER_H_ */
