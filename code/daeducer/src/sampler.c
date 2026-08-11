// vim: noet:ts=2:sts=2:sw=2

// SPDX-License-Identifier: AGPL-3.0-or-later
// Copyright © 2026 David Llewellyn-Jones

#include <stdlib.h>
#include <string.h>

#include "llama.h"
#include "fileutils.h"
#include "budget.h"
#include "proof.h"

#include "sampler.h"

VECTOR_SIGS(llama_token_data)
VECTOR(llama_token_data)
VECTOR_SIGS(llama_token)
VECTOR(llama_token)

typedef struct _Sampler {
	struct llama_sampler* psGrammar;
	struct llama_sampler* psReasoningBudget;
	struct llama_sampler* psChain;

	Vector_llama_token_data* psCur;

	llama_token_data_array sCurP;

	bool boApplyGrammar;
} Sampler;

Sampler* sampler_new() {
	Sampler* psSampler;

	psSampler = calloc(1, sizeof(Sampler));

	// initialize the sampler
	psSampler->psChain = llama_sampler_chain_init(llama_sampler_chain_default_params());
	llama_sampler_chain_add(psSampler->psChain, llama_sampler_init_min_p(0.05f, 1));
	llama_sampler_chain_add(psSampler->psChain, llama_sampler_init_temp(0.8f));
	llama_sampler_chain_add(psSampler->psChain, llama_sampler_init_dist(LLAMA_DEFAULT_SEED));

	psSampler->psReasoningBudget = NULL;

	psSampler->psCur = vector_new_llama_token_data();
	psSampler->boApplyGrammar = false;
	psSampler->sCurP = (llama_token_data_array){ vector_data_llama_token_data(psSampler->psCur), vector_size_llama_token_data(psSampler->psCur), 0, true };

	return psSampler;
}

void sampler_delete(Sampler* psSampler) {
	if (psSampler) {
		llama_sampler_free(psSampler->psChain);
		psSampler->psChain = NULL;

		if (psSampler->psCur) {
			vector_delete_llama_token_data(psSampler->psCur);
			psSampler->psCur = NULL;
		}

		if (psSampler->psReasoningBudget) {
			llama_sampler_free(psSampler->psReasoningBudget);
			psSampler->psReasoningBudget = NULL;
		}

		if (psSampler->psGrammar) {
			llama_sampler_free(psSampler->psGrammar);
			psSampler->psGrammar = NULL;
		}

		free(psSampler);
		psSampler = NULL;
	}
}

void sampler_set_grammar(Sampler* psSampler, char const* const szGrammarFile, struct llama_vocab const* psVocab) {
	char *szGrammar;

	szGrammar = file_read(szGrammarFile);
	if (szGrammar) {
		psSampler->psGrammar = llama_sampler_init_grammar(psVocab, szGrammar, "root");
		free(szGrammar);
	}
	else {
		printf("Failed to open grammar file.\n");
		psSampler->psGrammar = NULL;
	}
}

void sampler_set_reasoning_budget(Sampler* psSampler, Config* psConfig, int32_t nBudget) {
	Vector_llama_token* psStartTokens;
	Vector_llama_token* psEndTokens;
	Vector_llama_token* psForcedTokens;

	psStartTokens = vector_new_llama_token();
	psEndTokens = vector_new_llama_token();
	psForcedTokens = vector_new_llama_token();

	vector_push_llama_token(psStartTokens, psConfig->uThinkingStartToken);
	vector_push_llama_token(psEndTokens, psConfig->uThinkingEndToken);
	vector_push_llama_token(psForcedTokens, psConfig->uThinkingEndToken);

	psSampler->psReasoningBudget = reasoning_budget_init(psConfig->psVocab, psStartTokens, psEndTokens, psForcedTokens, nBudget, REASONING_BUDGET_IDLE);

	vector_delete_llama_token(psStartTokens);
	psStartTokens = NULL;
	vector_delete_llama_token(psEndTokens);
	psEndTokens = NULL;
	vector_delete_llama_token(psForcedTokens);
	psForcedTokens = NULL;
}

void set_logits(Sampler* psSampler, struct llama_context * ctx, int idx) {
	float const* sampled_probs = llama_get_sampled_probs_ith(ctx, idx);
	float const* sampled_logits = llama_get_sampled_logits_ith(ctx, idx);
	llama_token const* sampled_ids = llama_get_sampled_candidates_ith(ctx, idx);

	struct llama_model const* model = llama_get_model(ctx);
	struct llama_vocab const* vocab = llama_model_get_vocab(model);

	int const n_vocab = llama_vocab_n_tokens(vocab);

	if (sampled_probs) {
		uint32_t const sampled_probs_count = llama_get_sampled_probs_count_ith(ctx, idx);
		vector_allocate_llama_token_data(psSampler->psCur, sampled_probs_count);

		for (uint32_t i = 0; i < sampled_probs_count; ++i) {
			vector_data_llama_token_data(psSampler->psCur)[i] = (llama_token_data){sampled_ids[i], sampled_logits[i], sampled_probs[i]};
		}
		vector_set_size_llama_token_data(psSampler->psCur, sampled_probs_count);
	} else if (sampled_logits) {
		uint32_t const sampled_logits_count = llama_get_sampled_logits_count_ith(ctx, idx);
		vector_allocate_llama_token_data(psSampler->psCur, sampled_logits_count);

		for (uint32_t i = 0; i < sampled_logits_count; i++) {
			vector_data_llama_token_data(psSampler->psCur)[i] = (llama_token_data){sampled_ids[i], sampled_logits[i], 0.0f};
		}
		vector_set_size_llama_token_data(psSampler->psCur, sampled_logits_count);
	} else {
		float const* logits = llama_get_logits_ith(ctx, idx);
		GGML_ASSERT(logits != NULL);
		vector_allocate_llama_token_data(psSampler->psCur, n_vocab);

		for (llama_token token_id = 0; token_id < n_vocab; token_id++) {
			vector_data_llama_token_data(psSampler->psCur)[token_id] = (llama_token_data){token_id, logits[token_id], 0.0f};
		}
		vector_set_size_llama_token_data(psSampler->psCur, n_vocab);
	}

	psSampler->sCurP = (llama_token_data_array){ vector_data_llama_token_data(psSampler->psCur), vector_size_llama_token_data(psSampler->psCur), -1, false };
}

static bool grammar_should_apply(Sampler* psSampler) {
	return psSampler->boApplyGrammar && psSampler->psGrammar;
}

llama_token sampler_sample(Sampler* psSampler, struct llama_context * ctx, int nIdx, struct llama_vocab const* vocab) {
	llama_synchronize(ctx);

	llama_token id = LLAMA_TOKEN_NULL;

	struct llama_sampler* psGrammar = psSampler->psGrammar;
	struct llama_sampler* psReasoningBudget = psSampler->psReasoningBudget;
	struct llama_sampler* psChain = psSampler->psChain;
	//llama_token_data_array sCurP = psSampler->sCurP; // initialized by set_logits

	// Check if a backend sampler has already sampled a token in which case we
	// return that token id directly.
	{
		id = llama_get_sampled_token_ith(ctx, nIdx);

		if (id != LLAMA_TOKEN_NULL) {
			printf("%s: Backend sampler selected token: '%d'. Will not run any CPU samplers\n", __func__, id);

			GGML_ASSERT(!psSampler->psGrammar && "using grammar in combination with backend sampling is not supported");
			GGML_ASSERT(!psSampler->psReasoningBudget && "using reasoning budget in combination with backend sampling is not supported");

			vector_set_size_llama_token_data(psSampler->psCur, 1);
			vector_data_llama_token_data(psSampler->psCur)[0] = (llama_token_data){ id, 0.0f, 1.0f };
			psSampler->sCurP = (llama_token_data_array){ vector_data_llama_token_data(psSampler->psCur), 1, 0, true };

			return id;
		}
	}

	set_logits(psSampler, ctx, nIdx);

	// apply reasoning budget first
	llama_sampler_apply(psReasoningBudget, &psSampler->sCurP);

	llama_sampler_apply(psChain, &psSampler->sCurP);

	id = psSampler->sCurP.data[psSampler->sCurP.selected].id;

	if (llama_vocab_is_eog(vocab, id)) {
		return id;
	}

	if (!grammar_should_apply(psSampler)) {
		return id;
	}

	// check if it the sampled token fits the grammar (grammar-based rejection sampling)
	{
		llama_token_data single_token_data = { id, 1.0f, 0.0f };
		llama_token_data_array single_token_data_array = { &single_token_data, 1, -1, false };

		llama_sampler_apply(psGrammar, &single_token_data_array);

		bool const is_valid = single_token_data_array.data[0].logit != -INFINITY;
		if (is_valid) {
			return id;
		}
	}

	// resampling:
	// if the token is not valid, sample again, but first apply the grammar sampler and then the sampling chain
	set_logits(psSampler, ctx, nIdx);

	llama_sampler_apply(psReasoningBudget, &psSampler->sCurP);

	if (grammar_should_apply(psSampler)) {
		llama_sampler_apply(psGrammar, &psSampler->sCurP);
	}

	llama_sampler_apply(psChain, &psSampler->sCurP);

	GGML_ASSERT(psSampler->sCurP.selected != -1 && "no selected token during sampling - check your sampling configuration");

	id = psSampler->sCurP.data[psSampler->sCurP.selected].id;

	return id;
}

void sampler_accept(Config* psConfig, Sampler* psSampler, llama_token nToken) {
	bool boAcceptGrammar;
	if (!psSampler) {
		return;
	}

	// grammar_should_apply() checks the reasoning budget state, so calculate this before we accept
	boAcceptGrammar = grammar_should_apply(psSampler) && (!llama_vocab_is_eog(psConfig->psVocab, nToken));

	llama_sampler_accept(psSampler->psReasoningBudget, nToken);

	if (psSampler->psGrammar && boAcceptGrammar) {
		llama_sampler_accept(psSampler->psGrammar, nToken);
	}

	llama_sampler_accept(psSampler->psChain, nToken);
}

bool sampler_apply_grammar(Sampler* psSampler) {
	return psSampler->boApplyGrammar;
}

void sampler_set_apply_grammar(Sampler* psSampler, bool boApplyGrammar) {
	psSampler->boApplyGrammar = boApplyGrammar;
}

void sampler_reset(Sampler* psSampler) {
	llama_sampler_reset(psSampler->psGrammar);
	llama_sampler_reset(psSampler->psReasoningBudget);
	llama_sampler_reset(psSampler->psChain);

	sampler_set_apply_grammar(psSampler, false);
}

void sampler_neuralize(Sampler* psSampler) {
	sampler_reset(psSampler);

	vector_clear_llama_token_data(psSampler->psCur);
	psSampler->sCurP = (llama_token_data_array){ vector_data_llama_token_data(psSampler->psCur), vector_size_llama_token_data(psSampler->psCur), 0, true };
}

void sampler_output_progress(Sampler* psSampler) {
	int32_t nBudget;
	int32_t nRemaining;
	ReasoningBudgetContext* psContext;

	psContext = (ReasoningBudgetContext*)psSampler->psReasoningBudget->ctx;
	nRemaining = reasoning_budget_remaining(psContext);
	nBudget = reasoning_budget_total(psContext);

	printf(COL_RESET COL_YELLOW "Tokens: %d / %d  \r" COL_RESET, (nBudget - nRemaining), nBudget);
}

void sampler_clear_progress() {
	printf("                       \r");
}

