// vim: noet:ts=2:sts=2:sw=2

// SPDX-License-Identifier: AGPL-3.0-or-later
// Copyright © 2026 David Llewellyn-Jones

#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include "symbolic.h"

#include "config.h"

#define THINK_START "<think>"
#define THINK_END "</think>"

Config* config_new() {
	Config* psConfig;

	psConfig = calloc(1, sizeof(Config));
	psConfig->boMonologue = FALSE;

	return psConfig;
}

void config_delete(Config* psConfig) {
	if (psConfig) {
		if (psConfig->psContext) {
			llama_free(psConfig->psContext);
			psConfig->psContext = NULL;
		}
		if (psConfig->psModel) {
			// The model shoudl be freed by whatever created it
			//llama_model_free(psConfig->psModel);
			psConfig->psModel = NULL;
		}

		free(psConfig);
		psConfig = NULL;
	}
}

void config_set_model(Config* psConfig, struct llama_model* psModel, int nContextSize) {
	int32_t nConverted;
	struct llama_context_params sCtxParams;

	psConfig->psModel = psModel;
	psConfig->psVocab = llama_model_get_vocab(psModel);

	llama_token think_tokens[1];
	nConverted = llama_tokenize(psConfig->psVocab, THINK_START, strlen(THINK_START), think_tokens, 1, false, true);
	assert(nConverted == 1);
	psConfig->uThinkingStartToken = think_tokens[0];

	nConverted = llama_tokenize(psConfig->psVocab, THINK_END, strlen(THINK_END), think_tokens, 1, false, true);
	assert(nConverted == 1);
	psConfig->uThinkingEndToken = think_tokens[0];

	psConfig->nContextSize = nContextSize;

	sCtxParams = llama_context_default_params();
	sCtxParams.n_ctx = nContextSize;
	sCtxParams.n_batch = nContextSize;

	psConfig->psContext = llama_init_from_model(psConfig->psModel, sCtxParams);
	if (!psConfig->psContext) {
		fprintf(stderr , "%s: error: failed to create the llama_context\n" , __func__);
		exit(0);
	}
}


