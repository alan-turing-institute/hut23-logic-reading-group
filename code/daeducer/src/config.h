// vim: noet:ts=2:sts=2:sw=2

// SPDX-License-Identifier: AGPL-3.0-or-later
// Copyright © 2026 David Llewellyn-Jones

#ifndef _CONFIG_H_
#define _CONFIG_H_

#include "llama.h"

typedef struct _Config {
	struct llama_model* psModel;
	struct llama_vocab const* psVocab;
	struct llama_context* psContext;

	llama_token uThinkingStartToken;
	llama_token uThinkingEndToken;

	int nContextSize;
	bool boMonologue;
} Config;

Config* config_new();
void config_delete(Config* psConfig);
void config_set_model(Config* psConfig, struct llama_model* psModel, int nContextSize);

#endif /* _CONFIG_H_ */
