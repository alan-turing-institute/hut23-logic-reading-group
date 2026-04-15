// vim: noet:ts=2:sts=2:sw=2

// SPDX-License-Identifier: AGPL-3.0-or-later
// Copyright © 2026 David Llewellyn-Jones

#ifndef _BUDGET_H_
#define _BUDGET_H_

#include "vector.h"

VECTOR_SIGS(llama_token)

typedef enum _REASONING_BUDGET {
	REASONING_BUDGET_INVALID = -1,

	REASONING_BUDGET_IDLE,
	REASONING_BUDGET_COUNTING,
	REASONING_BUDGET_FORCING,
	REASONING_BUDGET_WAITING_UTF8,
	REASONING_BUDGET_DONE,

	REASONING_BUDGET_NUM,
} REASONING_BUDGET;

typedef struct _ReasoningBudgetContext ReasoningBudgetContext;

struct llama_sampler* reasoning_budget_init(struct llama_vocab const* psVocab, Vector_llama_token* psStartTokens, Vector_llama_token* psEndTokens, Vector_llama_token* psForcedTokens, int32_t nBudget, REASONING_BUDGET eInitialState);
int32_t reasoning_budget_remaining(ReasoningBudgetContext* psContext);
int32_t reasoning_budget_total(ReasoningBudgetContext* psContext);

#endif /* _BUDGET_H_ */
