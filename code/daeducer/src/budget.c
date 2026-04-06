// vim: noet:ts=2:sts=2:sw=2

// SPDX-License-Identifier: AGPL-3.0-or-later
// Copyright © 2026 David Llewellyn-Jones

#include <stdlib.h>
#include <string.h>

#include "llama.h"
#include "sampler.h"

#include "budget.h"

VECTOR_SIGS(llama_token_data)
VECTOR_SIGS(llama_token)

typedef struct _TokenMatcher {
	Vector_llama_token* psTokens;
	size_t uPos;
} TokenMatcher;

TokenMatcher* token_matcher_new() {
	TokenMatcher* psTokenMatcher;

	psTokenMatcher = calloc(1, sizeof(psTokenMatcher));

	psTokenMatcher->psTokens = vector_new_llama_token();
	psTokenMatcher->uPos = 0;

	return psTokenMatcher;
}

void token_matcher_delete(TokenMatcher* psTokenMatcher) {
	if (psTokenMatcher) {
		vector_delete_llama_token(psTokenMatcher->psTokens);

		free(psTokenMatcher);
		psTokenMatcher = NULL;
	}
}

bool token_matcher_advance(TokenMatcher* psTokenMatcher, llama_token nToken) {
	llama_token* anTokens;
	size_t nTokenSize;

	nTokenSize = vector_size_llama_token(psTokenMatcher->psTokens);
	if (nTokenSize == 0) {
		return false;
	}

	anTokens = vector_data_llama_token(psTokenMatcher->psTokens);

	if (nToken == anTokens[psTokenMatcher->uPos]) {
		psTokenMatcher->uPos++;
		if (psTokenMatcher->uPos >= nTokenSize) {
			psTokenMatcher->uPos = 0;
			return true;
		}
	} else {
		psTokenMatcher->uPos = 0;
		if (nToken == anTokens[0]) {
			psTokenMatcher->uPos = 1;
		}
	}
	return false;
}

void token_matcher_reset(TokenMatcher* psTokenMatcher) {
	psTokenMatcher->uPos = 0;
}

void token_matcher_set(TokenMatcher* psTokenMatcher, Vector_llama_token* psTokens) {
	vector_copy_llama_token(psTokenMatcher->psTokens, psTokens);
}

char const* reasoning_budget_name(struct llama_sampler const* /* psSampler */);
void reasoning_budget_accept(struct llama_sampler* psSampler, llama_token nToken);
void reasoning_budget_apply(struct llama_sampler* psSampler, llama_token_data_array* psCurP);
void reasoning_budget_reset(struct llama_sampler* psSampler);
struct llama_sampler* reasoning_budget_clone(struct llama_sampler const* psSampler);
void reasoning_budget_free(struct llama_sampler* psSampler);

static struct llama_sampler_i sReasoningBudgetInterface = {
	reasoning_budget_name,
	reasoning_budget_accept,
	reasoning_budget_apply,
	reasoning_budget_reset,
	reasoning_budget_clone,
	reasoning_budget_free,
	NULL,
	NULL,
	NULL,
	NULL,
};

struct _ReasoningBudgetContext {
	struct llama_vocab const* psVocab;

	TokenMatcher* psStartMatcher;
	TokenMatcher* psEndMatcher;
	Vector_llama_token* psForcedTokens;

	int32_t nBudget;
	int32_t nRemaining;

	REASONING_BUDGET eState;

	size_t uForcePos;
};

ReasoningBudgetContext* reasoning_budget_context_new() {
	ReasoningBudgetContext* psContext;

	psContext = calloc(1, sizeof(ReasoningBudgetContext));

	psContext->psStartMatcher = token_matcher_new();
	psContext->psEndMatcher = token_matcher_new();
	psContext->psForcedTokens = vector_new_llama_token();

	return psContext;
}

void reasoning_budget_context_delete(ReasoningBudgetContext* psContext) {
	if (psContext) {
		if (psContext->psStartMatcher) {
			token_matcher_delete(psContext->psStartMatcher);
			psContext->psStartMatcher = NULL;
		}
		if (psContext->psEndMatcher) {
			token_matcher_delete(psContext->psEndMatcher);
			psContext->psEndMatcher = NULL;
		}
		if (psContext->psForcedTokens) {
			vector_delete_llama_token(psContext->psForcedTokens);
			psContext->psForcedTokens = NULL;
		}

		free(psContext);
		psContext = NULL;
	}
}

struct llama_sampler* reasoning_budget_init(struct llama_vocab const* psVocab, Vector_llama_token* psStartTokens, Vector_llama_token* psEndTokens, Vector_llama_token* psForcedTokens, int32_t nBudget, REASONING_BUDGET eInitialState) {
	ReasoningBudgetContext* psContext;

	// Promote COUNTING with budget <= 0 to FORCING
	if (eInitialState == REASONING_BUDGET_COUNTING && nBudget <= 0) {
		eInitialState = REASONING_BUDGET_FORCING;
	}

	psContext = reasoning_budget_context_new();

	psContext->psVocab = psVocab;
	token_matcher_set(psContext->psStartMatcher, psStartTokens);
	token_matcher_set(psContext->psEndMatcher, psEndTokens);
	vector_copy_llama_token(psContext->psForcedTokens, psForcedTokens);
	psContext->nBudget = nBudget;
	psContext->nRemaining = nBudget;
	psContext->eState = eInitialState;
	psContext->uForcePos = 0;

	return llama_sampler_init(&sReasoningBudgetInterface, psContext);
}

char const* reasoning_budget_name(struct llama_sampler const* /* psSampler */) {
		return "reasoning-budget";
}

void token_to_piece(String* psPiece, struct llama_vocab const* psVocab, llama_token nToken, bool boSpecial) {
	int nChars;
	int nCheck;

	string_allocate(psPiece, 32);

	nChars = llama_token_to_piece(psVocab, nToken, string_data(psPiece), string_capacity(psPiece), 0, boSpecial);
	if (nChars < 0) {
		string_allocate(psPiece, -nChars);

		nCheck = llama_token_to_piece(psVocab, nToken, string_data(psPiece), string_capacity(psPiece), 0, boSpecial);
		GGML_ASSERT(nCheck == -nChars);
	}
	else {
		string_allocate(psPiece, nChars);
	}

}

bool utf8_is_complete(String const* psString) {
	size_t uLength = string_length(psString);
	size_t uTruncated;
	size_t uPos;

	if (uLength == 0) {
		return true;
	}

	if (uLength > 4) {
		uTruncated = 4;
	}
	else {
		uTruncated = uLength;
	}

	for (uPos = 1; uPos <= uTruncated; uPos++) {
		unsigned char uChar = string_data(psString)[uLength - uPos];
		if ((uChar & 0xC0) != 0x80) {
			int expected = (uChar >= 0xF0) ? 4 : (uChar >= 0xE0) ? 3 : (uChar >= 0xC0) ? 2 : 1;
			return uPos >= expected;
		}
	}
	return false;
}

void reasoning_budget_accept(struct llama_sampler* smpl, llama_token nToken) {
	ReasoningBudgetContext* psContext = (ReasoningBudgetContext*)smpl->ctx;
	bool boAdvance;
	String* psPiece;

	switch (psContext->eState) {
		case REASONING_BUDGET_IDLE:
		{
			boAdvance = token_matcher_advance(psContext->psStartMatcher, nToken);
			if (boAdvance) {
				psContext->eState = REASONING_BUDGET_COUNTING;
				psContext->nRemaining = psContext->nBudget;

				if (psContext->nRemaining <= 0) {
					psContext->eState = REASONING_BUDGET_FORCING;
					psContext->uForcePos = 0;
				}
			}
			break;
		}
		case REASONING_BUDGET_COUNTING:
		case REASONING_BUDGET_WAITING_UTF8:
		{
			boAdvance = token_matcher_advance(psContext->psEndMatcher, nToken);
			if (boAdvance) {
				psContext->eState = REASONING_BUDGET_DONE;
				break;
			}

			bool utf8_complete = true;
			if (psContext->psVocab != NULL) {
				psPiece = string_new();
				token_to_piece(psPiece, psContext->psVocab, nToken, false);
				utf8_complete = utf8_is_complete(psPiece);
				string_delete(psPiece);
			}

			if (psContext->eState == REASONING_BUDGET_WAITING_UTF8) {
				if (utf8_complete) {
					psContext->eState = REASONING_BUDGET_FORCING;
					psContext->uForcePos = 0;
					token_matcher_reset(psContext->psEndMatcher);
				}
			} else if (psContext->eState == REASONING_BUDGET_COUNTING) {
				psContext->nRemaining--;
				if (psContext->nRemaining <= 0) {
					if (utf8_complete) {
						psContext->eState = REASONING_BUDGET_FORCING;
						psContext->uForcePos = 0;
						token_matcher_reset(psContext->psEndMatcher);
					} else {
						psContext->eState = REASONING_BUDGET_WAITING_UTF8;
						token_matcher_reset(psContext->psEndMatcher);
					}
				}
			}
			break;
		}
		case REASONING_BUDGET_FORCING:
			psContext->uForcePos++;
			if (psContext->uForcePos >= vector_size_llama_token(psContext->psForcedTokens)) {
				psContext->eState = REASONING_BUDGET_DONE;
			}
			break;
		default:
		case REASONING_BUDGET_DONE:
			break;
	}
}

void reasoning_budget_apply(struct llama_sampler* psSampler, llama_token_data_array* psCurP) {
	ReasoningBudgetContext* psContext = (ReasoningBudgetContext*)psSampler->ctx;
	size_t uPos;

	if (psContext->eState != REASONING_BUDGET_FORCING) {
		// Passthrough — don't modify logits
		return;
	}

	if (psContext->uForcePos >= vector_size_llama_token(psContext->psForcedTokens)) {
		return;
	}

	llama_token const forced = vector_data_llama_token(psContext->psForcedTokens)[psContext->uForcePos];

	// Set all logits to -inf except the forced token
	for (uPos = 0; uPos < psCurP->size; ++uPos) {
		if (psCurP->data[uPos].id != forced) {
			psCurP->data[uPos].logit = -INFINITY;
		}
	}
}

void reasoning_budget_reset(struct llama_sampler* psSampler) {
	ReasoningBudgetContext* psContext = (ReasoningBudgetContext*)psSampler->ctx;
	psContext->eState = REASONING_BUDGET_IDLE;
	psContext->nRemaining = psContext->nBudget;
	token_matcher_reset(psContext->psStartMatcher);
	token_matcher_reset(psContext->psEndMatcher);
	psContext->uForcePos = 0;
}

struct llama_sampler* reasoning_budget_clone(struct llama_sampler const* psSampler) {
	ReasoningBudgetContext const* psContext = (ReasoningBudgetContext const*)psSampler->ctx;
	return reasoning_budget_init(psContext->psVocab, psContext->psStartMatcher->psTokens, psContext->psEndMatcher->psTokens, psContext->psForcedTokens, psContext->nBudget, psContext->eState);
}

void reasoning_budget_free(struct llama_sampler* psSampler) {
	reasoning_budget_context_delete(psSampler->ctx);
}

int32_t reasoning_budget_remaining(ReasoningBudgetContext* psContext) {
	return psContext->nRemaining;
}

int32_t reasoning_budget_total(ReasoningBudgetContext* psContext) {
	return psContext->nBudget;
}
