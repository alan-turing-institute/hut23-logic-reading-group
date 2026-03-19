// vim: noet:ts=2:sts=2:sw=2

// SPDX-License-Identifier: AGPL-3.0-or-later
// Copyright © 2026 David Llewellyn-Jones

#ifndef _LEMMA_H
#define _LEMMA_H

#include "daeducer.h"

struct _Lemma {
	char* szCommand;
	char* szAnnotation;
	size_t uRefNum;
	size_t uOpNum;
	Operation** apsPattern;
	Operation* psResult;
};

Lemma* lemma_new();
void lemma_delete(Lemma* psLemma);
bool lemma_apply(Proof *psProof, Command* psCommand, size_t uRefNum, size_t uOpNum, char const** aszPattern, char const* szResult, Step* psStep, char** pszError);
Lemma* lemma_compile(char const* szCommand, char const* szAnnotation, size_t uRefNum, size_t uOpNum, char const** aszPattern, char const* szResult);
bool lemma_apply_compiled(Lemma* psLemma, Proof *psProof, Command* psCommand, Step* psStep, char** pszError);
Lemma* lemma_from_proof(Proof* psProof);

#endif // _LEMMA_H

