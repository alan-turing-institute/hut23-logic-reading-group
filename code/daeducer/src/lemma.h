// vim: noet:ts=2:sts=2:sw=2

// SPDX-License-Identifier: AGPL-3.0-or-later
// Copyright © 2026 David Llewellyn-Jones

#ifndef _LEMMA_H
#define _LEMMA_H

#include "proof.h"

typedef struct _Lemma Lemma;

Lemma* lemma_new();
void lemma_delete(Lemma* psLemma);
bool lemma(Proof *psProof, char const* szCommand, size_t* uPiece, size_t uCount, size_t uRefNum, char const** aszPattern, char const* szResult, Step* psStep, char** szError);
bool lemma_compiled(Lemma* psLemma, Proof *psProof, char const* szCommand, size_t* uPiece, size_t uCount, Step* psStep, char** szError);
Lemma* lemma_from_proof(Proof* psProof);

#endif // _LEMMA_H

