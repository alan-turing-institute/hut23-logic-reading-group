// vim: noet:ts=2:sts=2:sw=2

// SPDX-License-Identifier: AGPL-3.0-or-later
// Copyright © 2026 David Llewellyn-Jones

#ifndef _UTILS_H
#define _UTILS_H

#include "proof.h"

size_t split_command(char* szCommand, size_t* uPlace, size_t* uLength);
bool lemma(Proof *psProof, char const* szCommand, size_t* uPiece, size_t uCount, size_t uRefNum, char const** aszPattern, char const* szResult, Step* psStep, char** szError);

#endif // _UTILS_H

