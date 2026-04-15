// vim: noet:ts=2:sts=2:sw=2

// SPDX-License-Identifier: AGPL-3.0-or-later
// Copyright © 2026 David Llewellyn-Jones

#ifndef _LOGIC_H_
#define _LOGIC_H_

#include "symbolic.h"
#include "proof.h"
#include "daeducer.h"

bool model_prove(Model* psModel, Proof *psProof, Operation* psClaim);
Model* model_initialise();
void model_delete(Model* psModel);
void model_neuralize(Model* psModel);

#endif /* _LOGIC_H_ */
