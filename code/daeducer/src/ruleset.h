// vim: noet:ts=2:sts=2:sw=2

// SPDX-License-Identifier: AGPL-3.0-or-later
// Copyright © 2026 David Llewellyn-Jones

#ifndef _RULESET_H
#define _RULSET_H

#include "proof.h"

typedef struct _Ruleset Ruleset;

Ruleset* ruleset_new();
void ruleset_delete(Ruleset* psRuleset);
Ruleset* ruleset_load(char const* szDirectory);

#endif // _LEMMA_H

