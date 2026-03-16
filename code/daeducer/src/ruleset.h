// vim: noet:ts=2:sts=2:sw=2

// SPDX-License-Identifier: AGPL-3.0-or-later
// Copyright © 2026 David Llewellyn-Jones

#ifndef _RULESET_H
#define _RULESET_H

#include "daeducer.h"

Ruleset* ruleset_new();
void ruleset_delete(Ruleset* psRuleset);
Ruleset* ruleset_load(char const* szDirectory);
bool ruleset_get_command_index(Ruleset* psRuleset, char const* szCommand, size_t* puIndex);
bool ruleset_get_command_index_start(Ruleset* psRuleset, char const* szCommand, size_t uStartPos, size_t* puIndex);
Lemma* ruleset_get_lemma(Ruleset* psRuleset, size_t uIndex);

#endif // _RULESET_H

