// vim: noet:ts=2:sts=2:sw=2

// SPDX-License-Identifier: AGPL-3.0-or-later
// Copyright © 2026 David Llewellyn-Jones

#ifndef _PROOF_H
#define _PROOF_H

#include "symbolic.h"

#include "daeducer.h"

#define COL_RESET "\x1b[m"
#define COL_GREEN "\x1b[32m"
#define COL_RED "\x1b[31m"
#define COL_CYAN "\x1b[36m"
#define COL_MAGENTA "\x1b[35m"
#define COL_YELLOW "\x1b[33m"
#define COL_BOLD "\x1b[1m"

struct _Proof {
	char* szCommand;
	char* szAnnotation;
	size_t uStepCount;
	Step** apsStep;
	bool boError;
	bool boComplete;
	char* szError;
	Ruleset* psRuleset;
};

Proof* proof_new();
void proof_delete(Proof* psProof);
Proof* proof_load(Ruleset* psRuleset, char const* szFilename);
void proof_attach_ruleset(Proof* psProof, Ruleset* psRuleset);
Ruleset* proof_detach_ruleset(Proof* psProof);
Step* proof_get_step(Proof* psProof, size_t uPos);
void proof_process_step(Proof* psProof, char* szCommand);
void proof_print_step(Proof* psProof, size_t uStep);
void proof_print_last_step(Proof* psProof);
size_t proof_indent(Proof* psProof);
bool proof_step_scoped(Proof* psProof, size_t uStep);
bool proof_complete(Proof* psProof);
bool proof_error(Proof* psProof, char** pszError);
void proof_print(Proof* psProof);

#endif // _PROOF_H
