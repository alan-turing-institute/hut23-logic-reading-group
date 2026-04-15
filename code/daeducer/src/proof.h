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
#define COL_BLUE "\x1b[34m"
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
bool proof_find_step_index(Proof* psProof, char const* szName, size_t* puStep);
Step* proof_get_step(Proof* psProof, size_t uPos);
bool proof_find_step_indices(Proof* psProof, char** aszName, size_t* auIndex, size_t uCount);
bool proof_get_steps(Proof* psProof, size_t auIndex[], Step* apsStep[], size_t uCount);

void proof_process_step(Proof* psProof, Model* psModel, Command* psCommand);
void proof_print_step(Proof* psProof, size_t uStep);
void proof_print_last_step(Proof* psProof);
size_t proof_indent(Proof* psProof);
bool proof_step_scoped(Proof* psProof, size_t uStep);
bool proof_complete(Proof* psProof);
bool proof_error(Proof* psProof, char** pszError);
void proof_print(Proof* psProof);
bool proof_save(Proof* psProof, char const* szFilenzme, char const* szCommand, char const* szAnnotation);
void proof_reset(Proof* psProof);
void proof_clear(Proof* psProof);
void proof_print_prompt(Proof* psProof);

#endif // _PROOF_H
