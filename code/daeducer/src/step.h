// vim: noet:ts=2:sts=2:sw=2

// SPDX-License-Identifier: AGPL-3.0-or-later
// Copyright © 2026 David Llewellyn-Jones

#ifndef _STEP_H
#define _STEP_H

#include "symbolic.h"

typedef enum _STEP {
	STEP_INVALID = -1,

	STEP_PREMISE,
	STEP_REITERATION,
	STEP_CONJUNCTION_INTRO,
	STEP_CONJUNCTION_ELIM_LEFT,
	STEP_CONJUNCTION_ELIM_RIGHT,
	STEP_IMPLICATION_ELIM,
	STEP_IMPLICATION_INTRO,
	STEP_DISJUNCTION_INTRO_LEFT,
	STEP_DISJUNCTION_INTRO_RIGHT,
	STEP_DISJUNCTION_ELIM,
	STEP_NEGATION_ELIM,
	STEP_NEGATION_INTRO,
	STEP_INDIRECT_PROOF,
	STEP_EXPLOSION,
	STEP_ASSUMPTION,
	STEP_DISCHARGE,
	STEP_QED,
	STEP_CONTROL,
	STEP_PRINT = STEP_CONTROL,
	STEP_HELP,

	STEP_NUM
} STEP;

typedef struct _Step Step;

struct _Step {
	char* szName;
	STEP eCommand;
	size_t uRefCount;
	Step** psRef;
	Operation* psInput;
	Operation* psResult;
	size_t uIndent;
};

static char const aszCommand[STEP_NUM][16] = {
	"premise",
	"reiteration",
	"and_intro",
	"and_elim_left",
	"and_elim_right",
	"imp_elim",
	"imp_intro",
	"or_intro_left",
	"or_intro_right",
	"or_elim",
	"not_elim",
	"not_intro",
	"indirect",
	"explosion",
	"assumption",
	"discharge",
	"qed",
	"print",
	"help",
};

static char const aszHelp[STEP_NUM][48] = {
	"premise        <expression>",
	"reiteration    <ref>",
	"and_intro      <ref> <ref>",
	"and_elim_left  <ref>",
	"and_elim_right <ref>",
	"imp_elim       <ref> <ref>",
	"imp_intro      <ref> <ref>",
	"or_intro_left  <ref> <expression>",
	"or_intro_right <ref> <expression>",
	"or_elim        <ref> <ref> <ref> <ref> <ref>",
	"not_elim       <ref> <ref>",
	"not_intro      <ref> <ref>",
	"indirect       <ref> <ref>",
	"explosion      <ref> <expression>",
	"assumption     <expression>",
	"discharge",
	"qed",
	"print",
	"help",
};

Step* step_new();
void step_delete(Step* psStep);
void step_print(Step* psStep);

#endif // _STEP_H
