// vim: noet:ts=2:sts=2:sw=2

// SPDX-License-Identifier: AGPL-3.0-or-later
// Copyright © 2026 David Llewellyn-Jones

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "proof.h"
#include "utils.h"
#include "symbolic.h"

#include "step.h"

Step* step_new() {
	Step* psStep;

	psStep = calloc(1, sizeof(Step));
	psStep->eCommand = STEP_INVALID;

	return psStep;
}

void step_delete(Step* psStep) {
	size_t uPos;

	if (psStep) {
		if (psStep->szName) {
			free(psStep->szName);
		}

		if (psStep->apsInput) {
			for (uPos = 0; uPos < psStep->uInputCount; ++uPos) {
				if (psStep->apsInput[uPos]) {
					FreeRecursive(psStep->apsInput[uPos]);
					psStep->apsInput = NULL;
				}
			}
			free(psStep->apsInput);
			psStep->apsInput = NULL;
		}

		free(psStep);
	}
}

void step_print(Step* psStep) {
	int nLength;
	char* szResult;
	size_t uIndent;
	char* szIndent;
	char* szCommand;
	size_t uCommandLength;

	if (psStep->psResult) {
		nLength = OperationToStringLength(psStep->psResult);
		szResult = calloc(nLength + 1, sizeof(char));
		OperationToString(psStep->psResult, szResult, nLength + 1);
	}
	else {
		nLength = 0;
		szResult = "";
	}

	szCommand = NULL;
	uCommandLength = 0;
	switch (psStep->eCommand) {
		case STEP_PREMISE: {
			uCommandLength = 3;
			szCommand = calloc(uCommandLength, sizeof(char));
			snprintf(szCommand, uCommandLength, "PR");
		}
		break;
		case STEP_REITERATION: {
			uCommandLength = snprintf(NULL, 0, "RE, %s", psStep->apsRef[0]->szName) + 1;
			szCommand = calloc(uCommandLength, sizeof(char));
			snprintf(szCommand, uCommandLength, "RE, %s", psStep->apsRef[0]->szName);
		}
		break;
		case STEP_CONJUNCTION_INTRO: {
			uCommandLength = snprintf(NULL, 0, "^I, %s, %s", psStep->apsRef[0]->szName, psStep->apsRef[1]->szName) + 1;
			szCommand = calloc(uCommandLength, sizeof(char));
			snprintf(szCommand, uCommandLength, "^I, %s, %s", psStep->apsRef[0]->szName, psStep->apsRef[1]->szName);
		}
		break;
		case STEP_CONJUNCTION_ELIM_LEFT: {
			uCommandLength = snprintf(NULL, 0, "^E, %s", psStep->apsRef[0]->szName) + 1;
			szCommand = calloc(uCommandLength, sizeof(char));
			snprintf(szCommand, uCommandLength, "^E, %s", psStep->apsRef[0]->szName);
		}
		break;
		case STEP_CONJUNCTION_ELIM_RIGHT: {
			uCommandLength = snprintf(NULL, 0, "^E, %s", psStep->apsRef[0]->szName) + 1;
			szCommand = calloc(uCommandLength, sizeof(char));
			snprintf(szCommand, uCommandLength, "^E, %s", psStep->apsRef[0]->szName);
		}
		break;
		case STEP_IMPLICATION_ELIM: {
			uCommandLength = snprintf(NULL, 0, "->E, %s, %s", psStep->apsRef[0]->szName, psStep->apsRef[1]->szName) + 1;
			szCommand = calloc(uCommandLength, sizeof(char));
			snprintf(szCommand, uCommandLength, "->E, %s, %s", psStep->apsRef[0]->szName, psStep->apsRef[1]->szName);
		}
		break;
		case STEP_IMPLICATION_INTRO: {
			uCommandLength = snprintf(NULL, 0, "->I, %s, %s", psStep->apsRef[0]->szName, psStep->apsRef[1]->szName) + 1;
			szCommand = calloc(uCommandLength, sizeof(char));
			snprintf(szCommand, uCommandLength, "->I, %s, %s", psStep->apsRef[0]->szName, psStep->apsRef[1]->szName);
		}
		break;
		case STEP_DISJUNCTION_INTRO_LEFT: {
			uCommandLength = snprintf(NULL, 0, "vI, %s", psStep->apsRef[0]->szName) + 1;
			szCommand = calloc(uCommandLength, sizeof(char));
			snprintf(szCommand, uCommandLength, "vI, %s", psStep->apsRef[0]->szName);
		}
		break;
		case STEP_DISJUNCTION_INTRO_RIGHT: {
			uCommandLength = snprintf(NULL, 0, "vI, %s", psStep->apsRef[0]->szName) + 1;
			szCommand = calloc(uCommandLength, sizeof(char));
			snprintf(szCommand, uCommandLength, "vI, %s", psStep->apsRef[0]->szName);
		}
		break;
		case STEP_DISJUNCTION_ELIM: {
			uCommandLength = snprintf(NULL, 0, "vE, %s, %s-%s, %s-%s", psStep->apsRef[0]->szName, psStep->apsRef[1]->szName, psStep->apsRef[2]->szName, psStep->apsRef[3]->szName, psStep->apsRef[4]->szName) + 1;
			szCommand = calloc(uCommandLength, sizeof(char));
			snprintf(szCommand, uCommandLength, "vE, %s, %s-%s, %s-%s", psStep->apsRef[0]->szName, psStep->apsRef[1]->szName, psStep->apsRef[2]->szName, psStep->apsRef[3]->szName, psStep->apsRef[4]->szName);
		}
		break;
		case STEP_NEGATION_ELIM: {
			uCommandLength = snprintf(NULL, 0, "!E, %s, %s", psStep->apsRef[0]->szName, psStep->apsRef[1]->szName) + 1;
			szCommand = calloc(uCommandLength, sizeof(char));
			snprintf(szCommand, uCommandLength, "!E, %s, %s", psStep->apsRef[0]->szName, psStep->apsRef[1]->szName);
		}
		break;
		case STEP_NEGATION_INTRO: {
			uCommandLength = snprintf(NULL, 0, "!I, %s-%s", psStep->apsRef[0]->szName, psStep->apsRef[1]->szName) + 1;
			szCommand = calloc(uCommandLength, sizeof(char));
			snprintf(szCommand, uCommandLength, "!I, %s-%s", psStep->apsRef[0]->szName, psStep->apsRef[1]->szName);
		}
		break;
		case STEP_INDIRECT_PROOF: {
			uCommandLength = snprintf(NULL, 0, "IP, %s-%s", psStep->apsRef[0]->szName, psStep->apsRef[1]->szName) + 1;
			szCommand = calloc(uCommandLength, sizeof(char));
			snprintf(szCommand, uCommandLength, "IP, %s-%s", psStep->apsRef[0]->szName, psStep->apsRef[1]->szName);
		}
		break;
		case STEP_EXPLOSION: {
			uCommandLength = snprintf(NULL, 0, "X, %s", psStep->apsRef[0]->szName) + 1;
			szCommand = calloc(uCommandLength, sizeof(char));
			snprintf(szCommand, uCommandLength, "X, %s", psStep->apsRef[0]->szName);
		}
		break;
		case STEP_ASSUMPTION: {
			uCommandLength = 3;
			szCommand = calloc(uCommandLength, sizeof(char));
			snprintf(szCommand, uCommandLength, "AS");
		}
		break;
		case STEP_DISCHARGE: {
			uCommandLength = 4;
			szCommand = calloc(uCommandLength, sizeof(char));
			snprintf(szCommand, uCommandLength, "DIS");
		}
		break;
		case STEP_QED: {
			uCommandLength = 4;
			szCommand = calloc(uCommandLength, sizeof(char));
			snprintf(szCommand, uCommandLength, "QED");
		}
		break;
		default: {
			uCommandLength = 3;
			szCommand = calloc(uCommandLength, sizeof(char));
			snprintf(szCommand, uCommandLength, "??");
		}
		break;
	}

	szIndent = calloc((psStep->uIndent * 2) + 1, sizeof(char));
	for (uIndent = 0; uIndent < psStep->uIndent; ++uIndent) {
		strcpy(szIndent + uIndent * 2, "| ");
	}

	printf(COL_RESET "%4s" COL_RED " | %s" COL_CYAN COL_BOLD "%*s" COL_RESET COL_YELLOW " %s" COL_RESET, psStep->szName, szIndent, ((int)psStep->uIndent * 2) - 32, szResult, szCommand);
	free(szIndent);

	if (szCommand) {
		free(szCommand);
	}
	if (psStep->psResult) {
		free(szResult);
	}
}


