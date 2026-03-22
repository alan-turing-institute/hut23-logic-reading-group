// vim: noet:ts=2:sts=2:sw=2

// SPDX-License-Identifier: AGPL-3.0-or-later
// Copyright © 2026 David Llewellyn-Jones

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "proof.h"
#include "utils.h"
#include "lemma.h"
#include "ruleset.h"
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
					psStep->apsInput[uPos] = NULL;
				}
			}
			free(psStep->apsInput);
			psStep->apsInput = NULL;
		}
		if (psStep->psResult) {
			FreeRecursive(psStep->psResult);
			psStep->psResult = NULL;
		}
		if (psStep->apsRef) {
			free(psStep->apsRef);
			psStep->apsRef = NULL;
		}

		free(psStep);
	}
}

void step_print(Step* psStep, Ruleset* psRuleset) {
	int nLength;
	char* szResult;
	size_t uIndent;
	char* szIndent;
	char* szCommand;
	size_t uCommandLength;
	Lemma* psLemma;
	size_t uPos;
	size_t uWritten;

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

	if (psRuleset) {
		psLemma = ruleset_get_lemma(psRuleset, psStep->eCommand);

		if (psLemma) {
			uCommandLength = snprintf(NULL, 0, "%s", psLemma->szAnnotation) + 1;
			for (uPos = 0; uPos < psLemma->uRefNum; ++uPos) {
				uCommandLength += snprintf(NULL, 0, ", %s", psStep->apsRef[uPos]->szName);
			}

			szCommand = calloc(uCommandLength, sizeof(char));
			uWritten = snprintf(szCommand, uCommandLength, "%s", psLemma->szAnnotation);

			for (uPos = 0; uPos < psLemma->uRefNum; ++uPos) {
				uWritten += snprintf(szCommand + uWritten, uCommandLength - uWritten, ", %s", psStep->apsRef[uPos]->szName);
			}
		}
	}

	if (szCommand == NULL) {
		switch (psStep->eCommand) {
			case STEP_PREMISE: {
				uCommandLength = 3;
				szCommand = calloc(uCommandLength, sizeof(char));
				snprintf(szCommand, uCommandLength, "PR");
			}
			break;
			case STEP_IMPLICATION_INTRO: {
				uCommandLength = snprintf(NULL, 0, "->I, %s, %s", psStep->apsRef[0]->szName, psStep->apsRef[1]->szName) + 1;
				szCommand = calloc(uCommandLength, sizeof(char));
				snprintf(szCommand, uCommandLength, "->I, %s, %s", psStep->apsRef[0]->szName, psStep->apsRef[1]->szName);
			}
			break;
			case STEP_DISJUNCTION_ELIM: {
				uCommandLength = snprintf(NULL, 0, "vE, %s, %s-%s, %s-%s", psStep->apsRef[0]->szName, psStep->apsRef[1]->szName, psStep->apsRef[2]->szName, psStep->apsRef[3]->szName, psStep->apsRef[4]->szName) + 1;
				szCommand = calloc(uCommandLength, sizeof(char));
				snprintf(szCommand, uCommandLength, "vE, %s, %s-%s, %s-%s", psStep->apsRef[0]->szName, psStep->apsRef[1]->szName, psStep->apsRef[2]->szName, psStep->apsRef[3]->szName, psStep->apsRef[4]->szName);
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
			case STEP_REITERATION:
			case STEP_CONJUNCTION_INTRO:
			case STEP_CONJUNCTION_ELIM_LEFT:
			case STEP_CONJUNCTION_ELIM_RIGHT:
			case STEP_IMPLICATION_ELIM:
			case STEP_DISJUNCTION_INTRO_LEFT:
			case STEP_DISJUNCTION_INTRO_RIGHT:
			case STEP_EXPLOSION:
			case STEP_NEGATION_ELIM:
			default: {
				uCommandLength = 3;
				szCommand = calloc(uCommandLength, sizeof(char));
				snprintf(szCommand, uCommandLength, "??");
			}
			break;
		}
	}

	szIndent = calloc((psStep->uIndent * 2) + 1, sizeof(char));
	for (uIndent = 0; uIndent < psStep->uIndent; ++uIndent) {
		strcpy(szIndent + uIndent * 2, "| ");
	}

	printf(COL_RESET "%7s" COL_RED " | %s" COL_CYAN COL_BOLD "%*s" COL_RESET COL_YELLOW " %s" COL_RESET, psStep->szName, szIndent, ((int)psStep->uIndent * 2) - 32, szResult, szCommand);
	free(szIndent);

	if (szCommand) {
		free(szCommand);
	}
	if (psStep->psResult) {
		free(szResult);
	}
}

void step_command_output(Step* psStep, Ruleset* psRuleset, FILE* fhFile) {
	Lemma* psLemma;
	size_t uParameters;
	size_t uPos;
	size_t uLength;
	char* szOperation;
	char const* szCommand;

	if (psRuleset) {
		szCommand = NULL;
		psLemma = ruleset_get_lemma(psRuleset, psStep->eCommand);
		if (psLemma) {
			szCommand = psLemma->szCommand;
		}
		else {
			if ((psStep->eCommand > STEP_INVALID) && (psStep->eCommand < STEP_CONTROL)) {
				szCommand = aszCommand[psStep->eCommand];
			}
		}
		if (szCommand) {
			fprintf(fhFile, "%s", szCommand);

			uParameters = psStep->uRefCount + psStep->uInputCount;
			if (uParameters > 0) {
				fprintf(fhFile, " ");

				uPos = 0;
				while (uPos < uParameters) {
					if (uPos < psStep->uRefCount) {
						fprintf(fhFile, "%s", psStep->apsRef[uPos]->szName);
					}
					else {
						uLength = OperationToStringLength(psStep->apsInput[(uPos - psStep->uRefCount)]) + 1;
						szOperation = malloc(uLength);
						OperationToString(psStep->apsInput[(uPos - psStep->uRefCount)], szOperation, uLength);
						fprintf(fhFile, "%s", szOperation);
						free(szOperation);
					}
					uPos += 1;
					if (uPos < uParameters) {
						fprintf(fhFile, ", ");
					}
				}
			}
			fprintf(fhFile, "\n");
		}
	}
}

