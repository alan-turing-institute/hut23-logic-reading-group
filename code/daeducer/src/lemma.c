// vim: noet:ts=2:sts=2:sw=2

// SPDX-License-Identifier: AGPL-3.0-or-later
// Copyright © 2026 David Llewellyn-Jones

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>

#include "symbolic.h"
#include "proof.h"
#include "step.h"
#include "command.h"

#include "lemma.h"

Lemma* lemma_new() {
	Lemma* psLemma;

	psLemma = calloc(1, sizeof(Lemma));

	return psLemma;
}

void lemma_delete(Lemma* psLemma) {
	if (psLemma) {
		if (psLemma->szCommand) {
			free(psLemma->szCommand);
			psLemma->szCommand = NULL;
		}
		if (psLemma->szAnnotation) {
			free(psLemma->szAnnotation);
			psLemma->szAnnotation = NULL;
		}

		free(psLemma);
	}
}

Lemma* lemma_compile(char const* szCommand, char const* szAnnotation, size_t uRefNum, char const** aszPattern, char const* szResult) {
	Lemma* psLemma;
	size_t uPos;

	psLemma = lemma_new();

	psLemma->szCommand = strdup(szCommand);

	psLemma->szAnnotation = strdup(szAnnotation);

	psLemma->apsPattern = calloc(uRefNum, sizeof(Operation*));

	for (uPos = 0; uPos < uRefNum; ++uPos) {
		psLemma->apsPattern[uPos] = StringToOperation(aszPattern[uPos]);
	}
	psLemma->uRefNum = uRefNum;
	psLemma->psResult = StringToOperation(szResult);

	return psLemma;
}

bool lemma_apply(Proof *psProof, Command* psCommand, size_t uRefNum, char const** aszPattern, char const* szResult, Step* psStep, char** pszError) {
	bool boSuccess = FALSE;
	Lemma* psLemma;

	psLemma = lemma_compile(psCommand->szCommand, "", uRefNum, aszPattern, szResult);
	boSuccess = lemma_apply_compiled(psLemma, psProof, psCommand, psStep, pszError);
	lemma_delete(psLemma);

	return boSuccess;
}

bool lemma_apply_compiled(Lemma* psLemma, Proof *psProof, Command* psCommand, Step* psStep, char** pszError) {
	bool boSuccess = FALSE;
	size_t* auRef;
	size_t uReadCount;
	size_t uPos;
	Step** apsRef;
	Extract* psExtract;
	Operation** apsScrutinee;
	Operation** apsFind;
	Operation** apsSub;
	size_t uVarCount;

	if (psCommand->uCount == psLemma->uRefNum) {
		auRef = calloc(psLemma->uRefNum, sizeof(size_t));
		apsRef = calloc(psLemma->uRefNum, sizeof(Operation*));
		uReadCount = 1;
		for (uPos = 0; (uPos < psLemma->uRefNum) && (uReadCount == 1); ++uPos) {
			uReadCount = sscanf(psCommand->aszParameter[uPos], "%lu", &auRef[uPos]);
		}
		if ((uPos == psLemma->uRefNum) && (uReadCount == 1)) {
			boSuccess = TRUE;
			for (uPos = 0; (uPos < psLemma->uRefNum) && boSuccess; ++uPos) {
				boSuccess = proof_step_scoped(psProof, auRef[uPos] - 1);
			}
			if (boSuccess) {
				apsScrutinee = calloc(psLemma->uRefNum, sizeof(Operation*));

				for (uPos = 0; (uPos < psLemma->uRefNum) && boSuccess; ++uPos) {
					apsRef[uPos] = proof_get_step(psProof, auRef[uPos] - 1);
					apsScrutinee[uPos] = apsRef[uPos]->psResult;
				}
				psExtract = ExtractPatternMany(psLemma->apsPattern, apsScrutinee, psLemma->uRefNum);
				boSuccess = (psExtract != NULL);

				if (boSuccess) {
					uVarCount = ExtractCount(psExtract);

					apsFind = calloc(uVarCount, sizeof(Operation*));
					apsSub = calloc(uVarCount, sizeof(Operation*));

					for (uPos = 0; uPos < uVarCount; ++uPos) {
						apsFind[uPos] = CreateVariable(ExtractName(psExtract, uPos));
						apsSub[uPos] = ExtractValueFromPos(psExtract, uPos);
					}

					psStep->uRefCount = psLemma->uRefNum;
					psStep->psRef = calloc(psLemma->uRefNum, sizeof(Step*));

					for (uPos = 0; uPos < psLemma->uRefNum; ++uPos) {
						psStep->psRef[uPos] = apsRef[uPos];
					}
					psStep->psResult = SubstituteOperationMany(psLemma->psResult, apsFind, apsSub, uVarCount);

					FreeExtract(psExtract);
					psExtract = NULL;

					for (uPos = 0; uPos < uVarCount; ++uPos) {
						FreeRecursive(apsFind[uPos]);
					}
					free(apsFind);
					apsFind = NULL;
					free(apsSub);
					apsSub = NULL;
				}
				else {
					*pszError = "The referenced expressions must match the rule structure.";
				}

				free(apsScrutinee);
				apsScrutinee = NULL;
			}
			else {
				*pszError = "At least one of the back references is out of scope.";
			}
		}
		free(auRef);
		auRef = NULL;
		free(apsRef);
		apsRef = NULL;
	}
	else {
		if (psLemma->uRefNum == 1) {
			*pszError = "The command takes exactly one back reference as a parameter.";
		}
		else {
			*pszError = "Incorrect number of back references passed to the command as parameters.";
		}
		printf("Was %lu, should have been %lu\n", psCommand->uCount, psLemma->uRefNum);
	}

	return boSuccess;
}

Lemma* lemma_from_proof(Proof* psProof) {
	Lemma* psLemma;
	size_t uLength;
	size_t uPos;

	psLemma = lemma_new();

	uLength = strlen(psProof->szCommand);
	psLemma->szCommand = malloc(uLength + 1);
	strncpy(psLemma->szCommand, psProof->szCommand, uLength + 1);

	uLength = strlen(psProof->szAnnotation);
	psLemma->szAnnotation = malloc(uLength + 1);
	strncpy(psLemma->szAnnotation, psProof->szAnnotation, uLength + 1);

	uPos = 0;
	while ((uPos < psProof->uStepCount) && (psProof->apsStep[uPos]->eCommand == STEP_PREMISE)) {
		uPos += 1;
	}

	psLemma->uRefNum = uPos;
	psLemma->apsPattern = calloc(uPos, sizeof(Operation*));

	for (uPos = 0; uPos < psLemma->uRefNum; ++uPos) {
		psLemma->apsPattern[uPos] = CopyRecursive(psProof->apsStep[uPos]->psResult);
	}

	if ((psProof->uStepCount > 2) && (psProof->uStepCount > psLemma->uRefNum) && (psProof->apsStep[(psProof->uStepCount - 1)]->eCommand == STEP_QED)) {
		psLemma->psResult = CopyRecursive(psProof->apsStep[(psProof->uStepCount - 2)]->psResult);
	}

	return psLemma;
}

