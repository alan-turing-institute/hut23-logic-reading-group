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
	size_t uPos;

	if (psLemma) {
		if (psLemma->szCommand) {
			free(psLemma->szCommand);
			psLemma->szCommand = NULL;
		}
		if (psLemma->szAnnotation) {
			free(psLemma->szAnnotation);
			psLemma->szAnnotation = NULL;
		}
		if (psLemma->apsPattern) {
			for (uPos = 0; uPos < (psLemma->uRefNum + psLemma->uOpNum); ++uPos) {
				if (psLemma->apsPattern[uPos]) {
					FreeRecursive(psLemma->apsPattern[uPos]);
					psLemma->apsPattern[uPos] = NULL;
				}
			}
			psLemma->uRefNum = 0;
			psLemma->uOpNum = 0;
			free(psLemma->apsPattern);
			psLemma->apsPattern = NULL;
		}
		if (psLemma->psResult) {
			FreeRecursive(psLemma->psResult);
			psLemma->psResult = NULL;
		}
		free(psLemma);
	}
}

Lemma* lemma_compile(char const* szCommand, char const* szAnnotation, size_t uRefNum, size_t uOpNum, char const** aszPattern, char const* szResult) {
	Lemma* psLemma;
	size_t uPos;

	psLemma = lemma_new();

	psLemma->szCommand = strdup(szCommand);

	psLemma->szAnnotation = strdup(szAnnotation);

	psLemma->apsPattern = calloc(uRefNum + uOpNum, sizeof(Operation*));
	for (uPos = 0; uPos < (uRefNum + uOpNum); ++uPos) {
		psLemma->apsPattern[uPos] = StringToOperationCheck(aszPattern[uPos]);
	}
	psLemma->uRefNum = uRefNum;
	psLemma->uOpNum = uOpNum;

	psLemma->psResult = StringToOperationCheck(szResult);

	psLemma->boRequiresValidation = !ExtractPatternMappingUnique(psLemma->apsPattern, (uRefNum + uOpNum));

	return psLemma;
}

bool lemma_apply(Proof *psProof, Command* psCommand, size_t uRefNum, size_t uOpNum, char const** aszPattern, char const* szResult, Step* psStep, char const** pszError) {
	bool boSuccess = FALSE;
	Lemma* psLemma;

	psLemma = lemma_compile(psCommand->szCommand, "", uRefNum, uOpNum, aszPattern, szResult);
	boSuccess = lemma_apply_compiled(psLemma, psProof, psCommand, psStep, pszError);
	lemma_delete(psLemma);

	return boSuccess;
}

bool lemma_apply_compiled(Lemma* psLemma, Proof *psProof, Command* psCommand, Step* psStep, char const** pszError) {
	bool boSuccess = FALSE;
	size_t* auRef;
	size_t uPos;
	Step** apsRef;
	Extract* psExtract;
	Operation** apsScrutinee;
	Operation* psValidator = NULL;
	size_t uParameters;
	size_t uValidators;

	uParameters = psLemma->uRefNum + psLemma->uOpNum;
	uValidators = psLemma->boRequiresValidation ? 1 : 0;

	if (psCommand->uCount == (uParameters + uValidators)) {
		auRef = calloc(psLemma->uRefNum, sizeof(size_t));
		apsRef = calloc(psLemma->uRefNum, sizeof(Step*));
		boSuccess = proof_find_step_indices(psProof, psCommand->aszParameter, auRef, psLemma->uRefNum);

		if (boSuccess) {
			for (uPos = 0; (uPos < psLemma->uRefNum) && boSuccess; ++uPos) {
				boSuccess = proof_step_scoped(psProof, auRef[uPos]);
			}
			if (boSuccess) {
				boSuccess = proof_get_steps(psProof, auRef, apsRef, psLemma->uRefNum);
				if (boSuccess) {
					apsScrutinee = calloc(uParameters, sizeof(Operation*));

					for (uPos = 0; uPos < psLemma->uRefNum; ++uPos) {
						apsScrutinee[uPos] = apsRef[uPos]->psResult;
					}

					for (uPos = psLemma->uRefNum; uPos < uParameters; ++uPos) {
						apsScrutinee[uPos] = StringToOperationCheck(psCommand->aszParameter[uPos]);
					}

					if (psLemma->boRequiresValidation) {
						psValidator = StringToOperationCheck(psCommand->aszParameter[uParameters]);
					}

					psExtract = ExtractPatternMany(psLemma->apsPattern, apsScrutinee, uParameters);
					boSuccess = (psExtract != NULL);

					if (boSuccess) {
						psStep->uRefCount = psLemma->uRefNum;
						psStep->apsRef = calloc(psLemma->uRefNum, sizeof(Step*));

						for (uPos = 0; uPos < psLemma->uRefNum; ++uPos) {
							psStep->apsRef[uPos] = apsRef[uPos];
						}

						psStep->uInputCount = psLemma->uOpNum + uValidators;
						psStep->apsInput = calloc(psStep->uInputCount, sizeof(Step*));

						for (uPos = 0; uPos < psLemma->uOpNum; ++uPos) {
							psStep->apsInput[uPos] = apsScrutinee[(psLemma->uRefNum + uPos)];
						}

						if (psLemma->boRequiresValidation) {
							psStep->apsInput[psLemma->uOpNum] = psValidator;

							boSuccess = ExtractSubstituteCheckValidate(psExtract, psLemma->psResult, pszError);

							if (boSuccess) {
								boSuccess = ExtractSubstituteCheck(psExtract, psLemma->psResult, psValidator);

								if (boSuccess) {
									psStep->psResult = CopyRecursive(psValidator);
								}
								else {
									*pszError = "Propsed result does not match lemma requirements.";
								}
							}
						}
						else {
							boSuccess = ExtractSubstituteValidate(psExtract, psLemma->psResult, pszError);

							if (boSuccess) {
								psStep->psResult = ExtractSubstitute(psExtract, CopyRecursive(psLemma->psResult));
							}
						}

						FreeExtract(psExtract);
						psExtract = NULL;
					}
					else {
						*pszError = "The referenced expressions must match the rule structure.";
					}

					free(apsScrutinee);
					apsScrutinee = NULL;
				}
				else {
					*pszError = "Back references are missing.";
				}
			}
			else {
				*pszError = "At least one of the back references is out of scope.";
			}
		}
		else {
			*pszError = "Back references could not be found.";
		}
		free(auRef);
		auRef = NULL;
		free(apsRef);
		apsRef = NULL;
	}
	else {
		if (uParameters == 1) {
			*pszError = "The command takes exactly one parameter.";
		}
		else {
			*pszError = "Incorrect number of parameters.";
		}
	}

	return boSuccess;
}

Lemma* lemma_from_proof(Proof* psProof) {
	Lemma* psLemma;
	size_t uLength;
	size_t uPos;
	size_t uRefPos;
	size_t uRelationsCount;
	Operation const* psRelation;
	RelationList* psRefRelations;
	RelationList* psResultRelations;

	psLemma = lemma_new();

	uLength = strlen(psProof->szCommand);
	psLemma->szCommand = malloc(uLength + 1);
	strncpy(psLemma->szCommand, psProof->szCommand, uLength + 1);

	uLength = strlen(psProof->szAnnotation);
	psLemma->szAnnotation = malloc(uLength + 1);
	strncpy(psLemma->szAnnotation, psProof->szAnnotation, uLength + 1);

	psRefRelations = CreateRelationList();
	psLemma->uRefNum = 0;
	psLemma->uOpNum = 0;
	uPos = 0;
	while (uPos < psProof->uStepCount) {
		if (psProof->apsStep[uPos]->eCommand == STEP_PREMISE) {
			psLemma->uRefNum += 1;
			RelationListExtract(psRefRelations, psProof->apsStep[uPos]->psResult);
		}
		uPos += 1;
	}

	if ((psProof->uStepCount > 2) && (psProof->uStepCount > psLemma->uRefNum) && (psProof->apsStep[(psProof->uStepCount - 1)]->eCommand == STEP_QED)) {
		psLemma->psResult = CopyRecursive(psProof->apsStep[(psProof->uStepCount - 2)]->psResult);
	}

	psResultRelations = CreateRelationList();
	RelationListExtract(psResultRelations, psLemma->psResult);

	uRelationsCount = RelationListCount(psRefRelations);
	for (uPos = 0; uPos < uRelationsCount; ++uPos) {
		psRelation = RelationListGet(psRefRelations, uPos);
		RelationListRemove(psResultRelations, psRelation);
	}

	psLemma->uOpNum = RelationListCount(psResultRelations);
	psLemma->apsPattern = calloc((psLemma->uRefNum + psLemma->uOpNum), sizeof(Operation*));

	uPos = 0;
	uRefPos = 0;
	while (uPos < psProof->uStepCount) {
		if (psProof->apsStep[uPos]->eCommand == STEP_PREMISE) {
			psLemma->apsPattern[uRefPos] = CopyRecursive(psProof->apsStep[uPos]->psResult);
			uRefPos += 1;
		}
		uPos += 1;
	}

	for (uPos = 0; uPos < psLemma->uOpNum; ++uPos) {
		psRelation = RelationListGet(psResultRelations, uPos);
		psLemma->apsPattern[psLemma->uRefNum + uPos] = CopyRecursive(psRelation);
	}

	FreeRelationList(psResultRelations);
	FreeRelationList(psRefRelations);

	psLemma->boRequiresValidation = !ExtractPatternMappingUnique(psLemma->apsPattern, (psLemma->uRefNum + psLemma->uOpNum));

	return psLemma;
}

Operation * StringToOperationCheck(char const * szString) {
	Operation* psOp = NULL;
	if (szString) {
		if (szString[0] == '$') {
			psOp = StringToOperationLatex(szString);
		}
		else {
			psOp = StringToOperation(szString);
		}
	}

	return psOp;
}

