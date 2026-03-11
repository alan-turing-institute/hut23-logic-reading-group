// vim: noet:ts=2:sts=2:sw=2

// SPDX-License-Identifier: AGPL-3.0-or-later
// Copyright © 2026 David Llewellyn-Jones

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>

#include "symbolic.h"

#include "proof.h"

size_t split_command(char* szCommand, size_t* uPlace, size_t* uLength) {
	size_t uCount;
	size_t uPos;
	bool boConsume;

	// Split the command into pieces
	uCount = 0;
	boConsume = FALSE;
	uPos = 0;
	while (szCommand[uPos] != 0) {
		if ((!boConsume) && (szCommand[uPos] != ' ')) {
			if (uPlace) {
				uPlace[uCount] = uPos;
			}
			boConsume = TRUE;
		}
		else {
			if (boConsume && (szCommand[uPos] == ' ')) {
				if (uLength) {
					uLength[uCount] = uPos - uPlace[uCount];
				}
				uCount += 1;
				boConsume = FALSE;
			}
		}
		uPos += 1;
	}
	if (uPlace && (uCount == 0)) {
		uPlace[uCount] = 0;
	}
	if (uLength) {
		if (uPos == uPlace[uCount]) {
			uLength[uCount] = 0;
		}
		else {
			uLength[uCount] = uPos - uPlace[uCount] - 1;
		}
	}
	uCount += 1;

	return uCount;
}

char* allocate_error(char const* szFormat, ...) {
  va_list args;
	int nLength;
	char* szResult;

  va_start (args, szFormat);
	nLength = vsnprintf(NULL, 0, szFormat, args);
  va_end (args);

	szResult = malloc(nLength + 2);

  va_start (args, szFormat);
	vsnprintf(szResult, nLength + 1, szFormat, args);
  va_end (args);

	return szResult;
}

bool lemma(Proof *psProof, char const* szCommand, size_t* uPiece, size_t uCount, size_t uRefNum, char const** aszPattern, char const* szResult, Step* psStep, char** szError) {
	bool boSuccess = FALSE;
	size_t* auRef;
	size_t uReadCount;
	size_t uPos;
	Operation** apsPattern;
	Step** apsRef;
	Extract* psExtract;
	Operation* psResult;
	Operation** apsScrutinee;
	Operation** apsFind;
	Operation** apsSub;
	size_t uVarCount;

	if (uCount == (uRefNum + 1)) {
		auRef = calloc(uRefNum, sizeof(size_t));
		apsRef = calloc(uRefNum, sizeof(Operation*));
		uReadCount = 1;
		for (uPos = 0; (uPos < uRefNum) && (uReadCount == 1); ++uPos) {
			uReadCount = sscanf(szCommand + uPiece[(uPos + 1)], "%lu", &auRef[uPos]);
		}
		if ((uPos == uRefNum) && (uReadCount == 1)) {
			boSuccess = TRUE;
			for (uPos = 0; (uPos < uRefNum) && boSuccess; ++uPos) {
				boSuccess = proof_step_scoped(psProof, auRef[uPos] - 1);
			}
			if (boSuccess) {
				psResult = StringToOperation(szResult);
				apsPattern = calloc(uRefNum, sizeof(Operation*));
				apsScrutinee = calloc(uRefNum, sizeof(Operation*));

				for (uPos = 0; (uPos < uRefNum) && boSuccess; ++uPos) {
					apsPattern[uPos] = StringToOperation(aszPattern[uPos]);
					apsRef[uPos] = proof_get_step(psProof, auRef[uPos] - 1);
					apsScrutinee[uPos] = apsRef[uPos]->psResult;
				}
				psExtract = ExtractPatternMany(apsPattern, apsScrutinee, uRefNum);
				boSuccess = (psExtract != NULL);

				if (boSuccess) {
					uVarCount = ExtractCount(psExtract);

					apsFind = calloc(uVarCount, sizeof(Operation*));
					apsSub = calloc(uVarCount, sizeof(Operation*));

					for (uPos = 0; uPos < uVarCount; ++uPos) {
						apsFind[uPos] = CreateVariable(ExtractName(psExtract, uPos));
						apsSub[uPos] = ExtractValueFromPos(psExtract, uPos);
					}

					psStep->uRefCount = uRefNum;
					psStep->psRef = calloc(uRefNum, sizeof(Step*));

					for (uPos = 0; uPos < uRefNum; ++uPos) {
						psStep->psRef[uPos] = apsRef[uPos];
					}
					psStep->psResult = SubstituteOperationMany(psResult, apsFind, apsSub, uVarCount);

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
					*szError = "The referenced expressions must match the rule structure.";
				}

				for (uPos = 0; uPos < uRefNum; ++uPos) {
					FreeRecursive(apsPattern[uPos]);
				}
				free(apsPattern);
				apsPattern = NULL;
				free(apsScrutinee);
				apsScrutinee = NULL;
			}
			else {
				*szError = "At least one of the back references is out of scope.";
			}
		}
		free(auRef);
		auRef = NULL;
		free(apsRef);
		apsRef = NULL;
	}
	else {
		if (uRefNum == 1) {
			*szError = "The command takes exactly one back reference as a parameter.";
		}
		else {
			*szError = "Incorrect number of back references passed to the command as parameters.";
		}
	}

	return boSuccess;
}
