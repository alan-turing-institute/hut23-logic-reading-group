// vim: noet:ts=2:sts=2:sw=2

// SPDX-License-Identifier: AGPL-3.0-or-later
// Copyright © 2026 David Llewellyn-Jones

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "utils.h"
#include "step.h"
#include "symbolic.h"

#include "proof.h"

Proof* proof_new() {
	Proof* psProof;

	psProof = calloc(1, sizeof(Proof));

	return psProof;
}

void proof_delete(Proof* psProof) {
	free(psProof);
}

Step* proof_get_step(Proof* psProof, size_t uPos) {
	Step* psResult = NULL;

	if (uPos < psProof->uStepCount) {
		psResult = psProof->apsStep[uPos];
	}

	return psResult;
}

bool proof_step_scoped(Proof* psProof, size_t uStep) {
	bool boScoped = FALSE;
	size_t uPos;

	if (uStep < psProof->uStepCount) {
		boScoped = TRUE;
		for (uPos = uStep; (uPos < psProof->uStepCount) && (boScoped == TRUE); ++uPos) {
			if (psProof->apsStep[uPos]->uIndent < psProof->apsStep[uStep]->uIndent) {
				boScoped = FALSE;
			}
		}
	}

	return boScoped;
}

bool proof_steps_same_scope(Proof* psProof, size_t uStep1, size_t uStep2) {
	bool boScoped = FALSE;
	size_t uPos;

	if ((uStep1 < psProof->uStepCount) && (uStep2 < psProof->uStepCount) && (uStep1 < uStep2)) {
		if (psProof->apsStep[uStep1]->uIndent == psProof->apsStep[uStep2]->uIndent) {
			boScoped = TRUE;
			for (uPos = uStep1; (uPos <= uStep2) && (boScoped == TRUE); ++uPos) {
				if (psProof->apsStep[uPos]->uIndent < psProof->apsStep[uStep1]->uIndent) {
					boScoped = FALSE;
				}
			}
		}
	}

	return boScoped;
}

bool proof_scoped_subproof(Proof* psProof, size_t uStep1, size_t uStep2) {
	bool boScoped = FALSE;
	Step* psStep1;
	Step* psStep2;

	if ((uStep1 < psProof->uStepCount) && ((uStep2 + 1) < psProof->uStepCount) && (uStep1 < uStep2)) {
		if (proof_step_scoped(psProof, uStep2 + 1)) {
			psStep1 = proof_get_step(psProof, uStep1);
			psStep2 = proof_get_step(psProof, uStep2 + 1);
			if ((psStep1->eCommand == STEP_ASSUMPTION) && (psStep2->eCommand == STEP_DISCHARGE)) {
				if (proof_steps_same_scope(psProof, uStep1, uStep2)) {
					boScoped = TRUE;
				}
			}
		}
	}

	return boScoped;
}

void proof_print(Proof* psProof) {
	size_t uPos;
	for (uPos = 0; uPos < psProof->uStepCount; ++uPos) {
		printf("\n");
		proof_print_step(psProof, uPos);
	}
	printf("\n");
}

void proof_print_help() {
	size_t uPos;
	printf("\n");
	printf("The following commands are available in Daeducer.\n");
	printf("  Proof construction commands:\n");
	for (uPos = 0; uPos < STEP_NUM; ++uPos) {
		if (uPos == STEP_CONTROL) {
			printf("  Programme control commands:\n");
		}
		printf("    %s\n", aszHelp[uPos]);
	}
	printf("Enter help to show this output. Enter <ctrl>-d to exit.\n");
	printf("\n");
}

bool proof_process_step(Proof* psProof, char* szCommand) {
	size_t uCount;
	size_t uPos;
	Step* psStep;
	Operation* psPattern;
	Extract* psExtract;
	bool boError;
	char * szError = "Unknown error.";
	bool boContinue;
	bool boStep;

	boContinue = TRUE;
	boStep = TRUE;

	// Find out how many pieces there are
	uCount = split_command(szCommand, NULL, NULL);

	// Split the command into pieces
	size_t* uPiece = calloc(uCount, sizeof(size_t));
	size_t* uLength = calloc(uCount, sizeof(size_t));
	uCount = split_command(szCommand, uPiece, uLength);

	uPos = 0;
	STEP eCommand = STEP_INVALID;
	while ((eCommand == STEP_INVALID) && (uPos < STEP_NUM)) {
		if (strncmp(aszCommand[uPos], szCommand + uPiece[0], uLength[0]) == 0) {
			eCommand = (STEP)uPos;
		}
		uPos += 1;
	}

	psStep = step_new();
	psStep->eCommand = eCommand;
	if (psProof->uStepCount > 0) {
		psStep->uIndent = psProof->apsStep[(psProof->uStepCount - 1)]->uIndent;
	}
	else {
		psStep->uIndent = 0;
	}
	size_t uNameSize = snprintf(NULL, 0, "%lu", psProof->uStepCount + 1) + 1;
	psStep->szName = calloc(uNameSize, sizeof(char));
	snprintf(psStep->szName, uNameSize, "%lu", psProof->uStepCount + 1);

	boError = TRUE;
	switch (eCommand) {
		case STEP_PREMISE: {
			if (uCount >= 2) {
				if ((psProof->uStepCount == 0) || (psProof->apsStep[(psProof->uStepCount - 1)]->eCommand == STEP_PREMISE)) {
					psStep->psResult = StringToOperation(szCommand + uPiece[1]);
					boError = FALSE;
				}
				else {
					szError = "Premises can only be added at the start; create an assumption instead.";
				}
			}
			else {
				szError = "The premise command takes one logical expression as a parameter.";
			}
		}
		break;
		case STEP_REITERATION: {
			boError = !lemma(psProof, szCommand, uPiece, uCount, 1, (char const*[]) {"A"}, "A", psStep, &szError);
		}
		break;
		case STEP_CONJUNCTION_INTRO: {
			boError = !lemma(psProof, szCommand, uPiece, uCount, 2, (char const*[]) {"A", "B"}, "(A ^ B)", psStep, &szError);
		}
		break;
		case STEP_CONJUNCTION_ELIM_LEFT: {
			boError = !lemma(psProof, szCommand, uPiece, uCount, 1, (char const*[]) {"(A ^ B)"}, "A", psStep, &szError);
		}
		break;
		case STEP_CONJUNCTION_ELIM_RIGHT: {
			boError = !lemma(psProof, szCommand, uPiece, uCount, 1, (char const*[]) {"(A ^ B)"}, "B", psStep, &szError);
		}
		break;
		case STEP_IMPLICATION_ELIM: {
			boError = !lemma(psProof, szCommand, uPiece, uCount, 2, (char const*[]) {"(A -> B)", "A"}, "B", psStep, &szError);
		}
		break;
		case STEP_IMPLICATION_INTRO: {
			if (uCount == 3) {
				size_t auRef[2];
				size_t uReadCount;
				uReadCount = sscanf(szCommand + uPiece[1], "%lu %lu", &auRef[0], &auRef[1]);
				if (uReadCount == 2) {
					Step* apsRef[2];
					apsRef[0] = proof_get_step(psProof, auRef[0] - 1);
					apsRef[1] = proof_get_step(psProof, auRef[1] - 1);
					if (apsRef[0] && apsRef[1]) {
						if (proof_scoped_subproof(psProof, auRef[0] - 1, auRef[1] - 1)) {
							psStep->uRefCount = 2;
							psStep->psRef = calloc(psStep->uRefCount, sizeof(Step*));
							psStep->psRef[0] = apsRef[0];
							psStep->psRef[1] = apsRef[1];
							psStep->psResult = CreateBinary(OPBINARY_LIMP, CopyRecursive(psStep->psRef[0]->psResult), CopyRecursive(psStep->psRef[1]->psResult));
							boError = FALSE;
						}
						else {
							szError = "The subproof is out of scope.";
						}
					}
				}
				else {
					szError = "Back reference is out of scope.";
				}
			}
			else {
				szError = "The imp_intro command takes two back references as parameters.";
			}
		}
		break;
		case STEP_DISJUNCTION_INTRO_LEFT: {
			if (uCount >= 3) {
				size_t uRef;
				size_t uReadCount;
				uReadCount = sscanf(szCommand + uPiece[1], "%lu", &uRef);
				if (uReadCount == 1) {
					psStep->psInput = StringToOperation(szCommand + uPiece[2]);
					if (proof_step_scoped(psProof, uRef - 1)) {
						Step* psRef;
						psRef = proof_get_step(psProof, uRef - 1);
						if (psRef) {
							psStep->uRefCount = 1;
							psStep->psRef = calloc(psStep->uRefCount, sizeof(Step*));
							psStep->psRef[0] = psRef;
							psStep->psResult = CreateBinary(OPBINARY_LOR, CopyRecursive(psStep->psRef[0]->psResult), CopyRecursive(psStep->psInput));
							boError = FALSE;
						}
					}
					else {
						szError = "Back reference is out of scope.";
					}
				}
				else {
					szError = "The first parameter of or_intro_left must be a back reference.";
				}
			}
			else {
				szError = "The or_intro_left command takes one back reference and an expression as parameters.";
			}
		}
		break;
		case STEP_DISJUNCTION_INTRO_RIGHT: {
			if (uCount >= 3) {
				size_t uRef;
				size_t uReadCount;
				uReadCount = sscanf(szCommand + uPiece[1], "%lu", &uRef);
				if (uReadCount == 1) {
					psStep->psInput = StringToOperation(szCommand + uPiece[2]);
					if (proof_step_scoped(psProof, uRef - 1)) {
						Step* psRef;
						psRef = proof_get_step(psProof, uRef - 1);
						if (psRef) {
							psStep->uRefCount = 1;
							psStep->psRef = calloc(psStep->uRefCount, sizeof(Step*));
							psStep->psRef[0] = psRef;
							psStep->psResult = CreateBinary(OPBINARY_LOR, CopyRecursive(psStep->psInput), CopyRecursive(psStep->psRef[0]->psResult));
							boError = FALSE;
						}
					}
					else {
						szError = "Back reference is out of scope.";
					}
				}
				else {
					szError = "The first parameter of or_intro_right must be a back reference.";
				}
			}
			else {
				szError = "The or_intro_right command takes one back reference and an expression as parameters.";
			}
		}
		break;
		case STEP_DISJUNCTION_ELIM: {
			if (uCount == 6) {
				size_t auRef[5];
				size_t uReadCount;
				uReadCount = sscanf(szCommand + uPiece[1], "%lu %lu %lu %lu %lu", &auRef[0], &auRef[1], &auRef[2], &auRef[3], &auRef[4]);
				if (uReadCount == 5) {
					if (proof_step_scoped(psProof, auRef[0] - 1)) {
						if (proof_scoped_subproof(psProof, auRef[1] - 1, auRef[2] - 1)) {
							if (proof_scoped_subproof(psProof, auRef[3] - 1, auRef[4] - 1)) {
								Step* apsRef[5];
								apsRef[0] = proof_get_step(psProof, auRef[0] - 1);
								apsRef[1] = proof_get_step(psProof, auRef[1] - 1);
								apsRef[2] = proof_get_step(psProof, auRef[2] - 1);
								apsRef[3] = proof_get_step(psProof, auRef[3] - 1);
								apsRef[4] = proof_get_step(psProof, auRef[4] - 1);

								psPattern = CreateBinary(OPBINARY_LOR, CreateVariable("A"), CreateVariable("B"));
								psExtract = ExtractPattern(psPattern, apsRef[0]->psResult);
								if (psExtract) {
									if (CompareOperations(ExtractValue(psExtract, "A"), apsRef[1]->psResult)) {
										if (CompareOperations(ExtractValue(psExtract, "B"), apsRef[3]->psResult)) {
											if (CompareOperations(apsRef[2]->psResult, apsRef[4]->psResult)) {

												psStep->uRefCount = 5;
												psStep->psRef = calloc(psStep->uRefCount, sizeof(Step*));
												psStep->psRef[0] = apsRef[0];
												psStep->psRef[1] = apsRef[1];
												psStep->psRef[2] = apsRef[2];
												psStep->psRef[3] = apsRef[3];
												psStep->psRef[4] = apsRef[4];

												psStep->psResult = CopyRecursive(apsRef[2]->psResult);
												boError = FALSE;
											}
											else {
												szError = "Both subproofs must conclude the same result.";
											}
										}
										else {
											szError = "The right hand side of the disjunction in the first reference must match the assumption of the second subproof.";
										}
									}
									else {
										szError = "The left hand side of the disjunction in the first reference must match the assumption of the first subproof.";
									}
									FreeExtract(psExtract);
									psExtract = NULL;
								}
								else {
									szError = "First backreference must be in the form (A v B).";
								}
								FreeRecursive(psPattern);
								psPattern = NULL;
							}
							else {
								szError = "The second subproof is out of scope";
							}
						}
						else {
							szError = "The first subproof is out of scope";
						}
					}
					else {
						szError = "The first back reference is out of scope.";
					}
				}
				else {
					szError = "The or_elim command takes five back references as parameters.";
				}
			}
			else {
				szError = "The or_elim command takes five back references as parameters.";
			}
		}
		break;
		case STEP_NEGATION_ELIM: {
			boError = !lemma(psProof, szCommand, uPiece, uCount, 2, (char const*[]) {"!A", "A"}, "FALSE", psStep, &szError);
		}
		break;
		case STEP_NEGATION_INTRO: {
			if (uCount == 3) {
				size_t auRef[2];
				size_t uReadCount;
				uReadCount = sscanf(szCommand + uPiece[1], "%lu %lu", &auRef[0], &auRef[1]);
				if (uReadCount == 2) {
					Step* apsRef[2];
					apsRef[0] = proof_get_step(psProof, auRef[0] - 1);
					apsRef[1] = proof_get_step(psProof, auRef[1] - 1);
					if (apsRef[0] && apsRef[1]) {
						if (proof_scoped_subproof(psProof, auRef[0] - 1, auRef[1] - 1)) {
							Operation* psOp = CreateTruthValue(FALSE);
							if (CompareOperations(apsRef[1]->psResult, psOp)) {
								psStep->uRefCount = 2;
								psStep->psRef = calloc(psStep->uRefCount, sizeof(Step*));
								psStep->psRef[0] = apsRef[0];
								psStep->psRef[1] = apsRef[1];
								psStep->psResult = CreateUnary(OPUNARY_NOT, CopyRecursive(apsRef[0]->psResult));
								boError = FALSE;
							}
							else {
								szError = "The not_intro command requires a subproof that ends in a contradiction.";
							}
							FreeRecursive(psOp);
							psOp = NULL;
						}
						else {
							szError = "The subproof is out of scope.";
						}
					}
				}
				else {
					szError = "Back reference is out of scope.";
				}
			}
			else {
				szError = "The not_intro command takes two back references as parameters.";
			}
		}
		break;
		case STEP_INDIRECT_PROOF: {

			if (uCount == 3) {
				size_t auRef[2];
				size_t uReadCount;
				uReadCount = sscanf(szCommand + uPiece[1], "%lu %lu", &auRef[0], &auRef[1]);
				if (uReadCount == 2) {
					Step* apsRef[2];
					apsRef[0] = proof_get_step(psProof, auRef[0] - 1);
					apsRef[1] = proof_get_step(psProof, auRef[1] - 1);
					if (apsRef[0] && apsRef[1]) {
						if (proof_scoped_subproof(psProof, auRef[0] - 1, auRef[1] - 1)) {
							Operation* psOp = CreateTruthValue(FALSE);
							if (CompareOperations(apsRef[1]->psResult, psOp)) {
								psPattern = CreateUnary(OPUNARY_NOT, CreateVariable("A"));
								psExtract = ExtractPattern(psPattern, apsRef[0]->psResult);
								if (psExtract) {
									psStep->uRefCount = 2;
									psStep->psRef = calloc(psStep->uRefCount, sizeof(Step*));
									psStep->psRef[0] = apsRef[0];
									psStep->psRef[1] = apsRef[1];
									psStep->psResult = CopyRecursive(ExtractValue(psExtract, "A"));
									boError = FALSE;
									FreeExtract(psExtract);
									psExtract = NULL;
								}
								else {
									szError = "First backreference must be in the form !A.";
								}
								FreeRecursive(psPattern);
								psPattern = NULL;
							}
							else {
								szError = "The indirect command requires a subproof that ends in a contradiction.";
							}
							FreeRecursive(psOp);
							psOp = NULL;
						}
						else {
							szError = "The subproof is out of scope.";
						}
					}
				}
				else {
					szError = "Back reference is out of scope.";
				}
			}
			else {
				szError = "The indirect command takes two back references as parameters.";
			}
		}
		break;
		case STEP_EXPLOSION: {
			if (uCount >= 3) {
				size_t uRef;
				size_t uReadCount;
				uReadCount = sscanf(szCommand + uPiece[1], "%lu", &uRef);
				if (uReadCount == 1) {
					psStep->psInput = StringToOperation(szCommand + uPiece[2]);
					if (proof_step_scoped(psProof, uRef - 1)) {
						Step* psRef;
						psRef = proof_get_step(psProof, uRef - 1);
						if (psRef) {
							Operation* psOp = CreateTruthValue(FALSE);
							if (CompareOperations(psRef->psResult, psOp)) {
								psStep->uRefCount = 1;
								psStep->psRef = calloc(psStep->uRefCount, sizeof(Step*));
								psStep->psRef[0] = psRef;
								psStep->psResult = CopyRecursive(psStep->psInput);
								boError = FALSE;
							}
							else {
								szError = "The explosion command requires a subproof that ends in a contradiction.";
							}
							FreeRecursive(psOp);
							psOp = NULL;
						}
					}
					else {
						szError = "Back reference is out of scope.";
					}
				}
				else {
					szError = "The first parameter of explosion must be a back reference.";
				}
			}
			else {
				szError = "The explosion command takes one back reference and an expression as parameters.";
			}
		}
		break;
		case STEP_ASSUMPTION: {
			if (uCount >= 2) {
				psStep->psResult = StringToOperation(szCommand + uPiece[1]);
				psStep->uIndent += 1;
				boError = FALSE;
			}
			else {
				szError = "The assumption command takes one logical expression as a parameter.";
			}
		}
		break;
		case STEP_DISCHARGE: {
			if (uCount == 1) {
				if (psStep->uIndent > 0) {
					psStep->uIndent -= 1;
					boError = FALSE;
				}
				else {
					szError = "No assumption to discharge.";
				}
			}
			else {
				szError = "The discharge command takes no parameters.";
			}
		}
		break;
		case STEP_QED: {
			if (uCount == 1) {
				if (psStep->uIndent == 0) {
					boError = FALSE;
					boContinue = FALSE;
					proof_print(psProof);
				}
				else {
					szError = "You must discharge your subproofs before you can complete your main proof.";
				}
			}
			else {
				szError = "The qed command takes no parameters.";
			}
		}
		break;
		case STEP_PRINT: {
			if (uCount == 1) {
				boError = FALSE;
				boStep = FALSE;
				proof_print(psProof);
				printf("\n");
			}
			else {
				szError = "The print command takes no parameters.";
			}
		}
		break;
		case STEP_HELP: {
			if (uCount == 1) {
				boError = FALSE;
				boStep = FALSE;
				proof_print_help();
			}
			else {
				szError = "The help command takes no parameters.";
			}
		}
		break;
		default: {
			szError = "Command not recognised.";
		}
		break;
	};

	if ((!boError) && boStep) {
		size_t uPos = psProof->uStepCount;
		psProof->uStepCount += 1;
		psProof->apsStep = realloc(psProof->apsStep, psProof->uStepCount * sizeof(Step));
		psProof->apsStep[uPos] = psStep;
		proof_print_step(psProof, uPos);
		printf("\n");
	}
	else {
		if (boError) {
			printf("Error: %s\n", szError);
		}
		step_delete(psStep);
	}

	return boContinue;
}

void proof_print_step(Proof* psProof, size_t uStep) {
	if (uStep < psProof->uStepCount) {
		step_print(psProof->apsStep[uStep]);
	}
}

size_t proof_indent(Proof* psProof) {
	size_t uIndent;

	if (psProof->uStepCount > 0) {
		uIndent = psProof->apsStep[psProof->uStepCount - 1]->uIndent;
	}
	else {
		uIndent = 0;
	}

	return uIndent;
}

