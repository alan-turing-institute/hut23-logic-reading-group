// vim: noet:ts=2:sts=2:sw=2

// SPDX-License-Identifier: AGPL-3.0-or-later
// Copyright © 2026 David Llewellyn-Jones

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "utils.h"
#include "step.h"
#include "ruleset.h"
#include "symbolic.h"
#include "lemma.h"
#include "command.h"

#include "proof.h"

Proof* proof_new() {
	Proof* psProof;

	psProof = calloc(1, sizeof(Proof));

	return psProof;
}

void proof_delete(Proof* psProof) {
	if (psProof) {
		if (psProof->szCommand) {
			free(psProof->szCommand);
			psProof->szCommand = NULL;
		}
		if (psProof->szAnnotation) {
			free(psProof->szAnnotation);
			psProof->szAnnotation = NULL;
		}
		if (psProof->szError) {
			//free(psProof->szError);
			psProof->szError = NULL;
		}
		free(psProof);
	}
}

void proof_attach_ruleset(Proof* psProof, Ruleset* psRuleset) {
	psProof->psRuleset = psRuleset;
}

Ruleset* proof_detach_ruleset(Proof* psProof) {
	return psProof->psRuleset;
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

void proof_process_step(Proof* psProof, Command* psCommand) {
	size_t uPos;
	Step* psStep;
	Operation* psPattern;
	Extract* psExtract;
	bool boError;
	char * szError = "Unknown error.";
	bool boContinue;
	bool boStep;
	bool boFound;
	size_t uIndex;
	Lemma* psLemma;

	boContinue = TRUE;
	boStep = TRUE;

	uPos = 0;
	while ((psCommand->eCommand == STEP_INVALID) && (uPos < STEP_NUM)) {
		if (strcmp(aszCommand[uPos], psCommand->szCommand) == 0) {
			psCommand->eCommand = (STEP)uPos;
		}
		uPos += 1;
	}

	psStep = step_new();
	psStep->eCommand = psCommand->eCommand;
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
	switch (psCommand->eCommand) {
		case STEP_PREMISE: {
			if (psCommand->uCount == 1) {
				if ((psProof->uStepCount == 0) || (psProof->apsStep[(psProof->uStepCount - 1)]->eCommand == STEP_PREMISE)) {
					psStep->psResult = StringToOperation(psCommand->aszParameter[0]);
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
		case STEP_REITERATION:
			// Intentional fallthrough
		case STEP_CONJUNCTION_INTRO:
			// Intentional fallthrough
		case STEP_CONJUNCTION_ELIM_LEFT:
			// Intentional fallthrough
		case STEP_CONJUNCTION_ELIM_RIGHT:
			// Intentional fallthrough
		case STEP_IMPLICATION_ELIM:
			// Intentional fallthrough
		case STEP_NEGATION_ELIM: {
			psLemma = ruleset_get_lemma(psProof->psRuleset, psCommand->eCommand);
			boError = !lemma_apply_compiled(psLemma, psProof, psCommand, psStep, &szError);
		}
		break;
		case STEP_IMPLICATION_INTRO: {
			if (psCommand->uCount == 2) {
				size_t auRef[2];
				size_t uReadCount;
				uReadCount = sscanf(psCommand->aszParameter[0], "%lu", &auRef[0]);
				uReadCount += sscanf(psCommand->aszParameter[1], "%lu", &auRef[1]);
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
			if (psCommand->uCount == 2) {
				size_t uRef;
				size_t uReadCount;
				uReadCount = sscanf(psCommand->aszParameter[0], "%lu", &uRef);
				if (uReadCount == 1) {
					psStep->psInput = StringToOperation(psCommand->aszParameter[1]);
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
			if (psCommand->uCount == 2) {
				size_t uRef;
				size_t uReadCount;
				uReadCount = sscanf(psCommand->aszParameter[0], "%lu", &uRef);
				if (uReadCount == 1) {
					psStep->psInput = StringToOperation(psCommand->aszParameter[1]);
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
			if (psCommand->uCount == 5) {
				size_t auRef[5];
				size_t uReadCount;
				uReadCount = sscanf(psCommand->aszParameter[0], "%lu", &auRef[0]);
				uReadCount += sscanf(psCommand->aszParameter[1], "%lu", &auRef[1]);
				uReadCount += sscanf(psCommand->aszParameter[2], "%lu", &auRef[2]);
				uReadCount += sscanf(psCommand->aszParameter[3], "%lu", &auRef[3]);
				uReadCount += sscanf(psCommand->aszParameter[4], "%lu", &auRef[4]);
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
		case STEP_NEGATION_INTRO: {
			if (psCommand->uCount == 2) {
				size_t auRef[2];
				size_t uReadCount;
				uReadCount = sscanf(psCommand->aszParameter[0], "%lu", &auRef[0]);
				uReadCount += sscanf(psCommand->aszParameter[1], "%lu", &auRef[1]);
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
			if (psCommand->uCount == 2) {
				size_t auRef[2];
				size_t uReadCount;
				uReadCount = sscanf(psCommand->aszParameter[0], "%lu", &auRef[0]);
				uReadCount += sscanf(psCommand->aszParameter[1], "%lu", &auRef[1]);
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
			if (psCommand->uCount == 2) {
				size_t uRef;
				size_t uReadCount;
				uReadCount = sscanf(psCommand->aszParameter[0], "%lu", &uRef);
				if (uReadCount == 1) {
					psStep->psInput = StringToOperation(psCommand->aszParameter[1]);
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
			if (psCommand->uCount == 1) {
				psStep->psResult = StringToOperation(psCommand->aszParameter[0]);
				psStep->uIndent += 1;
				boError = FALSE;
			}
			else {
				szError = "The assumption command takes one logical expression as a parameter.";
			}
		}
		break;
		case STEP_DISCHARGE: {
			if (psCommand->uCount == 0) {
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
			if (psCommand->uCount == 0) {
				if (psStep->uIndent == 0) {
					boError = FALSE;
					boContinue = FALSE;
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
			if (psCommand->uCount == 0) {
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
			if (psCommand->uCount == 0) {
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
			boFound = ruleset_get_command_index_start(psProof->psRuleset, psCommand->szCommand, STEP_CONTROL, &uIndex);
			if (boFound) {
				psLemma = ruleset_get_lemma(psProof->psRuleset, uIndex);
				boError = !lemma_apply_compiled(psLemma, psProof, psCommand, psStep, &szError);
			}
			if (!boFound) {
				szError = "Command not recognised.";
			}
		}
		break;
	};

	if (!boContinue) {
		psProof->boComplete = TRUE;
	}

	if ((!boError) && boStep) {
		size_t uPos = psProof->uStepCount;
		psProof->uStepCount += 1;
		psProof->apsStep = realloc(psProof->apsStep, psProof->uStepCount * sizeof(Step));
		psProof->apsStep[uPos] = psStep;
		//proof_print_step(psProof, uPos);
	}
	else {
		if (boError) {
			//printf("Error: %s\n", szError);
			psProof->boError = TRUE;
			psProof->szError = szError;
		}
		step_delete(psStep);
	}
}

void proof_print_last_step(Proof* psProof) {
	if (psProof->uStepCount > 0) {
		step_print(psProof->apsStep[(psProof->uStepCount - 1)]);
	}
}

void proof_print_step(Proof* psProof, size_t uStep) {
	if (uStep < psProof->uStepCount) {
		step_print(psProof->apsStep[uStep]);
	}
}

bool proof_complete(Proof* psProof) {
	return psProof->boComplete;
}

bool proof_error(Proof* psProof, char** pszError) {
	if (psProof->boError) {
		if (pszError) {
			*pszError = psProof->szError;
		}
	}
	return psProof->boError;
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

Proof* proof_load(Ruleset* psRuleset, char const* szFilename) {
	Proof* psProof;
	FILE* fhFile;
	char* szLine;
	size_t uLength;
	ssize_t nRead;
	bool boSuccess;
	size_t uLine;
	bool boComplete;
	char* szError;
	Command* psCommand;

	psProof = proof_new();
	proof_attach_ruleset(psProof, psRuleset);
	fhFile = fopen(szFilename, "r");
	psCommand = command_new();

	if (fhFile) {
		uLength = 64;
		szLine = calloc(uLength, sizeof(char));
		nRead = 0;

		boSuccess = TRUE;
		uLine = 0;
		while (boSuccess && (nRead != -1)) {
			nRead = getline(&szLine, &uLength, fhFile);
			if (nRead != -1) {
				switch (uLine) {
					case 0: {
						psProof->szCommand = calloc(nRead, sizeof(char));
						strncpy(psProof->szCommand, szLine, nRead);
						psProof->szCommand[(nRead - 1)] = 0;
					}
					break;
					case 1: {
						psProof->szAnnotation = calloc(nRead, sizeof(char));
						strncpy(psProof->szAnnotation, szLine, nRead);
						psProof->szAnnotation[(nRead - 1)] = 0;
					}
					break;
					default: {
						boSuccess = command_parse(psCommand, szLine);
						if (boSuccess) {
							proof_process_step(psProof, psCommand);
							boSuccess = !proof_error(psProof, NULL);
							command_reset(psCommand);
						}
					}
					break;
				}
				uLine += 1;
			}
		}
	}

	command_delete(psCommand);
	boComplete = proof_complete(psProof);

	if (boSuccess && boComplete) {
		//printf("Loaded: %s\n", psProof->szCommand);
		psProof->uStepCount = uLine - 2;
	}
	else {
		proof_error(psProof, &szError);
		if (szError) {
			printf("Error loading proof %s: %s\n", psProof->szCommand, szError);
		}
		else {
			printf("Error loading proof: %s\n", szFilename);
		}
		proof_delete(psProof);
		psProof = NULL;
	}

	return psProof;
}


