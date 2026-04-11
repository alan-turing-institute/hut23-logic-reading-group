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
		proof_reset(psProof);

		free(psProof);
	}
}

void proof_reset(Proof* psProof) {
	size_t uPos = 0;

	if (psProof) {
		if (psProof->szCommand) {
			free(psProof->szCommand);
			psProof->szCommand = NULL;
		}
		if (psProof->szAnnotation) {
			free(psProof->szAnnotation);
			psProof->szAnnotation = NULL;
		}
		if (psProof->apsStep) {
			for (uPos = 0; uPos < psProof->uStepCount; ++uPos) {
				step_delete(psProof->apsStep[uPos]);
				psProof->apsStep[uPos] = NULL;
			}
			free(psProof->apsStep);
			psProof->apsStep = NULL;
		}
		psProof->uStepCount = 0;
		if (psProof->szError) {
			//free(psProof->szError);
			psProof->szError = NULL;
		}
		psProof->boComplete = FALSE;
		psProof->psRuleset = NULL;
	}
}

void proof_transfer(Proof* psProof, Proof* psFrom) {
	proof_reset(psProof);

	psProof->szCommand = psFrom->szCommand;
	psFrom->szCommand = NULL;

	psProof->szAnnotation = psFrom->szAnnotation;
	psFrom->szAnnotation = NULL;

	psProof->uStepCount = psFrom->uStepCount;
	psFrom->uStepCount = 0;

	psProof->apsStep = psFrom->apsStep;
	psFrom->apsStep = NULL;

	psProof->boError = psFrom->boError;
	psFrom->boError = FALSE;

	psProof->boComplete = psFrom->boComplete;
	psFrom->boComplete = FALSE;

	psProof->szError = psFrom->szError;
	psFrom->szError = NULL;

	psProof->psRuleset = psFrom->psRuleset;
	psFrom->psRuleset = NULL;
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

bool proof_find_step_index(Proof* psProof, char const* szName, size_t* puStep) {
	bool boFound;
	size_t uPos;

	// TODO: Use a hash table to map names to indices
	boFound = FALSE;
	for (uPos = 0; (uPos < psProof->uStepCount) && (!boFound); ++uPos) {
		if (strcmp(psProof->apsStep[uPos]->szName, szName) == 0) {
			boFound = TRUE;
			if (puStep) {
				*puStep = uPos;
			}
		}
	}
	return boFound;
}

bool proof_find_step_indices(Proof* psProof, char** aszName, size_t* auIndex, size_t uCount) {
	size_t uPos;
	bool boResult = TRUE;

	for (uPos = 0; (uPos < uCount) && boResult; ++uPos) {
		boResult &= proof_find_step_index(psProof, aszName[uPos], &auIndex[uPos]);
	}

	return boResult;
}

bool proof_get_steps(Proof* psProof, size_t auIndex[], Step* apsStep[], size_t uCount) {
	size_t uPos;
	bool boResult = TRUE;
	Step* psStep;

	for (uPos = 0; (uPos < uCount) && boResult; ++uPos) {
		psStep = proof_get_step(psProof, auIndex[uPos]);
		apsStep[uPos] = psStep;
		if (!psStep) {
			boResult = FALSE;
		}
	}

	return boResult;
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

void proof_print_help(Ruleset* psRuleset) {
	size_t uPos;
	size_t uLemmaNum;

	printf("\n");
	printf("The following commands are available in Daeducer.\n");
	printf("\n");
	printf("  Proof construction commands:\n");
	for (uPos = 0; uPos < STEP_NUM; ++uPos) {
		if (uPos == STEP_CONTROL) {
			printf("\n");
			printf("  Programme control commands:\n");
		}
		printf("    %s\n", aszHelp[uPos]);
	}

	uLemmaNum = ruleset_get_lemma_num(psRuleset);
	if (uLemmaNum > STEP_CONTROL) {
		printf("\n  Additional loaded lemmas:\n");

		for (uPos = STEP_CONTROL; uPos < uLemmaNum; ++uPos) {
			ruleset_print_help_line(psRuleset, uPos);
		}
	}

	printf("\n");
	printf("  <ref>: A reference back to a previous step (a number or label).\n");
	printf("  <exp>: A well-formed logical expression.\n");
	printf("         Use ^, v, ->, ! for conjunction, disjunction, conditional and negation respectively.\n");
	printf("  Optionally prefix a command with <label>: to assign a label to the step.\n");

	printf("\n");
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
	Ruleset* psRuleset;
	Proof* psLoad;
	size_t uNameSize;
	size_t uReadCount;
	size_t uLabel;

	boContinue = TRUE;
	boStep = TRUE;

	psProof->boError = FALSE;
	psProof->szError = NULL;
	boError = FALSE;

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
	if (psCommand->szLabel) {
		uReadCount = sscanf(psCommand->szLabel, "%lu", &uLabel);
		if (uReadCount == 1) {
			szError = "Labels cannot be integers.";
			boError = TRUE;
		}
		if (!boError) {
			boFound = proof_find_step_index(psProof, psCommand->szLabel, NULL);
			if (boFound) {
				szError = "Label already used.";
				boError = TRUE;
			}
		}
		if (!boError) {
			psStep->szName = strdup(psCommand->szLabel);
		}
	}
	else {
		uNameSize = snprintf(NULL, 0, "%lu", psProof->uStepCount + 1) + 1;
		psStep->szName = calloc(uNameSize, sizeof(char));
		snprintf(psStep->szName, uNameSize, "%lu", psProof->uStepCount + 1);
	}

	if ((!boError) && psProof->boComplete && (psCommand->eCommand < STEP_CONTROL)) {
		szError = "Proof rules can only be applied within active proofs. Use reset to start a new proof.";
		boError = TRUE;
	}

	if (!boError) {
		boError = TRUE;
		switch (psCommand->eCommand) {
			case STEP_PREMISE: {
				if (psCommand->uCount == 1) {
					if ((psProof->uStepCount == 0) || (psProof->apsStep[(psProof->uStepCount - 1)]->eCommand == STEP_PREMISE)) {
						psStep->uInputCount = 1;
						psStep->apsInput = calloc(psStep->uInputCount, sizeof(Operation*));
						psStep->apsInput[0] = StringToOperation(psCommand->aszParameter[0]);

						psStep->psResult = CopyRecursive(psStep->apsInput[0]);
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
			case STEP_DISJUNCTION_INTRO_LEFT:
				// Intentional fallthrough
			case STEP_DISJUNCTION_INTRO_RIGHT:
				// Intentional fallthrough
			case STEP_NEGATION_ELIM:
				// Intentional fallthrough
			case STEP_EXPLOSION: {
				psLemma = ruleset_get_lemma(psProof->psRuleset, psCommand->eCommand);
				boError = !lemma_apply_compiled(psLemma, psProof, psCommand, psStep, &szError);
			}
			break;
			case STEP_IMPLICATION_INTRO: {
				if (psCommand->uCount == 2) {
					size_t auRef[2];
					bool boFound;
					boFound = proof_find_step_indices(psProof, psCommand->aszParameter, auRef, 2);
					if (boFound) {
						Step* apsRef[2];
						boFound = proof_get_steps(psProof, auRef, apsRef, 2);
						if (boFound) {
							if (proof_scoped_subproof(psProof, auRef[0], auRef[1])) {
								psStep->uRefCount = 2;
								psStep->apsRef = calloc(psStep->uRefCount, sizeof(Step*));
								psStep->apsRef[0] = apsRef[0];
								psStep->apsRef[1] = apsRef[1];
								psStep->psResult = CreateBinary(OPBINARY_LIMP, CopyRecursive(psStep->apsRef[0]->psResult), CopyRecursive(psStep->apsRef[1]->psResult));
								boError = FALSE;
							}
							else {
								szError = "The subproof is out of scope.";
							}
						}
						else {
							szError = "Back references are missing.";
						}
					}
					else {
						szError = "Back references could not be found.";
					}
				}
				else {
					szError = "The imp_intro command takes two back references as parameters.";
				}
			}
			break;
			case STEP_DISJUNCTION_ELIM: {
				if (psCommand->uCount == 5) {
					size_t auRef[5];
					bool boFound;
					boFound = proof_find_step_indices(psProof, psCommand->aszParameter, auRef, 5);
					if (boFound) {
						if (proof_step_scoped(psProof, auRef[0])) {
							if (proof_scoped_subproof(psProof, auRef[1], auRef[2])) {
								if (proof_scoped_subproof(psProof, auRef[3], auRef[4])) {
									Step* apsRef[5];
									boFound = proof_get_steps(psProof, auRef, apsRef, 5);
									if (boFound) {
										psPattern = CreateBinary(OPBINARY_LOR, CreateVariable("A"), CreateVariable("B"));
										psExtract = ExtractPattern(psPattern, apsRef[0]->psResult);
										if (psExtract) {
											if (CompareOperations(ExtractValue(psExtract, "A"), apsRef[1]->psResult)) {
												if (CompareOperations(ExtractValue(psExtract, "B"), apsRef[3]->psResult)) {
													if (CompareOperations(apsRef[2]->psResult, apsRef[4]->psResult)) {

														psStep->uRefCount = 5;
														psStep->apsRef = calloc(psStep->uRefCount, sizeof(Step*));
														psStep->apsRef[0] = apsRef[0];
														psStep->apsRef[1] = apsRef[1];
														psStep->apsRef[2] = apsRef[2];
														psStep->apsRef[3] = apsRef[3];
														psStep->apsRef[4] = apsRef[4];

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
										szError = "Back references are missing.";
									}
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
						szError = "Back references could not be found.";
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
					bool boFound;
					boFound = proof_find_step_indices(psProof, psCommand->aszParameter, auRef, 2);
					if (boFound) {
						Step* apsRef[2];
						boFound = proof_get_steps(psProof, auRef, apsRef, 2);
						if (boFound) {
							if (proof_scoped_subproof(psProof, auRef[0], auRef[1])) {
								Operation* psOp = CreateTruthValue(FALSE);
								if (CompareOperations(apsRef[1]->psResult, psOp)) {
									psStep->uRefCount = 2;
									psStep->apsRef = calloc(psStep->uRefCount, sizeof(Step*));
									psStep->apsRef[0] = apsRef[0];
									psStep->apsRef[1] = apsRef[1];
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
						else {
							szError = "Back references are missing.";
						}
					}
					else {
						szError = "Back references could not be found.";
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
					bool boFound;
					boFound = proof_find_step_indices(psProof, psCommand->aszParameter, auRef, 2);
					if (boFound) {
						Step* apsRef[2];
						boFound = proof_get_steps(psProof, auRef, apsRef, 2);
						if (boFound) {
							if (proof_scoped_subproof(psProof, auRef[0], auRef[1])) {
								Operation* psOp = CreateTruthValue(FALSE);
								if (CompareOperations(apsRef[1]->psResult, psOp)) {
									psPattern = CreateUnary(OPUNARY_NOT, CreateVariable("A"));
									psExtract = ExtractPattern(psPattern, apsRef[0]->psResult);
									if (psExtract) {
										psStep->uRefCount = 2;
										psStep->apsRef = calloc(psStep->uRefCount, sizeof(Step*));
										psStep->apsRef[0] = apsRef[0];
										psStep->apsRef[1] = apsRef[1];
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
						else {
							szError = "Back references are missing.";
						}
					}
					else {
						szError = "Back references could not be found.";
					}
				}
				else {
					szError = "The indirect command takes two back references as parameters.";
				}
			}
			break;
			case STEP_ASSUMPTION: {
				if (psCommand->uCount == 1) {
					psStep->uInputCount = 1;
					psStep->apsInput = calloc(psStep->uInputCount, sizeof(Operation*));
					psStep->apsInput[0] = StringToOperation(psCommand->aszParameter[0]);

					psStep->psResult = CopyRecursive(psStep->apsInput[0]);
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
					proof_print_help(psProof->psRuleset);
				}
				else {
					szError = "The help command takes no parameters.";
				}
			}
			break;
			case STEP_LOAD: {
				if (psCommand->uCount == 1) {
					psLoad = proof_load(psProof->psRuleset, psCommand->aszParameter[0]);
					if (psLoad) {
						boError = FALSE;
						proof_reset(psProof);
						proof_transfer(psProof, psLoad);
						proof_delete(psLoad);
						printf("Proof loaded\n");
					}
					boStep = FALSE;
				}
				else {
					szError = "The reset command takes no parameters.";
				}
			}
			break;
			case STEP_SAVE: {
				if (psCommand->uCount == 3) {
					boError = FALSE;
					boStep = FALSE;
					proof_save(psProof, psCommand->aszParameter[0], psCommand->aszParameter[1], psCommand->aszParameter[2]);
				}
				else {
					szError = "The save command takes three parameters: filename, command name and command annotation.";
				}
			}
			break;
			case STEP_RESET: {
				if (psCommand->uCount == 0) {
					boError = FALSE;
					boStep = FALSE;
					psRuleset = psProof->psRuleset;
					proof_reset(psProof);
					psProof->psRuleset = psRuleset;
					printf("Proof reset\n");
				}
				else {
					szError = "The reset command takes no parameters.";
				}
			}
			break;
			default: {
				boFound = ruleset_get_command_index_start(psProof->psRuleset, psCommand->szCommand, STEP_CONTROL, &uIndex);
				if (boFound) {
					psStep->eCommand = uIndex;
					psLemma = ruleset_get_lemma(psProof->psRuleset, uIndex);
					boError = !lemma_apply_compiled(psLemma, psProof, psCommand, psStep, &szError);
				}
				if (!boFound) {
					szError = "Command not recognised.";
				}
			}
			break;
		};
	}

	if (!boContinue) {
		psProof->boComplete = TRUE;
	}

	if ((!boError) && boStep) {
		size_t uPos = psProof->uStepCount;
		psProof->uStepCount += 1;
		psProof->apsStep = realloc(psProof->apsStep, psProof->uStepCount * sizeof(Step));
		psProof->apsStep[uPos] = psStep;
	}
	else {
		if (boError) {
			psProof->boError = TRUE;
			psProof->szError = szError;
		}
		step_delete(psStep);
	}
}

void proof_print_last_step(Proof* psProof) {
	if (psProof->uStepCount > 0) {
		step_print(psProof->apsStep[(psProof->uStepCount - 1)], psProof->psRuleset);
	}
}

void proof_print_step(Proof* psProof, size_t uStep) {
	if (uStep < psProof->uStepCount) {
		step_print(psProof->apsStep[uStep], psProof->psRuleset);
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

	boSuccess = FALSE;
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
		free(szLine);
		fclose(fhFile);
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

bool proof_save(Proof* psProof, char const* szFilename, char const* szCommand, char const* szAnnotation) {
	bool boSuccess;
	size_t uPos;

	boSuccess = FALSE;
	FILE* fhFile = fopen(szFilename, "w");

	if (fhFile) {
		fprintf(fhFile, "%s\n", szCommand);
		fprintf(fhFile, "%s\n", szAnnotation);

		for (uPos = 0; uPos < psProof->uStepCount; ++uPos) {
			step_command_output(psProof->apsStep[uPos], psProof->psRuleset, fhFile);
		}
		fclose(fhFile);
		boSuccess = TRUE;
	}

	return boSuccess;
}

