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
#include "model.h"

#include "proof.h"

Proof* proof_new() {
	Proof* psProof;

	psProof = calloc(1, sizeof(Proof));

	return psProof;
}

void proof_delete(Proof* psProof) {
	if (psProof) {
		proof_clear(psProof);

		free(psProof);
	}
}

void proof_clear(Proof* psProof) {
	if (psProof) {
		proof_reset(psProof);
		psProof->psRuleset = NULL;
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
			for (uPos = 0; uPos < (psProof->uStepCount + psProof->uRedoCount); ++uPos) {
				step_delete(psProof->apsStep[uPos]);
				psProof->apsStep[uPos] = NULL;
			}
			free(psProof->apsStep);
			psProof->apsStep = NULL;
		}
		psProof->uStepCount = 0;
		psProof->uRedoCount = 0;
		if (psProof->szError) {
			//free(psProof->szError);
			psProof->szError = NULL;
		}
		psProof->boComplete = FALSE;
	}
}

void proof_transfer(Proof* psProof, Proof* psFrom) {
	proof_clear(psProof);

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

	psProof->uRedoCount = psFrom->uRedoCount;
	psFrom->uRedoCount = 0;
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
		if (!psStep || !psStep->psResult) {
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

bool proof_variable_assumed_in_scope(Proof* psProof, size_t uStep, char const* szVar) {
	size_t uCount;
	size_t uPos;
	bool boUnbound;
	size_t uScoped;

	uScoped = psProof->apsStep[uStep]->uIndent;
	boUnbound = FALSE;

	// Best to work backwards
	for (uCount = uStep + 1; (uCount > 0) && !boUnbound; --uCount) {
		uPos = uCount - 1;
		if (psProof->apsStep[uPos]->uIndent <= uScoped) {
			uScoped = psProof->apsStep[uPos]->uIndent;
			// We're in scope
			if ((psProof->apsStep[uPos]->eCommand == STEP_PREMISE) || (psProof->apsStep[uPos]->eCommand == STEP_ASSUMPTION)) {
				boUnbound = OccursUnbound(psProof->apsStep[uPos]->psResult, szVar);
			}
		}
	}

	return boUnbound;
}

bool proof_replaced_variables_match(VariableNameMap* psVariableNameMap, Operation const* psOpFrom, Operation const* psOpTo) {
	bool boResult = FALSE;
	char const* szVarFrom;
	char const* szVarTo;
	Operation* psReplacedFrom;
	Operation* psReplacedTo;
	size_t uCount;

	boResult = VariableNameMapExtract(psVariableNameMap, psOpFrom, psOpTo);
	uCount = VariableNameMapCount(psVariableNameMap);

	if (boResult && (uCount == 1)) {
		szVarFrom = VariableNameMapGetFrom(psVariableNameMap, 0);
		szVarTo = VariableNameMapGetTo(psVariableNameMap, 0);
		psReplacedFrom = CopyRecursive(psOpFrom);
		psReplacedTo = CopyRecursive(psOpTo);
		ReplaceUnbound(psReplacedFrom, szVarFrom, szVarTo);
		ReplaceUnbound(psReplacedTo, szVarFrom, szVarTo);
		boResult = CompareOperations(psReplacedFrom, psReplacedTo);
		FreeRecursive(psReplacedTo);
		FreeRecursive(psReplacedFrom);
	}
	else {
		boResult = FALSE;
	}

	return boResult;
}

void proof_print(Proof* psProof) {
	size_t uPos;

	for (uPos = 0; uPos < psProof->uStepCount; ++uPos) {
		printf("\n");
		proof_print_step(psProof, uPos);
	}
	printf("\n");
}

void proof_print_latex(Proof* psProof) {
	size_t uPos;

	printf("$\n");
	printf("\\begin{nd}");

	for (uPos = 0; uPos < psProof->uStepCount; ++uPos) {
		printf("\n");
		proof_print_step_latex(psProof, uPos);
	}
	printf("\n\\end{nd}\n");
	printf("$\n");
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

void proof_process_step(Proof* psProof, Model* psModel, Command* psCommand) {
	size_t uPos;
	Step* psStep;
	Operation* psPattern;
	Extract* psExtract;
	bool boError;
	char const* szError = "Unknown error.";
	bool boContinue;
	bool boStep;
	bool boFound;
	size_t uIndex;
	Lemma* psLemma;
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
						psStep->apsInput[0] = StringToOperationCheck(psCommand->aszParameter[0]);

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
			case STEP_BICONDITIONAL_ELIM: {
				if (psCommand->uCount == 2) {
					size_t auRef[2];
					Operation* psRelation1;
					Operation* psRelation2;
					Operation const* psOp1;
					Operation const* psOp2;
					bool boFound;
					boFound = proof_find_step_indices(psProof, psCommand->aszParameter, auRef, 2);
					if (boFound) {
						if (proof_step_scoped(psProof, auRef[0])) {
							if (proof_step_scoped(psProof, auRef[1])) {
									Step* apsRef[2];
									boFound = proof_get_steps(psProof, auRef, apsRef, 2);
									if (boFound) {
										psPattern = CreateBinary(OPBINARY_LEQUIV, CreateRelation("A", 0, NULL), CreateRelation("B", 0, NULL));
										psExtract = ExtractPattern(psPattern, apsRef[0]->psResult);
										if (psExtract) {
											psRelation1 = CreateRelation ("A", 0, NULL);
											psRelation2 = CreateRelation ("B", 0, NULL);
											psOp1 = ExtractValue(psExtract, psRelation1);
											psOp2 = ExtractValue(psExtract, psRelation2);

											if (CompareOperations(psOp1, apsRef[1]->psResult)) {
												psStep->uRefCount = 2;
												psStep->apsRef = calloc(psStep->uRefCount, sizeof(Step*));
												psStep->apsRef[0] = apsRef[0];
												psStep->apsRef[1] = apsRef[1];
												psStep->psResult = CopyRecursive(psOp2);
												boError = FALSE;
											}
											else {
												if (CompareOperations(psOp2, apsRef[1]->psResult)) {
													psStep->uRefCount = 2;
													psStep->apsRef = calloc(psStep->uRefCount, sizeof(Step*));
													psStep->apsRef[0] = apsRef[0];
													psStep->apsRef[1] = apsRef[1];
													psStep->psResult = CopyRecursive(psOp1);
													boError = FALSE;
												}
												else {
													szError = "The second back refererence must match one of the sides of the biconditional.";
												}
											}

											FreeRecursive(psRelation2);
											psRelation2 = NULL;
											FreeRecursive(psRelation1);
											psRelation1 = NULL;
											FreeExtract(psExtract);
											psExtract = NULL;
										}
										else {
											szError = "The first back reference must be in the form (A <-> B).";
										}
										FreeRecursive(psPattern);
										psPattern = NULL;
									}
									else {
										szError = "Back references are missing.";
									}
							}
							else {
								szError = "The second back reference is out of scope.";
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
			case STEP_BICONDITIONAL_INTRO: {
				if (psCommand->uCount == 4) {
					size_t auRef[4];
					bool boFound;
					boFound = proof_find_step_indices(psProof, psCommand->aszParameter, auRef, 4);
					if (boFound) {
							if (proof_scoped_subproof(psProof, auRef[0], auRef[1])) {
								if (proof_scoped_subproof(psProof, auRef[2], auRef[3])) {
									Step* apsRef[4];
									boFound = proof_get_steps(psProof, auRef, apsRef, 4);
									if (boFound) {
										boFound = CompareOperations(apsRef[0]->psResult, apsRef[3]->psResult) && CompareOperations(apsRef[1]->psResult, apsRef[2]->psResult);
										if (boFound) {
											psStep->uRefCount = 4;
											psStep->apsRef = calloc(psStep->uRefCount, sizeof(Step*));
											psStep->apsRef[0] = apsRef[0];
											psStep->apsRef[1] = apsRef[1];
											psStep->apsRef[2] = apsRef[2];
											psStep->apsRef[3] = apsRef[3];
											psStep->psResult = CreateBinary(OPBINARY_LEQUIV, CopyRecursive(apsRef[3]->psResult), CopyRecursive(apsRef[1]->psResult));
											boError = FALSE;
										}
										else {
											szError = "The first subproof premise must match the second conclusion and vice versa.";
										}
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
						szError = "Back references could not be found.";
					}
				}
				else {
					szError = "The or_elim command takes five back references as parameters.";
				}
			}
			break;
			case STEP_DISJUNCTION_ELIM: {
				if (psCommand->uCount == 5) {
					size_t auRef[5];
					Operation* psRelation1;
					Operation* psRelation2;
					bool boFound;
					boFound = proof_find_step_indices(psProof, psCommand->aszParameter, auRef, 5);
					if (boFound) {
						if (proof_step_scoped(psProof, auRef[0])) {
							if (proof_scoped_subproof(psProof, auRef[1], auRef[2])) {
								if (proof_scoped_subproof(psProof, auRef[3], auRef[4])) {
									Step* apsRef[5];
									boFound = proof_get_steps(psProof, auRef, apsRef, 5);
									if (boFound) {
										psPattern = CreateBinary(OPBINARY_LOR, CreateRelation("A", 0, NULL), CreateRelation("B", 0, NULL));
										psExtract = ExtractPattern(psPattern, apsRef[0]->psResult);
										if (psExtract) {
											psRelation1 = CreateRelation ("A", 0, NULL);
											if (CompareOperations(ExtractValue(psExtract, psRelation1), apsRef[1]->psResult)) {
												psRelation2 = CreateRelation ("B", 0, NULL);
												if (CompareOperations(ExtractValue(psExtract, psRelation2), apsRef[3]->psResult)) {
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
												FreeRecursive(psRelation2);
												psRelation2 = NULL;
											}
											else {
												szError = "The left hand side of the disjunction in the first reference must match the assumption of the first subproof.";
											}
											FreeExtract(psExtract);
											psExtract = NULL;
											FreeRecursive(psRelation1);
											psRelation1 = NULL;
										}
										else {
											szError = "The first back reference must be in the form (A v B).";
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
					Operation* psRelation;
					boFound = proof_find_step_indices(psProof, psCommand->aszParameter, auRef, 2);
					if (boFound) {
						Step* apsRef[2];
						boFound = proof_get_steps(psProof, auRef, apsRef, 2);
						if (boFound) {
							if (proof_scoped_subproof(psProof, auRef[0], auRef[1])) {
								Operation* psOp = CreateTruthValue(FALSE);
								if (CompareOperations(apsRef[1]->psResult, psOp)) {
									psPattern = CreateUnary(OPUNARY_NOT, CreateRelation("A", 0, NULL));
									psExtract = ExtractPattern(psPattern, apsRef[0]->psResult);
									if (psExtract) {
										psRelation = CreateRelation("A", 0, NULL);
										psStep->uRefCount = 2;
										psStep->apsRef = calloc(psStep->uRefCount, sizeof(Step*));
										psStep->apsRef[0] = apsRef[0];
										psStep->apsRef[1] = apsRef[1];
										psStep->psResult = CopyRecursive(ExtractValue(psExtract, psRelation));
										boError = FALSE;
										FreeExtract(psExtract);
										psExtract = NULL;
										FreeRecursive(psRelation);
										psRelation = NULL;
									}
									else {
										szError = "The first back reference must be in the form !A.";
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
					psStep->apsInput[0] = StringToOperationCheck(psCommand->aszParameter[0]);

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
			case STEP_UNIVERSAL_INTRO: {
				if (psCommand->uCount == 3) {
					size_t auRef[1];
					Step* apsRef[1];
					Operation* apsScrutinee[1];
					bool boFound;
					char* szVarFrom;
					char* szVarTo;
					int nUnboundBefore;
					int nUnboundAfter;
					Operation* psResult;

					boFound = proof_find_step_indices(psProof, psCommand->aszParameter, auRef, 1);
					if (boFound) {
						boFound = proof_step_scoped(psProof, auRef[0]);
						if (boFound) {
							boFound = proof_get_steps(psProof, auRef, apsRef, 1);
							if (boFound) {
								apsScrutinee[0] = apsRef[0]->psResult;

								psStep->uVarCount = 2;
								psStep->aszVar = calloc(psStep->uVarCount, sizeof(char*));
								psStep->aszVar[0] = strdup(psCommand->aszParameter[1]);
								psStep->aszVar[1] = strdup(psCommand->aszParameter[2]);
								szVarFrom = psStep->aszVar[0];
								szVarTo = psStep->aszVar[1];

								// Check that szVarFrom doesn't occur in a premise or undischarged assumption
								boFound = proof_variable_assumed_in_scope(psProof, psProof->uStepCount - 1, szVarFrom);
								if (!boFound) {
									psResult = CopyRecursive(apsScrutinee[0]);
									nUnboundBefore = OccursUnbound(psResult, szVarFrom);
									ReplaceUnbound(psResult, szVarFrom, szVarTo);
									nUnboundAfter = OccursUnbound(psResult, szVarTo);
									if (nUnboundBefore == nUnboundAfter) {
										psStep->uRefCount = 1;
										psStep->apsRef = calloc(psStep->uRefCount, sizeof(Step*));
										psStep->apsRef[0] = apsRef[0];

										psStep->psResult = CreateQuantifier(QUANTIFIER_UNIVERSAL, szVarTo, psResult);
										boError = FALSE;
									}
									else {
										FreeRecursive(psResult);
										psResult = NULL;
										szError = "The replaced variable cannot become bound by an existing quantifer.";
									}
								}
								else {
									szError = "The variable to replace must not occur in a premise or undischarged assumption";
								}
							}
							else {
								szError = "Thew back reference is missing";
							}
						}
						else {
							szError = "The back reference is out of scope.";
						}
					}
					else {
						szError = "Back reference could not be found";
					}
				}
				else {
					szError = "The universal introduction command takes one reference and two variable names as parameters.";
				}
			}
			break;
			case STEP_UNIVERSAL_ELIM: {
				if (psCommand->uCount == 2) {
					size_t auRef[1];
					Step* apsRef[1];
					Operation* apsScrutinee[1];
					bool boFound;
					char* szVarTo;
					char const* szVarFrom;
					QUANTIFIER eQuType;

					boFound = proof_find_step_indices(psProof, psCommand->aszParameter, auRef, 1);
					if (boFound) {
						boFound = proof_step_scoped(psProof, auRef[0]);
						if (boFound) {
							boFound = proof_get_steps(psProof, auRef, apsRef, 1);

							if (boFound) {
								apsScrutinee[0] = apsRef[0]->psResult;

								psStep->uVarCount = 1;
								psStep->aszVar = calloc(psStep->uVarCount, sizeof(char*));
								psStep->aszVar[0] = strdup(psCommand->aszParameter[1]);
								szVarTo = psStep->aszVar[0];

								// Check whether it's a universal quantifier
								eQuType = QuantifierGetType(apsScrutinee[0]);
								if (eQuType == QUANTIFIER_UNIVERSAL) {
									psStep->uRefCount = 1;
									psStep->apsRef = calloc(psStep->uRefCount, sizeof(Step*));
									psStep->apsRef[0] = apsRef[0];

									szVarFrom = QuantifierGetVariable(apsScrutinee[0]);
									psStep->psResult = CopyRecursive(QuantifierGetSub(apsScrutinee[0]));
									ReplaceUnbound(psStep->psResult, szVarFrom, szVarTo);
									boError = FALSE;
								}
								else {
									szError = "The referenced expressions must match the rule structure.";
								}
							}
							else {
								szError = "Thew back reference is missing";
							}
						}
						else {
							szError = "The back reference is out of scope.";
						}
					}
					else {
						szError = "Back reference could not be found";
					}
				}
				else {
					szError = "The universal elimination command takes one reference and a variable name as parameters.";
				}
			}
			break;
			case STEP_EXISTENTIAL_INTRO: {
				if (psCommand->uCount == 2) {
					size_t auRef[1];
					Step* apsRef[1];
					Operation* apsScrutinee[1];
					bool boFound;
					VariableNameMap* psVariableNameMap;
					char const* szVarTo;

					boFound = proof_find_step_indices(psProof, psCommand->aszParameter, auRef, 1);
					if (boFound) {
						boFound = proof_step_scoped(psProof, auRef[0]);
						if (boFound) {
							boFound = proof_get_steps(psProof, auRef, apsRef, 1);
							if (boFound) {
								apsScrutinee[0] = apsRef[0]->psResult;

								psStep->uInputCount = 1;
								psStep->apsInput = calloc(psStep->uInputCount, sizeof(Operation*));
								psStep->apsInput[0] = StringToOperationCheck(psCommand->aszParameter[1]);

								// Check that every unbound instance of szVarTo in the result used to be an instance of the same thing

								psVariableNameMap = CreateVariableNameMap();
								boFound = proof_replaced_variables_match(psVariableNameMap, apsScrutinee[0], psStep->apsInput[0]);
								if (boFound) {
									psStep->uRefCount = 1;
									psStep->apsRef = calloc(psStep->uRefCount, sizeof(Step*));
									psStep->apsRef[0] = apsRef[0];

									psStep->psResult = CopyRecursive(psStep->apsInput[0]);
									szVarTo = VariableNameMapGetTo(psVariableNameMap, 0);
									psStep->psResult = CreateQuantifier(QUANTIFIER_EXISTENTIAL, szVarTo, psStep->psResult);
									boError = FALSE;
								}
								else {
									szError = "Only unbound variables with the same name can be replaced with existential introduction.";
								}
								psVariableNameMap = FreeVariableNameMap(psVariableNameMap);
							}
							else {
								szError = "Thew back reference is missing";
							}
						}
						else {
							szError = "The back reference is out of scope.";
						}
					}
					else {
						szError = "Back reference could not be found";
					}
				}
				else {
					szError = "The existential introduction command takes one reference and an expression as parameters.";
				}
			}
			break;
			case STEP_EXISTENTIAL_ELIM: {
				if (psCommand->uCount == 3) {
					size_t auRef[3];
					VariableNameMap* psVariableNameMap;
					char const* szVarTo;
					QUANTIFIER eQuType;
					bool boFound;

					boFound = proof_find_step_indices(psProof, psCommand->aszParameter, auRef, 3);
					if (boFound) {
						if (proof_step_scoped(psProof, auRef[0])) {
							if (proof_scoped_subproof(psProof, auRef[1], auRef[2])) {
								Step* apsRef[3];
								boFound = proof_get_steps(psProof, auRef, apsRef, 3);
								if (boFound) {
									eQuType = QuantifierGetType(apsRef[0]->psResult);
									if (eQuType == QUANTIFIER_EXISTENTIAL) {
										Operation const* psSub = QuantifierGetSub(apsRef[0]->psResult);
										psVariableNameMap = CreateVariableNameMap();
										boFound = proof_replaced_variables_match(psVariableNameMap, psSub, apsRef[1]->psResult);

										if (boFound) {
											szVarTo = VariableNameMapGetTo(psVariableNameMap, 0);
											// Check that szVarTo doesn't occur in a premise or undischarged assumption
											boFound = proof_variable_assumed_in_scope(psProof, psProof->uStepCount - 1, szVarTo);
											if (!boFound) {
												boFound = OccursUnbound(apsRef[0]->psResult, szVarTo);
												if (!boFound) {
													boFound = OccursUnbound(apsRef[2]->psResult, szVarTo);
													if (!boFound) {
														psStep->uRefCount = 3;
														psStep->apsRef = calloc(psStep->uRefCount, sizeof(Step*));
														psStep->apsRef[0] = apsRef[0];
														psStep->apsRef[1] = apsRef[1];
														psStep->apsRef[2] = apsRef[2];

														psStep->psResult = CopyRecursive(apsRef[2]->psResult);
														boError = FALSE;
													}
													else {
														szError = "The variable to replace must not occur in the subproof conclusion.";
													}
												}
												else {
													szError = "The variable to replace must not occur in the sentence being quantified over.";
												}
											}
											else {
												szError = "The variable to replace must not occur in a premise or undischarged assumption";
											}
										}
										else {
												szError = "The subproof assumption must match the sentence being quantified over.";
										}
										psVariableNameMap = FreeVariableNameMap(psVariableNameMap);
									}
									else {
										szError = "Existential elimination can only be applied to an existentially quantified expression.";
									}
								}
								else {
									szError = "Back references are missing.";
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
					szError = "The exists_elim command takes three back references as parameters.";
				}
			}
			break;
			case STEP_IDENTITY_INTRO: {
				if (psCommand->uCount == 1) {
					char* aszVar[2];

					psStep->uVarCount = 1;
					psStep->aszVar = calloc(psStep->uVarCount, sizeof(char*));
					psStep->aszVar[0] = strdup(psCommand->aszParameter[0]);
					aszVar[0] = psStep->aszVar[0];
					aszVar[1] = psStep->aszVar[0];

					psStep->psResult = CreateRelation("=", 2, aszVar);
					boError = FALSE;
				}
				else {
					szError = "The identity introduction command takes one variable name as a parameter.";
				}
			}
			break;
			case STEP_IDENTITY_ELIM: {
				if (psCommand->uCount == 3) {
					size_t auRef[2];
					Step* apsRef[2];
					Operation* apsScrutinee[1];
					bool boFound;
					VariableNameMap* psVariableNameMap;
					char const* szVarTo;
					char const* szVarFrom;
					size_t uCount;
					char const * aszVar[2];
					Operation* psIdentity;
					VarStack* psInputs;
					int nArity;

					boFound = proof_find_step_indices(psProof, psCommand->aszParameter, auRef, 2);
					if (boFound) {
						boFound = proof_step_scoped(psProof, auRef[0]);
						if (boFound) {
							boFound = proof_step_scoped(psProof, auRef[1]);
							if (boFound) {
								boFound = proof_get_steps(psProof, auRef, apsRef, 2);
								if (boFound) {

									psIdentity = CreateRelation("=", 2, (char* const[]) {"x", "y"});

									boFound = CompareOperationPatterns(psIdentity, apsRef[0]->psResult);

									if (boFound) {
										psInputs = CreateVarStack();
										nArity = OperationInputList (apsRef[0]->psResult, psInputs);
										if (nArity == 2) {
											aszVar[0] = VarStackGet(psInputs, 0);
											aszVar[1] = VarStackGet(psInputs, 1);

											apsScrutinee[0] = apsRef[1]->psResult;

											psStep->uInputCount = 1;
											psStep->apsInput = calloc(psStep->uInputCount, sizeof(Operation*));
											psStep->apsInput[0] = StringToOperationCheck(psCommand->aszParameter[2]);

											// Check that every unbound instance of szVarTo in the result used to be an instancxe of the same thing

											psVariableNameMap = CreateVariableNameMap();
											boFound = proof_replaced_variables_match(psVariableNameMap, apsScrutinee[0], psStep->apsInput[0]);
											if (boFound) {
												uCount = VariableNameMapCount(psVariableNameMap);
												if (uCount == 1) {
													szVarTo = VariableNameMapGetTo(psVariableNameMap, 0);
													szVarFrom = VariableNameMapGetFrom(psVariableNameMap, 0);
													if (((strcmp(szVarFrom, aszVar[0]) == 0) || (strcmp(szVarTo, aszVar[0]) == 0)) && ((strcmp(szVarFrom, aszVar[1]) == 0) || (strcmp(szVarTo, aszVar[1]) == 0))) {

														psStep->uRefCount = 2;
														psStep->apsRef = calloc(psStep->uRefCount, sizeof(Step*));
														psStep->apsRef[0] = apsRef[0];
														psStep->apsRef[1] = apsRef[1];

														psStep->psResult = CopyRecursive(psStep->apsInput[0]);
														boError = FALSE;
													}
													else {
														szError = "The variables changed in the expression must match those from the equality.";
													}
												}
												else {
													szError = "Exactly one of the two variable names in the equality must be changed in the expression.";
												}
											}
											else {
												szError = "Only unbound variables with the same name can be replaced with existential introduction.";
											}
											psVariableNameMap = FreeVariableNameMap(psVariableNameMap);
										}
										else {
											szError = "The first reference must be an identity relation with distinct variables.";
										}
										psInputs = FreeVarStack(psInputs);
									}
									else {
										szError = "The first reference must be to an identity relation.";
									}
									FreeRecursive(psIdentity);
									psIdentity = NULL;
								}
								else {
									szError = "Thew back reference is missing";
								}
							}
							else {
								szError = "Back reference could not be found";
							}
						}
						else {
							szError = "The back reference is out of scope.";
						}
					}
					else {
						szError = "The back reference is out of scope.";
					}
				}
				else {
					szError = "The existential introduction command takes one reference and an expression as parameters.";
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
				if ((psCommand->uCount == 0) || ((psCommand->uCount == 1) && (strcmp("default", psCommand->aszParameter[0]) == 0))) {
					boError = FALSE;
					boStep = FALSE;
					proof_print(psProof);
					printf("\n");
				}
				else {
					if (psCommand->uCount == 1) {
						if (strcmp("latex", psCommand->aszParameter[0]) == 0) {
							boError = FALSE;
							boStep = FALSE;
							proof_print_latex(psProof);
							printf("\n");
						}
						else {
							szError = "The print command parameter must be either 'default' or 'latex'.";
						}
					}
					else {
						szError = "The print command takes zero or one parameters.";
					}
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
					psLoad = proof_load(psProof->psRuleset, psCommand->aszParameter[0], &szError);
					if (psLoad) {
						boError = FALSE;
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
					proof_reset(psProof);
					printf("Proof reset\n");
				}
				else {
					szError = "The reset command takes no parameters.";
				}
			}
			break;
			case STEP_PROVE: {
				if (psCommand->uCount == 1) {
					boError = TRUE;
					boStep = FALSE;

					if (psModel) {
						Operation* psClaim = StringToOperationCheck(psCommand->aszParameter[0]);
						size_t uLength;
						char* szString;
						uLength = OperationToStringLengthLatex(psClaim) + 1;
						szString = malloc(uLength);
						OperationToStringLatex(psClaim, szString, uLength);
						free(szString);
						boError = !model_prove(psModel, psProof, psClaim);
						FreeRecursive(psClaim);
						psClaim = NULL;
						if (boError) {
							szError = "The model failed to generate a valid proof; consider retrying after a neuralize.";
						}
					}
					else {
						szError = "No model loaded for generating a proof.";
					}
				}
				else {
					szError = "The prove command takes the logical expression to prove as a parameter.";
				}
			}
			break;
			case STEP_NEURALIZE: {
				boError = FALSE;
				boStep = FALSE;

				if (psModel) {
					model_neuralize(psModel);
				}
			}
			break;
			case STEP_UNDO: {
				size_t uSteps = 1;
				size_t uReadCount = 1;
				if (psCommand->uCount <= 1) {
					if (psCommand->uCount == 1) {
						uReadCount = sscanf(psCommand->aszParameter[0], "%lu", &uSteps);
					}
					if (uReadCount == 1) {
						boError = proof_undo_steps(psProof, uSteps, &szError);
						boStep = FALSE;
					}
					else {
						szError = "The parameter to undo must be a number of steps as a non-negative integer.";
					}
				}
				else {
					szError = "The undo command takes either zero or one parameter.";
				}
			}
			break;
			case STEP_REDO: {
				size_t uSteps = 1;
				size_t uReadCount = 1;
				if (psCommand->uCount <= 1) {
					if (psCommand->uCount == 1) {
						uReadCount = sscanf(psCommand->aszParameter[0], "%lu", &uSteps);
					}
					if (uReadCount == 1) {
						boError = proof_redo_steps(psProof, uSteps, &szError);
						boStep = FALSE;
					}
					else {
						szError = "The parameter to redo must be a number of steps as a non-negative integer.";
					}
				}
				else {
					szError = "The redo command takes either zero or one parameter.";
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
		proof_allocate_length(psProof, (psProof->uStepCount + 1));
		psProof->uStepCount += 1;
		psProof->uRedoCount = 0;
		//psProof->apsStep = realloc(psProof->apsStep, psProof->uStepCount * sizeof(Step));
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

void proof_print_step_latex(Proof* psProof, size_t uStep) {
	if (uStep < psProof->uStepCount) {
		step_print_latex(psProof->apsStep[uStep], psProof->psRuleset);
	}
}

bool proof_complete(Proof* psProof) {
	return psProof->boComplete;
}

bool proof_error(Proof* psProof, char const** pszError) {
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

Proof* proof_load(Ruleset* psRuleset, char const* szFilename, char const** pszError) {
	Proof* psProof;
	FILE* fhFile;
	char* szLine;
	size_t uLength;
	ssize_t nRead;
	bool boSuccess;
	size_t uLine;
	bool boComplete;
	char const* szError;
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
							proof_process_step(psProof, NULL, psCommand);
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
		szError = NULL;
		proof_error(psProof, &szError);
		if (szError) {
			printf("Error loading file %s: %s\n", psProof->szCommand, szError);
		}
		if (pszError) {
			*pszError = "Error Loading proof.";
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

void proof_print_prompt(Proof* psProof) {
	size_t uIndent;
	size_t uCount;

	printf("\r");
	if (proof_complete(psProof)) {
		printf(COL_RESET COL_RED "        ");
	}
	else {
		uIndent = proof_indent(psProof);
		printf(COL_RESET COL_RED "        | ");
		for (uCount = 0; uCount < uIndent; ++uCount) {
			printf("| ");
		}
	}

	printf(COL_GREEN "> ");
}

bool proof_undo_steps(Proof* psProof, size_t uSteps, char const** pszError) {
	bool boError = TRUE;

	if (psProof->uStepCount >= uSteps) {
		if (psProof->boComplete) {
			// If the last step was QED, mark the proof as incomplete
			if (uSteps > 0) {
				if (psProof->apsStep[(psProof->uStepCount - 1)]->eCommand == STEP_QED) {
					psProof->boComplete = FALSE;
				}
			}
		}

		psProof->uStepCount -= uSteps;
		psProof->uRedoCount += uSteps;
		boError = FALSE;
	}
	else {
		*pszError = "There are not enough steps in the proof to undo.";
	}

	return boError;
}

bool proof_redo_steps(Proof* psProof, size_t uSteps, char const** pszError) {
	bool boError = TRUE;

	if (uSteps <= psProof->uRedoCount) {
		psProof->uStepCount += uSteps;
		psProof->uRedoCount -= uSteps;
		boError = FALSE;
		if ((psProof->uStepCount > 0) && (psProof->apsStep[(psProof->uStepCount - 1)]->eCommand == STEP_QED)) {
				psProof->boComplete = TRUE;
		}
	}
	else {
		*pszError = "There are not enough steps in the redo buffer to restore.";
	}

	return boError;
}

void proof_allocate_length(Proof* psProof, size_t uSteps) {
	size_t uPos;

	// Remove anny excess steps
	for (uPos = uSteps; uPos < (psProof->uStepCount + psProof->uRedoCount); ++uPos) {
		if (psProof->apsStep[uPos] != NULL) {
			step_delete(psProof->apsStep[uPos]);
			psProof->apsStep[uPos] = NULL;
		}
	}

	psProof->apsStep = realloc(psProof->apsStep, uSteps * sizeof(Step));
	for (uPos = (psProof->uStepCount + psProof->uRedoCount); uPos < uSteps; ++uPos) {
		psProof->apsStep[uPos] = NULL;
	}
}
