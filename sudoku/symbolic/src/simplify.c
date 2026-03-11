/* vim: et:ts=4:sts=4:sw=4 */

/**
 * Symbolic
 *
 * @file
 * @author  David Llewellyn-Jones <david@flypig.co.uk>
 * @version 1.0
 *
 * @section LICENSE
 *
 * SPDX-License-Identifier: MIT
 * Copyright © 2026 David Llewellyn-Jones
 * See symbolic.h, COPYING file or website for licence
 *
 * @section DESCRIPTION
 *
 * Library for the construction of nested symbolic propositions.
 * 5/8/2003
 * http://www.flypig.co.uk/symbolic
 *
 * Provides functions for the simplification of Operations. The
 * process recursively applies a collection of rule (tautologies)
 * until no further simplifications are possible. This is a simple
 * form of theorom proving, with the input and output statements
 * being mathematically equivalent.
 * 
 */

//////////////////////////////////////////////////////////////////
// Includes

#include "symbolic.h"
#include "symbolic_private.h"

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <time.h>
#include <ctype.h>
#include <math.h>

//////////////////////////////////////////////////////////////////
// Defines

//////////////////////////////////////////////////////////////////
// Structures

//////////////////////////////////////////////////////////////////
// Global variables

//////////////////////////////////////////////////////////////////
// Function prototypes

int gcd (int nA, int nB);
Operation * SimplifyNOT (Operation * psOp, Operation * psVar1);
void SimplifyDepthFirstAnd (Operation * psOp);

void SimplifyDepthFirstAnd (Operation * psOp) {
	OpBinary * psBin = NULL;
	Variable * psVar = NULL;

	if (psOp) {
		switch (psOp->eOpType) {
			case OPTYPE_VARIABLE:
				psVar = psOp->Vars.psVar->psValue;
				// If the value is unset, we can set it to TRUE
				if (!GetVariableValid(psVar)) {
					//printf("Setting %s to FALSE\n", psOp->Vars.psVar->szVar);
					SetVariable (psVar, TRUE);
				}
				break;
			case OPTYPE_BINARY:
				// Simplify a binary operation
				psBin = psOp->Vars.psBinary;
				// First simplify the parameters
				switch (psBin->eOpType) {
					case OPBINARY_LAND:
						SimplifyDepthFirstAnd (psBin->psVar1);
						SimplifyDepthFirstAnd (psBin->psVar2);
						break;
					default:
						break;
				}
				break;
			default:
				break;
		}
	}
}

//////////////////////////////////////////////////////////////////
// Main application

/**
 * Recursively simplify an operation if possible.
 * Applies a variety of proof rules (tautologies) to the original operation.
 * The result will therefore be mathematically equivalent.
 * Because a new version may created and the original may be freed
 * it's good practice to overwrite the variable pointing to the
 * input structure with the return value.
 *
 * @param psOp the operation to simplify, which may be entirely freed.
 * @return a simplifed version of the operation, which should be freed once no 
 *		 longer needed.
 *
 */
Operation * SimplifyOperation (Operation * psOp) {
	Operation * psReturn = NULL;
	OpUnary * psUna = NULL;
	OpBinary * psBin = NULL;
	Operation * psTemp = NULL;

	if (psOp) {
		switch (psOp->eOpType) {
			case OPTYPE_TRUTHVALUE:
				// No simplification possible
				psReturn = psOp;
				break;
			case OPTYPE_VARIABLE:
				// No simplification possible
				psReturn = psOp;
				break;
			case OPTYPE_UNARY:
				psReturn = psOp;
				psUna = psReturn->Vars.psUnary;
				// Simplify the parameter operation
				psUna->psVar1 = SimplifyOperation (psUna->psVar1);
				switch (psUna->eOpType) {
					case OPUNARY_NOT:
						// Simplify unary Logical NOT
						psReturn = SimplifyNOT (psOp, psUna->psVar1);
						break;
					default:
						// Do nothing
						break;
				}
				break;
			case OPTYPE_BINARY:
				// Simplify a binary operation
				psBin = psOp->Vars.psBinary;
				// First simplify the parameters
				psBin->psVar1 = SimplifyOperation (psBin->psVar1);
				psBin->psVar2 = SimplifyOperation (psBin->psVar2);
				psReturn = psOp;
				switch (psBin->eOpType) {
					case OPBINARY_LAND:
						// Simplify a logical AND
						// If the first parameter is FALSE, reduce to FALSE
						if (((psBin->psVar1->eOpType == OPTYPE_TRUTHVALUE)
							&& (psBin->psVar1->Vars.boTruth == FALSE))
							|| ((psBin->psVar1->eOpType == OPTYPE_VARIABLE)
							&& GetVariableValid(psBin->psVar1->Vars.psVar->psValue)
							&& (GetVariable(psBin->psVar1->Vars.psVar->psValue) == FALSE))) {
							psReturn = CreateTruthValue (FALSE);
							FreeRecursive (psOp);
						}
						else {
							// If the second parameter is FALSE, reduce to FALSE
							if (((psBin->psVar2->eOpType == OPTYPE_TRUTHVALUE)
								&& (psBin->psVar2->Vars.boTruth == FALSE))
								|| ((psBin->psVar2->eOpType == OPTYPE_VARIABLE)
								&& GetVariableValid(psBin->psVar2->Vars.psVar->psValue)
								&& (GetVariable(psBin->psVar2->Vars.psVar->psValue) == FALSE))) {
								psReturn = CreateTruthValue (FALSE);
								FreeRecursive (psOp);
							}
							else {
								// If the first parameter is TRUE, remove it
								if (((psBin->psVar1->eOpType == OPTYPE_TRUTHVALUE)
									&& (psBin->psVar1->Vars.boTruth == TRUE))
									|| ((psBin->psVar1->eOpType == OPTYPE_VARIABLE)
									&& GetVariableValid(psBin->psVar1->Vars.psVar->psValue)
									&& (GetVariable(psBin->psVar1->Vars.psVar->psValue) == TRUE))) {
									psReturn = psBin->psVar2;
									FreeRecursive (psBin->psVar1);
									PropFree (psOp);
								}
								else {
									// If the second parameter is TRUE, remove it
									if (((psBin->psVar2->eOpType == OPTYPE_TRUTHVALUE)
										&& (psBin->psVar2->Vars.boTruth == TRUE))
										|| ((psBin->psVar2->eOpType == OPTYPE_VARIABLE)
										&& GetVariableValid(psBin->psVar2->Vars.psVar->psValue)
										&& (GetVariable(psBin->psVar2->Vars.psVar->psValue) == TRUE))) {
										psReturn = psBin->psVar1;
										FreeRecursive (psBin->psVar2);
										PropFree (psOp);
									}
									else {
										// If either is FALSE, reduce to FALSE
										if (((psBin->psVar1->eOpType == OPTYPE_TRUTHVALUE)
											&& (!psBin->psVar1->Vars.boTruth))
											|| ((psBin->psVar2->eOpType == OPTYPE_TRUTHVALUE)
											&& (!psBin->psVar2->Vars.boTruth))) {
											psReturn = CreateTruthValue (FALSE);
											FreeRecursive (psOp);
										}
										else {
											// If the first parameter is TRUE, reduce to the second parameter
											if (((psBin->psVar1->eOpType == OPTYPE_TRUTHVALUE)
												&& (psBin->psVar1->Vars.boTruth))
												|| (CompareOperations (psBin->psVar1, psBin->psVar2))) {
												psReturn = CopyRecursive (psBin->psVar2);
												FreeRecursive (psOp);
											}
											else {
												// If the second parameter is TRUE, reduce to the first parameter
												if ((psBin->psVar2->eOpType == OPTYPE_TRUTHVALUE)
													&& (psBin->psVar2->Vars.boTruth)) {
													psReturn = CopyRecursive (psBin->psVar1);
													FreeRecursive (psOp);
												}
											}
										}
									}
								}
							}
						}
						break;
					case OPBINARY_LOR:
						// Simplify a logical Or
						// If first parameter is TRUE, reduce to TRUE
						if (((psBin->psVar1->eOpType == OPTYPE_TRUTHVALUE)
							&& (psBin->psVar1->Vars.boTruth == TRUE))
							|| ((psBin->psVar1->eOpType == OPTYPE_VARIABLE)
							&& (GetVariableValid(psBin->psVar1->Vars.psVar->psValue) == TRUE)
							&& (GetVariable(psBin->psVar1->Vars.psVar->psValue) == TRUE))) {
							psReturn = CreateTruthValue (TRUE);
							FreeRecursive (psOp);
						}
						else {
							// If second parameter is TRUE, reduce to TRUE
							if (((psBin->psVar2->eOpType == OPTYPE_TRUTHVALUE)
								&& (psBin->psVar2->Vars.boTruth == TRUE))
								|| ((psBin->psVar2->eOpType == OPTYPE_VARIABLE)
								&& (GetVariableValid(psBin->psVar2->Vars.psVar->psValue) == TRUE)
								&& (GetVariable(psBin->psVar2->Vars.psVar->psValue) == TRUE))) {
								psReturn = CreateTruthValue (TRUE);
								FreeRecursive (psOp);
							}
							else {
								// If first parameter is FALSE, reduce to the second parameter
								if (((psBin->psVar1->eOpType == OPTYPE_TRUTHVALUE)
									&& (psBin->psVar1->Vars.boTruth == FALSE))
									|| ((psBin->psVar1->eOpType == OPTYPE_VARIABLE)
									&& (GetVariableValid(psBin->psVar1->Vars.psVar->psValue) == TRUE)
									&& (GetVariable(psBin->psVar1->Vars.psVar->psValue) == FALSE))) {
									psReturn = CopyRecursive (psBin->psVar2);
									FreeRecursive (psOp);
								}
								else {
									// If second parameter is FALSE, reduce to the first parameter
									if (((psBin->psVar2->eOpType == OPTYPE_TRUTHVALUE)
										&& (psBin->psVar2->Vars.boTruth == FALSE))
										|| ((psBin->psVar2->eOpType == OPTYPE_VARIABLE)
										&& (GetVariableValid(psBin->psVar2->Vars.psVar->psValue) == TRUE)
										&& (GetVariable(psBin->psVar2->Vars.psVar->psValue) == FALSE))) {
										psReturn = CopyRecursive (psBin->psVar1);
										FreeRecursive (psOp);
									}
									else {
										// If either paremter is TRUE, reduce to TRUE
										if (((psBin->psVar1->eOpType == OPTYPE_TRUTHVALUE)
											&& (psBin->psVar1->Vars.boTruth))
											|| ((psBin->psVar2->eOpType == OPTYPE_TRUTHVALUE)
											&& (psBin->psVar2->Vars.boTruth))) {
											psReturn = CreateTruthValue (TRUE);
											FreeRecursive (psOp);
										}
										else {
											// If both parameters are FALSE, reduce to FALSE
											if ((psBin->psVar1->eOpType == OPTYPE_TRUTHVALUE)
												&& (!psBin->psVar1->Vars.boTruth)
												&& (psBin->psVar2->eOpType == OPTYPE_TRUTHVALUE)
												&& (!psBin->psVar2->Vars.boTruth)) {
												psReturn = CreateTruthValue (FALSE);
												FreeRecursive (psOp);
											}
											else {
												// If either paremter is FALSE, reduce to the other parameter
												if ((psBin->psVar1->eOpType == OPTYPE_TRUTHVALUE)
													&& (!psBin->psVar1->Vars.boTruth)) {
													psReturn = CopyRecursive (psBin->psVar2);
													FreeRecursive (psOp);
												}
												else {
													if ((psBin->psVar2->eOpType == OPTYPE_TRUTHVALUE)
														&& (!psBin->psVar2->Vars.boTruth)) {
														psReturn = CopyRecursive (psBin->psVar1);
														FreeRecursive (psOp);
													}
												}
											}
										}
									}
								}
							}
						}
						break;
					case OPBINARY_LEOR:
						// Simplify a logical Eor
						// If the first parameter is FALSE, remove it
						if (((psBin->psVar1->eOpType == OPTYPE_TRUTHVALUE)
							&& (psBin->psVar1->Vars.boTruth == FALSE))
							|| ((psBin->psVar1->eOpType == OPTYPE_VARIABLE)
							&& GetVariableValid(psBin->psVar1->Vars.psVar->psValue)
							&& (GetVariable(psBin->psVar1->Vars.psVar->psValue) == FALSE))) {
							psReturn = psBin->psVar2;
							FreeRecursive (psBin->psVar1);
							PropFree (psOp);
						}
						else {
							// If the second parameter is FALSE, remove it
							if (((psBin->psVar2->eOpType == OPTYPE_TRUTHVALUE)
								&& (psBin->psVar2->Vars.boTruth == FALSE))
								|| ((psBin->psVar2->eOpType == OPTYPE_VARIABLE)
								&& GetVariableValid(psBin->psVar2->Vars.psVar->psValue)
								&& (GetVariable(psBin->psVar2->Vars.psVar->psValue) == FALSE))) {
								psReturn = psBin->psVar1;
								FreeRecursive (psBin->psVar2);
								PropFree (psOp);
							}
							else {
								// If either parameter is TRUE and the other FALSE, reduce to TRUE
								if ((((psBin->psVar1->eOpType == OPTYPE_TRUTHVALUE)
									&& ((psBin->psVar2->eOpType == OPTYPE_TRUTHVALUE)
									&& (psBin->psVar1->Vars.boTruth != psBin->psVar2->Vars.boTruth))))
									|| ((psBin->psVar1->eOpType == OPTYPE_VARIABLE)
									&& (psBin->psVar2->eOpType == OPTYPE_VARIABLE)
									&& (GetVariableValid(psBin->psVar1->Vars.psVar->psValue))
									&& (GetVariableValid(psBin->psVar2->Vars.psVar->psValue))
									&& (GetVariable(psBin->psVar1->Vars.psVar->psValue) != GetVariable(psBin->psVar1->Vars.psVar->psValue)))) {
									psReturn = CreateTruthValue (TRUE);
									FreeRecursive (psOp);
								}
								else {
									// If both parameters are the same, reduce to FALSE
									if (CompareOperations (psBin->psVar1, psBin->psVar2)) {
										psReturn = CreateTruthValue (FALSE);
										FreeRecursive (psOp);
									}
									else {
										// If both parameters are the same truth value, reduce to FALSE
										if ((((psBin->psVar1->eOpType == OPTYPE_TRUTHVALUE)
											&& ((psBin->psVar2->eOpType == OPTYPE_TRUTHVALUE)
											&& (psBin->psVar1->Vars.boTruth == psBin->psVar2->Vars.boTruth))))
											|| ((psBin->psVar1->eOpType == OPTYPE_VARIABLE)
											&& (psBin->psVar2->eOpType == OPTYPE_VARIABLE)
											&& (GetVariableValid(psBin->psVar1->Vars.psVar->psValue))
											&& (GetVariableValid(psBin->psVar2->Vars.psVar->psValue))
											&& (GetVariable(psBin->psVar1->Vars.psVar->psValue) == GetVariable(psBin->psVar1->Vars.psVar->psValue)))) {
											psReturn = CreateTruthValue (FALSE);
											FreeRecursive (psOp);
										}
									}
								}
							}
						}
						break;
					case OPBINARY_LIMP:
						// Simplify logical implication
						// If the first parameter is FALSE or the second is TRUE, reduce to TRUE
						if ((((psBin->psVar1->eOpType == OPTYPE_TRUTHVALUE)
							&& (!psBin->psVar1->Vars.boTruth))
							|| ((psBin->psVar2->eOpType == OPTYPE_TRUTHVALUE)
							&& (psBin->psVar2->Vars.boTruth)))
							|| CompareOperations (psBin->psVar1, psBin->psVar2)) {
							psReturn = CreateTruthValue (TRUE);
							FreeRecursive (psOp);
						}
						else {
							/// If the first parameeter is TRUE, reduce to the second parameter
							if ((psBin->psVar1->eOpType == OPTYPE_TRUTHVALUE)
								&& (psBin->psVar1->Vars.boTruth)) {
								psReturn = CopyRecursive (psBin->psVar2);
								FreeRecursive (psOp);
							}
						}
						break;
						break;
					default:
						break;
				}
				break;
			default:
				psReturn = psOp;
				printf("Invalid operation type\n");
				break;
		}
	}
	return psReturn;
}

/**
 * Return the Greatest Common Denominator of nA and nB.
 *
 * @param nA input integer.
 * @param nB input integer.
 * @return the greatest common denominator of nA and nB.
 *
 */
int gcd (int nA, int nB) {
	return (nB == 0 ? nA : gcd (nB, nA % nB));
}

/**
 * Recursively simplify a NOT operation if possible.
 * This is for internal use.
 *
 * @param psOp the operation to be simplified.
 * @param psVar1 the parameter of the unary operation.
 * @return a simplifed version of the operation, which should be freed once no 
 *		 longer needed.
 *
 */
Operation * SimplifyNOT (Operation * psOp, Operation * psVar1) {
	Operation * psReturn = psOp;
	OpBinary * psBin = NULL;

	switch (psVar1->eOpType) {
		case OPTYPE_UNARY:
			if (psVar1->Vars.psUnary->eOpType == OPUNARY_NOT) {
				// Remove pairs of 'nots'
				psReturn = CopyRecursive (psVar1->Vars.psUnary->psVar1);
				FreeRecursive (psOp);
			}
			break;
		case OPTYPE_TRUTHVALUE:
			// Negate the truth value
			psReturn = CreateTruthValue (!psVar1->Vars.boTruth);
			FreeRecursive (psOp);
			break;
		case OPTYPE_VARIABLE:
			// Negate the truth value
			if (GetVariableValid(psVar1->Vars.psVar->psValue)) {
				bool value = GetVariable(psVar1->Vars.psVar->psValue);
				SetVariable(psVar1->Vars.psVar->psValue, !value);
				psReturn = CreateTruthValue (!value);
				FreeRecursive (psOp);
			}
			else {
				SetVariable(psVar1->Vars.psVar->psValue, FALSE);
				psReturn = CreateTruthValue (TRUE);
				FreeRecursive (psOp);
			}
			break;
		case OPTYPE_BINARY:
			psBin = psVar1->Vars.psBinary;
			switch (psBin->eOpType) {
				default:
					// Do nothing
					break;
			}
			break;
		default:
			// Do nothing
			break;
	}

	return psReturn;
}

int CountOperations(int count, Operation * psOp) {
	switch (psOp->eOpType) {
		case OPTYPE_UNARY:
			return CountOperations(count, psOp->Vars.psUnary->psVar1) + 1;
			break;
		case OPTYPE_BINARY:
			return CountOperations(count, psOp->Vars.psBinary->psVar1) + CountOperations(count, psOp->Vars.psBinary->psVar2) + 1;
			break;
		default:
			return count + 1;
			break;
	}

	return count;
}

/**
 * Continue simpliying an operation until it no longer changes. This 
 * repeadedly applies the proof rules until the Operation no longer changes.
 * Because a new version may created and the original may be freed
 * it's good practice to overwrite the variable pointing to the
 * input structure with the return value.
 * In general, it makes sense to use this rather than SimplifyOperation, since
 * the UberSimplifyOperation is more effective at simplifying an Operation.
 *
 * @param psOp the operation to simplify, which may be entirely freed.
 * @return a simplifed version of the operation, which should be freed once no 
 *		 longer needed.
 *
 */
Operation * UberSimplifyOperation (Operation * psOp) {
	Operation * psPrev = NULL;
	int depth;

	depth = CountOperations(0, psOp);
	printf("Starting depth: %d\n", depth);

	// Repeatedly simplify
	do {
		FreeRecursive (psPrev);
		// Make a copy so we can compare the original with the result
		psPrev = CopyRecursive (psOp);
		psOp = SimplifyOperation (psOp);
		SimplifyDepthFirstAnd (psOp);

		depth = CountOperations(0, psOp);
		printf("Depth: %d\n", depth);
		// Continue until the result no longer changes
	} while (!CompareOperations (psOp, psPrev));

	FreeRecursive (psPrev);

	return psOp;
}

