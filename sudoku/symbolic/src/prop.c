/* vim: noet:ts=4:sts=4:sw=4 */

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
 * Implements a main function for simple testing of the library.
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

//////////////////////////////////////////////////////////////////
// Defines

//////////////////////////////////////////////////////////////////
// Structures

//////////////////////////////////////////////////////////////////
// Global variables

#if defined _MEM_PROFILE
static int gnPropMemAllocated = 0;
#endif

//////////////////////////////////////////////////////////////////
// Function prototypes

bool SubstituteRecursive (Operation * psMain, Operation const * psFind, Operation const * psSub);
int SubstituteRecursivePair (Operation * psMain, Operation const * psFind1, Operation const * psSub1, Operation const * psFind2, Operation const * psSub2);
int SubstituteRecursiveMany (Operation * psMain, Operation ** apsFind, Operation const ** apsSub, int nCount);
int CompareOperationsPair (Operation * psMain, Operation const * psCompare1, Operation const * psCompare2);
int CompareOperationsMany (Operation * psMain, Operation ** psCompare, int nCount);
bool CompareOperationPatternsRecursive (Operation const * psOp1, Operation const * psOp2, VarStack * psVarStack1, VarStack * psVarStack2);

//////////////////////////////////////////////////////////////////
// Main application

//#if !defined _DEBUG
//inline void noprintf (char * szNull, ...) {
//	szNull = szNull;
//}
//#endif

/**
 * Reset memory allocation details.
 *
 */
void PropMemReset (void) {
#if defined _MEM_PROFILE
	printf ("Memory profiling reset\n");
	gnPropMemAllocated = 0;
#else
	printf ("No memory profiling\n");
#endif
}

/**
 * Output current memory allocation details.
 *
 */
void PropMemOutput (void) {
#if defined _MEM_PROFILE
	printf ("Total memory blocks allocated = %d\n", gnPropMemAllocated);
#endif
}

/**
 * Replacement for malloc to also profile memory usage. Replicates
 * void * malloc (size_t size)
 *
 * @param size the amount of memory to allocate in bytes.
 * @return pointer to the allocated memory.
 *
 */
void * PropMemMalloc (size_t size) {
#if defined _MEM_PROFILE
	gnPropMemAllocated += 1;
#endif
	return malloc (size);
}

/**
 * Replacement for calloc to also profile memory usage. Replicates
 * void * calloc (size_t n, size_t size)
 * A chunk of memor of size (n * size) will be allocted on the heap
 * and its contents set to all zero bytes.
 *
 * @param n number of blocks to allocate.
 * @param size the size of each memory block to allocate in bytes.
 * @return pointer to the allocated memory.
 *
 */
//////////////////////////////////////////////////////////////////
void * PropMemCalloc (size_t n, size_t size) {
#if defined _MEM_PROFILE
	gnPropMemAllocated += 1;
#endif
	return calloc (n, size);
}

/**
 * Replacement for realloc to also profile memory usage. Replicates
 * void * realloc (void * ptr, size_t size)
 *
 * @param ptr pointer to the memory to resize.
 * @param size the amount of memory to allocate in bytes.
 * @return pointer to the allocated memory.
 *
 */
void * PropMemRealloc (void * ptr, size_t size) {
	return realloc (ptr, size);
}

/**
 * Replacement for free to also profile memory usage. Replicates
 * void free (void * ptr)
 *
 * @param ptr pointer to the memory to free.
 *
 */
void PropMemFree (void * ptr) {
#if defined _MEM_PROFILE
	gnPropMemAllocated -= 1;
#endif
	return free (ptr);
}

/**
 * Create true/false truth value operation.
 *
 * @param boTruth the initial value of the Operation.
 * @return pointer to the created Operation.
 *
 */
Operation * CreateTruthValue (bool const boTruth) {
	Operation * psOp;

	psOp = (Operation*)PropMalloc (sizeof(Operation));
	psOp->eOpType = OPTYPE_TRUTHVALUE;
	psOp->Vars.boTruth = boTruth;

	return psOp;
}

/**
 * Create a variable.
 *
 * The szVar string will be copied.
 *
 * @param szVar the name of the variable.
 * @return pointer to the created Operation.
 *
 */
Operation * CreateVariable (char const * szVar) {
	Operation * psOp;
	int nNameLen;

	psOp = (Operation*)PropMalloc (sizeof(Operation));
	psOp->eOpType = OPTYPE_VARIABLE;
	psOp->Vars.psVar = (OpVariable*)PropMalloc(sizeof(OpVariable));

	// Store the user function name
	nNameLen = (int)strlen (szVar);
	psOp->Vars.psVar->szVar = (char *)PropMalloc (nNameLen + 1);
	strncpy (psOp->Vars.psVar->szVar, szVar, nNameLen);
	psOp->Vars.psVar->szVar[nNameLen] = 0;

	psOp->Vars.psVar->psValue = NULL;

	return psOp;
}

/**
 * Create a unary operation.
 * Note that psVar1 is used directly, rather than being copied. As such it will 
 * be freed when the resulting combined Operation is freed recursively.
 *
 * @param eOpType the type of the operation (see OPUNARY for the acceptable
 *        types).
 * @param psVar1 the Operation that the unary operator will be applied to.
 * @return pointer to the created Operation.
 *
 */
Operation * CreateUnary (OPUNARY eOpType, Operation * psVar1) {
	Operation * psOp;

	psOp = (Operation*)PropMalloc (sizeof(Operation));
	psOp->eOpType = OPTYPE_UNARY;
	psOp->Vars.psUnary = (OpUnary*)PropMalloc(sizeof(OpUnary));
	psOp->Vars.psUnary->eOpType = eOpType;
	psOp->Vars.psUnary->psVar1 = psVar1;

	return psOp;
}

/**
 * Create a binary operation.
 * Note that psVar1 and psVar2 are used directly, rather than being copied. As
 * such they will be freed when the resulting combined Operation is freed
 * recursively.
 *
 * @param eOpType the type of the operation (see OPBINARY for the acceptable
 *        types).
 * @param psVar1 the LHS Operation that the binary operator will apply to.
 * @param psVar2 the RHS Operation that the binary operator will apply to.
 * @return pointer to the created Operation.
 *
 */
Operation * CreateBinary (OPBINARY eOpType, Operation * psVar1, Operation * psVar2) {
	Operation * psOp;

	psOp = (Operation*)PropMalloc (sizeof(Operation));
	psOp->eOpType = OPTYPE_BINARY;
	psOp->Vars.psBinary = (OpBinary*)PropMalloc(sizeof(OpBinary));
	psOp->Vars.psBinary->eOpType = eOpType;
	psOp->Vars.psBinary->psVar1 = psVar1;
	psOp->Vars.psBinary->psVar2 = psVar2;

	return psOp;
}

/**
 * Create a quantifier operation.
 *
 * The szVar string will be copied.
 *
 * Contrariwise psVar1 is used directly, rather than being copied. As
 * such it will be freed when the resulting combined Operation is freed
 * recursively.
 *
 * @param eQuantType the type of quantifer (universal or existential).
 * @param psVar the name of the variable to quantify over.
 * @param psVar1 the Operation that the quantifier applies to.
 * @return pointer to the created Operation.
 *
 */
Operation * CreateQuantifier (QUANTIFIER eQuType, char const * szVar, Operation * psVar1) {
	Operation * psOp;
	int nNameLen;

	psOp = (Operation*)PropMalloc (sizeof(Operation));
	psOp->eOpType = OPTYPE_QUANTIFIER;
	psOp->Vars.psQuantifier = (OpQuantifier*)PropMalloc(sizeof(OpQuantifier));
	psOp->Vars.psQuantifier->eQuType = eQuType;

	// Store the user function name
	nNameLen = (int)strlen (szVar);
	psOp->Vars.psQuantifier->szVar = (char *)PropMalloc (nNameLen + 1);
	strncpy (psOp->Vars.psQuantifier->szVar, szVar, nNameLen);
	psOp->Vars.psQuantifier->szVar[nNameLen] = 0;

	psOp->Vars.psQuantifier->psVar1 = psVar1;

	return psOp;
}

/**
 * Recursively free up all of the memory used by a formula and its sub formulas.
 * Care should be taken not to perform multiple frees, by freeing up an
 * Operation that was already freed by this.
 *
 * @param psOp the Operation that will be freed, along with all of its
 *        sub-Operations.
 *
 */
void FreeRecursive (Operation * psOp) {
	if (psOp) {
		switch (psOp->eOpType) {
			case OPTYPE_TRUTHVALUE:
				// Nothing else to free - backtrack
				break;
			case OPTYPE_VARIABLE:
				// Free up the variable name string and decrement
				// variable reference if there is one,
				// then backtrack
				if (psOp->Vars.psVar->psValue) {
					DecrementVarRef (psOp->Vars.psVar->psValue);
				}
				PropFree (psOp->Vars.psVar->szVar);
				PropFree (psOp->Vars.psVar);
				break;
			case OPTYPE_UNARY:
				// Free up any operations further down the tree
				if (psOp->Vars.psUnary) {
					FreeRecursive (psOp->Vars.psUnary->psVar1);
					PropFree (psOp->Vars.psUnary);
				}
				// Then backtrack
				break;
			case OPTYPE_BINARY:
				// Free up any operations further down the tree
				if (psOp->Vars.psBinary) {
					FreeRecursive (psOp->Vars.psBinary->psVar1);
					FreeRecursive (psOp->Vars.psBinary->psVar2);
					PropFree (psOp->Vars.psBinary);
				}
				// Then backtrack
				break;
			case OPTYPE_QUANTIFIER:
				// Free up the variable name string and any operations further down the tree
				if (psOp->Vars.psQuantifier) {
					PropFree (psOp->Vars.psQuantifier->szVar);
					FreeRecursive (psOp->Vars.psQuantifier->psVar1);
					PropFree (psOp->Vars.psQuantifier);
				}
				break;
			case OPTYPE_RELATION:
				RelationFreeRecursive (psOp);
				break;
			default:
				printf("Invalid operation type\n");
				break;
		}

		PropFree (psOp);
	}
}

/**
 * Recursively copy a formula and all its subformulas. Note that copies will
 * also be made of all sub-Operations, so the original and copy should be
 * freed separately.
 *
 * @param psOp the Operation that will be copied, along with all of its
 *        sub-Operations.
 * @return the newly created copy.
 *
 */
Operation * CopyRecursive (Operation const * psOp)
{
	Operation * psReturn = NULL;
	if (psOp) {
		switch (psOp->eOpType) {
			case OPTYPE_TRUTHVALUE:
				psReturn = CreateTruthValue (psOp->Vars.boTruth);
				break;
			case OPTYPE_VARIABLE:
				psReturn = CreateVariable (psOp->Vars.psVar->szVar);
				psReturn->Vars.psVar->psValue = psOp->Vars.psVar->psValue;
				break;
			case OPTYPE_UNARY:
				psReturn = CreateUnary (psOp->Vars.psUnary->eOpType,
					CopyRecursive (psOp->Vars.psUnary->psVar1));
				break;
			case OPTYPE_BINARY:
				psReturn = CreateBinary (psOp->Vars.psBinary->eOpType,
					CopyRecursive (psOp->Vars.psBinary->psVar1),
					CopyRecursive (psOp->Vars.psBinary->psVar2));
				break;
			case OPTYPE_QUANTIFIER:
				psReturn = CreateQuantifier (psOp->Vars.psQuantifier->eQuType,
					psOp->Vars.psQuantifier->szVar,
					CopyRecursive (psOp->Vars.psQuantifier->psVar1));
				break;
			case OPTYPE_RELATION:
				psReturn = CopyRelation (psOp);
				break;
			default:
				printf("Invalid operation type\n");
				break;
		}
	}
	return psReturn;
}

/**
 * Compare two formulae recursively. This will return true if and only if
 * the Operation and all its sub-Operations have the same content.
 *
 * @param psOp1 the Operation to compare against psOp2.
 * @param psOp2 the Operation to compare against psOp1.
 * @return true iff the two Operations have identical content.
 *
 */
bool CompareOperations (Operation const * psOp1, Operation const * psOp2) {
	bool boReturn = TRUE;

	if ((psOp1) && (psOp2)) {
		if (psOp1->eOpType == psOp2->eOpType) {
			switch (psOp1->eOpType) {
				case OPTYPE_TRUTHVALUE:
					if (psOp1->Vars.boTruth != psOp2->Vars.boTruth) {
						boReturn = FALSE;
					}
					break;
				case OPTYPE_VARIABLE:
					if (strcmp (psOp1->Vars.psVar->szVar, psOp2->Vars.psVar->szVar) != 0) {
						boReturn = FALSE;
					}
					break;
				case OPTYPE_UNARY:
					if (psOp1->Vars.psUnary->eOpType != psOp2->Vars.psUnary->eOpType) {
						boReturn = FALSE;
					}
					else {
						boReturn = CompareOperations (psOp1->Vars.psUnary->psVar1,
							psOp2->Vars.psUnary->psVar1);
					}
					break;
				case OPTYPE_BINARY:
					if (psOp1->Vars.psBinary->eOpType != psOp2->Vars.psBinary->eOpType) {
						boReturn = FALSE;
					}
					else {
						boReturn = (CompareOperations (psOp1->Vars.psBinary->psVar1,
							psOp2->Vars.psBinary->psVar1)
							&& CompareOperations (psOp1->Vars.psBinary->psVar2,
							psOp2->Vars.psBinary->psVar2));
					}
					break;
				case OPTYPE_QUANTIFIER:
					if (psOp1->Vars.psQuantifier->eQuType != psOp2->Vars.psQuantifier->eQuType) {
						boReturn = FALSE;
					}
					else {
						if (strcmp (psOp1->Vars.psQuantifier->szVar, psOp2->Vars.psQuantifier->szVar) != 0) {
							boReturn = FALSE;
						}
						else {
							boReturn = CompareOperations (psOp1->Vars.psQuantifier->psVar1,
								psOp2->Vars.psQuantifier->psVar1);
						}
					}
					break;
				case OPTYPE_RELATION:
					boReturn = RelationCompare (psOp1, psOp2);
					break;
				default:
					printf("Invalid operation type\n");
					break;
			}
		}
		else {
			boReturn = FALSE;
		}
	}
	else {
		boReturn = FALSE;
	}

	return boReturn;
}

/**
 * Search a formula for a given subformula. Performs a recursive comparison,
 * so there will only be a match if both the psFind Operation and its
 * sub-Operations match an Operation within psMain.
 *
 * @param psMain the Operation to search in.
 * @param psFind the Operation to search for.
 * @return pointer to the Operation found, or NULL o/w.
 *
 */
Operation * FindOperation (Operation * psMain, Operation * psFind) {
	Operation * psReturn = NULL;
	bool boSame;

	if ((psMain) && (psFind)) {
		switch (psMain->eOpType) {
			case OPTYPE_VARIABLE:
			case OPTYPE_TRUTHVALUE:
				boSame = CompareOperations (psMain, psFind);
				if (boSame) {
					psReturn = psMain;
				}
				break;
			case OPTYPE_UNARY:
				psReturn = FindOperation (psMain->Vars.psUnary->psVar1, psFind);
				if (!psReturn) {
					boSame = CompareOperations (psMain, psFind);
					if (boSame) {
						psReturn = psMain;
					}
				}
				break;
			case OPTYPE_BINARY:
				psReturn = FindOperation (psMain->Vars.psBinary->psVar1, psFind);
				if (!psReturn) {
					psReturn = FindOperation (psMain->Vars.psBinary->psVar2, psFind);
					if (!psReturn) {
						boSame = CompareOperations (psMain, psFind);
						if (boSame) {
							psReturn = psMain;
						}
					}
				}
				break;
			case OPTYPE_QUANTIFIER:
				psReturn = FindOperation (psMain->Vars.psQuantifier->psVar1, psFind);
				if (!psReturn) {
					boSame = CompareOperations (psMain, psFind);
					if (boSame) {
						psReturn = psMain;
					}
				}
				break;
			case OPTYPE_RELATION:
				boSame = RelationCompare (psMain, psFind);
				if (boSame) {
					psReturn = psMain;
				}
				break;
			default:
				printf("Invalid operation type\n");
				break;
		}
	}
	return psReturn;
}

/**
 * Substitute all instances of a given subformula for a formula. When found
 * the substituted formula will be a copy of psSub (rather than a pointer to
 * it). A substitution may cause the root operation to move in memory, so any
 * stored instances of psMain should be replaces by whatever this function
 * return.
 *
 * @param psMain the Operation to search in.
 * @param psFind the Operation to search for.
 * @param psSub the Operation to substitue instances of psFind for.
 * @return new pointer to the altered Operation. This may, or may not, be the
 *         same as psMain depending on whether a substitution occurs.
 *
 */
Operation * SubstituteOperation (Operation * psMain, Operation const * psFind, Operation const * psSub) {
	bool boFind;
	Operation * psReturn;

	boFind = SubstituteRecursive (psMain, psFind, psSub);
	if (boFind) {
		FreeRecursive (psMain);
		psReturn = CopyRecursive (psSub);
	}
	else {
		psReturn = psMain;
	}
	return psReturn;
}

/**
 * Substitute recursively all instances of a given subformula for a formula.
 * When found the substituted formula will be a copy of psSub (rather than a 
 * pointer to it).
 * Internal operation. Use SubstituteOperation instead.
 *
 * @param psMain the Operation to search in.
 * @param psFind the Operation to search for.
 * @param psSub the Operation to substitue instances of psFind for.
 * @return true iff the root Operation entirely matches psFind (and so should
 *         be substituted).
 *
 */
bool SubstituteRecursive (Operation * psMain, Operation const * psFind, Operation const * psSub) {
	bool boSubstitute = FALSE;
	bool boFind;

	if ((psMain) && (psSub)) {
		switch (psMain->eOpType) {
			case OPTYPE_VARIABLE:
			case OPTYPE_TRUTHVALUE:
				boSubstitute = CompareOperations (psMain, psFind);
				break;
			case OPTYPE_UNARY:
				boSubstitute = CompareOperations (psMain, psFind);
				if (!boSubstitute) {
					boFind = SubstituteRecursive (psMain->Vars.psUnary->psVar1, psFind, psSub);
					if (boFind) {
						FreeRecursive (psMain->Vars.psUnary->psVar1);
						psMain->Vars.psUnary->psVar1 = CopyRecursive (psSub);
					}
				}
				break;
			case OPTYPE_BINARY:
				boSubstitute = CompareOperations (psMain, psFind);
				if (!boSubstitute) {
					boFind = SubstituteRecursive (psMain->Vars.psBinary->psVar1,
						psFind, psSub);
					if (boFind) {
						FreeRecursive (psMain->Vars.psBinary->psVar1);
						psMain->Vars.psBinary->psVar1 = CopyRecursive (psSub);
					}
					boFind = SubstituteRecursive (psMain->Vars.psBinary->psVar2, psFind, psSub);
					if (boFind) {
						FreeRecursive (psMain->Vars.psBinary->psVar2);
						psMain->Vars.psBinary->psVar2 = CopyRecursive (psSub);
					}
				}
				break;
			case OPTYPE_QUANTIFIER:
				boSubstitute = CompareOperations (psMain, psFind);
				if (!boSubstitute) {
					boFind = SubstituteRecursive (psMain->Vars.psQuantifier->psVar1, psFind, psSub);
					if (boFind) {
						FreeRecursive (psMain->Vars.psQuantifier->psVar1);
						psMain->Vars.psQuantifier->psVar1 = CopyRecursive (psSub);
					}
				}
				break;
			case OPTYPE_RELATION:
				boSubstitute = RelationCompare (psMain, psFind);
				break;
			default:
				printf("Invalid operation type\n");
				break;
		}
	}
	return boSubstitute;
}

/**
 * Substitute all instances of a given pair of subformula for respective 
 * formulae. When found the substituted formulae will be a copy of psSub 
 * (rather than a pointer to it). A substitution may cause the root operation 
 * to move in memory, so any stored instances of psMain should be replaces by 
 * whatever this function return.
 * Note that this is different from applying SubstituteOperation twice. Use
 * of this function will ensure the two substitutions don't interact (for 
 * example, in the case where one substitution might otherwise cause a match
 * for the second substitution).
 *
 * @param psMain the Operation to search in.
 * @param psFind1 the first Operation to search for.
 * @param psSub1 the first Operation to substitue instances of psFind1 for.
 * @param psFind2 the second Operation to search for.
 * @param psSub2 the first Operation to substitue instances of psFind2 for.
 * @return new pointer to the altered Operation. This may, or may not, be the
 *         same as psMain depending on whether a substitution occurs.
 *
 */
Operation * SubstituteOperationPair (Operation * psMain, Operation const * psFind1, Operation const * psSub1, Operation const * psFind2, Operation const * psSub2) {
	int nFind;
	Operation * psReturn;

	nFind = SubstituteRecursivePair (psMain, psFind1, psSub1, psFind2, psSub2);
	switch (nFind) {
		default:
		case 0:
			psReturn = psMain;
			break;
		case 1:
			FreeRecursive (psMain);
			psReturn = CopyRecursive (psSub1);
			break;
		case 2:
			FreeRecursive (psMain);
			psReturn = CopyRecursive (psSub2);
			break;
	}
	return psReturn;
}

/**
 * Substitute recursively all instances of a given pair of subformula for 
 * respective formulae. When found the substituted formula will be a copy of 
 * psSub (rather than a pointer to it).
 * Note that this is different from applying SubstituteOperation twice. Use
 * of this function will ensure the two substitutions don't interact (for 
 * example, in the case where one substitution might otherwise cause a match
 * for the second substitution).
 * Internal operation. Use SubstituteOperationPair instead.
 *
 * @param psMain the Operation to search in.
 * @param psFind1 the first Operation to search for.
 * @param psSub1 the first Operation to substitue instances of psFind1 for.
 * @param psFind2 the second Operation to search for.
 * @param psSub2 the first Operation to substitue instances of psFind2 for.
 * @return 1 if the root Operation entirely matches psFind1;
 *         2 if the root Operation entirely matches psFind2;
 *         0 o/w.
 *         In the case of 1 or 2 the root operation should be entirely 
 *         substituted by psSub1 or psSub2 respectively.
 *
 */
int SubstituteRecursivePair (Operation * psMain, Operation const * psFind1, Operation const * psSub1, Operation const * psFind2, Operation const * psSub2) {
	int nSubstitute = 0;
	int nFind;

	if ((psMain) && (psSub1) && (psSub2)) {
		switch (psMain->eOpType) {
			case OPTYPE_TRUTHVALUE:
			case OPTYPE_VARIABLE:
				nSubstitute = CompareOperationsPair (psMain, psFind1, psFind2);
				break;
			case OPTYPE_UNARY:
				nSubstitute = CompareOperationsPair (psMain, psFind1, psFind2);
				if (nSubstitute == 0) {
					nFind = SubstituteRecursivePair (psMain->Vars.psUnary->psVar1,
						psFind1, psSub1, psFind2, psSub2);
					switch (nFind) {
						case 1:
							FreeRecursive (psMain->Vars.psUnary->psVar1);
							psMain->Vars.psUnary->psVar1 = CopyRecursive (psSub1);
							break;
						case 2:
							FreeRecursive (psMain->Vars.psUnary->psVar1);
							psMain->Vars.psUnary->psVar1 = CopyRecursive (psSub2);
							break;
						default:
							// Do nothing
							break;
					}
				}
				break;
			case OPTYPE_BINARY:
				nSubstitute = CompareOperationsPair (psMain, psFind1, psFind2);
				if (nSubstitute == 0) {
					nFind = SubstituteRecursivePair (psMain->Vars.psBinary->psVar1,
						psFind1, psSub1, psFind2, psSub2);
					switch (nFind) {
						case 1:
							FreeRecursive (psMain->Vars.psBinary->psVar1);
							psMain->Vars.psBinary->psVar1 = CopyRecursive (psSub1);
							break;
						case 2:
							FreeRecursive (psMain->Vars.psBinary->psVar1);
							psMain->Vars.psBinary->psVar1 = CopyRecursive (psSub2);
							break;
						default:
							// Do nothing
							break;
					}
					nFind = SubstituteRecursivePair (psMain->Vars.psBinary->psVar2,
						psFind1, psSub1, psFind2, psSub2);
					switch (nFind) {
						case 1:
							FreeRecursive (psMain->Vars.psBinary->psVar2);
							psMain->Vars.psBinary->psVar2 = CopyRecursive (psSub1);
							break;
						case 2:
							FreeRecursive (psMain->Vars.psBinary->psVar2);
							psMain->Vars.psBinary->psVar2 = CopyRecursive (psSub2);
							break;
						default:
							// Do nothing
							break;
					}
				}
				break;
			case OPTYPE_QUANTIFIER:
				nSubstitute = CompareOperationsPair (psMain, psFind1, psFind2);
				if (nSubstitute == 0) {
					nFind = SubstituteRecursivePair (psMain->Vars.psQuantifier->psVar1,
						psFind1, psSub1, psFind2, psSub2);
					switch (nFind) {
						case 1:
							FreeRecursive (psMain->Vars.psQuantifier->psVar1);
							psMain->Vars.psQuantifier->psVar1 = CopyRecursive (psSub1);
							break;
						case 2:
							FreeRecursive (psMain->Vars.psQuantifier->psVar1);
							psMain->Vars.psQuantifier->psVar1 = CopyRecursive (psSub2);
							break;
						default:
							// Do nothing
							break;
					}
				}
				break;
			case OPTYPE_RELATION:
				nSubstitute = CompareOperationsPair (psMain, psFind1, psFind2);
				break;
			default:
				printf("Invalid operation type\n");
				break;
		}
	}
	return nSubstitute;
}

/**
 * Substitute all instances of a given array of subformula for respective
 * formulae. When found the substituted formulae will be a copy of an instance
 * from apsSub rather than a pointer to it). A substitution may cause the root
 * operation to move in memory, so any stored instances of psMain should be
 * replaced by whatever this function return.
 * Note that this is different from applying SubstituteOperation multiple
 * times. Use of this function will ensure all substitutions are applied
 * without interacting (for example, in the case where one substitution
 * might otherwise cause a match for a later substitution).
 *
 * @param psMain the Operation to search in.
 * @param apsFind an array of Operation instances to search for.
 * @param apsSub an array of Operation instances to substitue instances of psFind for.
 * @param nCount the length of the arrays (they must both be the same length).
 * @return new pointer to the altered Operation. This may, or may not, be the
 *         same as psMain depending on whether a substitution occurs.
 *
 */
Operation * SubstituteOperationMany (Operation * psMain, Operation ** apsFind, Operation const ** apsSub, int nCount) {
	int nFind;
	Operation * psReturn;
	bool boNull;

	boNull = FALSE;
	for (nFind = 0; (nFind < nCount) && !boNull; ++nFind) {
		boNull = (apsSub[nFind] == NULL);
	}

	if (boNull) {
		psReturn = psMain;
	}
	else {
		nFind = SubstituteRecursiveMany (psMain, apsFind, apsSub, nCount);
		if (nFind == 0) {
			psReturn = psMain;
		}
		else {
			FreeRecursive (psMain);
			psReturn = CopyRecursive (apsSub[(nFind - 1)]);
		}
	}

	return psReturn;
}

/**
 * Substitute recursively all instances of a given array of subformula for
 * respective formulae. When found the substituted formulae will be a copy of
 * an instance from apsSub rather than a pointer to it). A substitution may
 * cause the root operation to move in memory, so any stored instances of
 * psMain should be replaced by whatever this function return.
 * Note that this is different from applying SubstituteOperation multiple
 * times. Use of this function will ensure all substitutions are applied
 * without interacting (for example, in the case where one substitution
 * might otherwise cause a match for a later substitution).
 * Internal operation. Use SubstituteOperationPair instead.
 *
 * @param psMain the Operation to search in.
 * @param apsFind an array of Operation instances to search for.
 * @param apsSub an array of Operation instances to substitue instances of psFind for.
 * @param nCount the length of the arrays (they must both be the same length).
 * @return an integer representing which item in the apsFind array was matched;
 *         This is an enumeration, not an index, so the first item returns the value1
 *         the second item the value 2 and so on.
 *         0 is returned if there is no match.
 *         In the case a non-zero value the root operation should be entirely
 *         substituted the respective entry in apsSub.
 *
 */
int SubstituteRecursiveMany (Operation * psMain, Operation ** apsFind, Operation const ** apsSub, int nCount) {
	int nSubstitute = 0;
	int nFind;
	int nPos;

	if (psMain != NULL) {
		switch (psMain->eOpType) {
			case OPTYPE_TRUTHVALUE:
			case OPTYPE_VARIABLE:
				nSubstitute = CompareOperationsMany (psMain, apsFind, nCount);
				break;
			case OPTYPE_UNARY:
				nSubstitute = CompareOperationsMany (psMain, apsFind, nCount);
				if (nSubstitute == 0) {
					nFind = SubstituteRecursiveMany (psMain->Vars.psUnary->psVar1,
						apsFind, apsSub, nCount);
					if (nFind != 0) {
						FreeRecursive (psMain->Vars.psUnary->psVar1);
						psMain->Vars.psUnary->psVar1 = CopyRecursive (apsSub[(nFind - 1)]);
					}
				}
				break;
			case OPTYPE_BINARY:
				nSubstitute = CompareOperationsMany (psMain, apsFind, nCount);
				if (nSubstitute == 0) {
					nFind = SubstituteRecursiveMany (psMain->Vars.psBinary->psVar1,
						apsFind, apsSub, nCount);
					if (nFind != 0) {
						FreeRecursive (psMain->Vars.psBinary->psVar1);
						psMain->Vars.psBinary->psVar1 = CopyRecursive (apsSub[(nFind - 1)]);
					}

					nFind = SubstituteRecursiveMany (psMain->Vars.psBinary->psVar2,
						apsFind, apsSub, nCount);
					if (nFind != 0) {
						FreeRecursive (psMain->Vars.psBinary->psVar2);
						psMain->Vars.psBinary->psVar2 = CopyRecursive (apsSub[(nFind - 1)]);
					}
				}
				break;
			case OPTYPE_QUANTIFIER:
				nSubstitute = CompareOperationsMany (psMain, apsFind, nCount);
				if (nSubstitute == 0) {
					nFind = SubstituteRecursiveMany (psMain->Vars.psQuantifier->psVar1,
						apsFind, apsSub, nCount);
					if (nFind != 0) {
						FreeRecursive (psMain->Vars.psQuantifier->psVar1);
						psMain->Vars.psQuantifier->psVar1 = CopyRecursive (apsSub[(nFind - 1)]);
					}
				}
				break;
			case OPTYPE_RELATION:
				nSubstitute = CompareOperationsMany (psMain, apsFind, nCount);
				break;
			default:
				printf("Invalid operation type\n");
				break;
		}
	}
	return nSubstitute;
}

/**
 * Compare a pair of formulae against another formula.
 *
 * @param psMain the Operation to compare to.
 * @param psCompare1 the first Operation to compare against.
 * @param psCompare2 the second Operation to compare against.
 * @return 1 if psMain entirely matches psCompare1;
 *         2 if psMain entirely matches psCompare2;
 *         0 o/w.
 *
 */
int CompareOperationsPair (Operation * psMain, Operation const * psCompare1, Operation const * psCompare2) {
	int nReturn = 0;

	if (CompareOperations (psMain, psCompare1)) {
		nReturn = 1;
	}
	else {
		if (CompareOperations (psMain, psCompare2)) {
			nReturn = 2;
		}
	}
	return nReturn;
}

/**
 * Compare multiple formulae against a single formula.
 *
 * @param psMain the Operation to compare to.
 * @param apsCompare an array of Operation instances to compare against.
 * @param nCount the number of operations in the array.
 * @return an integer representing which item in the array matched.
 *         This is an enumeration not an index, so the first item takes the value 1,
 *         the second item the value 2 and so on;
 *         0 if there is no match.
 *
 */
int CompareOperationsMany (Operation * psMain, Operation ** apsCompare, int nCount) {
	int nReturn = 0;
	int nPos;

	for (nPos = 0; (nPos < nCount) && nReturn == 0; ++nPos) {
		if (CompareOperations (psMain, apsCompare[nPos])) {
			nReturn = nPos + 1;
		}
	}
	return nReturn;
}

// TODO: Remove the following functions

void PrintOperation (Operation const * psOp) {
	char *szString;
	int nLength;

	nLength = OperationToStringLength (psOp) + 1;
	szString = PropMalloc (nLength);
	OperationToString (psOp, szString, nLength);
	printf("Operation: %s\n", szString);
	PropFree (szString);
}

QUANTIFIER QuantifierGetType(Operation const* psOp) {
	QUANTIFIER eQuType = QUANTIFIER_INVALID;

	if (psOp->eOpType == OPTYPE_QUANTIFIER) {
		eQuType = psOp->Vars.psQuantifier->eQuType;
	}

	return eQuType;
}

char const* QuantifierGetVariable(Operation const* psOp) {
	char* szVariable = NULL;

	if (psOp->eOpType == OPTYPE_QUANTIFIER) {
		szVariable = psOp->Vars.psQuantifier->szVar;
	}

	return szVariable;
}

Operation const* QuantifierGetSub(Operation const* psOp) {
	Operation* psResult = NULL;

	if (psOp->eOpType == OPTYPE_QUANTIFIER) {
		psResult = psOp->Vars.psQuantifier->psVar1;
	}

	return psResult;
}

/**
 * Compare two formulae recursively. This will return true if and only if
 * the Operation and all its sub-Operations have the same content.
 *
 * @param psOp1 the Operation to compare against psOp2.
 * @param psOp2 the Operation to compare against psOp1.
 * @return true iff the two Operations have identical content.
 *
 */
bool CompareOperationPatterns (Operation const * psOp1, Operation const * psOp2) {
	bool boResult;
    VarStack * psVarStack1;
    VarStack * psVarStack2;

    psVarStack1 = CreateVarStack();
    psVarStack2 = CreateVarStack();

	boResult = CompareOperationPatternsRecursive (psOp1, psOp2, psVarStack1, psVarStack2);

    psVarStack1 = FreeVarStack (psVarStack1);
    psVarStack2 = FreeVarStack (psVarStack2);

	return boResult;
}

bool CompareOperationPatternsRecursive (Operation const * psOp1, Operation const * psOp2, VarStack * psVarStack1, VarStack * psVarStack2) {
	bool boReturn = TRUE;

	if ((psOp1) && (psOp2)) {
		if (psOp1->eOpType == psOp2->eOpType) {
			switch (psOp1->eOpType) {
				case OPTYPE_TRUTHVALUE:
					if (psOp1->Vars.boTruth != psOp2->Vars.boTruth) {
						boReturn = FALSE;
					}
					break;
				case OPTYPE_VARIABLE:
					if (strcmp (psOp1->Vars.psVar->szVar, psOp2->Vars.psVar->szVar) != 0) {
						boReturn = FALSE;
					}
					break;
				case OPTYPE_UNARY:
					if (psOp1->Vars.psUnary->eOpType != psOp2->Vars.psUnary->eOpType) {
						boReturn = FALSE;
					}
					else {
						boReturn = CompareOperationPatternsRecursive (psOp1->Vars.psUnary->psVar1, psOp2->Vars.psUnary->psVar1, psVarStack1, psVarStack2);
					}
					break;
				case OPTYPE_BINARY:
					if (psOp1->Vars.psBinary->eOpType != psOp2->Vars.psBinary->eOpType) {
						boReturn = FALSE;
					}
					else {
						boReturn = (CompareOperationPatternsRecursive (psOp1->Vars.psBinary->psVar1, psOp2->Vars.psBinary->psVar1, psVarStack1, psVarStack2)
							&& CompareOperationPatternsRecursive (psOp1->Vars.psBinary->psVar2, psOp2->Vars.psBinary->psVar2, psVarStack1, psVarStack2));
					}
					break;
				case OPTYPE_QUANTIFIER:
					if (psOp1->Vars.psQuantifier->eQuType != psOp2->Vars.psQuantifier->eQuType) {
						boReturn = FALSE;
					}
					else {
					    VarStackPush (psVarStack1, psOp1->Vars.psQuantifier->szVar);
					    VarStackPush (psVarStack2, psOp2->Vars.psQuantifier->szVar);
						boReturn = CompareOperationPatternsRecursive (psOp1->Vars.psQuantifier->psVar1, psOp2->Vars.psQuantifier->psVar1, psVarStack1, psVarStack2);
					    VarStackDrop (psVarStack2);
					    VarStackDrop (psVarStack1);
					}
					break;
				case OPTYPE_RELATION:
			        boReturn = RelationComparePatternStack (psOp1, psOp2, psVarStack1, psVarStack2);
					break;
				default:
					printf("Invalid operation type\n");
					break;
			}
		}
		else {
			boReturn = FALSE;
		}
	}
	else {
		boReturn = FALSE;
	}

	return boReturn;
}
