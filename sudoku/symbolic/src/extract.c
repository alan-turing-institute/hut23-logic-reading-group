/**
 * Symbolic
 *
 * @file
 * @author  David Llewellyn-Jones <david@flypig.co.uk>
 * @version 1.0
 *
 * @section LICENSE
 *
 * The MIT License
 * See symbolic.h, COPYING file or website for licence
 *
 * @section DESCRIPTION
 *
 * Library for the construction of nested symbolic propositions.
 * The Flying Pig!
 * Started 5/8/2003
 * http://www.flypig.co.uk?to=symbolic
 *
 * Code for creating, manipulating and destroying propositions
 * and Operations. Manipulations supported include copy,
 * search, substitution and comparison.
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
#include <assert.h>

//////////////////////////////////////////////////////////////////
// Defines

//////////////////////////////////////////////////////////////////
// Structures

//////////////////////////////////////////////////////////////////
// Global variables

//////////////////////////////////////////////////////////////////
// Function prototypes

Extract * CreateExtract ();
bool ExtractRecursive (Extract * psExtract, Operation * psPattern, Operation * psScrutinee, VarStack * psPatternVars, VarStack * psScrutineeVars);
void ReplaceUnboundRecurse (Operation * psOp, char const * const szVarFrom, char const * const szVarTo);
int OccursUnboundRecurse (Operation const * psOp, char const * const szVar);
char const* FindFirstVaraibleDiffRecurse (Operation const* psOpFrom, Operation const* psOpTo);
int OperationInputListRecurse (Operation const * psOp, VarStack * psVarStack, VarStack * psInputs);
int ExtractSubstituteRecursive (Extract const * psExtract, Operation * psMain, VarStack * psVarStack);
void ReplaceUnboundRecurseMany (Operation * psOp, VarStack const * const psVarsFrom, VarStack const * const psVarsTo, VarStack * psVarStack);
bool ExtractCheckSubstitutionRecursive (Extract const * psExtract, Operation const * psMain, VarStack * psVarStack);
bool ExtractCheckRecursive (Extract * psExtract, Operation * psPattern, VarStack * psPatternVars);
bool ExtractSubstituteCheckRecursive (Extract const * psExtract, Operation const * psMain, Operation const * psResult, VarStack * psMainVars, VarStack * psResultVars);

//////////////////////////////////////////////////////////////////
// Main application

Extract * CreateExtract() {
    Extract * psExtract = NULL;

    psExtract = PropCalloc (1, sizeof (Extract));
    psExtract->psVarsFrom = CreateVarStack ();
    psExtract->psVarsTo = CreateVarStack ();

    return psExtract;
}

Extract * FreeExtract (Extract * psExtract) {
    int nPos;

    if (psExtract) {
        if (psExtract->apsOps) {
            for (nPos = 0; nPos < psExtract->nCount; ++nPos) {
                psExtract->apsOps[nPos] = FreeOperationMap(psExtract->apsOps[nPos]);
            }

            PropFree (psExtract->apsOps);
            psExtract->apsOps = NULL;
        }

        psExtract->psVarsFrom = FreeVarStack (psExtract->psVarsFrom);
        psExtract->psVarsTo = FreeVarStack (psExtract->psVarsTo);

        PropFree (psExtract);
    }

    return NULL;
}

Extract * ExtractPattern (Operation * psPattern, Operation * psScrutinee) {
    Extract * psExtract = NULL;
    bool boResult;
    int nRelationCount;
    int nPos;
    Operation const * psOp;
    RelationList * psRelationList;
    VarStack * psPatternVars;
    VarStack * psScrutineeVars;
    int nUnbound;

    // Patterns must have all of their variables bound
    nUnbound = OperationArity (psPattern);
    if (nUnbound == 0) {
        psExtract = CreateExtract ();
        psRelationList = CreateRelationList();
        psPatternVars = CreateVarStack();
        psScrutineeVars = CreateVarStack();

        RelationListExtract (psRelationList, psPattern);
        nRelationCount = RelationListCount (psRelationList);
        psExtract->nCount = nRelationCount;

        if (nRelationCount > 0) {
            psExtract->apsOps = PropCalloc (nRelationCount, sizeof(OperationMap *));

            for (nPos = 0; nPos < nRelationCount; ++nPos) {
                psExtract->apsOps[nPos] = CreateOperationMap ();

                psOp = RelationListGet (psRelationList, nPos);

                OperationMapSetFrom (psExtract->apsOps[nPos], psOp);
            }
        }

        psRelationList = FreeRelationList (psRelationList);
        boResult = ExtractRecursive (psExtract, psPattern, psScrutinee, psPatternVars, psScrutineeVars);

        psScrutineeVars = FreeVarStack (psScrutineeVars);
        psPatternVars = FreeVarStack (psPatternVars);

        if (boResult == FALSE) {
            psExtract = FreeExtract(psExtract);
        }
    }

    return psExtract;
}

bool ExtractRecursive (Extract * psExtract, Operation * psPattern, Operation * psScrutinee, VarStack * psPatternVars, VarStack * psScrutineeVars) {
    bool boSuccess = FALSE;
    Operation * psRelation;
    OperationMap * psOperationMap;
    int nVarFrom;
    int nVarTo;

    switch (psPattern->eOpType) {
        case OPTYPE_TRUTHVALUE:
            // Intentional fallthrough
        case OPTYPE_VARIABLE:
            // Intentional fallthrough
        default: {
            boSuccess = CompareOperations (psPattern, psScrutinee);
        }
        break;
        case OPTYPE_UNARY: {
            if ((psScrutinee->eOpType == OPTYPE_UNARY) && (psPattern->Vars.psUnary->eOpType == psScrutinee->Vars.psUnary->eOpType)) {
                boSuccess = ExtractRecursive (psExtract, psPattern->Vars.psUnary->psVar1, psScrutinee->Vars.psUnary->psVar1, psPatternVars, psScrutineeVars);
            }
        }
        break;
        case OPTYPE_BINARY: {
            if ((psScrutinee->eOpType == OPTYPE_BINARY) && (psPattern->Vars.psBinary->eOpType == psScrutinee->Vars.psBinary->eOpType)) {
                boSuccess = ExtractRecursive (psExtract, psPattern->Vars.psBinary->psVar1, psScrutinee->Vars.psBinary->psVar1, psPatternVars, psScrutineeVars) && ExtractRecursive (psExtract, psPattern->Vars.psBinary->psVar2, psScrutinee->Vars.psBinary->psVar2, psPatternVars, psScrutineeVars);
            }
        }
        break;
        case OPTYPE_QUANTIFIER: {
            if ((psScrutinee->eOpType == OPTYPE_QUANTIFIER) && (psPattern->Vars.psQuantifier->eQuType == psScrutinee->Vars.psQuantifier->eQuType)) {
                nVarFrom = VarStackFind (psExtract->psVarsFrom, psPattern->Vars.psQuantifier->szVar);
                nVarTo = VarStackFind (psExtract->psVarsTo, psScrutinee->Vars.psQuantifier->szVar);

                if (nVarFrom == nVarTo) {
                    if (nVarFrom < 0) {
                        VarStackPush (psExtract->psVarsFrom, psPattern->Vars.psQuantifier->szVar);
                        VarStackPush (psExtract->psVarsTo, psScrutinee->Vars.psQuantifier->szVar);
                    }

                    VarStackPush (psPatternVars, psPattern->Vars.psQuantifier->szVar);
                    VarStackPush (psScrutineeVars, psScrutinee->Vars.psQuantifier->szVar);

                    boSuccess = ExtractRecursive (psExtract, psPattern->Vars.psQuantifier->psVar1, psScrutinee->Vars.psQuantifier->psVar1, psPatternVars, psScrutineeVars);

                    VarStackDrop(psPatternVars);
                    VarStackDrop(psScrutineeVars);
                }
                else {
                    boSuccess = FALSE;
                }
            }
        }
        break;
        case OPTYPE_RELATION: {
            //boSuccess = VarStackMatchUnbound (psPatternVars, psScrutinee);
            boSuccess = TRUE;

            if (boSuccess) {
                psOperationMap = ExtractOperationMap (psExtract, psPattern);
                assert(psOperationMap != NULL);

                boSuccess = OperationMapSetTo (psOperationMap, psScrutinee, psScrutineeVars);

                if (boSuccess) {
                    boSuccess = OperationMapVarOriginsClear (psOperationMap, psPattern, psScrutinee, psPatternVars, psScrutineeVars);
                }
            }
        }
        break;
    }

    return boSuccess;
}

Extract * ExtractPatternMany (Operation ** apsPattern, Operation ** apsScrutinee, int nCount) {
    Extract * psExtract;
    bool boResult;
    int nRelationCount;
    int nPos;
    Operation const * psOp;
    RelationList * psRelationList;
    VarStack * psPatternVars;
    VarStack * psScrutineeVars;
    int nUnbound;

    boResult = TRUE;
    // Patterns must have all of their variables bound
    for (nPos = 0; (nPos < nCount) && boResult; ++nPos) {
        nUnbound = OperationArity (apsPattern[nPos]);
        if (nUnbound != 0) {
            boResult = FALSE;
        }
    }

    if (boResult) {
        psExtract = CreateExtract ();
        psRelationList = CreateRelationList ();
        psPatternVars = CreateVarStack ();
        psScrutineeVars = CreateVarStack ();

        for (nPos = 0; nPos < nCount; ++nPos) {
            RelationListExtract (psRelationList, apsPattern[nPos]);
        }
        nRelationCount = RelationListCount (psRelationList);
        psExtract->nCount = nRelationCount;

        if (nRelationCount > 0) {
            psExtract->apsOps = PropCalloc (nRelationCount, sizeof(OperationMap *));

            for (nPos = 0; nPos < nRelationCount; ++nPos) {
                psExtract->apsOps[nPos] = CreateOperationMap ();

                psOp = RelationListGet (psRelationList, nPos);

                OperationMapSetFrom (psExtract->apsOps[nPos], psOp);
            }
        }

        psRelationList = FreeRelationList (psRelationList);

        boResult = TRUE;
        for (nPos = 0; (nPos < nCount) && boResult; ++nPos) {
            boResult = ExtractRecursive (psExtract, apsPattern[nPos], apsScrutinee[nPos], psPatternVars, psScrutineeVars);
        }

        psScrutineeVars = FreeVarStack (psScrutineeVars);
        psPatternVars = FreeVarStack (psPatternVars);

        if (boResult == FALSE) {
            psExtract = FreeExtract (psExtract);
        }
    }

    return psExtract;
}

int ExtractCount (Extract * psExtract) {
    return psExtract->nCount;
}

Operation const * ExtractRelation(Extract const * psExtract, int nPosition) {
    Operation const * psOp = NULL;

    if ((nPosition >= 0) && (nPosition < psExtract->nCount)) {
        psOp = OperationMapGetFrom (psExtract->apsOps[nPosition]);
    }

    return psOp;
}

Operation const * ExtractValueFromPos (Extract const * psExtract, int nPosition) {
    Operation const * psValue = NULL;
    int nPos;

    if ((nPosition >= 0) && (nPosition < psExtract->nCount)) {
        psValue = OperationMapGetTo (psExtract->apsOps[nPosition]);
    }

    return psValue;
}

Operation const * ExtractValue (Extract const * psExtract, Operation const * const psRelation) {
    Operation const * psValue = NULL;
    int nPos;
    Operation const * psFrom;

    nPos = 0;
    while ((psValue == NULL) && (nPos < psExtract->nCount)) {
        psFrom = OperationMapGetFrom (psExtract->apsOps[nPos]);

        if (RelationComparePattern(psRelation, psFrom)) {
            psValue = OperationMapGetTo (psExtract->apsOps[nPos]);
        }
        nPos += 1;
    }

    return psValue;
}

OperationMap * ExtractOperationMap (Extract const * psExtract, Operation const * const psRelation) {
    OperationMap * psMap = NULL;
    int nPos;
    Operation const * psFrom;

    nPos = 0;
    while ((psMap == NULL) && (nPos < psExtract->nCount)) {
        psFrom = OperationMapGetFrom (psExtract->apsOps[nPos]);

        if (RelationComparePattern(psRelation, psFrom)) {
            psMap = psExtract->apsOps[nPos];
        }
        nPos += 1;
    }

    return psMap;
}

void ReplaceUnbound (Operation * psOp, char const * const szVarFrom, char const * const szVarTo) {
    ReplaceUnboundRecurse (psOp, szVarFrom, szVarTo);
}

void ReplaceUnboundRecurse (Operation * psOp, char const * const szVarFrom, char const * const szVarTo) {
    size_t nVar;

    // Check the operations recursively
    if (psOp) {
        switch (psOp->eOpType) {
            case OPTYPE_TRUTHVALUE:
                // Nothing to do
                break;
            case OPTYPE_VARIABLE:
                // Nothing to do
                break;
            case OPTYPE_UNARY:
                ReplaceUnboundRecurse (psOp->Vars.psUnary->psVar1, szVarFrom, szVarTo);
                break;
            case OPTYPE_BINARY:
                ReplaceUnboundRecurse (psOp->Vars.psBinary->psVar1, szVarFrom, szVarTo);
                ReplaceUnboundRecurse (psOp->Vars.psBinary->psVar2, szVarFrom, szVarTo);
                break;
            case OPTYPE_QUANTIFIER:
                // Once a variable is bound it'll be bound in all subformulae
                // So only recursive if we're not binding the variable
                if (strcmp(szVarFrom, psOp->Vars.psQuantifier->szVar) != 0) {
                    ReplaceUnboundRecurse (psOp->Vars.psQuantifier->psVar1, szVarFrom, szVarTo);
                }
                break;
            case OPTYPE_RELATION:
                for (nVar = 0; nVar < psOp->Vars.psRelation->nArity; ++nVar) {
                    // Replace any instances of szVarFrom with szVarTo
                    if (strcmp (szVarFrom, psOp->Vars.psRelation->aszVar[nVar]) == 0) {
                        PropFree (psOp->Vars.psRelation->aszVar[nVar]);
                        psOp->Vars.psRelation->aszVar[nVar] = (char *)PropMalloc (strlen (szVarTo) + 1);
                        strcpy (psOp->Vars.psRelation->aszVar[nVar], szVarTo);
                    }
                }
                break;
            default:
                // Not something we know about (shouldn't happen)
                printf("Invalid operation type\n");
                break;
        }
    }
}

int OccursUnbound (Operation const * psOp, char const * const szVar) {
   return OccursUnboundRecurse (psOp, szVar);
}

int OccursUnboundRecurse (Operation const * psOp, char const * const szVar) {
    size_t nVar;
    int nOccurences = 0;

    // Check the operations recursively
    if (psOp) {
        switch (psOp->eOpType) {
            case OPTYPE_TRUTHVALUE:
                // Nothing to do
                break;
            case OPTYPE_VARIABLE:
                // Nothing to do
                break;
            case OPTYPE_UNARY:
                nOccurences = OccursUnboundRecurse(psOp->Vars.psUnary->psVar1, szVar);
                break;
            case OPTYPE_BINARY:
                nOccurences += OccursUnboundRecurse(psOp->Vars.psBinary->psVar1, szVar);
                nOccurences += OccursUnboundRecurse(psOp->Vars.psBinary->psVar2, szVar);
                break;
            case OPTYPE_QUANTIFIER:
                // Once a variable is bound it'll be bound in all subformulae
                // So only recursive if we're not binding the variable
                if (strcmp(szVar, psOp->Vars.psQuantifier->szVar) != 0) {
                    nOccurences += OccursUnboundRecurse(psOp->Vars.psQuantifier->psVar1, szVar);
                }
                break;
            case OPTYPE_RELATION:
                nVar = 0;
                while (nVar < psOp->Vars.psRelation->nArity) {
                    // Replace any instances of szVarFrom with szVarTo
                    if (strcmp (szVar, psOp->Vars.psRelation->aszVar[nVar]) == 0) {
                        nOccurences += 1;
                    }
                    nVar += 1;
                }
                break;
            default:
                // Not something we know about (shouldn't happen)
                printf("Invalid operation type\n");
                break;
        }
    }

    return nOccurences;
}

int OperationArity (Operation const * psOp) {
    return OperationInputList (psOp, NULL);
}

int OperationInputList (Operation const * psOp, VarStack * psInputs) {
    VarStack * psVarStack;
    int nResult;

    psVarStack = CreateVarStack ();
    nResult = OperationInputListRecurse (psOp, psVarStack, psInputs);
    psVarStack = FreeVarStack (psVarStack);

    return nResult;
}

int OperationInputListRecurse (Operation const * psOp, VarStack * psVarStack, VarStack * psInputs) {
    size_t nVar;
    int nArity = 0;
    bool boBound;

    // Check the operations recursively
    if (psOp) {
        switch (psOp->eOpType) {
            case OPTYPE_TRUTHVALUE:
                // Nothing to do
                break;
            case OPTYPE_VARIABLE:
                // Nothing to do
                break;
            case OPTYPE_UNARY:
                nArity = OperationInputListRecurse(psOp->Vars.psUnary->psVar1, psVarStack, psInputs);
                break;
            case OPTYPE_BINARY:
                nArity += OperationInputListRecurse(psOp->Vars.psBinary->psVar1, psVarStack, psInputs);
                nArity += OperationInputListRecurse(psOp->Vars.psBinary->psVar2, psVarStack, psInputs);
                break;
            case OPTYPE_QUANTIFIER:
                VarStackPush(psVarStack, psOp->Vars.psQuantifier->szVar);
                nArity += OperationInputListRecurse(psOp->Vars.psQuantifier->psVar1, psVarStack, psInputs);
                VarStackDrop(psVarStack);
                break;
            case OPTYPE_RELATION:
                nVar = 0;
                while (nVar < psOp->Vars.psRelation->nArity) {
                    // Establsih how many unbound inputs there are
                    boBound = VarStackContains (psVarStack, psOp->Vars.psRelation->aszVar[nVar]);
                    if (!boBound) {
                        if (psInputs) {
                            VarStackPush (psInputs, psOp->Vars.psRelation->aszVar[nVar]);
                        }
                        nArity += 1;
                    }
                    nVar += 1;
                }
                break;
            default:
                // Not something we know about (shouldn't happen)
                printf("Invalid operation type\n");
                break;
        }
    }

    return nArity;
}

bool ExtractSubstituteValidate (Extract const * psExtract, Operation const * psMain, char const** pszError) {
    bool boSuccess = TRUE;
    int nFind;

    for (nFind = 0; (nFind < psExtract->nCount) && boSuccess; ++nFind) {
        // The mapping between to and from variables must be unique
        boSuccess = OperationMapVarMappingUnique (psExtract->apsOps[nFind]);

        if ((!boSuccess) && pszError) {
            *pszError = "The mapping between the premise and result pattern variables is not unique.";
        }
    }

    if (boSuccess) {
        // Check everything that applies to the non-unique case as well
        boSuccess = ExtractSubstituteCheckValidate (psExtract, psMain, pszError);
    }

    return boSuccess;
}

bool ExtractSubstituteCheckValidate (Extract const * psExtract, Operation const * psMain, char const** pszError) {
    bool boSuccess = TRUE;
    int nFind;
    int nUnbound;

    for (nFind = 0; (nFind < psExtract->nCount) && boSuccess; ++nFind) {
        if ((psExtract->apsOps == NULL) || (psExtract->apsOps[nFind]->psFrom == NULL) || (psExtract->apsOps[nFind]->psTo == NULL)) {
            // The extraction hasn't been completed successfully
            boSuccess = FALSE;
            if (pszError) {
                *pszError = "Pattern extraction failed.";
            }
        }
    }

	if (boSuccess) {
        nUnbound = OperationArity (psMain);
        if (nUnbound != 0) {
            // The result pattern contains unbound variables
            boSuccess = FALSE;
            if (pszError) {
                *pszError = "The result pattern cannot contain unbound variables.";
            }
        }
	}

    if (boSuccess) {
        // Ensure that no variables that shouldn't be bound get bound by the substitution
        boSuccess = ExtractCheckSubstitution (psExtract, psMain);
        if (pszError) {
            *pszError = "The substitution would result in a named object becoming bound.";
        }
    }

    return boSuccess;
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
Operation * ExtractSubstitute (Extract const * psExtract, Operation * psMain) {
    int nFind;
    Operation * psReturn;
    VarStack * psVarStack;

    psVarStack = CreateVarStack ();
    nFind = ExtractSubstituteRecursive (psExtract, psMain, psVarStack);
    if (nFind == 0) {
        psReturn = psMain;
    }
    else {
        ExtractPerformSubstitution (psExtract, &psMain, (nFind - 1));
        psReturn = psMain;
    }

    psVarStack = FreeVarStack (psVarStack);

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
int ExtractSubstituteRecursive (Extract const * psExtract, Operation * psMain, VarStack * psVarStack) {
    int nSubstitute = 0;
    int nFind;
    int nLength;
    char const * szVarTo;

    if (psMain != NULL) {
        switch (psMain->eOpType) {
            case OPTYPE_TRUTHVALUE:
            case OPTYPE_VARIABLE:
                // Do nothing
                break;
            case OPTYPE_UNARY:
                nFind = ExtractSubstituteRecursive (psExtract, psMain->Vars.psUnary->psVar1, psVarStack);
                if (nFind != 0) {
                    ExtractPerformSubstitution (psExtract, &psMain->Vars.psUnary->psVar1, (nFind - 1));
                }
                break;
            case OPTYPE_BINARY:
                nFind = ExtractSubstituteRecursive (psExtract, psMain->Vars.psBinary->psVar1, psVarStack);
                if (nFind != 0) {
                    ExtractPerformSubstitution (psExtract, &psMain->Vars.psBinary->psVar1, (nFind - 1));
                }

                nFind = ExtractSubstituteRecursive (psExtract, psMain->Vars.psBinary->psVar2, psVarStack);
                if (nFind != 0) {
                    ExtractPerformSubstitution (psExtract, &psMain->Vars.psBinary->psVar2, (nFind - 1));
                }
                break;
            case OPTYPE_QUANTIFIER:
                VarStackPush (psVarStack, psMain->Vars.psQuantifier->szVar);

                nFind = VarStackFind (psExtract->psVarsFrom, psMain->Vars.psQuantifier->szVar);
                if (nFind >= 0) {
                    szVarTo = VarStackGet (psExtract->psVarsTo, nFind);
                    nLength = strlen (szVarTo);
                    psMain->Vars.psQuantifier->szVar = PropRealloc (psMain->Vars.psQuantifier->szVar, (nLength + 1) * sizeof (char));
                    strcpy (psMain->Vars.psQuantifier->szVar, szVarTo);
                }

                nFind = ExtractSubstituteRecursive (psExtract, psMain->Vars.psQuantifier->psVar1, psVarStack);
                if (nFind != 0) {
                    ExtractPerformSubstitution (psExtract, &psMain->Vars.psQuantifier->psVar1, (nFind - 1));
                }

                VarStackDrop (psVarStack);
                break;
            case OPTYPE_RELATION:
                nSubstitute = ExtractCompareOperationsMany (psExtract, psMain, psVarStack);
                break;
            default:
                printf("Invalid operation type\n");
                break;
        }
    }
    return nSubstitute;
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
int ExtractCompareOperationsMany (Extract const * psExtract, Operation const * psMain, VarStack const * psVarStack) {
    int nReturn = 0;
    int nPos;

    for (nPos = 0; (nPos < psExtract->nCount) && nReturn == 0; ++nPos) {
        if (RelationComparePattern (psMain, psExtract->apsOps[nPos]->psFrom)) {
            nReturn = nPos + 1;
        }
    }
    return nReturn;
}

void ExtractPerformSubstitution (Extract const * psExtract, Operation ** psFrom, int nFind) {
    OperationMap const * psOperationMap;
    int nArityFrom;
    int nArityTo;
    int nPosFrom;
    int nPosTo;
    char const * szVarFrom;
    char const * szVarTo;
    VarStack * psInputsFrom;
    VarStack * psInputsTo;
    int nMapFrom;
    VarStack * psReplacements;
    int nReplace;

    psOperationMap = psExtract->apsOps[nFind];

    psInputsTo = CreateVarStack ();
    psInputsFrom = CreateVarStack ();

    nArityFrom = OperationInputList (*psFrom, psInputsFrom);
    nArityTo = OperationInputList (psOperationMap->psTo, psInputsTo);
    assert (nArityFrom == psOperationMap->nArityFrom);
    assert (nArityTo == psOperationMap->nArityTo);

    FreeRecursive (*psFrom);
    *psFrom = CopyRecursive (psOperationMap->psTo);
    psReplacements = CreateVarStack ();

    for (nPosTo = 0; nPosTo < nArityTo; ++nPosTo) {
        if (psOperationMap->aszUnbound[nPosTo] != NULL) {
            VarStackPush (psReplacements, psOperationMap->aszUnbound[nPosTo]);
        }
        else {
            szVarTo = VarStackGet (psInputsTo, nPosTo);

            nMapFrom = -1;
            for (nPosFrom = 0; (nMapFrom < 0) && (nPosFrom < nArityFrom); ++nPosFrom) {
                if (psOperationMap->aaboVarOrigin[(nPosTo * psOperationMap->nArityFrom) + nPosFrom] == TRUE) {
                    nMapFrom = nPosFrom;
                }
            }
            // There should be exactly one
            assert (nMapFrom >= 0);

            szVarFrom = VarStackGet (psInputsFrom, nMapFrom);

            nReplace = VarStackFind (psExtract->psVarsFrom, szVarFrom);
            if (nReplace >= 0) {
                szVarFrom = VarStackGet (psExtract->psVarsTo, nReplace);
            }

            VarStackPush(psReplacements, szVarFrom);
        }
    }

    ReplaceUnboundMany (*psFrom, psReplacements, psInputsTo);

    psReplacements = FreeVarStack (psReplacements);
    psInputsFrom = FreeVarStack (psInputsFrom);
    psInputsTo = FreeVarStack (psInputsTo);
}

void ReplaceUnboundMany (Operation * psOp, VarStack const * const psVarsFrom, VarStack const * const psVarsTo) {
    VarStack * psVarStack;

    psVarStack = CreateVarStack ();
    ReplaceUnboundRecurseMany (psOp, psVarsFrom, psVarsTo, psVarStack);
    psVarStack = FreeVarStack (psVarStack);

}

void ReplaceUnboundRecurseMany (Operation * psOp, VarStack const * const psVarsFrom, VarStack const * const psVarsTo, VarStack * psVarStack) {
    size_t nVar;
    int nVarStackPos;
    char const * szVarTo;

    // Check the operations recursively
    if (psOp) {
        switch (psOp->eOpType) {
            case OPTYPE_TRUTHVALUE:
                // Nothing to do
                break;
            case OPTYPE_VARIABLE:
                // Nothing to do
                break;
            case OPTYPE_UNARY:
                ReplaceUnboundRecurseMany (psOp->Vars.psUnary->psVar1, psVarsFrom, psVarsTo, psVarStack);
                break;
            case OPTYPE_BINARY:
                ReplaceUnboundRecurseMany (psOp->Vars.psBinary->psVar1, psVarsFrom, psVarsTo, psVarStack);
                ReplaceUnboundRecurseMany (psOp->Vars.psBinary->psVar2, psVarsFrom, psVarsTo, psVarStack);
                break;
            case OPTYPE_QUANTIFIER:
                VarStackPush (psVarStack, psOp->Vars.psQuantifier->szVar);
                ReplaceUnboundRecurseMany (psOp->Vars.psQuantifier->psVar1, psVarsFrom, psVarsTo, psVarStack);
                VarStackPop (psVarStack);
                break;
            case OPTYPE_RELATION:
                for (nVar = 0; nVar < psOp->Vars.psRelation->nArity; ++nVar) {
                    nVarStackPos = VarStackFind (psVarStack, psOp->Vars.psRelation->aszVar[nVar]);
                    if (nVarStackPos == -1) {
                        // Replace any unbound instances of szVarFrom with szVarTo
                        nVarStackPos = VarStackFind (psVarsFrom, psOp->Vars.psRelation->aszVar[nVar]);
                        if (nVarStackPos >= 0) {
                            szVarTo = VarStackGet (psVarsTo, nVarStackPos);
                            PropFree (psOp->Vars.psRelation->aszVar[nVar]);
                            psOp->Vars.psRelation->aszVar[nVar] = (char *)PropMalloc (strlen (szVarTo) + 1);
                            strcpy (psOp->Vars.psRelation->aszVar[nVar], szVarTo);
                        }
                    }
                }
                break;
            default:
                // Not something we know about (shouldn't happen)
                printf("Invalid operation type\n");
                break;
        }
    }
}

bool ExtractCheckSubstitution (Extract const * psExtract, Operation const * psMain) {
    bool boSuccess;
    VarStack * psVarStack;

    psVarStack = CreateVarStack ();
    boSuccess = ExtractCheckSubstitutionRecursive (psExtract, psMain, psVarStack);
    psVarStack = FreeVarStack (psVarStack);

    return boSuccess;
}

bool ExtractCheckSubstitutionRecursive (Extract const * psExtract, Operation const * psMain, VarStack * psVarStack) {
    int nSubstitute;
    bool boSuccess = TRUE;
    int nFind;
    int nVarTo;

    if (psMain != NULL) {
        switch (psMain->eOpType) {
            case OPTYPE_TRUTHVALUE:
            case OPTYPE_VARIABLE:
                // Do nothing
                break;
            case OPTYPE_UNARY:
                boSuccess = ExtractCheckSubstitutionRecursive (psExtract, psMain->Vars.psUnary->psVar1, psVarStack);
                break;
            case OPTYPE_BINARY:
                boSuccess = ExtractCheckSubstitutionRecursive (psExtract, psMain->Vars.psBinary->psVar1, psVarStack) && ExtractCheckSubstitutionRecursive (psExtract, psMain->Vars.psBinary->psVar2, psVarStack);
                break;
            case OPTYPE_QUANTIFIER:
                VarStackPush (psVarStack, psMain->Vars.psQuantifier->szVar);
                boSuccess = ExtractCheckSubstitutionRecursive (psExtract, psMain->Vars.psQuantifier->psVar1, psVarStack);
                VarStackDrop (psVarStack);
                break;
            case OPTYPE_RELATION:
                nSubstitute = ExtractCompareOperationsMany (psExtract, psMain, psVarStack);
                if (nSubstitute != 0) {
                    // We're replacing:
                    //    psMain (a relation)
                    // with:
                    //     psExtract->apsOps[(nSubstitute - 1)]->psTO
                    // We need to check if anything in:
                    //    psExtract->apsOps[(nSubstitute - 1)]->aszUnbound
                    // is also in:
                    //    psVarStack
                    // If there's any overlap, then a variable that shouldn't be bound will become bound
                    for (nVarTo = 0; (nVarTo < psExtract->apsOps[(nSubstitute - 1)]->nArityTo) && boSuccess; ++nVarTo) {
                        if (psExtract->apsOps[(nSubstitute - 1)]->aszUnbound[nVarTo] != NULL) {
                            boSuccess = !VarStackContains (psVarStack, psExtract->apsOps[(nSubstitute - 1)]->aszUnbound[nVarTo]);
                        }
                    }
                }
                break;
            default:
                printf("Invalid operation type\n");
                break;
        }
    }
    return boSuccess;
}

bool ExtractCheckRecursive (Extract * psExtract, Operation * psPattern, VarStack * psPatternVars) {
    bool boSuccess = FALSE;
    Operation * psRelation;
    OperationMap * psOperationMap;
    int nVarFrom;
    int nVarTo;

    switch (psPattern->eOpType) {
        case OPTYPE_TRUTHVALUE:
            // Intentional fallthrough
        case OPTYPE_VARIABLE:
            // Intentional fallthrough
        default: {
            boSuccess = TRUE;
        }
        break;
        case OPTYPE_UNARY: {
            boSuccess = ExtractCheckRecursive (psExtract, psPattern->Vars.psUnary->psVar1, psPatternVars);
        }
        break;
        case OPTYPE_BINARY: {
            boSuccess = ExtractCheckRecursive (psExtract, psPattern->Vars.psBinary->psVar1, psPatternVars) && ExtractCheckRecursive (psExtract, psPattern->Vars.psBinary->psVar2, psPatternVars);
        }
        break;
        case OPTYPE_QUANTIFIER: {
            VarStackPush (psPatternVars, psPattern->Vars.psQuantifier->szVar);

            boSuccess = ExtractCheckRecursive (psExtract, psPattern->Vars.psQuantifier->psVar1, psPatternVars);

            VarStackDrop(psPatternVars);
        }
        break;
        case OPTYPE_RELATION: {
            psOperationMap = ExtractOperationMap (psExtract, psPattern);
            assert(psOperationMap != NULL);

            boSuccess = OperationMapVarOriginsCheckClear (psOperationMap, psPattern);
        }
        break;
    }

    return boSuccess;
}

Extract * ExtractPatternCheck (Operation * psPattern) {
    Extract * psExtract = NULL;
    bool boResult;
    int nRelationCount;
    int nPos;
    Operation const * psOp;
    RelationList * psRelationList;
    VarStack * psPatternVars;
    int nUnbound;

    // Patterns must have all of their variables bound
    nUnbound = OperationArity (psPattern);
    if (nUnbound == 0) {
        psExtract = CreateExtract ();
        psRelationList = CreateRelationList();
        psPatternVars = CreateVarStack();

        RelationListExtract (psRelationList, psPattern);
        nRelationCount = RelationListCount (psRelationList);
        psExtract->nCount = nRelationCount;

        if (nRelationCount > 0) {
            psExtract->apsOps = PropCalloc (nRelationCount, sizeof(OperationMap *));

            for (nPos = 0; nPos < nRelationCount; ++nPos) {
                psExtract->apsOps[nPos] = CreateOperationMap ();

                psOp = RelationListGet (psRelationList, nPos);

                OperationMapSetFromCheck (psExtract->apsOps[nPos], psOp);
            }
        }

        psRelationList = FreeRelationList (psRelationList);
        boResult = ExtractCheckRecursive (psExtract, psPattern, psPatternVars);

        psPatternVars = FreeVarStack (psPatternVars);

        if (boResult == FALSE) {
            psExtract = FreeExtract(psExtract);
        }
    }

    return psExtract;
}

Extract * ExtractPatternManyCheck (Operation ** apsPattern, int nCount) {
    Extract * psExtract = NULL;
    bool boResult;
    int nRelationCount;
    int nPos;
    Operation const * psOp;
    RelationList * psRelationList;
    VarStack * psPatternVars;
    int nUnbound;

    boResult = TRUE;
    // Patterns must have all of their variables bound
    for (nPos = 0; (nPos < nCount) && boResult; ++nPos) {
        nUnbound = OperationArity (apsPattern[nPos]);
        if (nUnbound != 0) {
            boResult = FALSE;
        }
    }

    if (boResult) {
        psExtract = CreateExtract ();
        psRelationList = CreateRelationList ();
        psPatternVars = CreateVarStack ();

        for (nPos = 0; nPos < nCount; ++nPos) {
            RelationListExtract (psRelationList, apsPattern[nPos]);
        }
        nRelationCount = RelationListCount (psRelationList);
        psExtract->nCount = nRelationCount;

        if (nRelationCount > 0) {
            psExtract->apsOps = PropCalloc (nRelationCount, sizeof(OperationMap *));

            for (nPos = 0; nPos < nRelationCount; ++nPos) {
                psExtract->apsOps[nPos] = CreateOperationMap ();

                psOp = RelationListGet (psRelationList, nPos);

                OperationMapSetFromCheck (psExtract->apsOps[nPos], psOp);
            }
        }

        psRelationList = FreeRelationList (psRelationList);

        boResult = TRUE;
        for (nPos = 0; (nPos < nCount) && boResult; ++nPos) {
            boResult = ExtractCheckRecursive (psExtract, apsPattern[nPos], psPatternVars);
        }

        psPatternVars = FreeVarStack (psPatternVars);

        if (boResult == FALSE) {
            psExtract = FreeExtract (psExtract);
        }
    }

    return psExtract;
}

bool ExtractPatternMappingUnique (Operation ** apsPattern, int nCount) {
    Extract * psExtract;
    int nPos;
    bool boSuccess = FALSE;

    psExtract = ExtractPatternManyCheck (apsPattern, nCount);

    if (psExtract) {
        boSuccess = TRUE;
        for (nPos = 0; boSuccess && (nPos < psExtract->nCount); ++nPos) {
	        boSuccess = OperationMapVarMappingUnique (psExtract->apsOps[nPos]);
        }
        psExtract = FreeExtract (psExtract);
    }

    return boSuccess;
}

bool ExtractSubstituteCheck (Extract const * psExtract, Operation const * psMain, Operation const * psResult) {
    bool boValid;
    int nPos;
    int nVarPos;
    VarStack * psMainVars;
    VarStack * psResultVars;
    OperationMap * psOperationMap;

    psMainVars = CreateVarStack ();
    psResultVars = CreateVarStack ();

    boValid = ExtractSubstituteCheckRecursive (psExtract, psMain, psResult, psMainVars, psResultVars);

    psResultVars = FreeVarStack (psResultVars);
    psMainVars = FreeVarStack (psMainVars);

    return boValid;
}

bool ExtractSubstituteCheckRecursive (Extract const * psExtract, Operation const * psMain, Operation const * psResult, VarStack * psMainVars, VarStack * psResultVars) {
    bool boValid = FALSE;
    int nPos;
    OperationMap const * psOperationMap;
    VarStack * psVarsFrom;
    VarStack * psVarsTo;
    int nFindFrom;
    int nFindTo;
    char const * szVarTo;
    char const * szVarFrom;

    if ((psMain != NULL) && (psResult != NULL)) {
        switch (psMain->eOpType) {
            case OPTYPE_TRUTHVALUE:
                if (psResult->eOpType == psMain->eOpType) {
                    boValid = (psMain->Vars.boTruth == psResult->Vars.boTruth);
                }
                break;
            case OPTYPE_VARIABLE:
                if (psResult->eOpType == psMain->eOpType) {
                    boValid = (strcmp (psMain->Vars.psVar->szVar, psResult->Vars.psVar->szVar) == 0);
                }
                break;
            case OPTYPE_UNARY:
                if ((psResult->eOpType == psMain->eOpType) && (psMain->Vars.psUnary->eOpType == psResult->Vars.psUnary->eOpType)) {
                    boValid = ExtractSubstituteCheckRecursive (psExtract, psMain->Vars.psUnary->psVar1, psResult->Vars.psUnary->psVar1, psMainVars, psResultVars);
                }
                break;
            case OPTYPE_BINARY:
                if ((psResult->eOpType == psMain->eOpType) && (psMain->Vars.psBinary->eOpType == psResult->Vars.psBinary->eOpType)) {
                    boValid = ExtractSubstituteCheckRecursive (psExtract, psMain->Vars.psBinary->psVar1, psResult->Vars.psBinary->psVar1, psMainVars, psResultVars) && ExtractSubstituteCheckRecursive (psExtract, psMain->Vars.psBinary->psVar2, psResult->Vars.psBinary->psVar2, psMainVars, psResultVars);
                }
                break;
            case OPTYPE_QUANTIFIER:
                if ((psResult->eOpType == psMain->eOpType) && (psMain->Vars.psQuantifier->eQuType == psResult->Vars.psQuantifier->eQuType)) {
                    VarStackPush (psMainVars, psMain->Vars.psQuantifier->szVar);
                    VarStackPush (psResultVars, psResult->Vars.psQuantifier->szVar);

                    boValid = ExtractSubstituteCheckRecursive (psExtract, psMain->Vars.psQuantifier->psVar1, psResult->Vars.psQuantifier->psVar1, psMainVars, psResultVars);

                    VarStackDrop (psResultVars);
                    VarStackDrop (psMainVars);
                }
                break;
            case OPTYPE_RELATION:
                psOperationMap = ExtractOperationMap (psExtract, psMain);
                if (psOperationMap) {
                    if (psOperationMap->psTo != NULL) {
                        psVarsTo = CreateVarStack ();
                        psVarsFrom = CreateVarStack ();

                        OperationInputList (psMain, psVarsFrom);
                        OperationInputList (psResult, psVarsTo);

                        boValid = TRUE;
                        for (nPos = 0; boValid && (nPos < psOperationMap->nArityTo); ++nPos) {
                            szVarTo = VarStackGet (psVarsTo, nPos);
                            nFindTo = VarStackFind (psResultVars, szVarTo);
                            if (nFindTo >= 0) {
                                szVarFrom = VarStackGet (psMainVars, nFindTo);

                                nFindFrom = VarStackFind (psVarsFrom, szVarFrom);

                                if (nFindFrom >= 0) {
                                    boValid = psOperationMap->aaboVarOrigin[(nFindTo * psOperationMap->nArityFrom) + nFindFrom];
                                }
                                else {
                                    boValid = FALSE;
                                }
                            }
                            else {
                                // Constant variable
                                if (psOperationMap->aszUnbound[nPos]) {
                                    boValid = (strcmp (szVarTo, psOperationMap->aszUnbound[nPos]) == 0);
                                }
                                else {
                                    boValid = FALSE;
                                }
                            }
                        }

                        psVarsTo = FreeVarStack (psVarsTo);
                        psVarsFrom = FreeVarStack (psVarsFrom);
                    }
                }
                else {
                    boValid = FALSE;
                }
                break;
            default:
                printf("Invalid operation type\n");
                break;
        }
    }
    return boValid;
}

