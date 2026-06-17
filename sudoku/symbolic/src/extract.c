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
bool ExtractRecursive (Extract * psExtract, Operation * psPattern, Operation * psScrutinee, VarStack * psBoundVars);
OperationMap * ExtractOperationMap (Extract * psExtract, Operation const * const psRelation);
void ReplaceUnboundRecurse (Operation * psOp, char const * const szVarFrom, char const * const szVarTo);
bool OccursUnboundRecurse (Operation const * psOp, char const * const szVar);
char const* FindFirstVaraibleDiffRecurse (Operation const* psOpFrom, Operation const* psOpTo);

//////////////////////////////////////////////////////////////////
// Main application

Extract * CreateExtract() {
    Extract * psExtract = NULL;

    psExtract = PropCalloc(1, sizeof(Extract));

    return psExtract;
}

Extract * ExtractPattern (Operation * psPattern, Operation * psScrutinee) {
    Extract * psExtract;
    bool boResult;
    int nRelationCount;
    int nPos;
    Operation const * psOp;
    RelationList * psRelationList;
    VarStack * psBoundVars;

    psExtract = CreateExtract ();
    psRelationList = CreateRelationList();
    psBoundVars = CreateVarStack();

    RelationListExtract(psRelationList, psPattern);
    nRelationCount = RelationListCount (psRelationList);
    psExtract->nCount = nRelationCount;

    if (nRelationCount > 0) {
        psExtract->apsOps = PropCalloc(nRelationCount, sizeof(OperationMap));

        for (nPos = 0; nPos < nRelationCount; ++nPos) {
            psOp = RelationListGet (psRelationList, nPos);

            psExtract->apsOps[nPos].psFrom = CopyRecursive(psOp);
        }
    }

    psRelationList = FreeRelationList (psRelationList);
    boResult = ExtractRecursive(psExtract, psPattern, psScrutinee, psBoundVars);

    psBoundVars = FreeVarStack (psBoundVars);

    if (boResult == FALSE) {
        FreeExtract(psExtract);
        psExtract = NULL;
    }

    return psExtract;
}

bool ExtractRecursive (Extract * psExtract, Operation * psPattern, Operation * psScrutinee, VarStack * psBoundVars) {
    bool boSuccess = FALSE;
    Operation * psRelation;
    OperationMap * psMap;

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
                boSuccess = ExtractRecursive(psExtract, psPattern->Vars.psUnary->psVar1, psScrutinee->Vars.psUnary->psVar1, psBoundVars);
            }
        }
        break;
        case OPTYPE_BINARY: {
            if ((psScrutinee->eOpType == OPTYPE_BINARY) && (psPattern->Vars.psBinary->eOpType == psScrutinee->Vars.psBinary->eOpType)) {
                boSuccess = ExtractRecursive (psExtract, psPattern->Vars.psBinary->psVar1, psScrutinee->Vars.psBinary->psVar1, psBoundVars) && ExtractRecursive (psExtract, psPattern->Vars.psBinary->psVar2, psScrutinee->Vars.psBinary->psVar2, psBoundVars);
            }
        }
        break;
        case OPTYPE_QUANTIFIER: {
            if ((psScrutinee->eOpType == OPTYPE_QUANTIFIER) && (psPattern->Vars.psQuantifier->eQuType == psScrutinee->Vars.psQuantifier->eQuType) && (strcmp(psPattern->Vars.psQuantifier->szVar, psScrutinee->Vars.psQuantifier->szVar) == 0)) {
                VarStackPush(psBoundVars, psPattern->Vars.psQuantifier->szVar);
                boSuccess = ExtractRecursive (psExtract, psPattern->Vars.psQuantifier->psVar1, psScrutinee->Vars.psQuantifier->psVar1, psBoundVars);
                VarStackDrop(psBoundVars);
            }
        }
        break;
        case OPTYPE_RELATION: {
            boSuccess = VarStackMatchUnbound (psBoundVars, psScrutinee);

            if (boSuccess) {
                psMap = ExtractOperationMap (psExtract, psPattern);
                assert(psMap != NULL);
                if (psMap->psTo) {
                    boSuccess = CompareOperations (psMap->psTo, psScrutinee);

                }
                else {
                    psMap->psTo = psScrutinee;
                    boSuccess = TRUE;
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
    char const * szVar;
    Operation const * psOp;
    RelationList * psRelationList;
    VarStack * psBoundVars;

    psExtract = CreateExtract ();
    psRelationList = CreateRelationList();
    psBoundVars = CreateVarStack();

    for (nPos = 0; nPos < nCount; ++nPos) {
        RelationListExtract(psRelationList, apsPattern[nPos]);
    }
    nRelationCount = RelationListCount (psRelationList);
    psExtract->nCount = nRelationCount;

    if (nRelationCount > 0) {
        psExtract->apsOps = PropCalloc(nRelationCount, sizeof(OperationMap));

        for (nPos = 0; nPos < nRelationCount; ++nPos) {
            psOp = RelationListGet (psRelationList, nPos);

            psExtract->apsOps[nPos].psFrom = CopyRecursive(psOp);
        }
    }

    psRelationList = FreeRelationList (psRelationList);

    boResult = TRUE;
    for (nPos = 0; (nPos < nCount) && boResult; ++nPos) {
        boResult = ExtractRecursive(psExtract, apsPattern[nPos], apsScrutinee[nPos], psBoundVars);
    }

    if (boResult == FALSE) {
        FreeExtract(psExtract);
        psExtract = NULL;
    }

    psBoundVars = FreeVarStack (psBoundVars);

    return psExtract;
}

int ExtractCount (Extract * psExtract) {
    return psExtract->nCount;
}

Operation * ExtractRelation(Extract * psExtract, int nPosition) {
    Operation * psOp = NULL;

    if ((nPosition >= 0) && (nPosition < psExtract->nCount)) {
        psOp = psExtract->apsOps[nPosition].psFrom;
    }

    return psOp;
}

Operation * ExtractValueFromPos (Extract * psExtract, int nPosition) {
    Operation * psValue = NULL;
    int nPos;

    if ((nPosition >= 0) && (nPosition < psExtract->nCount)) {
        psValue = psExtract->apsOps[nPosition].psTo;
    }

    return psValue;
}

Operation * ExtractValue (Extract * psExtract, Operation const * const psRelation) {
    Operation * psValue = NULL;
    int nPos;

    nPos = 0;
    while ((psValue == NULL) && (nPos < psExtract->nCount)) {
        if (CompareOperations(psRelation, psExtract->apsOps[nPos].psFrom)) {
            psValue = psExtract->apsOps[nPos].psTo;
        }
        nPos += 1;
    }

    return psValue;
}

OperationMap * ExtractOperationMap (Extract * psExtract, Operation const * const psRelation) {
    OperationMap * psMap = NULL;
    int nPos;

    nPos = 0;
    while ((psMap == NULL) && (nPos < psExtract->nCount)) {
        if (CompareOperations(psRelation, psExtract->apsOps[nPos].psFrom)) {
            psMap = &psExtract->apsOps[nPos];
        }
        nPos += 1;
    }

    return psMap;
}

void FreeExtract (Extract * psExtract) {
    int nPos;

    if (psExtract) {
        if (psExtract->apsOps) {
            for (nPos = 0; nPos < psExtract->nCount; ++nPos) {
                if (psExtract->apsOps[nPos].psFrom) {
                    FreeRecursive(psExtract->apsOps[nPos].psFrom);
                    psExtract->apsOps[nPos].psFrom = NULL;
                    psExtract->apsOps[nPos].psTo = NULL;
                }
            }

            PropFree (psExtract->apsOps);
            psExtract->apsOps = NULL;
        }

        PropFree (psExtract);
    }
}

void ReplaceUnbound (Operation * psOp, char const * const szVarFrom, char const * const szVarTo) {
    ReplaceUnboundRecurse(psOp, szVarFrom, szVarTo);
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
                ReplaceUnboundRecurse(psOp->Vars.psUnary->psVar1, szVarFrom, szVarTo);
                break;
            case OPTYPE_BINARY:
                ReplaceUnboundRecurse(psOp->Vars.psBinary->psVar1, szVarFrom, szVarTo);
                ReplaceUnboundRecurse(psOp->Vars.psBinary->psVar2, szVarFrom, szVarTo);
                break;
            case OPTYPE_QUANTIFIER:
                // Once a variable is bound it'll be bound in all subformulae
                // So only recursive if we're not binding the variable
                if (strcmp(szVarFrom, psOp->Vars.psQuantifier->szVar) != 0) {
                    ReplaceUnboundRecurse(psOp->Vars.psQuantifier->psVar1, szVarFrom, szVarTo);
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

bool OccursUnbound (Operation const * psOp, char const * const szVar) {
   return OccursUnboundRecurse (psOp, szVar);
}

bool OccursUnboundRecurse (Operation const * psOp, char const * const szVar) {
    size_t nVar;
    bool boOccurs = FALSE;

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
                boOccurs = OccursUnboundRecurse(psOp->Vars.psUnary->psVar1, szVar);
                break;
            case OPTYPE_BINARY:
                boOccurs = OccursUnboundRecurse(psOp->Vars.psBinary->psVar1, szVar);
                boOccurs |= OccursUnboundRecurse(psOp->Vars.psBinary->psVar2, szVar);
                break;
            case OPTYPE_QUANTIFIER:
                // Once a variable is bound it'll be bound in all subformulae
                // So only recursive if we're not binding the variable
                if (strcmp(szVar, psOp->Vars.psQuantifier->szVar) != 0) {
                    boOccurs = OccursUnboundRecurse(psOp->Vars.psQuantifier->psVar1, szVar);
                }
                break;
            case OPTYPE_RELATION:
                nVar = 0;
                while ((boOccurs == FALSE) && (nVar < psOp->Vars.psRelation->nArity)) {
                    // Replace any instances of szVarFrom with szVarTo
                    if (strcmp (szVar, psOp->Vars.psRelation->aszVar[nVar]) == 0) {
                        boOccurs = TRUE;
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

    return boOccurs;
}
