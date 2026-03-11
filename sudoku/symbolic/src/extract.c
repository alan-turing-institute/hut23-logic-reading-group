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

Extract * CreateExtract();
bool ExtractRecursive (Extract * psExtract, Operation * psPattern, Operation * psScrutinee);
OperationMap * ExtractOperationMap(Extract * psExtract, char const * const szName);

//////////////////////////////////////////////////////////////////
// Main application

Extract * CreateExtract() {
    Extract * psExtract = NULL;

    psExtract = calloc(1, sizeof(Extract));

    return psExtract;
}

Extract * ExtractPattern (Operation * psPattern, Operation * psScrutinee) {
    Extract * psExtract;
    bool boResult;
    Variable * psVariables;
    Variable * psVariableCurrent;
    int nVariableCount;
    int nPos;
    char const * szVar;

    psExtract = CreateExtract ();

    psVariables = CreateVariables (psPattern, NULL);

    nVariableCount = VariableCount (psVariables);
    psExtract->nCount = nVariableCount;
    if (nVariableCount > 0) {
        psExtract->apsOps = calloc(nVariableCount, sizeof(OperationMap));

        psVariableCurrent = VariableFirst(psVariables);
        nPos = 0;
        while (psVariableCurrent) {
            szVar = VariableName (psVariableCurrent);

            psExtract->apsOps[nPos].szVar = calloc(strlen(szVar) + 1, sizeof(char));
            strcpy(psExtract->apsOps[nPos].szVar, szVar);

            psVariableCurrent = VariableNext (psVariableCurrent);
            nPos += 1;
            assert(nPos <= nVariableCount);
        }
    }

    psVariables = FreeVariables (psVariables);

    boResult = ExtractRecursive(psExtract, psPattern, psScrutinee);

    if (boResult == FALSE) {
        FreeExtract(psExtract);
        psExtract = NULL;
    }

    return psExtract;
}

bool ExtractRecursive (Extract * psExtract, Operation * psPattern, Operation * psScrutinee) {
    bool boSuccess = FALSE;
    char * szVar;
    OperationMap * psMap;

    switch (psPattern->eOpType) {
        case OPTYPE_TRUTHVALUE:
            // Intentional fallthrough
        default: {
            boSuccess = CompareOperations (psPattern, psScrutinee);
        }
        break;
	    case OPTYPE_VARIABLE: {
            szVar = psPattern->Vars.psVar->szVar;
            psMap = ExtractOperationMap(psExtract, szVar);
            assert(psMap != NULL);
            if (psMap->psOp) {
                boSuccess = CompareOperations (psMap->psOp, psScrutinee);
            }
            else {
                psMap->psOp = psScrutinee;
                boSuccess = TRUE;
            }
	    }
	    break;
	    case OPTYPE_UNARY: {
            if ((psScrutinee->eOpType == OPTYPE_UNARY) && (psPattern->Vars.psUnary->eOpType == psScrutinee->Vars.psUnary->eOpType)) {
                boSuccess = ExtractRecursive(psExtract, psPattern->Vars.psUnary->psVar1, psScrutinee->Vars.psUnary->psVar1);
            }
        }
        break;
	    case OPTYPE_BINARY: {
            if ((psScrutinee->eOpType == OPTYPE_BINARY) && (psPattern->Vars.psBinary->eOpType == psScrutinee->Vars.psBinary->eOpType)) {
                boSuccess = ExtractRecursive(psExtract, psPattern->Vars.psBinary->psVar1, psScrutinee->Vars.psBinary->psVar1) && ExtractRecursive(psExtract, psPattern->Vars.psBinary->psVar2, psScrutinee->Vars.psBinary->psVar2);
            }
        }
        break;
    }

    return boSuccess;
}

Extract * ExtractPatternMany (Operation ** apsPattern, Operation ** apsScrutinee, int nCount) {
    Extract * psExtract;
    bool boResult;
    Variable * psVariables;
    Variable * psVariableCurrent;
    int nVariableCount;
    int nPos;
    char const * szVar;

    psExtract = CreateExtract ();

    psVariables = NULL;
    for (nPos = 0; nPos < nCount; ++nPos) {
        psVariables = CreateVariables (apsPattern[nPos], psVariables);
    }

    nVariableCount = VariableCount (psVariables);
    psExtract->nCount = nVariableCount;
    if (nVariableCount > 0) {
        psExtract->apsOps = calloc(nVariableCount, sizeof(OperationMap));

        psVariableCurrent = VariableFirst(psVariables);
        nPos = 0;
        while (psVariableCurrent) {
            szVar = VariableName (psVariableCurrent);

            psExtract->apsOps[nPos].szVar = calloc(strlen(szVar) + 1, sizeof(char));
            strcpy(psExtract->apsOps[nPos].szVar, szVar);

            psVariableCurrent = VariableNext (psVariableCurrent);
            nPos += 1;
            assert(nPos <= nVariableCount);
        }
    }

    psVariables = FreeVariables (psVariables);

    boResult = TRUE;
    for (nPos = 0; (nPos < nCount) && boResult; ++nPos) {
        boResult = ExtractRecursive(psExtract, apsPattern[nPos], apsScrutinee[nPos]);
    }

    if (boResult == FALSE) {
        FreeExtract(psExtract);
        psExtract = NULL;
    }

    return psExtract;
}

int ExtractCount(Extract * psExtract) {
    return psExtract->nCount;
}

char * ExtractName(Extract * psExtract, int nPosition) {
    char * szName = NULL;

    if ((nPosition >= 0) && (nPosition < psExtract->nCount)) {
        szName = psExtract->apsOps[nPosition].szVar;
    }

    return szName;
}

Operation * ExtractValueFromPos(Extract * psExtract, int nPosition) {
    Operation * psValue = NULL;
    int nPos;

    if ((nPosition >= 0) && (nPosition < psExtract->nCount)) {
        psValue = psExtract->apsOps[nPosition].psOp;
    }

    return psValue;
}

Operation * ExtractValue(Extract * psExtract, char const * const szName) {
    Operation * psValue = NULL;
    int nPos;

    nPos = 0;
    while ((psValue == NULL) && (nPos < psExtract->nCount)) {
        if (strcmp(szName, psExtract->apsOps[nPos].szVar) == 0) {
            psValue = psExtract->apsOps[nPos].psOp;
        }
        nPos += 1;
    }

    return psValue;
}

OperationMap * ExtractOperationMap(Extract * psExtract, char const * const szName) {
    OperationMap * psMap = NULL;
    int nPos;

    nPos = 0;
    while ((psMap == NULL) && (nPos < psExtract->nCount)) {
        if (strcmp(szName, psExtract->apsOps[nPos].szVar) == 0) {
            psMap = &psExtract->apsOps[nPos];
        }
        nPos += 1;
    }

    return psMap;
}

void FreeExtract(Extract * psExtract) {
    int nPos;

    if (psExtract) {
        if (psExtract->apsOps) {
            for (nPos = 0; nPos < psExtract->nCount; ++nPos) {
                if (psExtract->apsOps[nPos].szVar) {
                    free (psExtract->apsOps[nPos].szVar);
                    psExtract->apsOps[nPos].szVar = NULL;
                    psExtract->apsOps[nPos].psOp = NULL;
                }
            }

            free (psExtract->apsOps);
            psExtract->apsOps = NULL;
        }

        free (psExtract);
    }
}

