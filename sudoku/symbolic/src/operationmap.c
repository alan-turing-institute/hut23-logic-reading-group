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

//////////////////////////////////////////////////////////////////
// Main application

OperationMap * CreateOperationMap () {
    OperationMap * psOperationMap = NULL;

    psOperationMap = PropCalloc(1, sizeof(OperationMap));

    return psOperationMap;
}

OperationMap * FreeOperationMap (OperationMap * psOperationMap) {
    if (psOperationMap) {
        if (psOperationMap->psFrom) {
            FreeRecursive(psOperationMap->psFrom);
            psOperationMap->psFrom = NULL;
        }
        if (psOperationMap->psTo) {
            FreeRecursive(psOperationMap->psTo);
            psOperationMap->psTo = NULL;
        }

        PropFree(psOperationMap);
    }

    return NULL;
}

void OperationMapSetFrom (OperationMap * psOperationMap, Operation const * psOp) {
    if (psOperationMap) {
        if (psOperationMap->psFrom) {
            FreeRecursive(psOperationMap->psFrom);
        }

        psOperationMap->psFrom = CopyRecursive(psOp);
    }
}

bool OperationMapSetTo (OperationMap * psOperationMap, Operation const * psScrutinee, VarStack * psScrutineeVars) {
    bool boSuccess;
    VarStack * psToInputs;
    int nPos;
    char const * szVar;
    int nVarLength;
    int nStackPos;

    if (psOperationMap->psTo) {
        // TODO: This should be comparing operations, not relations
        boSuccess = CompareOperationPatterns (psOperationMap->psTo, psScrutinee);
    }
    else {
        psOperationMap->psTo = CopyRecursive(psScrutinee);

        psOperationMap->nArityFrom = OperationArity (psOperationMap->psFrom);

        psToInputs = CreateVarStack();
        psOperationMap->nArityTo = OperationInputList (psOperationMap->psTo, psToInputs);

        assert (psOperationMap->aszUnbound == NULL);
        psOperationMap->aszUnbound = PropMalloc (psOperationMap->nArityTo * sizeof(char *));
        for (nPos = 0; nPos < psOperationMap->nArityTo; ++nPos) {
            szVar = VarStackGet (psToInputs, nPos);

            nStackPos = VarStackFind (psScrutineeVars, szVar);
            if (nStackPos >= 0) {
                // This is a bound variable
                psOperationMap->aszUnbound[nPos] = NULL;
            }
            else {
                // This is an unbound variable
                nVarLength = strlen (szVar);
                psOperationMap->aszUnbound[nPos] = PropMalloc (nVarLength + 1);
                strcpy (psOperationMap->aszUnbound[nPos], szVar);
                psOperationMap->aszUnbound[nPos][nVarLength] = 0;
            }
        }
        psToInputs = FreeVarStack(psToInputs);

        OperationMapInitVarOrigins (psOperationMap);

        boSuccess = TRUE;
    }

    //OperationMapVarOriginsClear (psOperationMap, psScrutinee);


    return boSuccess;
}

Operation const * OperationMapGetFrom (OperationMap const * psOperationMap) {
    return psOperationMap->psFrom;
}

Operation const * OperationMapGetTo (OperationMap const * psOperationMap) {
    return psOperationMap->psTo;
}

void OperationMapInitVarOrigins (OperationMap * psOperationMap) {
    int nFromPos;
    int nToPos;

    if (psOperationMap) {
        assert (psOperationMap->aaboVarOrigin == NULL);

        psOperationMap->aaboVarOrigin = PropMalloc(psOperationMap->nArityFrom * psOperationMap->nArityTo * sizeof(bool));

        for (nToPos = 0; nToPos < psOperationMap->nArityTo; ++nToPos) {
            for (nFromPos = 0; nFromPos < psOperationMap->nArityFrom; ++nFromPos) {
                psOperationMap->aaboVarOrigin[(nToPos * psOperationMap->nArityFrom) + nFromPos] = TRUE;
            }
        }
    }
}

void OperationMapVarOriginClear (OperationMap * psOperationMap, int nFrom, int nTo) {
    if (psOperationMap) {
        if (psOperationMap->aaboVarOrigin) {
            psOperationMap->aaboVarOrigin[(nTo * psOperationMap->nArityFrom) + nFrom] = FALSE;
        }
    }
}

bool OperationMapVarMappingUnique (OperationMap * psOperationMap) {
    int nFromPos;
    int nToPos;
    int nToCount;
    bool boResult = TRUE;

    if (psOperationMap) {
        for (nToPos = 0; (boResult && (nToPos < psOperationMap->nArityTo)); ++nToPos) {
            nToCount = 0;
            for (nFromPos = 0; nFromPos < psOperationMap->nArityFrom; ++nFromPos) {
                if (psOperationMap->aaboVarOrigin[(nToPos * psOperationMap->nArityFrom) + nFromPos]) {
                    nToCount += 1;
                }
            }
            boResult = boResult && (nToCount <= 1);
        }
    }
    return boResult;
}

bool OperationMapVarOriginsClear (OperationMap * psOperationMap, Operation const * psFrom, Operation const * psTo, VarStack * psPatternVars, VarStack * psScrutineeVars) {
    int nArityFrom;
    int nArityTo;
    int nVarFrom;
    int nVarTo;
    VarStack * psInputsFrom;
    VarStack * psInputsTo;
    char const * szVarFrom;
    char const * szVarTo;
    char const * szRelationVar;
    int nStackPos;
    bool boValidMap;
    int nValidCount;

    psInputsFrom = CreateVarStack ();
    psInputsTo = CreateVarStack ();
    nArityFrom = OperationInputList (psFrom, psInputsFrom);
    nArityTo = OperationInputList (psTo, psInputsTo);

    assert (psOperationMap->nArityFrom == nArityFrom);
    assert (psOperationMap->nArityTo == nArityTo);
    assert (VarStackCount(psPatternVars) == VarStackCount(psScrutineeVars));

    boValidMap = TRUE;
    for (nVarTo = 0; nVarTo < nArityTo; ++nVarTo) {
        szVarTo = VarStackGet (psInputsTo, nVarTo);
        nStackPos = VarStackFind (psScrutineeVars, szVarTo);

        // Bound and unbound variables must match across usage
        boValidMap = ((psOperationMap->aszUnbound[nVarTo] == NULL) == (nStackPos >= 0));

        if (boValidMap) {
            if (nStackPos >= 0) {
                // This is a bound variable, so map it to an input
                szVarFrom = VarStackGet (psPatternVars, nStackPos);

                nValidCount = 0;
                for (nVarFrom = 0; nVarFrom < nArityFrom; ++ nVarFrom) {
                    szRelationVar = VarStackGet (psInputsFrom, nVarFrom);
                    if (strcmp (szVarFrom, szRelationVar) != 0) {
                        psOperationMap->aaboVarOrigin[(nVarTo * nArityFrom) + nVarFrom] = FALSE;
                    }
                    if (psOperationMap->aaboVarOrigin[(nVarTo * nArityFrom) + nVarFrom]) {
                        nValidCount += 1;
                    }
                }
                if (nValidCount == 0) {
                    boValidMap = FALSE;
                }
            }
            else {
                // This is an unbound variable, so it must match with previous instances
                boValidMap = ((psOperationMap->aszUnbound[nVarTo] != NULL) && (strcmp (szVarTo, psOperationMap->aszUnbound[nVarTo]) == 0));
            }
        }
    }

    psInputsTo = FreeVarStack (psInputsTo);
    psInputsFrom = FreeVarStack (psInputsFrom);

    return boValidMap;
}

bool OperationMapVarOriginsCheckClear (OperationMap * psOperationMap, Operation const * psFrom) {
    int nArityFrom;
    int nArityTo;
    int nVarFrom;
    int nVarTo;
    VarStack * psInputsFrom;
    char const * szVarFrom;
    char const * szRelationVar;
    bool boValidMap;
    int nValidCount;

    psInputsFrom = CreateVarStack ();
    nArityFrom = OperationInputList (psFrom, psInputsFrom);

    nArityFrom = psOperationMap->nArityFrom;
    nArityTo = psOperationMap->nArityTo;


    boValidMap = TRUE;
    for (nVarTo = 0; nVarTo < nArityTo; ++nVarTo) {
        szVarFrom = VarStackGet (psInputsFrom, nVarTo);

        nValidCount = 0;
        for (nVarFrom = 0; nVarFrom < nArityFrom; ++ nVarFrom) {
            szRelationVar = VarStackGet (psInputsFrom, nVarFrom);
            if (strcmp (szVarFrom, szRelationVar) != 0) {
                psOperationMap->aaboVarOrigin[(nVarTo * nArityFrom) + nVarFrom] = FALSE;
            }
            if (psOperationMap->aaboVarOrigin[(nVarTo * nArityFrom) + nVarFrom]) {
                nValidCount += 1;
            }
        }
        if (nValidCount == 0) {
            boValidMap = FALSE;
        }
    }

    psInputsFrom = FreeVarStack (psInputsFrom);

    return boValidMap;
}

void OperationMapSetFromCheck (OperationMap * psOperationMap, Operation const * psOp) {
    if (psOperationMap) {
        if (psOperationMap->psFrom) {
            FreeRecursive(psOperationMap->psFrom);
        }
        psOperationMap->psFrom = CopyRecursive(psOp);

        psOperationMap->nArityFrom = OperationArity (psOperationMap->psFrom);
        psOperationMap->nArityTo = psOperationMap->nArityFrom;

        OperationMapInitVarOrigins (psOperationMap);
    }
}

