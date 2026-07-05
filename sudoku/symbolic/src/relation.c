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
 * Symbolic has a collection of build in functions that it knows
 * about automatically. Sometimes these may not be enough. The
 * UserUnary functions provide an interface for registering
 * new unary functions defined by the developer, extending the
 * library's capabilities.
 *
 */

//////////////////////////////////////////////////////////////////
// Includes

#include "symbolic.h"
#include "symbolic_private.h"

#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include <math.h>

//////////////////////////////////////////////////////////////////
// Defines

//////////////////////////////////////////////////////////////////
// Structures

// Binary operations
typedef enum _RELBINARY {
	RELBINARY_INVALID = -1,

	RELBINARY_EQUALS,

	RELBINARY_NUM
} RELBINARY;

//////////////////////////////////////////////////////////////////
// Global variables

// Textual equivalents of the binary relations
static char const aszRelBinary[RELBINARY_NUM][6] = {
	"=",
};

//////////////////////////////////////////////////////////////////
// Function prototypes

//////////////////////////////////////////////////////////////////
// Main application

/**
 * Create a new Relation structure with the given details.
 *
 * The strings representing the name and variable names must all be zero-terminated.
 *
 * @param szName the zero-terminated name of the relation.
 * @param nArity the number of variables the relation acts on.
 * @param aszVar array of length nArity containing the zero-terminated variable names.
 * @return the resulting Relation operation structure.
 *
 */
Operation * CreateRelation (char const * szName, size_t nArity, char * const * aszVar) {
    Operation * psOp;
    size_t nVar;

    psOp = (Operation*)PropMalloc (sizeof(Operation));
    psOp->eOpType = OPTYPE_RELATION;
    psOp->Vars.psRelation = (OpRelation*)PropMalloc(sizeof(OpRelation));

    psOp->Vars.psRelation->szName = PropMalloc (strlen (szName) + 1);
    strcpy (psOp->Vars.psRelation->szName, szName);

    psOp->Vars.psRelation->nArity = nArity;

    psOp->Vars.psRelation->aszVar = (char **)PropMalloc (sizeof(char *) * nArity);
    for (nVar = 0; nVar < nArity; ++nVar) {
        psOp->Vars.psRelation->aszVar[nVar] = (char *)PropMalloc (strlen (aszVar[nVar]) + 1);
        strcpy (psOp->Vars.psRelation->aszVar[nVar], aszVar[nVar]);
    }

    return psOp;
}

/**
 * Create a new Relation structure with the given details with string lengths specified.
 *
 * For the name and each of the variable names, the length of each string is specified
 * rather than them having to be zero-terminated strings.
 *
 * @param szName the name of the relation.
 * @param uLength the length of the name in bytes.
 * @param nArity the number of variables the relation acts on.
 * @param aszVar array of length nArity containing the names of the variables.
 * @param auVarLen array of length nArity containing the length of each variable name.
 * @return the resulting Relation operation structure.
 *
 */
Operation * CreateRelationLength (char const * szName, size_t uLength, size_t nArity, char const * const * aszVar, size_t * auVarLen) {
    Operation * psOp;
    size_t uVar;

    psOp = (Operation*)PropMalloc (sizeof(Operation));
    psOp->eOpType = OPTYPE_RELATION;
    psOp->Vars.psRelation = (OpRelation*)PropMalloc(sizeof(OpRelation));

    psOp->Vars.psRelation->szName = PropMalloc (uLength + 1);
    strncpy (psOp->Vars.psRelation->szName, szName, uLength);
    psOp->Vars.psRelation->szName[uLength] = 0;

    psOp->Vars.psRelation->nArity = nArity;

    psOp->Vars.psRelation->aszVar = (char **)PropMalloc (sizeof(char *) * nArity);
    for (uVar = 0; uVar < nArity; ++uVar) {
        psOp->Vars.psRelation->aszVar[uVar] = (char *)PropMalloc (auVarLen[uVar] + 1);
        strncpy (psOp->Vars.psRelation->aszVar[uVar], aszVar[uVar], auVarLen[uVar]);
        psOp->Vars.psRelation->aszVar[uVar][auVarLen[uVar]] = 0;
    }

    return psOp;
}

Operation * CreateRelationBinaryLength (char const * szName, size_t uLength, char const * szVar1, size_t uVar1Len, char const * szVar2, size_t uVar2Len) {
    Operation * psOp;
    int nVarLen;
    int nStart;

    psOp = (Operation*)PropMalloc (sizeof(Operation));
    psOp->eOpType = OPTYPE_RELATION;
    psOp->Vars.psRelation = (OpRelation*)PropMalloc(sizeof(OpRelation));

    psOp->Vars.psRelation->szName = PropMalloc (uLength + 1);
    strncpy (psOp->Vars.psRelation->szName, szName, uLength);
    psOp->Vars.psRelation->szName[uLength] = 0;

    psOp->Vars.psRelation->nArity = 2;

    psOp->Vars.psRelation->aszVar = (char **)PropMalloc (sizeof(char *) * 2);

    nVarLen = uVar1Len;
    StringGetBounds (szVar1, &nStart, &nVarLen);
    psOp->Vars.psRelation->aszVar[0] = (char *)PropMalloc (nVarLen + 1);
    strncpy (psOp->Vars.psRelation->aszVar[0], szVar1 + nStart, nVarLen);
    psOp->Vars.psRelation->aszVar[0][nVarLen] = 0;

    nVarLen = uVar2Len;
    StringGetBounds (szVar2, &nStart, &nVarLen);
    psOp->Vars.psRelation->aszVar[1] = (char *)PropMalloc (nVarLen + 1);
    strncpy (psOp->Vars.psRelation->aszVar[1], szVar2 + nStart, nVarLen);
    psOp->Vars.psRelation->aszVar[1][nVarLen] = 0;

    return psOp;
}

/**
 * Recursively copy a formula and all its subformulas.
 *
 * @param
 * @return
 *
 */
Operation * CopyRelation (Operation const * psOp) {
    Operation * psReturn;

    psReturn = CreateRelation (psOp->Vars.psRelation->szName, psOp->Vars.psRelation->nArity, psOp->Vars.psRelation->aszVar);

    return psReturn;
}

/**
 * Compare two formulae recursively.
 *
 * @param
 * @return
 *
 */
bool RelationCompare (Operation const * psOp1, Operation const * psOp2) {
    bool boReturn = TRUE;
    size_t nVar;

    boReturn = RelationComparePattern (psOp1, psOp2);

    nVar = 0;
    while (boReturn && (nVar < psOp1->Vars.psRelation->nArity)) {
        boReturn = (strcmp(psOp1->Vars.psRelation->aszVar[nVar], psOp2->Vars.psRelation->aszVar[nVar]) == 0);
        nVar += 1;
    }

    return boReturn;
}

/**
 * Recursively turn a formula into a string
 *
 * Internal method. Directly allocates memory for the result,
 * which must be freed manually once it's no longer needed
 * using PropFree
 *
 * @param psOp the operation to convert
 * @param nStrLen the maximum length the string can take.
 * @return the resulting string in allocated memory.
 *
 */
char * RelationToString (Operation const * psOp, char * szString, int nStrLen) {
    size_t nPos;
    size_t nVar;
    bool boFound;

    // Convert the operations recursively
    if ((psOp) && (psOp->eOpType == OPTYPE_RELATION)) {
        // If the arity is zero, skip the brackets
        if (psOp->Vars.psRelation->nArity == 0) {
            // This operations is of the form "name"
            nPos = snprintf (szString, nStrLen, "%s", psOp->Vars.psRelation->szName);
        }
        else {
            boFound = FALSE;
            if (psOp->Vars.psRelation->nArity == 2) {
                for (nPos = 0; (nPos < RELBINARY_NUM) && (!boFound); ++nPos) {
                    if (strcmp (aszRelBinary[nPos], psOp->Vars.psRelation->szName) == 0) {
                        // This operation is of the form "var name var"
                        snprintf (szString, nStrLen, "%s %s %s", psOp->Vars.psRelation->aszVar[0], psOp->Vars.psRelation->szName, psOp->Vars.psRelation->aszVar[1]);
                        boFound = TRUE;
                    }
                }
            }

            if (!boFound) {
                // This operation is of the form "name(var, var, var,...)"
                nPos = snprintf (szString, nStrLen, "%s(", psOp->Vars.psRelation->szName);

                nVar = 0;
                while ((nVar < psOp->Vars.psRelation->nArity) && (nPos < nStrLen)) {
                    if (nVar < (psOp->Vars.psRelation->nArity - 1)) {
                        nPos += snprintf (szString + nPos, nStrLen - nPos, "%s, ", psOp->Vars.psRelation->aszVar[nVar]);
                    }
                    else {
                        nPos += snprintf (szString + nPos, nStrLen - nPos, "%s)", psOp->Vars.psRelation->aszVar[nVar]);
                    }
                    nVar += 1;
                }
            }
        }
        szString[nStrLen - 1] = 0;
    }
    else {
        // The operation is NULL
        strncpy (szString, "", nStrLen);
    }

    return szString;
}

/**
 * Recursively return the length of a formula turned into a string.
 *
 * @param psOp the operation to check.
 * @return the length.
 *
 */
int RelationToStringLength (Operation const * psOp) {
    size_t nPos;
    int nReturn;
    size_t nVar;
    bool boFound;

    // The string will have zero length unless we determine otherwise
    nReturn = 0;

    // Convert the operations recursively
    if ((psOp) && (psOp->eOpType == OPTYPE_RELATION)) {
        // If the arity is zero, skip the brackets
        if (psOp->Vars.psRelation->nArity == 0) {
            // This operations is of the form name(var, var, var,...)
            nReturn += strlen (psOp->Vars.psRelation->szName);
        }
        else {
            boFound = FALSE;
            if (psOp->Vars.psRelation->nArity == 2) {
                for (nPos = 0; (nPos < RELBINARY_NUM) && (!boFound); ++nPos) {
                    if (strcmp (aszRelBinary[nPos], psOp->Vars.psRelation->szName) == 0) {
                        // This operation is of the form "var name var"
                        nReturn += strlen (psOp->Vars.psRelation->szName) + strlen(psOp->Vars.psRelation->aszVar[0]) + strlen(psOp->Vars.psRelation->aszVar[1]) + 2;
                        boFound = TRUE;
                    }
                }
            }

            if (!boFound) {
                // This operation is of the form name(var, var, var,...)
                nReturn += strlen (psOp->Vars.psRelation->szName) + 1;

                nVar = 0;
                while (nVar < psOp->Vars.psRelation->nArity) {
                    if (nVar < (psOp->Vars.psRelation->nArity - 1)) {
                        nReturn += strlen(psOp->Vars.psRelation->aszVar[nVar]) + 2;
                    }
                    else {
                        nReturn += strlen(psOp->Vars.psRelation->aszVar[nVar]) + 1;
                    }
                    nVar += 1;
                }
            }
        }
    }
    else {
        // The operation is NULL
        nReturn = 0;
    }

    return nReturn;
}

/**
 * Recursively turn a formula into a string using float (0.0f) notation
 *
 * This will output a string that can be compiled into C/C++ or GLSL
 * Internal method. Directly allocates memory for the result,
 * which must be freed manually once it's no longer needed
 * using PropFree
 *
 * @param psOp the operation to convert.
 * @param nStrLen the maximum length the string can take.
 * @return the resulting string in allocated memory.
 *
 */
char * RelationToStringC (Operation * psOp, char * szString, int nStrLen) {
    return RelationToString (psOp, szString, nStrLen);
}

/**
 * Recursively turn a formula into a LaTeX string
 *
 * Internal method. Directly allocates memory for the result,
 * which must be freed manually once it's no longer needed
 * using PropFree
 *
 * @param psOp the operation to convert
 * @param nStrLen the maximum length the string can take.
 * @return the resulting LaTeX string in allocated memory.
 *
 */
int RelationToStringCLength (Operation * psOp) {
    return RelationToStringLength (psOp);
}

/**
 * Recursively return the length of a formula turned into a LaTeX string.
 *
 * @param psOp the operation to check.
 * @return the length of the LaTeX string.
 *
 */
char * RelationToStringLatex (Operation * psOp, char * szString, int nStrLen) {
    return RelationToString (psOp, szString, nStrLen);
}

/**
 * Recursively return the length of a formula turned into a string.
 *
 * @param psOp the operation to check.
 * @return the length.
 *
 */
int RelationToStringLengthLatex (Operation * psOp) {
    return RelationToStringLength (psOp);
}

/**
 * Recursively free up all of the memory used by a formula and its sub formulas.
 *
 * @param
 * @return
 *
 */
void RelationFreeRecursive (Operation * psOp) {
    size_t nVar;

    if ((psOp) && (psOp->eOpType == OPTYPE_RELATION)) {
        PropFree (psOp->Vars.psRelation->szName);
        nVar = 0;
        while (nVar < psOp->Vars.psRelation->nArity) {
            PropFree (psOp->Vars.psRelation->aszVar[nVar]);
            nVar += 1;
        }

        PropFree (psOp->Vars.psRelation->aszVar);
        PropFree (psOp->Vars.psRelation);
    }
}

/**
 * Test whether a string can be converted into a relation operation.
 * Internal method.
 *
 * @param szString the string to convert (may not be zero terminated).
 * @param nStrLen the length of the string.
 * @param pnArity filled with the number of variables if non-NULL and a valid relation.
 * @return TRUE if the string is a valid relation, FALSE o/w.
 *
 */
bool TryRelation (char const * szString, int nStrLen, int *pnArity) {
    bool boMatch;
    int nPos;
    int nEnd;
    int nNameStart;
    int nNameLen;
    int nVars;
    int nBrackets;
	RELBINARY eRelation = RELBINARY_INVALID;

    // Check for special binary relations
	boMatch = FALSE;
	nVars = 2;
	// We need to check for each binary relation until we match
	nBrackets = 0;
	for (nPos = 0; ((nPos < nStrLen) && (!boMatch)); nPos++) {
		if (szString[nPos] == '(') {
			// Opening bracket (increase bracket count)
			nBrackets++;
		}
		if (szString[nPos] == ')') {
			// Closing bracket (decrease bracket count)
			nBrackets--;
		}
		if ((nBrackets == 0) && (nPos > 0)) {
			// We're at the lowest level, right in the bowels of the formula
			// So we should check whether this is a binary relation
	        eRelation = (RELBINARY)((int)RELBINARY_INVALID + 1);
	        while ((!boMatch) && (eRelation < RELBINARY_NUM)) {
		        boMatch = StringCheckBinary (szString + nPos, nStrLen - nPos, aszRelBinary[eRelation]);
		        // Move on to check the next operation
		        eRelation = (RELBINARY)((int)eRelation + 1);
	        }
		}
	}

    if (!boMatch) {
        // Let's assume this is a relation
        boMatch = TRUE;

        // Read until we reach a non-space character
        nPos = 0;
        while ((nPos < nStrLen) && (strchr(WHITESPACE_CHARS, szString[nPos]) != NULL)) {
            nPos++;
        }
        nNameStart = nPos;

        // Read until we reach a non variable-name character
        while ((nPos < nStrLen) && (strchr(VARIABLE_CHARS, szString[nPos]) != NULL)) {
            nPos++;
        }
        nNameLen = nPos - nNameStart;
        boMatch = (nNameLen > 0);

        if (boMatch) {
            // Read until we reach a non-space character
            while ((nPos < nStrLen) && (strchr(WHITESPACE_CHARS, szString[nPos]) != NULL)) {
                nPos++;
            }

            // Check what character we've reached
            nVars = 0;
            if ((nPos < nStrLen) && (szString[nPos] == '(')) {
                // This might just work

                while (boMatch && (nPos < nStrLen) && (strchr("(,", szString[nPos]) != NULL)) {
                    nPos++;
                    while ((nPos < nStrLen) && (strchr(WHITESPACE_CHARS, szString[nPos]) != NULL)) {
                        nPos++;
                    }
                    nNameStart = nPos;

                    while ((nPos < nStrLen) && (strchr(VARIABLE_CHARS, szString[nPos]) != NULL)) {
                        nPos++;
                    }
                    nNameLen = nPos - nNameStart;
                    boMatch = (nNameLen > 0);

                    while ((nPos < nStrLen) && (strchr(WHITESPACE_CHARS, szString[nPos]) != NULL)) {
                        nPos++;
                    }
                    nVars += 1;
                }

                if (boMatch) {
                    boMatch = ((nPos < nStrLen) && (szString[nPos] == ')'));
                    nPos += 1;
                }
            }
        }

        if (boMatch) {
            while ((nPos < nStrLen) && (strchr(WHITESPACE_CHARS, szString[nPos]) != NULL)) {
                nPos++;
            }

            boMatch = (nPos == nStrLen);
        }
    }


    if (boMatch && (pnArity != NULL)) {
        *pnArity = nVars;
    }

    return boMatch;
}

/**
 * Convert a string into a relation operation.
 * Internal method.
 *
 * The nArity parameter is likely to have been collected previously from a call to
 * TryRelation().
 *
 * @param szString the string to convert (may not be zero terminated).
 * @param nStrLen the length of the string.
 * @param nArity The number of variables of the relation
 * @return The resulting operation structure or NULL if an error occurred.
 *
 */
Operation * StringToRelation (char const * szString, int nStrLen, int nArity) {
    bool boMatch;
    int nPos;
    int nEnd;
    int nNameStart;
    int nNameLen;
    int nVars;
    char const * * aszVarStart;
    size_t * anVarLen;
    Operation * psOp;
    int nBrackets;
	RELBINARY eRelation = RELBINARY_INVALID;
	int nRightStart;

    // Check for special binary relations
	boMatch = FALSE;
	nVars = 2;
	// We need to check for each binary relation until we match
	nBrackets = 0;
	for (nPos = 0; ((nPos < nStrLen) && (!boMatch)); nPos++) {
		if (szString[nPos] == '(') {
			// Opening bracket (increase bracket count)
			nBrackets++;
		}
		if (szString[nPos] == ')') {
			// Closing bracket (decrease bracket count)
			nBrackets--;
		}
		if ((nBrackets == 0) && (nPos > 0)) {
			// We're at the lowest level, right in the bowels of the formula
			// So we should check whether this is a binary relation
	        eRelation = (RELBINARY)((int)RELBINARY_INVALID + 1);
	        while ((!boMatch) && (eRelation < RELBINARY_NUM)) {
		        boMatch = StringCheckBinary (szString + nPos, nStrLen - nPos, aszRelBinary[eRelation]);
		        // Move on to check the next operation
		        eRelation = (RELBINARY)((int)eRelation + 1);
	        }
		}
	}

    if (boMatch) {
	    // We found a binary relation
	    // Split into two pieces and recurse
	    eRelation = (RELBINARY)((int)eRelation - 1);
	    nRightStart = nPos + (int)strlen(aszRelBinary[eRelation]) - 1;
	    while ((nRightStart < nStrLen) && (strchr (WHITESPACE_CHARS, szString[nRightStart]) != NULL)) {
		    nRightStart += 1;
	    }
	    psOp = CreateRelationBinaryLength (aszRelBinary[eRelation], strlen (aszRelBinary[eRelation]), szString, nPos - 1, szString + nRightStart, nStrLen - nRightStart);
    }
    else {
    // Let's assume this is a relation
        boMatch = TRUE;
        aszVarStart = (char const * *)PropMalloc(sizeof(char **) * nArity);
        anVarLen = (size_t *)PropMalloc(sizeof(size_t) * nArity);
        psOp = NULL;

        // Read until we reach a non-space character
        nPos = 0;
        while ((nPos < nStrLen) && (strchr(WHITESPACE_CHARS, szString[nPos]) != NULL)) {
            nPos++;
        }
        nNameStart = nPos;

        // Read until we reach a non variable-name character
        while ((nPos < nStrLen) && (strchr(VARIABLE_CHARS, szString[nPos]) != NULL)) {
            nPos++;
        }
        nNameLen = nPos - nNameStart;
        boMatch = (nNameLen > 0);

        if (boMatch) {
            // Read until we reach a non-space character
            while ((nPos < nStrLen) && (strchr(WHITESPACE_CHARS, szString[nPos]) != NULL)) {
                nPos++;
            }

            // Check what character we've reached
            nVars = 0;
            if ((nPos < nStrLen) && (szString[nPos] == '(')) {
                // This might just work

                while (boMatch && (nPos < nStrLen) && (nVars < nArity) && (strchr("(,", szString[nPos]) != NULL)) {
                    nPos++;
                    while ((nPos < nStrLen) && (strchr(WHITESPACE_CHARS, szString[nPos]) != NULL)) {
                        nPos++;
                    }
                    aszVarStart[nVars] = szString + nPos;

                    while ((nPos < nStrLen) && (strchr(VARIABLE_CHARS, szString[nPos]) != NULL)) {
                        nPos++;
                    }
                    anVarLen[nVars] = nPos - (aszVarStart[nVars] - szString);
                    boMatch = (anVarLen[nVars] > 0);

                    while ((nPos < nStrLen) && (strchr(WHITESPACE_CHARS, szString[nPos]) != NULL)) {
                        nPos++;
                    }
                    nVars += 1;
                }

                if (boMatch) {
                    boMatch = ((nPos < nStrLen) && (szString[nPos] == ')'));
                    nPos += 1;
                }
            }
        }

        if (boMatch) {
            while ((nPos < nStrLen) && (strchr(WHITESPACE_CHARS, szString[nPos]) != NULL)) {
                nPos++;
            }

            boMatch = (nPos == nStrLen);
        }

        if (boMatch) {
            psOp = CreateRelationLength (szString + nNameStart, nNameLen, nArity, aszVarStart, anVarLen);
        }
        PropFree(aszVarStart);
        PropFree(anVarLen);
    }

    return psOp;
}

/**
 * Compare two relations, but ignoring variable names.
 *
 * @param
 * @return
 *
 */
bool RelationComparePattern (Operation const * psOp1, Operation const * psOp2) {
    bool boReturn = TRUE;

    boReturn = ((psOp1) && (psOp1->eOpType == OPTYPE_RELATION) && (psOp2) && (psOp2->eOpType == OPTYPE_RELATION));

    if (boReturn && ((strcmp (psOp1->Vars.psRelation->szName, psOp2->Vars.psRelation->szName) != 0)
        || (psOp1->Vars.psRelation->nArity != psOp2->Vars.psRelation->nArity))) {
        boReturn = FALSE;
    }

    return boReturn;
}

/**
 * Compare two relations, but ignoring variable names.
 *
 * @param
 * @return
 *
 */
bool RelationComparePatternStack (Operation const * psOp1, Operation const * psOp2, VarStack const * psVarStack1, VarStack const * psVarStack2) {
    bool boReturn = TRUE;
    int nPos;
	int nVarPos1;
	int nVarPos2;

    boReturn = ((psOp1) && (psOp1->eOpType == OPTYPE_RELATION) && (psOp2) && (psOp2->eOpType == OPTYPE_RELATION));

    if (boReturn && ((strcmp (psOp1->Vars.psRelation->szName, psOp2->Vars.psRelation->szName) != 0)
        || (psOp1->Vars.psRelation->nArity != psOp2->Vars.psRelation->nArity))) {
        boReturn = FALSE;
    }

    for (nPos = 0; boReturn && (nPos < psOp1->Vars.psRelation->nArity); ++nPos) {
	    nVarPos1 = VarStackFind (psVarStack1, psOp1->Vars.psRelation->aszVar[nPos]);
	    nVarPos2 = VarStackFind (psVarStack2, psOp2->Vars.psRelation->aszVar[nPos]);

        boReturn = (nVarPos1 == nVarPos2);
    }

    return boReturn;
}
