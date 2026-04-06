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
 * Implements functionality for converting to and from LaTeX
 * strings.
 *
 */

//////////////////////////////////////////////////////////////////
// Includes

#include "symbolic.h"
#include "symbolic_private.h"

#include <stdio.h>
#include <string.h>
#include <stdarg.h>

//////////////////////////////////////////////////////////////////
// Defines

// Once the continued fraction approximation is within this epsilon
// of the actual value of the fraction the approximation operation
// will finish
#define VARIABLE_CHARS "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_"

//////////////////////////////////////////////////////////////////
// Structures

//////////////////////////////////////////////////////////////////
// Global variables

// Textual equivalents of the unary operations
static char const aszOpUnaryLatex[OPUNARY_NUM][7] = {
	"\\\\neg",
};

// Textual equivalents of the binary operations
static char const aszOpBinaryLatex[OPBINARY_NUM][17] = {
	"\\\\land",
	"\\\\lor",
	"\\\\to",
	"\\\\nleftrightarrow",
};

//////////////////////////////////////////////////////////////////
// Function prototypes

Operation * RecurseToOperationLatex (char const * szString, int nStrLen);
bool StringCheckBinaryLatex (char const * szString, int const nStrLen, char const * szOperator);
bool TryStringToTruthLatex (char const * const szString, int const nStrLen, bool * pboTruth);

//////////////////////////////////////////////////////////////////
// Main application

/**
 * Turn a formula into a LaTeX string.
 *
 * @param psOp the operaton to convert.
 * @param szString pre-allocated buffer to store the result.
 * @param nStrLen the length of the buffer. Use OperationToStringLength to find out how much is needed.
 * @return pointer to the resulting string (which will be the start of the buffer).
 */
char * OperationToStringLatex (Operation * psOp, char * szString, int nStrLen) {
	char * szRecurse;

	// Recursively convert the operation to a string
	// Memory is dynamically allocated for this
	szRecurse = RecurseToStringLatex (psOp, nStrLen);
	// Store the result in the user's buffer
	snprintf(szString, nStrLen, "$%s$", szRecurse);

	// Ensure the string is correctly terminated no matter what
	szString[nStrLen - 1] = 0;
	// Free up the temporary buffer
	PropFree (szRecurse);

	return szString;
}

/**
 * Recursively turn a formula into a LaTeX string.
 * Internal method. Directly allocates memory for the result,
 * which must be freed manually once it's no longer needed
 * using PropFree.
 *
 * @param psOp the operation to convert.
 * @param nStrLen the maximum length the string can take.
 * @return the resulting string in allocated memory.
 *
 */
char * RecurseToStringLatex (Operation * psOp, int nStrLen) {
	char * szReturn;
	char * szVar1;
	char * szVar2;
	char * szVar3;

	// Allcate memory for the result, initialised to zeros
	szReturn = (char*)PropCalloc (nStrLen, 1);

	// Convert the operations recursively
	if (psOp) {
		switch (psOp->eOpType) {
			case OPTYPE_TRUTHVALUE:
				// Truth values
				if (psOp->Vars.boTruth) {
					snprintf (szReturn, nStrLen, "\\\\top");
					szReturn[nStrLen - 1] = 0;
				}
				else {
					snprintf (szReturn, nStrLen, "\\\\bot");
					szReturn[nStrLen - 1] = 0;
				}
				break;
			case OPTYPE_VARIABLE:
				// Variables
				snprintf (szReturn, nStrLen, "%s", psOp->Vars.psVar->szVar);
				szReturn[nStrLen - 1] = 0;
				break;
			case OPTYPE_UNARY:
				// Unary operations must be handled recursively
				// First convert the result the unary operation is applieed to
				szVar1 = RecurseToStringLatex (psOp->Vars.psUnary->psVar1, nStrLen);
				// We use a couple of arrays representing the operations
				switch (psOp->Vars.psUnary->eOpType) {
					case OPUNARY_NOT:
						// These operations are of the form *a (operation * applied to a)
						snprintf (szReturn, nStrLen, "%s %s", aszOpUnaryLatex[psOp->Vars.psUnary->eOpType], szVar1);
						szReturn[nStrLen - 1] = 0;
						break;
					default:
						// Whoa there, we don't know how to handle that
						printf("Invalid unary operator\n");
						break;
				}
				// Free up the allocated string since we've already a copy
				PropFree (szVar1);
				break;
			case OPTYPE_BINARY:
				// Binary operations must be applied recursively to their parameters
				// First convert the result for both parameters
				szVar1 = RecurseToStringLatex (psOp->Vars.psBinary->psVar1, nStrLen);
				szVar2 = RecurseToStringLatex (psOp->Vars.psBinary->psVar2, nStrLen);
				// We can use an array of strings for most of these
				switch (psOp->Vars.psBinary->eOpType) {
					case OPBINARY_LAND:
					case OPBINARY_LOR:
					case OPBINARY_LIMP:
					case OPBINARY_LEOR:
						// Of the form (a * b) where * is the operation
						snprintf (szReturn, nStrLen, "(%s %s %s)", szVar1, aszOpBinaryLatex[psOp->Vars.psBinary->eOpType], szVar2);
						szReturn[nStrLen - 1] = 0;
						break;
					default:
						// These operations are of the form *a (operation * applied to a)
						printf("Invalid binary operator\n");
						break;
				}
				// Free up the allocated strings since we've already copied them
				PropFree (szVar1);
				PropFree (szVar2);
				break;
			default:
				// These operations are of the form *a (operation * applied to a)
				printf("Invalid operation type\n");
				break;
		}
	}
	else {
		// The operation is NULL
		strncpy (szReturn, "", nStrLen);
	}

	return szReturn;
}

/**
 * Return length of a formula turned into a LaTeX string.
 *
 * @param psOp the operation to check.
 * @return the length.
 *
 */
int OperationToStringLengthLatex (Operation * psOp) {
	int nLength;

	// This method is just a wrapper around the internal version
	// This is done for function naming consistency
	// We include two extra characters for the surrounding $ symbols
	nLength = RecurseToStringLengthLatex (psOp) + 2;

	return nLength;
}

/**
 * Recursively return the length of a formula turned into a LaTeX string.
 *
 * @param psOp the operation to check.
 * @return the length.
 *
 */
int RecurseToStringLengthLatex (Operation * psOp) {
	int nReturn;
	int nVar1;
	int nVar2;
	int nVar3;

	// The string will have zero length unless we determine otherwise
	nReturn = 0;

	// Check the operations reccursively
	if (psOp) {
		switch (psOp->eOpType) {
			case OPTYPE_TRUTHVALUE:
				// TRUE or FALSE
				if (psOp->Vars.boTruth) {
					//snprintf (NULL, 0, "\\\\top");
					nReturn = 5;
				}
				else {
					//snprintf (NULL, 0, "\\\\bot");
					nReturn = 5;
				}
				break;
			case OPTYPE_VARIABLE:
				// The length of the variable name
				nReturn = snprintf (NULL, 0, "%s", psOp->Vars.psVar->szVar);
				break;
			case OPTYPE_UNARY:
				// The length of the function combined with the parameter
				// Calculate the length of the parameter first
				nVar1 = RecurseToStringLengthLatex (psOp->Vars.psUnary->psVar1);
				switch (psOp->Vars.psUnary->eOpType) {
					case OPUNARY_NOT:
						//nReturn = snprintf (NULL, 0, "%s %s", aszOpUnaryLatex[psOp->Vars.psUnary->eOpType], szVar1);
						nReturn = strlen (aszOpUnaryLatex[psOp->Vars.psUnary->eOpType]) + 1 + nVar1;
						break;
					default:
						// Not something we know about (shouldn't happen)
						printf("Invalid unary operator\n");
						break;
				}
				break;
			case OPTYPE_BINARY:
				// The length of the function combined with the parameters
				// Calculate the lengths of the parameters first
				nVar1 = RecurseToStringLengthLatex (psOp->Vars.psBinary->psVar1);
				nVar2 = RecurseToStringLengthLatex (psOp->Vars.psBinary->psVar2);
				switch (psOp->Vars.psBinary->eOpType) {
					case OPBINARY_LAND:
					case OPBINARY_LOR:
					case OPBINARY_LIMP:
					case OPBINARY_LEOR:
						//nReturn = snprintf (NULL, 0, "(%s %s %s)", szVar1, aszOpBinaryLatex[psOp->Vars.psBinary->eOpType], szVar2);
						nReturn = nVar1 + strlen (aszOpBinaryLatex[psOp->Vars.psBinary->eOpType]) + nVar2 + 4;
						break;
					default:
						// Not something we know about (shouldn't happen)
						printf("Invalid binary operator\n");
						break;
				}
				break;
			default:
				// Not something we know about (shouldn't happen)
				printf("Invalid operation type\n");
				break;
		}
	}
	else {
		// NULL operation
		nReturn = 0;
	}

	return nReturn;
}

/**
 * Turn a LaTeX string into a formula.
 * The string has to be well-formed for this to work
 * This will allocate memory for the operations structures
 * on the heap. The result is guaranteed to be acyclyc and
 * should be freed using FreeRecursive once it's no longer
 * needed further.
 * TODO: Tackle the case of a non-well-formed string.
 *
 * @param szString: the null-terminated string to convert.
 * @return the resulting nested operation structure.
 *
 */
Operation * StringToOperationLatex (char const * szString) {
	int nStrLen;
	Operation * psOperation;
	char * szNoSpaces;
	int nStrPos;
	int nNoSpacePos;

	// Establish the length of the string
	nStrLen = (int)strlen (szString);

	// Remove all spaces, newlines and dollar symbols
	szNoSpaces = (char *)PropMalloc (nStrLen + 1);
	nNoSpacePos = 0;
	for (nStrPos = 0; nStrPos < nStrLen; nStrPos++) {
		// Check whether this is a character to skip
		if (strchr (" \n\r\t$", szString[nStrPos]) == NULL) {
			// If not, shift characters down in memory
			szNoSpaces[nNoSpacePos] = szString[nStrPos];
			// Move the copy-to position onwards if we write a character
			nNoSpacePos++;
		}
	}
	// Ensure we terminate the string
	szNoSpaces[nNoSpacePos] = '\0';

	// Now turn it in to an operation recursively
	psOperation = RecurseToOperationLatex (szNoSpaces, nNoSpacePos);

	// Free up our copy of the string with no spaces
	PropFree (szNoSpaces);

	return psOperation;
}

/**
 * Check a LaTeX string fragment to see if it's a binary operator
 * Internal method.
 *
 * @param szString the string to check.
 * @param nStrLen the length of the string.
 * @return TRUE if the operator and string match, FALSE otherwise.
 *
 */
bool StringCheckBinaryLatex (char const * szString, int const nStrLen, char const * szOperator) {
	bool boMatch = FALSE;
	int nOperatorLen;

	// Establish the length of the operator
	nOperatorLen = (int)strlen (szOperator);

	// Compare the two as a single string match
	if ((nStrLen >= nOperatorLen) && (strncmp (szString, szOperator, nOperatorLen) == 0)) {
		boMatch = TRUE;
	}

	return boMatch;
}

/**
 * Recursively turn a LaTeX string into a formula
 * Internal method; use StringToOperation instead
 * The string has to be well-formed for this to work
 * This will allocate memory for the operations structures
 * on the heap. The result is guaranteed to be acyclyc and
 * should be freed using FreeRecursive once it's no longer
 * needed further.
 * Note that in this case the string may not be null-terminated
 * TODO: Tackle the case of a non-well-formed string.
 * Internal operation.
 *
 * @param szString the string to convert.
 * @param nStrLen the length of the string still to be processed.
 * @return the resulting nested operation structure.
 *
 */
Operation * RecurseToOperationLatex (char const * szString, int nStrLen) {
	int nBrackets;
	int nPos = 0;
	bool boMatch;
	OPUNARY eUnary = OPUNARY_INVALID;
	OPBINARY eBinary = OPBINARY_INVALID;
	Operation * psReturnOp = NULL;
	int nRightStart;
	double fDecimal;
	bool boScanned;
	char * szVariable = NULL;
	int nNameEnd;
	bool boTruth;

	// Remove the edge brackets
	boMatch = TRUE;
	while ((nStrLen > 1) && boMatch && (szString[0] == '(') && (szString[(nStrLen - 1)] == ')')) {
		// Check whether these brackets match
		nBrackets = 0;
		for (nPos = 0; ((nPos < (nStrLen - 1)) && (boMatch)); nPos++) {
			if (szString[nPos] == '(') {
				// Opening bracket (increase bracket count)
				nBrackets++;
			}
			if (szString[nPos] == ')') {
				// Closing bracket (decrease bracket count)
				nBrackets--;
			}
			if (nBrackets == 0) {
				// We ran out of brackts, which we shouldn't have
				// so there's no way the brackets can be fixed
				// later in the string
				boMatch = FALSE;
			}
		}
		if (boMatch) {
			// We dealt with two characters at either end of the string
			szString++;
			nStrLen -= 2;
		}
	}

	// Find the operation with highest precedent
	boMatch = FALSE;
	eBinary = (OPBINARY)((int)OPBINARY_INVALID + 1);
	// We need to check for each binary operation until we match
	// Potentially this could be optimised by looping through the operations
	// after finding the hightest precedent operation, rather than the other
	// way around.
	while ((!boMatch) && (eBinary < OPBINARY_NUM)) {
		nBrackets = 0;
		// Note we start with nPos = 1, so because the LHS of the binary
		// operation has to exist in this case
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
				// So we should check whether this is the binary operation we need
				boMatch = StringCheckBinaryLatex (szString + nPos, nStrLen - nPos, aszOpBinaryLatex[eBinary]);
			}
		}
		// Move on to check the next operation
		eBinary = (OPBINARY)((int)eBinary + 1);
	}

	if (boMatch) {
		// Split into two pieces and recurse
		eBinary = (OPBINARY)((int)eBinary - 1);
		nRightStart = nPos + (int)strlen(aszOpBinaryLatex[eBinary]) - 1;

		psReturnOp = CreateBinary (eBinary, RecurseToOperationLatex (szString, nPos - 1), RecurseToOperationLatex (szString + nRightStart, nStrLen - nRightStart));
	}
	else {
		// Check if it's a unary operation
		boMatch = FALSE;
		eUnary = (OPUNARY)((int)OPUNARY_INVALID + 1);
		while ((!boMatch) && (eUnary < OPUNARY_NUM)) {
			if (nStrLen > (int)strlen (aszOpUnaryLatex[eUnary])) {
				// String compare with the possible unary operations
				if (strncmp (aszOpUnaryLatex[eUnary], szString, strlen (aszOpUnaryLatex[eUnary])) == 0) {
					boMatch = TRUE;
				}
			}

			// Move on to check the next operation
			eUnary = (OPUNARY)((int)eUnary + 1);
		}

		if (boMatch) {
			// Recurse on whatever is left
			eUnary = (OPUNARY)((int)eUnary - 1);
			nRightStart = (int)strlen(aszOpUnaryLatex[eUnary]);
			psReturnOp = CreateUnary (eUnary, RecurseToOperationLatex (szString + nRightStart, nStrLen - nRightStart));
		}
		else {
			boMatch = TryStringToTruthLatex (szString, nStrLen, &boTruth);
			if (boMatch) {
				psReturnOp = CreateTruthValue (boTruth);
			}
			else {
				// Interpret as a variable, since it's all that's left
				// TODO: Check whether this can really be a valid variable name (e.g. no brackets)
				szVariable = (char *)PropMalloc (nStrLen + 1);
				strncpy (szVariable, szString, nStrLen);
				szVariable[nStrLen] = '\0';
				psReturnOp = CreateVariable (szVariable);
				PropFree (szVariable);
				szVariable = NULL;
			}
		}
	}

	return psReturnOp;
}

/**
 * Try to convert a string to a boolean.
 * Internal method.
 *
 * @param szString the string to convert (may not be zero terminated).
 * @param nStrLen the length of the string.
 * @param pboTruth return value of the truth value if it could be converted (unchanged o/w).
 * @return TRUE if the conversion was successful, FALSE o/w.
 *
 */
bool TryStringToTruthLatex (char const * const szString, int const nStrLen, bool * pboTruth) {
	bool boResult = FALSE;
	int nMin;

	nMin = nStrLen < 5 ? nStrLen : 5;
	if ((nStrLen >= 5) && (strncmp ("\\\\top", szString, nMin) == 0)) {
		if (pboTruth) {
			*pboTruth = TRUE;
		}
		boResult = TRUE;
	}
	else {
		nMin = nStrLen < 5 ? nStrLen : 5;
		if ((nStrLen >= 5) && (strncmp ("\\\\bot", szString, nMin) == 0)) {
			if (pboTruth) {
				*pboTruth = FALSE;
			}
			boResult = TRUE;
		}
	}

	return boResult;
}

