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

#include <stdio.h>
#include <string.h>
#include <stdarg.h>

//////////////////////////////////////////////////////////////////
// Defines

// Once the continued fraction approximation is within this epsilon
// of the actual value of the fraction the approximation operation
// will finish
#define CONTINUED_FRACTION_ERROR (1.0e-10)
#define VARIABLE_CHARS "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_"

//////////////////////////////////////////////////////////////////
// Structures

//////////////////////////////////////////////////////////////////
// Global variables

//////////////////////////////////////////////////////////////////
// Function prototypes

Operation * RecurseToOperation (char const * szString, int nStrLen);
inline bool StringCheckBinary (char const * szString, int const nStrLen, char const * szOperator);
bool TryStringToDouble (char const * const szString, int const nStrLen, double * pfDecimal);
bool TryStringToTruth (char const * const szString, int const nStrLen, bool * pboTruth);
bool TryUndefinedUnary (char const * szString, int nStrLen, int * pnNameEnd);
bool CheckBracketsMatch (char const * szString, int nStrLen);

//////////////////////////////////////////////////////////////////
// Main application

/**
 * Turn a formula into a string.
 *
 * @param psOp the operaton to convert.
 * @param szString pre-allocated buffer to store the result.
 * @param nStrLen the length of the buffer. Use OperationToStringLength to find out how much is needed.
 * @return pointer to the resulting string (which will be the start of the buffer).
 */
char * OperationToString (Operation * psOp, char * szString, int nStrLen) {
	char * szRecurse;

	// Recursively convert the operation to a string
	// Memory is dynamically allocated for this
	szRecurse = RecurseToString (psOp, nStrLen);
	// Store the result in the user's buffer
	strncpy (szString, szRecurse, nStrLen);
	// Ensure the string is correctly terminated no matter what
	szString[nStrLen - 1] = 0;
	// Free up the temporary buffer
	PropFree (szRecurse);

	return szString;
}

/**
 * Recursively turn a formula into a string.
 * Internal method. Directly allocates memory for the result, 
 * which must be freed manually once it's no longer needed
 * using PropFree.
 *
 * @param psOp the operation to convert.
 * @param nStrLen the maximum length the string can take.
 * @return the resulting string in allocated memory.
 *
 */
char * RecurseToString (Operation * psOp, int nStrLen) {
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
					snprintf (szReturn, nStrLen, "TRUE");
					szReturn[nStrLen - 1] = 0;
				}
				else {
					snprintf (szReturn, nStrLen, "FALSE");
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
				szVar1 = RecurseToString (psOp->Vars.psUnary->psVar1, nStrLen);
				// We use a couple of arrays representing the operations
				switch (psOp->Vars.psUnary->eOpType) {
					case OPUNARY_NOT:
						// These operations are of the form *a (operation * applied to a)
						snprintf (szReturn, nStrLen, "%s%s", aszOpUnary[psOp->Vars.psUnary->eOpType], szVar1);
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
				szVar1 = RecurseToString (psOp->Vars.psBinary->psVar1, nStrLen);
				szVar2 = RecurseToString (psOp->Vars.psBinary->psVar2, nStrLen);
				// We can use an array of strings for most of these
				switch (psOp->Vars.psBinary->eOpType) {
					case OPBINARY_LAND:
					case OPBINARY_LOR:
					case OPBINARY_LIMP:
					case OPBINARY_LEOR:
						// Of the form (a * b) where * is the operation
						snprintf (szReturn, nStrLen, "(%s %s %s)", szVar1, aszOpBinary[psOp->Vars.psBinary->eOpType], szVar2);
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
 * Return length of a formula turned into a string.
 *
 * @param psOp the operation to check.
 * @return the length.
 *
 */
int OperationToStringLength (Operation * psOp) {
	int nLength;

	// This method is just a wrapper around the internal version
	// This is done for function naming consistency
	nLength = RecurseToStringLength (psOp);

	return nLength;
}

/**
 * Recursively return the length of a formula turned into a string.
 *
 * @param psOp the operation to check.
 * @return the length.
 *
 */
int RecurseToStringLength (Operation * psOp) {
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
					//snprintf (NULL, 0, "TRUE");
					nReturn = 4;
				}
				else {
					//snprintf (NULL, 0, "FALSE");
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
				nVar1 = RecurseToStringLength (psOp->Vars.psUnary->psVar1);
				switch (psOp->Vars.psUnary->eOpType) {
					case OPUNARY_NOT:
						//nReturn = snprintf (NULL, 0, "%s%s", aszOpUnary[psOp->Vars.psUnary->eOpType], szVar1);
						nReturn = strlen (aszOpUnary[psOp->Vars.psUnary->eOpType]) + nVar1;
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
				nVar1 = RecurseToStringLength (psOp->Vars.psBinary->psVar1);
				nVar2 = RecurseToStringLength (psOp->Vars.psBinary->psVar2);
				switch (psOp->Vars.psBinary->eOpType) {
					case OPBINARY_LAND:
					case OPBINARY_LOR:
					case OPBINARY_LIMP:
					case OPBINARY_LEOR:
						//nReturn = snprintf (NULL, 0, "(%s %s %s)", szVar1, aszOpBinary[psOp->Vars.psBinary->eOpType], szVar2);
						nReturn = nVar1 + strlen (aszOpBinary[psOp->Vars.psBinary->eOpType]) + nVar2 + 4;
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
 * Turn a string into a formula.
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
Operation * StringToOperation (char const * szString) {
	int nStrLen;
	Operation * psOperation;
	char * szNoSpaces;
	int nStrPos;
	int nNoSpacePos;

	// Establish the length of the string
	nStrLen = (int)strlen (szString);

	// Remove all spaces and newlines
	szNoSpaces = (char *)PropMalloc (nStrLen + 1);
	nNoSpacePos = 0;
	for (nStrPos = 0; nStrPos < nStrLen; nStrPos++) {
		// Check whether this is a character to skip
		if (strchr (" \n\r\t", szString[nStrPos]) == NULL) {
			// If not, shift characters down in memory
			szNoSpaces[nNoSpacePos] = szString[nStrPos];
			// Move the copy-to position onwards if we write a character
			nNoSpacePos++;
		}
	}
	// Ensure we terminate the string
	szNoSpaces[nNoSpacePos] = '\0';

	// Now turn it in to an operation recursively
	psOperation = RecurseToOperation (szNoSpaces, nNoSpacePos);

	// Free up our copy of the string with no spaces
	PropFree (szNoSpaces);

	return psOperation;
}

/**
 * Check a string fragment to see if it's a binary operator
 * Internal method.
 *
 * @param szString the string to check.
 * @param nStrLen the length of the string.
 * @return TRUE if the operator and string match, FALSE otherwise.
 *
 */
inline bool StringCheckBinary (char const * szString, int const nStrLen, char const * szOperator) {
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
 * Recursively turn a string into a formula
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
Operation * RecurseToOperation (char const * szString, int nStrLen) {
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
				boMatch = StringCheckBinary (szString + nPos, nStrLen - nPos, aszOpBinary[eBinary]);
			}
		}
		// Move on to check the next operation
		eBinary = (OPBINARY)((int)eBinary + 1);
	}

	if (boMatch) {
		// Split into two pieces and recurse
		eBinary = (OPBINARY)((int)eBinary - 1);
		nRightStart = nPos + (int)strlen(aszOpBinary[eBinary]) - 1;

		psReturnOp = CreateBinary (eBinary, RecurseToOperation (szString, nPos - 1), RecurseToOperation (szString + nRightStart, nStrLen - nRightStart));
	}
	else {
		// Check if it's a unary operation
		boMatch = FALSE;
		eUnary = (OPUNARY)((int)OPUNARY_INVALID + 1);
		while ((!boMatch) && (eUnary < OPUNARY_NUM)) {
			if (nStrLen > (int)strlen (aszOpUnary[eUnary])) {
				// String compare with the possible unary operations
				if (strncmp (aszOpUnary[eUnary], szString, strlen (aszOpUnary[eUnary])) == 0) {
					boMatch = TRUE;
				}
			}

			// Move on to check the next operation
			eUnary = (OPUNARY)((int)eUnary + 1);
		}

		if (boMatch) {
			// Recurse on whatever is left
			eUnary = (OPUNARY)((int)eUnary - 1);
			nRightStart = (int)strlen(aszOpUnary[eUnary]);
			psReturnOp = CreateUnary (eUnary, RecurseToOperation (szString + nRightStart, nStrLen - nRightStart));
		}
		else {
			boMatch = TryStringToTruth (szString, nStrLen, &boTruth);
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
 * Try to convert a string to a double.
 * Internal method.
 *
 * @param szString the string to convert (may not be zero terminated).
 * @param nStrLen the length of the string.
 * @param pfDeciimal return value of the decimal value if it could be converted (unchanged o/w).
 * @return TRUE if the conversion was successful, FALSE o/w.
 *
 */
bool TryStringToDouble (char const * const szString, int const nStrLen, double * pfDecimal) {
	int nScanned = 0;
	char * szCopied;

	// Allocate some memory to make a zero-terminated copy of the string
	szCopied = (char *)PropMalloc (nStrLen + 1);
	if (szCopied) {
		// Copy the string
		strncpy (szCopied, szString, nStrLen);
		// Zero terminate it
		szCopied[nStrLen] = '\0';

		// Let sscanf do the hard work of conversion
		nScanned = sscanf (szCopied, "%lf", pfDecimal);

		// Free up the copied version
		// 'cos we don't need it anymore
		PropFree (szCopied);
	}

	return (nScanned == 1);
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
bool TryStringToTruth (char const * const szString, int const nStrLen, bool * pboTruth) {
	bool boResult = FALSE;

	if (strcmp ("TRUE", szString) == 0) {
		if (pboTruth) {
			*pboTruth = TRUE;
		}
		boResult = TRUE;
	}
	else {
		if (strcmp ("FALSE", szString) == 0) {
			if (pboTruth) {
				*pboTruth = FALSE;
			}
			boResult = TRUE;
		}
	}

	return boResult;
}

/**
 * Try to convert a string into a user unary.
 * Internal method.
 *
 * @param szString the string to convert (may not be zero terminated).
 * @param nStrLen the length of the string.
 * @param pnNameEnd return the character index of the last character of the
 *        unary name. Unchanged if NULL on entry or conversion fails.
 * @return TRUE if the conversion was successful, FALSE o/w.
 *
 */
bool TryUndefinedUnary (char const * szString, int nStrLen, int * pnNameEnd) {
	bool boMatch;
	int nPos;

	// Let's assume this isn't an undefined unary
	boMatch = FALSE;

	// Read until we reach a non variable-name character
	nPos = 0;
	while ((nPos < nStrLen) && (strchr(VARIABLE_CHARS, szString[nPos]) != NULL)) {
		nPos++;
	}
	// Check what character we've reached
	if (szString[nPos] == '(') {
		// This might just work
		boMatch = CheckBracketsMatch (szString + nPos, nStrLen - nPos);
	}

	if ((pnNameEnd) && (boMatch)) {
		*pnNameEnd = nPos;
	}

	return boMatch;
}

/**
 * Test whether the brackets in the given string expression match up.
 * Internal method.
 *
 * @param szString the string to check (may not be zero terminated).
 * @param nStrLen the length of the string.
 * @return TRUE if the brackets match, FALSE o/w.
 *
 */
bool CheckBracketsMatch (char const * szString, int nStrLen) {
	bool boMatch;
	int nBrackets;
	int nPos;

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

	return boMatch;
}
