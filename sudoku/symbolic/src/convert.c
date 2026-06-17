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

//////////////////////////////////////////////////////////////////
// Structures

//////////////////////////////////////////////////////////////////
// Global variables

// Textual equivalents of the unary operations
static char const aszOpUnary[OPUNARY_NUM][7] = {
	"!",
};

// Textual equivalents of the binary operations
static char const aszOpBinary[OPBINARY_NUM][6] = {
	"^",
	"v",
	"->",
	"xor",
};

// Textual equivalents of the quantifiers
static char const aszQuantifier[QUANTIFIER_NUM][7] = {
	"forall",
	"exists",
};

//////////////////////////////////////////////////////////////////
// Function prototypes

Operation * RecurseToOperation (char const * szString, int nStrLen);
bool StringCheckBinary (char const * szString, int const nStrLen, char const * szOperator);
bool StringCheckQuantifier (char const * szString, int const nStrLen, char const * szQuantifier);
bool TryStringToDouble (char const * const szString, int const nStrLen, double * pfDecimal);
bool TryStringToTruth (char const * const szString, int const nStrLen, bool * pboTruth);
bool TryUndefinedUnary (char const * szString, int nStrLen, int * pnNameEnd);
bool CheckBracketsMatch (char const * szString, int nStrLen);
void StripSurroundingWhitespace(char * szString, int * pnStrLen);
char * DuplicateWithoutWhitespace(char const * szString, int nStrLen, int *pnOutLen);
char const * StringGetBounds(char const * szString, int * pnStart, int * pnStrLen);

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
char * OperationToString (Operation const * psOp, char * szString, int nStrLen) {
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
char * RecurseToString (Operation const * psOp, int nStrLen) {
	char * szReturn;
	char * szVar1;
	char * szVar2;
	char * szVar3;

	// Allocate memory for the result, initialised to zeros
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
				// First convert the result the unary operation is applied to
				szVar1 = RecurseToString (psOp->Vars.psUnary->psVar1, nStrLen);
				// We use a couple of arrays representing the operations
				switch (psOp->Vars.psUnary->eOpType) {
					case OPUNARY_NOT:
						// These operations are of the form "*a" (operation * applied to a)
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
						// Of the form "(a * b)" where * is the operation
						snprintf (szReturn, nStrLen, "(%s %s %s)", szVar1, aszOpBinary[psOp->Vars.psBinary->eOpType], szVar2);
						szReturn[nStrLen - 1] = 0;
						break;
					default:
						// These operations are of the form "*a" (operation * applied to a)
						printf("Invalid binary operator\n");
						break;
				}
				// Free up the allocated strings since we've already copied them
				PropFree (szVar1);
				PropFree (szVar2);
				break;
			case OPTYPE_QUANTIFIER:
				// Quantifiers must be handled recursively
				// First convert the result the quantifier is applied to
				szVar1 = RecurseToString (psOp->Vars.psQuantifier->psVar1, nStrLen);
				// We use a couple of arrays representing the operations
				switch (psOp->Vars.psQuantifier->eQuType) {
					case QUANTIFIER_UNIVERSAL:
					case QUANTIFIER_EXISTENTIAL:
						// These operations are of the form "forall x a" (variable x, operation a)
						snprintf (szReturn, nStrLen, "%s %s %s", aszQuantifier[psOp->Vars.psQuantifier->eQuType], psOp->Vars.psQuantifier->szVar, szVar1);
						szReturn[nStrLen - 1] = 0;
						break;
					default:
						// Whoa there, we don't know how to handle that
						printf("Invalid quantifier\n");
						break;
				}
				// Free up the allocated string since we've already a copy
				PropFree (szVar1);
				break;
			case OPTYPE_RELATION:
				// Relations don't have a recursive element
				szReturn = RelationToString (psOp, szReturn, nStrLen);
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
int OperationToStringLength (Operation const * psOp) {
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
int RecurseToStringLength (Operation const * psOp) {
	int nReturn;
	int nVar1;
	int nVar2;
	int nVar3;

	// The string will have zero length unless we determine otherwise
	nReturn = 0;

	// Check the operations recursively
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
			case OPTYPE_QUANTIFIER:
				// The length of the quantifier, variable and operation combined
				// Calculate the lengths of the variable and operation first
				nVar1 = RecurseToStringLength (psOp->Vars.psQuantifier->psVar1);
				switch (psOp->Vars.psQuantifier->eQuType) {
					case QUANTIFIER_UNIVERSAL:
					case QUANTIFIER_EXISTENTIAL:
						// These operations are of the form "forall x a" (variable x, operation a)
						// nReturn = snprintf (szReturn, nStrLen, "%s %s %s", aszQuantifier[psOp->Vars.psQuantifier->eQuType], psOp->Vars.psQuantifier->szVar, szVar1);
						nReturn = strlen (aszQuantifier[psOp->Vars.psQuantifier->eQuType]) + strlen (psOp->Vars.psQuantifier->szVar) + nVar1 + 2;
						break;
					default:
						// Whoa there, we don't know how to handle that
						printf("Invalid quantifier\n");
						break;
				}
				break;
			case OPTYPE_RELATION:
				// Relations don't have a recursive element
				nReturn = RelationToStringLength (psOp);
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
	bool boCompacting;

	// Establish the length of the string
	nStrLen = (int)strlen (szString);

	// Combine consecutive whitespace into a single space
	szNoSpaces = (char *)PropMalloc (nStrLen + 1);
	nNoSpacePos = 0;
	boCompacting = TRUE;
	for (nStrPos = 0; nStrPos < nStrLen; nStrPos++) {
		// Check whether this is a character to skip
		if (strchr (WHITESPACE_CHARS, szString[nStrPos]) == NULL) {
			// If not, shift characters down in memory
			szNoSpaces[nNoSpacePos] = szString[nStrPos];
			// Move the copy-to position onwards if we write a character
			nNoSpacePos++;
			if (strchr ("()", szString[nStrPos]) == NULL) {
				boCompacting = FALSE;
			}
			else {
				boCompacting = TRUE;
			}
		}
		else {
			if (!boCompacting) {
				// This is the first in a potential sequence of whitespace
				szNoSpaces[nNoSpacePos] = ' ';
				// Move the copy-to position onwards if we write a character
				nNoSpacePos++;
				boCompacting = TRUE;
			}
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
 * @param szOperator the operator to check for
 * @return TRUE if the operator and string match, FALSE otherwise.
 *
 */
bool StringCheckBinary (char const * szString, int const nStrLen, char const * szOperator) {
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
 * Check a string fragment to see if it's a quantifier operator
 * Internal method.
 *
 * @param szString the string to check.
 * @param nStrLen the length of the string.
 * @param szQuantifier the quantifier to check for
 * @return TRUE if the operator and string match, FALSE otherwise.
 *
 */
bool StringCheckQuantifier (char const * szString, int const nStrLen, char const * szQuantifier) {
	bool boMatch = FALSE;
	int nQuantifierLen;

	// Establish the length of the operator
	nQuantifierLen = (int)strlen (szQuantifier);

	// Compare the two as a single string match
	if ((nStrLen >= nQuantifierLen) && (strncmp (szString, szQuantifier, nQuantifierLen) == 0)) {
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
	QUANTIFIER eQuantifier = QUANTIFIER_INVALID;
	Operation * psReturnOp = NULL;
	int nRightStart;
	double fDecimal;
	bool boScanned;
	char * szVariable = NULL;
	int nNameEnd;
	bool boTruth;
	int nLength;
	int nArity;

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
	// after finding the highest precedent operation, rather than the other
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
		while ((nRightStart < nStrLen) && (strchr (WHITESPACE_CHARS, szString[nRightStart]) != NULL)) {
			nRightStart += 1;
		}
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
			// Check if it's a quantifier
			eQuantifier = (QUANTIFIER)((int)QUANTIFIER_INVALID + 1);
			while ((!boMatch) && (eQuantifier < QUANTIFIER_NUM)) {
				nLength = (int)strlen (aszQuantifier[eQuantifier]);
				if (nStrLen > nLength) {
					// String compare with the possible quantifiers
					if (strncmp (aszQuantifier[eQuantifier], szString, nLength) == 0) {
						boMatch = TRUE;
					}
				}

				// Move on to check the next operation
				eQuantifier = (QUANTIFIER)((int)eQuantifier + 1);
			}

			if (boMatch) {
				// Extract the variable to quantify over
				nPos = nLength + 1;
				while ((nPos < nStrLen) && (szString[nPos] != ' ')) {
					nPos += 1;
				}
				szVariable = DuplicateWithoutWhitespace(szString + nLength + 1, nPos - nLength - 1, NULL);

				// Recurse on whatever is left
				eQuantifier = (QUANTIFIER)((int)eQuantifier - 1);
				nRightStart = nPos + 1;
				psReturnOp = CreateQuantifier (eQuantifier, szVariable, RecurseToOperation (szString + nRightStart, nStrLen - nRightStart));
				PropFree (szVariable);
			}
			else {
				szString = StringGetBounds(szString, NULL, &nStrLen);

				boMatch = TryStringToTruth (szString, nStrLen, &boTruth);
				if (boMatch) {
					psReturnOp = CreateTruthValue (boTruth);
				}
				else {
					boMatch = TryRelation (szString, nStrLen, &nArity);
					if (boMatch) {
						psReturnOp = StringToRelation (szString, nStrLen, nArity);
					}
					else {
						// Interpret as a variable, since it's all that's left
						// TODO: Check whether this can really be a valid variable name (e.g. no brackets)
						szVariable = DuplicateWithoutWhitespace(szString, nStrLen, NULL);
						psReturnOp = CreateVariable (szVariable);
						PropFree (szVariable);
						szVariable = NULL;
					}
				}
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
	int nMin;

	nMin = nStrLen < 4 ? nStrLen : 4;
	if ((nStrLen >= 4) && (strncmp ("TRUE", szString, nMin) == 0)) {
		if (pboTruth) {
			*pboTruth = TRUE;
		}
		boResult = TRUE;
	}
	else {
		nMin = nStrLen < 5 ? nStrLen : 5;
		if ((nStrLen >= 5) && (strncmp ("FALSE", szString, nMin) == 0)) {
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

/**
 * Remove leading and trailing whitespace from a string.
 *
 * The change is performed in-place and nStrLen updated to reflect the length
 * of the string after spaces have been stripped.
 *
 * @param szString the string to strip whitespace from.
 * @param nStrLen the length of the string.
 *
 */
void StripSurroundingWhitespace(char * szString, int * pnStrLen) {
	int nPos;
	int nStart;
	int nLength;
	bool boStarted;

	nPos = 0;
	nStart = 0;
	nLength = 0;
	boStarted = FALSE;
	while (nPos < *pnStrLen) {
		if (!boStarted) {
			if (strchr (WHITESPACE_CHARS, szString[nPos]) == NULL) {
				boStarted = TRUE;
				nStart = nPos;
			}
		}
		if (boStarted) {
			if (nStart > 0) {
				szString[nPos - nStart] = szString[nPos];
			}

			if (strchr (WHITESPACE_CHARS, szString[nPos]) == NULL) {
				nLength = nPos - nStart + 1;
			}
		}
		nPos += 1;
	}
	if (nLength < *pnStrLen) {
		szString[nLength] = 0;
	}

	*pnStrLen = nLength;
}

/**
 * Duplicate without leading and trailing whitespace from a string.
 *
 * The string is duplicated during the process. It is the caller's responsibility
 * to free the returned string once it's no longer needed.
 *
 * The length of the copied string is returned in the memory pointed to be pnOutLen.
 * If pnOutLen is NULL then no length is returned.
 *
 * @param szString the string to strip whitespace from.
 * @param nStrLen the input length of the string.
 * @param pnOutLen a pointer to where to store the output lenght, or NULL otherwise.
 * @return the duplicated string with leading and trailing whitespace removed
 *
 */
char * DuplicateWithoutWhitespace(char const * szString, int nStrLen, int *pnOutLen) {
	int nPos;
	int nStart;
	int nLength;
	bool boStarted;
	char * szCopy;

	nPos = 0;
	nStart = 0;
	nLength = 0;
	boStarted = FALSE;
	while (nPos < nStrLen) {
		if (!boStarted) {
			if (strchr (WHITESPACE_CHARS, szString[nPos]) == NULL) {
				boStarted = TRUE;
				nStart = nPos;
			}
		}
		if (boStarted) {
			if (strchr (WHITESPACE_CHARS, szString[nPos]) == NULL) {
				nLength = nPos - nStart + 1;
			}
		}
		nPos += 1;
	}

	szCopy = (char *)PropMalloc(nLength + 1);

	if (nLength > 0) {
		strncpy(szCopy, szString + nStart, nLength);
	}
	szCopy[nLength] = 0;

	if (pnOutLen) {
		*pnOutLen = nLength;
	}

	return szCopy;
}

/**
 * Remove leading and trailing whitespace from a string.
 *
 * The locations of the start and end of the string without whitespace
 * are returned.
 *
 * The string itself is unchanged.
 *
 * @param szString the string to strip whitespace from.
 * @param pnStart returns the start index if not NULL 
 * @param pnStrLen the length of the string, also returned if not NULL.
 * @return the start position of the non-whitespace string in memory.
 *
 */
char const * StringGetBounds(char const * szString, int * pnStart, int * pnStrLen) {
	int nLength;
	int nPos;
	int nStart;

	if (pnStrLen) {
		nLength = *pnStrLen;
	}
	else {
		nLength = strlen(szString);
	}

	if (nLength == 0) {
		// No characters
		nStart = 0;
		nPos = 0;
	}
	else {
		// Find the start
		nPos = 0;
		while ((nPos < nLength) && (strchr (WHITESPACE_CHARS, szString[nPos]) != NULL)) {
			nPos += 1;
		}

		if (nPos == nLength) {
			// All whitespace
			nStart = 0;
			nPos = 0;
		}
		else {
			nStart = nPos;
			nPos = nLength;
			while ((nPos > nStart) && (strchr (WHITESPACE_CHARS, szString[nPos - 1]) != NULL)) {
				nPos -= 1;
			}
		}
	}

	if (pnStart) {
		*pnStart = nStart;
	}

	if (pnStrLen) {
		*pnStrLen = nPos - nStart;
	}

	return szString + nStart;
}


