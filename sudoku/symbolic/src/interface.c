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

#define  _GNU_SOURCE
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <float.h>
#define _USE_MATH_DEFINES
#include <math.h>

//////////////////////////////////////////////////////////////////
// Defines

// The maximum input size allowed
#define INPUT_SIZE (2048)

//////////////////////////////////////////////////////////////////
// Global variables

//////////////////////////////////////////////////////////////////
// Function prototypes

//////////////////////////////////////////////////////////////////
// Main application

/**
 * Main program.
 * This is for testing purposes and isn't part of the libary.
 *
 */
int main (int argc, char * * argv) {
	Operation * psPattern;
	Operation * psScrutinee;
	char * szString;
	Operation * psSub;
	Operation * psFind;
	Operation * psFind2;
	Operation * psResult;
	double fResult;
	Variable * psVars = NULL;
	Variable * psVar = NULL;
	char *szInput;
	char * szRead;
	size_t nLength;
	size_t nRead;
	Extract * psExtract;
	int nInputs;
	VarStack * psInputs;
	int nPos;
	char const * szVar;
	int nExtracted;
	Operation const * psOp;
	int nArityFrom;
	int nFromPos;
	int nArityTo;
	int nToPos;
	OperationMap * psOperationMap;
	bool boMappable;

	// If we don't do this we get unused variable warnings
	argc = argc;
	argv = argv;

	printf("Pattern? \n");
	szInput = NULL;
	nRead = getline (&szInput, &nLength, stdin);

	if (nRead != -1) {
		psPattern = StringToOperation (szInput);

		nLength = OperationToStringLength (psPattern) + 1;
		szString = PropMalloc(nLength);
		OperationToString (psPattern, szString, nLength);
		printf("Pattern: %s\n", szString);
		PropFree(szString);
		free(szInput);
		szInput = NULL;

		psInputs = CreateVarStack();
		nInputs = OperationInputList (psPattern, psInputs);
		printf("Inputs: %d\n", nInputs);
		for (nPos = 0; nPos < nInputs; ++nPos) {
			szVar = VarStackGet(psInputs, nPos);
			printf("Input %d: %s\n", nPos, szVar);
		}
		psInputs = FreeVarStack(psInputs);


		printf("Scrutinee? \n");
		szInput = NULL;
		nRead = getline (&szInput, &nLength, stdin);

		if (nRead != -1) {
			psScrutinee = StringToOperation (szInput);

			nLength = OperationToStringLength (psScrutinee) + 1;
			szString = PropMalloc(nLength);
			OperationToString (psScrutinee, szString, nLength);
			printf("Scrutinee: %s\n", szString);
			PropFree(szString);
			free(szInput);
			szInput = NULL;

			psInputs = CreateVarStack();
			nInputs = OperationInputList (psPattern, psInputs);
			printf("Inputs: %d\n", nInputs);
			for (nPos = 0; nPos < nInputs; ++nPos) {
				szVar = VarStackGet(psInputs, nPos);
				printf("Input %d: %s\n", nPos, szVar);
			}
			psInputs = FreeVarStack(psInputs);

			psExtract = ExtractPattern (psPattern, psScrutinee);

			if (psExtract) {
				nExtracted = ExtractCount(psExtract);
				printf("Extracted: %d\n", nExtracted);

				for (nPos = 0; nPos < nExtracted; ++nPos) {
					printf("Extraction: %d\n", nPos);

					psOp = ExtractRelation (psExtract, nPos);

					nLength = OperationToStringLength (psOp) + 1;
					szString = PropMalloc(nLength);
					OperationToString (psOp, szString, nLength);
					printf("From: %s\n", szString);
					PropFree(szString);
					free(szInput);

					psOp = ExtractValueFromPos (psExtract, nPos);

					nLength = OperationToStringLength (psOp) + 1;
					szString = PropMalloc(nLength);
					OperationToString (psOp, szString, nLength);
					printf("To: %s\n", szString);
					PropFree(szString);
					free(szInput);

					printf("Variable mappings: \n");
					psOp = ExtractRelation (psExtract, nPos);
					psOperationMap = ExtractOperationMap (psExtract, psOp);

					nArityFrom = psOperationMap->nArityFrom;
					nArityTo = psOperationMap->nArityTo;

					for (nToPos = 0; nToPos < nArityTo; ++nToPos) {
						printf("To pos %d:", nToPos);
						for (nFromPos = 0; nFromPos < nArityFrom; ++nFromPos) {
							boMappable = psOperationMap->aaboVarOrigin[(nToPos * psOperationMap->nArityFrom) + nFromPos];
							if (boMappable) {
								printf(" T");
							}
							else {
								printf(" F");
							}
						}
						if (psOperationMap->aszUnbound[nToPos] != NULL) {
							printf(" (unbound name '%s')\n", psOperationMap->aszUnbound[nToPos]);
						}
						else {
							printf("\n");
						}
					}
				}

				printf("Result pattern? \n");
				szInput = NULL;
				nRead = getline (&szInput, &nLength, stdin);

				if (nRead != -1) {
					psResult = StringToOperation (szInput);

					nLength = OperationToStringLength (psResult) + 1;
					szString = PropMalloc(nLength);
					OperationToString (psResult, szString, nLength);
					printf("Result pattern: %s\n", szString);
					PropFree(szString);
					free(szInput);
					szInput = NULL;

					psResult = ExtractSubstitute (psExtract, psResult);

					nLength = OperationToStringLength (psResult) + 1;
					szString = PropMalloc(nLength);
					OperationToString (psResult, szString, nLength);
					printf("Result: %s\n", szString);
					PropFree(szString);
				}
				else {
					printf("Replacement failed\n");
				}
			}
			else {
				printf("Extraction failed\n");
			}

		}
	}

	// And relax
	return 0;
}

