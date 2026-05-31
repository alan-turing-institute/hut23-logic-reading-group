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
	Operation * psOp;
	Operation * psOp2;
	char * szString;
	Operation * psSub;
	Operation * psFind;
	Operation * psFind2;
	double fResult;
	Variable * psVars = NULL;
	Variable * psVar = NULL;
	char *szInput;
	char * szRead;
	size_t nLength;
	size_t nRead;

	// If we don't do this we get unused variable warnings
	argc = argc;
	argv = argv;

	szInput = NULL;
	nRead = getline (&szInput, &nLength, stdin);

	if (nRead != -1) {
		psOp = StringToOperation (szInput);

		nLength = OperationToStringLength (psOp) + 1;
		printf("Write length: %ld\n", nLength);
		szString = PropMalloc(nLength);
		OperationToString (psOp, szString, nLength);
		printf("Result: %s\n", szString);
		PropFree(szString);

		nLength = OperationToStringLengthLatex (psOp) + 1;
		printf("LaTeX length: %ld\n", nLength);
		szString = PropMalloc(nLength);
		OperationToStringLatex (psOp, szString, nLength);
		printf("LaTeX: %s\n", szString);
		PropFree(szString);

		free(szInput);
		szInput = NULL;
	}

	// And relax
	return 0;
}

