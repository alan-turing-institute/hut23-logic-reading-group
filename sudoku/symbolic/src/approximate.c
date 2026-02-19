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
 * The Approximate functions support conversion of a symbolic expression.
 * to a decimal with double precision.
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
#include <math.h>
#include <float.h>

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

/**
 * Recursively approximate an operation if possible.
 *
 * @param psOp the operation to evaluate recursively.
 * @return the approximated value.
 */
double ApproximateOperation (Operation * psOp) {
	double fReturn = NAN;
	double fVar1;
	double fVar2;

	// Check each of the structures
	if (psOp) {
		switch (psOp->eOpType) {
			case OPTYPE_TRUTHVALUE:
				// Truth values (T, F) evaluate to 1 and 0
				fReturn = (double)psOp->Vars.boTruth;
				break;
			case OPTYPE_VARIABLE:
				// Variables evaluate to the value stored in them
				if (psOp->Vars.psVar->psValue) {
					fReturn = GetVariable (psOp->Vars.psVar->psValue);
				}
				break;
			case OPTYPE_UNARY:
				// Unary operations evaluate to the operation applied to their evaluated parameter
				// Evaluate the parameter first
				fVar1 = ApproximateOperation (psOp->Vars.psUnary->psVar1);
				switch (psOp->Vars.psUnary->eOpType) {
					case OPUNARY_NOT:
						// The negation of the parameter
						fReturn = 1.0 - fVar1;
						break;
					default:
						// Do nothing (return NAN)
						break;
				}
				break;
			case OPTYPE_BINARY:
				// Binary operations are applied to the result of approximating their operands (recursive operation)
				// We evaluate the operands first
				fVar1 = ApproximateOperation (psOp->Vars.psBinary->psVar1);
				fVar2 = ApproximateOperation (psOp->Vars.psBinary->psVar2);
				switch (psOp->Vars.psBinary->eOpType) {
					case OPBINARY_LAND:
						// Logical AND
						fReturn = (int)fVar1 && (int)fVar2;
						break;
					case OPBINARY_LOR:
						// Logical OR
						fReturn = (int)fVar1 || (int)fVar2;
						break;
					case OPBINARY_LIMP:
						// Logical implication
						break;
					default:
						break;
				}
				break;
			default:
				// Do nothing (returns NAN)
				break;
		}
	}

	// Return the result
	return fReturn;
}

