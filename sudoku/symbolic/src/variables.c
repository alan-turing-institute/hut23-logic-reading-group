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
 * Provides an interface for defining variables. Variables may
 * be given values or left as true variables (without defined
 * values).
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

// Structure for head of the linked list
typedef struct _VariableHead {
	int nNumber;

	Variable * psVarFirst;
	Variable * psVarLast;
} VariableHead;

// Linked list of variables
struct _Variable {
	char * szVar;
	bool boValid;
	int nReferences;
	bool boValue;

	VariableHead * psVarHead;
	Variable * psVarNext;
	Variable * psVarPrev;
};

//////////////////////////////////////////////////////////////////
// Global variables

//////////////////////////////////////////////////////////////////
// Function prototypes

Variable * AddNewVariable (Variable * psVariables, char const * const szVar);

//////////////////////////////////////////////////////////////////
// Main application

/**
 * For reference counting.
 *
 */
void IncrementVarRef (Variable * psVar) {
	psVar->nReferences++;
}

/**
 * For reference counting.
 *
 */
void DecrementVarRef (Variable * psVar) {
	psVar->nReferences--;
}

/**
 * Create a new tracked variable.
 *
 * @param psOp the operation the variable relates to.
 * @param psVariables the linked list of variables for this operation.
 * @return the new head of the linked list.
 *
 */
Variable * CreateVariableValue (Operation * psOp, Variable * psVariables) {
	Variable * psVariable = NULL;

	if (psOp->eOpType == OPTYPE_VARIABLE) {
		if (psOp->Vars.psVar->psValue == NULL) {
			psVariable = AddNewVariable (psVariables, psOp->Vars.psVar->szVar);
			psOp->Vars.psVar->psValue = psVariable;
			IncrementVarRef (psVariable);
		}
		psVariable = psOp->Vars.psVar->psValue;
	}

	return psVariable;
}

/**
 * Pull out the variables from an operation and keep track of them.
 *
 * @param psOp the operation to check for variables.
 * @param psVariables the linked list of vvariables for this operation.
 * @return the new head of the linked list.
 *
 */
Variable * CreateVariables (Operation * psOp, Variable * psVariables) {
	if (psOp) {
		switch (psOp->eOpType) {
			case OPTYPE_TRUTHVALUE:
				// Nothing else to do - backtrack
				break;
			case OPTYPE_VARIABLE:
				if (psOp->Vars.psVar->psValue == NULL) {
					// Create a variable structure for this variable operation
					psVariables = AddNewVariable (psVariables, psOp->Vars.psVar->szVar);
					// Decrement the operations's variable reference if there is one,
					if (psOp->Vars.psVar->psValue) {
						DecrementVarRef (psOp->Vars.psVar->psValue);
					}
					psOp->Vars.psVar->psValue = psVariables;
					IncrementVarRef (psVariables);
				}
				break;
			case OPTYPE_UNARY:
				// Check any operations further down the tree
				if (psOp->Vars.psUnary) {
					psVariables = CreateVariables (psOp->Vars.psUnary->psVar1, psVariables);
				}
				// Then backtrack
				break;
			case OPTYPE_BINARY:
				// Check any operations further down the tree
				if (psOp->Vars.psBinary) {
					psVariables = CreateVariables (psOp->Vars.psBinary->psVar1, psVariables);
					psVariables = CreateVariables (psOp->Vars.psBinary->psVar2, psVariables);
				}
				// Then backtrack
				break;
			default:
				printf("Invalid operation type\n");
				break;
		}
	}

	return psVariables;
}

/**
 * Set the value of a variable.
 *
 * @param psVar the variable to assign a value to.
 * @param fValue the value to assign it.
 *
 */
void SetVariable (Variable * psVar, bool boValue) {
	psVar->boValue = boValue;
	psVar->boValid = TRUE;
}

/**
 * Get the valye oof a variable.
 *
 * @param psVar the variable to check.
 * @return the value assigned to the variable, or NAN if it's unset.
 *
 */
bool GetVariable (Variable * psVar) {
	bool boValue = FALSE;

	if (psVar && psVar->boValid)
	{
		boValue = psVar->boValue;
	}

	return boValue;
}

/**
 * Get the valye oof a variable.
 *
 * @param psVar the variable to check.
 * @return the value assigned to the variable, or NAN if it's unset.
 *
 */
bool GetVariableValid (Variable * psVar) {
	bool boValid = FALSE;

	if (psVar)
	{
		boValid = psVar->boValid;
	}

	return boValid;
}

/**
 * Unset the value of a variable
 *
 * @param psVar the variable to clear.
 *
 */
void UnsetVariable (Variable * psVar) {
	psVar->boValid = FALSE;
}

/**
 * Free up any varialbles that are no longer used in an operation.
 *
 * @param psVariables the linked list of variables to check.
 *
 */
Variable * FreeVariables (Variable * psVariables) {
	VariableHead * psVarHead = NULL;
	Variable * psVariable = NULL;
	Variable * psVarPrev = NULL;

	if (psVariables) {
		psVarHead = psVariables->psVarHead;
		psVariable = psVarHead->psVarLast;

		while (psVariable) {
			psVarPrev = psVariable->psVarPrev;
			if (psVariable->nReferences <= 0) {
				// Unlink the variable
				if (psVariable->psVarPrev) {
					psVariable->psVarPrev->psVarNext = psVariable->psVarNext;
				}
				if (psVariable->psVarNext) {
					psVariable->psVarNext->psVarPrev = psVariable->psVarPrev;
				}
				if (psVarHead->psVarFirst == psVariable) {
					psVarHead->psVarFirst = psVariable->psVarNext;
				}
				if (psVarHead->psVarLast == psVariable) {
					psVarHead->psVarLast = psVariable->psVarPrev;
				}
				// Delete and free up the variable
				PropFree ((void *)psVariable->szVar);
				PropFree ((void *)psVariable);
				psVarHead->nNumber--;
			}
			psVariable = psVarPrev;
		}

		psVariable = psVarHead->psVarFirst;

		if (psVarHead->nNumber <= 0) {
			// Free up the list header too
			PropFree ((void *)psVarHead);
		}
	}

	return psVariable;
}

/**
 * Find a variable with a given name.
 *
 * @param psVariables the linked list of variables to check.
 * @param szVar the name of the variable to find.
 * @return the structure for the variable if found, or NULL otherwise.
 *
 */
Variable * FindVariable (Variable * psVariables, char const * const szVar) {
	bool boFound = FALSE;
	Variable * psVariable = NULL;

	if (psVariables) {
		psVariable = psVariables->psVarHead->psVarFirst;
	}

	while ((!boFound) && psVariable) {
		if (strcmp (psVariable->szVar, szVar) == 0) {
			boFound = TRUE;
		}
		else {
			psVariable = psVariable->psVarNext;
		}
	}

	return psVariable;
}

/**
 * Add a new nambed variable to the list.
 *
 * @param psVariables the linked list of variables to check.
 * @param szVar the name of the variable to add.
 * @return the structure for the variable created.
 *
 */
Variable * AddNewVariable (Variable * psVariables, char const * const szVar) {
	Variable * psNewVariable = NULL;
	VariableHead * psVarHead = NULL;
	int nVarLen;

	// Check if the variable already exists
	psNewVariable = FindVariable (psVariables, szVar);

	if (psNewVariable == NULL) {
		psNewVariable = (Variable *)PropMalloc (sizeof (Variable));

		// Store the variable name
		nVarLen = (int)strlen (szVar);
		psNewVariable->szVar = (char *)PropMalloc (nVarLen + 1);
		strncpy (psNewVariable->szVar, szVar, nVarLen);
		psNewVariable->szVar[nVarLen] = 0;

		// Initialise value and counts
		psNewVariable->boValue = FALSE;
		psNewVariable->nReferences = 0;
		psNewVariable->boValid = FALSE;

		// Link into the linked list
		if (psVariables) {
			psVarHead = psVariables->psVarHead;

			psNewVariable->psVarHead = psVarHead;

			psNewVariable->psVarNext = NULL;
			psNewVariable->psVarPrev = psVarHead->psVarLast;

			psVarHead->psVarLast->psVarNext = psNewVariable;
			psVarHead->psVarLast = psNewVariable;
			psVarHead->nNumber++;
		}
		else {
			psVarHead = (VariableHead *)PropMalloc (sizeof (VariableHead));
			psVarHead->nNumber = 1;
			psVarHead->psVarFirst = psNewVariable;
			psVarHead->psVarLast = psNewVariable;

			psNewVariable->psVarHead = psVarHead;
			psNewVariable->psVarNext = NULL;
			psNewVariable->psVarPrev = NULL;
		}
	}

	return psNewVariable;
}

/**
 * Return the number of variables in the linked list.
 *
 * @param psVariables the linked list of variables to check.
 * @return the number of variables in the list.
 *
 */
int VariableCount (Variable * psVariables) {
	int nCount = 0;

	if (psVariables) {
		nCount = psVariables->psVarHead->nNumber;
	}

	return nCount;
}

/**
 * Return the first variable in the linked list.
 *
 * @param psVariables the linked list of variables to check.
 * @return the first variable in the list.
 *
 */
Variable * VariableFirst (Variable * psVariables) {
	Variable * psFind = NULL;
	if (psVariables) {
		psFind = psVariables->psVarHead->psVarFirst;
	}
	
	return psFind;
}

/**
 * Return the last variable in the linked list.
 *
 * @param psVariables the linked list of variables to check.
 * @return the last variable in the list.
 *
 */
Variable * VariableLast (Variable * psVariables) {
	Variable * psFind = NULL;
	if (psVariables) {
		psFind = psVariables->psVarHead->psVarLast;
	}
	
	return psFind;
}

/**
 * Return the next variable in the linked list.
 *
 * @param psVariables the variable to inspect.
 * @return the next variable in the list.
 *
 */
Variable * VariableNext (Variable * psVariables) {
	Variable * psFind = NULL;
	if (psVariables) {
		psFind = psVariables->psVarNext;
	}
	
	return psFind;
}

/**
 * Return the previous variable in the linked list.
 *
 * @param psVariables the variable to inspect.
 * @return the previous variable in the list.
 *
 */
Variable * VariablePrev (Variable * psVariables) {
	Variable * psFind = NULL;
	if (psVariables) {
		psFind = psVariables->psVarPrev;
	}
	
	return psFind;
}

/**
 * Return the variable name.
 *
 * @param psVariable the variable to inspect.
 * @return the name of the variable.
 *
 */
char const * VariableName (Variable const * const psVariable) {
	char const * szName = NULL;
	
	if (psVariable) {
		szName = psVariable->szVar;
	}
	
	return szName;
}

