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

#define VARSTACK_CHUNK	(16)

//////////////////////////////////////////////////////////////////
// Structures

struct _VarStack {
	char ** aszVars;
	int nCount;
	int nAllocated;
};

//////////////////////////////////////////////////////////////////
// Global variables

//////////////////////////////////////////////////////////////////
// Function prototypes

int VarStackMatchUnboundRecursive (VarStack const * psBoundVars, VarStack * psVarStack, Operation * psOp);

VarStack * CreateVarStack () {
	VarStack * psVarStack;

	psVarStack = PropCalloc(1, sizeof(VarStack));

	return psVarStack;
}

VarStack * FreeVarStack (VarStack * psVarStack) {
	int nPos;

	if (psVarStack) {
		if (psVarStack->aszVars) {
			for (nPos = 0; nPos < psVarStack->nCount; ++nPos) {
				if (psVarStack->aszVars[nPos] != NULL) {
					PropFree((void *)psVarStack->aszVars[nPos]);
					psVarStack->aszVars[nPos] = NULL;
				}
			}
			PropFree ((void *)psVarStack->aszVars);
			psVarStack->nCount = 0;
			psVarStack->nAllocated = 0;
		}
		PropFree((void *)psVarStack);
	}

	return NULL;
}

void VarStackPush(VarStack * psVarStack, char const* szVar) {
	int nSize;
	int nPos;
	int nLength;

	if (psVarStack) {
		psVarStack->nCount += 1;
		nSize = ((psVarStack->nCount / VARSTACK_CHUNK) + 1) * VARSTACK_CHUNK;
		if (psVarStack->nCount > psVarStack->nAllocated) {
			psVarStack->aszVars = (char **)PropRealloc (psVarStack->aszVars, nSize * VARSTACK_CHUNK * sizeof (char *));
			psVarStack->nAllocated = nSize;
		}
		nLength = strlen(szVar);
		psVarStack->aszVars[(psVarStack->nCount - 1)] = (char *)PropMalloc((nLength + 0) * sizeof (char *));
		strncpy(psVarStack->aszVars[(psVarStack->nCount - 1)], szVar, nLength);
		psVarStack->aszVars[(psVarStack->nCount - 1)][nLength] = 0;
	}
}

char * VarStackPop(VarStack * psVarStack) {
	int nSize;
	int nPos;
	int nRemoved;
	char * szVar;

	szVar = NULL;
	if (psVarStack) {
		if (psVarStack->nCount > 0) {
			psVarStack->nCount -= 1;
			szVar = psVarStack->aszVars[psVarStack->nCount];
			psVarStack->aszVars[psVarStack->nCount] = NULL;
			nSize = ((psVarStack->nCount / VARSTACK_CHUNK) + 1) * VARSTACK_CHUNK;
			if (nSize != psVarStack->nAllocated) {
				psVarStack->aszVars = (char **)PropRealloc (psVarStack->aszVars, nSize * VARSTACK_CHUNK * sizeof (char *));
				psVarStack->nAllocated = nSize;
			}
		}
	}
	return szVar;
}

void VarStackDrop(VarStack * psVarStack) {
	char * szVar;

	szVar = VarStackPop(psVarStack);
	PropFree(szVar);
}

int VarStackCount(VarStack * psVarStack) {
	return psVarStack->nCount;
}

bool VarStackMatchUnbound (VarStack const * psBoundVars, Operation * psOp) {
	int nCount ;
	VarStack * psVarStack;

	psVarStack = CreateVarStack();
	nCount = VarStackMatchUnboundRecursive (psBoundVars, psVarStack, psOp);
	psVarStack = FreeVarStack(psVarStack);

	return nCount == 0;
}

int VarStackMatchUnboundRecursive (VarStack const * psBoundVars, VarStack * psVarStack, Operation * psOp) {
	int nCount = 0;
	int nPos;

	switch (psOp->eOpType) {
		case OPTYPE_TRUTHVALUE:
			// Intentional fallthrough
		case OPTYPE_VARIABLE:
			// Intentional fallthrough
		default: {
			// Do nothing;
		}
		break;
		case OPTYPE_UNARY: {
			nCount += VarStackMatchUnboundRecursive(psBoundVars, psVarStack, psOp->Vars.psUnary->psVar1);
		}
		break;
		case OPTYPE_BINARY: {
			nCount += VarStackMatchUnboundRecursive(psBoundVars, psVarStack, psOp->Vars.psBinary->psVar1);
			nCount += VarStackMatchUnboundRecursive(psBoundVars, psVarStack, psOp->Vars.psBinary->psVar2);
		}
		break;
		case OPTYPE_QUANTIFIER: {
			VarStackPush (psVarStack, psOp->Vars.psQuantifier->szVar);
			nCount += VarStackMatchUnboundRecursive (psBoundVars, psVarStack, psOp->Vars.psQuantifier->psVar1);
			VarStackDrop (psVarStack);
		}
		break;
		case OPTYPE_RELATION: {
			for (nPos = 0; nPos < psOp->Vars.psRelation->nArity; ++nPos) {
				if (!VarStackContains (psVarStack, psOp->Vars.psRelation->aszVar[nPos]) && VarStackContains (psBoundVars, psOp->Vars.psRelation->aszVar[nPos])) {
					nCount += 1;
				}
			}
		}
		break;
	}

	return nCount;
}

bool VarStackContains (VarStack const * psVarStack, char const * szVar) {
	int nPos;
	bool boFound;

	boFound = FALSE;
	for (nPos = 0; (!boFound) && (nPos < psVarStack->nCount); ++nPos) {
		if (strcmp(szVar, psVarStack->aszVars[nPos]) == 0) {
			boFound = TRUE;
		}
	}

	return boFound;
}

int VarStackFind (VarStack const * psVarStack, char const * szVar) {
	int nPos;
	int nFound;

	nFound = -1;
	for (nPos = (psVarStack->nCount - 1); (nFound < 0) && (nPos >= 0); --nPos) {
		if (strcmp(szVar, psVarStack->aszVars[nPos]) == 0) {
			nFound = nPos;
		}
	}

	return nFound;
}

char const * VarStackGet(VarStack const * const psVarStack, int nPos) {
	char const * szVar = NULL;

	if (psVarStack && (nPos >= 0) && (nPos < psVarStack->nCount)) {
		szVar = psVarStack->aszVars[nPos];
	}

	return szVar;
}

