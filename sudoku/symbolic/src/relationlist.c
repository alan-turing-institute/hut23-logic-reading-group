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

#define RELATIONLIST_CHUNK	(16)

//////////////////////////////////////////////////////////////////
// Structures

struct _RelationList {
	Operation ** apsOp;
	int nCount;
	int nAllocated;
};

//////////////////////////////////////////////////////////////////
// Global variables

//////////////////////////////////////////////////////////////////
// Function prototypes

void RelationListExtractRecursive(RelationList * psRelationList, Operation * psOp);

RelationList * CreateRelationList () {
	RelationList * psRelationList;

	psRelationList = PropCalloc(1, sizeof(RelationList));

	return psRelationList;
}

RelationList * FreeRelationList (RelationList * psRelationList) {
	int nPos;

	if (psRelationList) {
		if (psRelationList->apsOp) {
			for (nPos = 0; nPos < psRelationList->nCount; ++nPos) {
				FreeRecursive ((void *)psRelationList->apsOp[nPos]);
			}
			PropFree ((void *)psRelationList->apsOp);
			psRelationList->nCount = 0;
			psRelationList->nAllocated = 0;
		}
		PropFree((void *)psRelationList);
	}

	return NULL;
}

void RelationListAdd(RelationList * psRelationList, Operation const * psRelation) {
	int nSize;
	int nPos;
	bool boExists;

	if (psRelationList) {
		boExists = FALSE;
		for (nPos = 0; (nPos < psRelationList->nCount) && (!boExists); ++nPos) {
			if (RelationComparePattern (psRelation, psRelationList->apsOp[nPos])) {
				boExists = TRUE;
			}
		}

		if (!boExists) {
			psRelationList->nCount += 1;
			nSize = ((psRelationList->nCount / RELATIONLIST_CHUNK) + 1) * RELATIONLIST_CHUNK;
			if (psRelationList->nCount > psRelationList->nAllocated) {
				psRelationList->apsOp = (Operation **)PropRealloc (psRelationList->apsOp, nSize * RELATIONLIST_CHUNK * sizeof (Operation *));
				psRelationList->nAllocated = nSize;
			}
			psRelationList->apsOp[(psRelationList->nCount - 1)] = CopyRecursive(psRelation);
		}
	}
}

void RelationListRemove(RelationList * psRelationList, Operation const * psOp) {
	int nSize;
	int nPos;
	int nRemoved;

	if (psRelationList) {
		nRemoved = 0;
		for (nPos = 0; nPos < psRelationList->nCount; ++nPos) {
			if (RelationComparePattern (psOp, psRelationList->apsOp[nPos])) {
				FreeRecursive (psRelationList->apsOp[nPos]);
				psRelationList->apsOp[nPos] = NULL;
				nRemoved += 1;
			}
			else {
				if (nRemoved > 0) {
					psRelationList->apsOp[nPos - nRemoved] = psRelationList->apsOp[nPos];
					psRelationList->apsOp[nPos] = NULL;
				}
			}
		}

		if (nRemoved > 0) {
			psRelationList->nCount -= nRemoved;
			nSize = ((psRelationList->nCount / RELATIONLIST_CHUNK) + 1) * RELATIONLIST_CHUNK;
			if (nSize != psRelationList->nAllocated) {
				psRelationList->apsOp = (Operation **)PropRealloc (psRelationList->apsOp, nSize * RELATIONLIST_CHUNK * sizeof (Operation *));
				psRelationList->nAllocated = nSize;
			}
		}
	}
}

int RelationListCount(RelationList * psRelationList) {
	return psRelationList->nCount;
}

Operation * RelationListGet(RelationList * psRelationList, int nPos) {
	Operation * psOp;

	if ((nPos >= 0) && (nPos < psRelationList->nCount)) {
		psOp = psRelationList->apsOp[nPos];
	}
	else {
		psOp = NULL;
	}
	return psOp;
}

void RelationListExtract(RelationList * psRelationList, Operation * psOp) {
	RelationListExtractRecursive(psRelationList, psOp);
}

void RelationListExtractRecursive(RelationList * psRelationList, Operation * psOp) {
	if (psOp) {
		switch (psOp->eOpType) {
			case OPTYPE_TRUTHVALUE:
				// Nothing else to do - backtrack
				break;
			case OPTYPE_VARIABLE:
				// Nothing else to do - backtrack
				break;
			case OPTYPE_UNARY:
				// Check any operations further down the tree
				if (psOp->Vars.psUnary) {
					RelationListExtractRecursive (psRelationList, psOp->Vars.psUnary->psVar1);
				}
				// Then backtrack
				break;
			case OPTYPE_BINARY:
				// Check any operations further down the tree
				if (psOp->Vars.psBinary) {
					RelationListExtractRecursive (psRelationList, psOp->Vars.psBinary->psVar1);
					RelationListExtractRecursive (psRelationList, psOp->Vars.psBinary->psVar2);
				}
				// Then backtrack
				break;
			case OPTYPE_QUANTIFIER:
				// Check any operations further down the tree
				if (psOp->Vars.psQuantifier) {
					RelationListExtractRecursive (psRelationList, psOp->Vars.psQuantifier->psVar1);
				}
				// Then backtrack
				break;
			case OPTYPE_RELATION:
				// Add the relation name to the list
				RelationListAdd(psRelationList, psOp);
				break;
			default:
				printf("Invalid operation type\n");
				break;
		}
	}
}


