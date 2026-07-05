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
 */

#if !defined _H_SYMBOLIC_PRIVATE
#define _H_SYMBOLIC_PRIVATE

//////////////////////////////////////////////////////////////////
// Includes

#if HAVE_CONFIG_H
#include <config.h>
#endif

//////////////////////////////////////////////////////////////////
// Defines

#define WORDALIGN (b) (((b) + 3) & ~3)
#define VARIABLE_CHARS "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_"
#define WHITESPACE_CHARS " \n\r\t"

//#define MAXVARSIZE (64)

#if defined _DEBUG
#define DPRINTF printf
#else
#define DPRINTF noprintf
#endif

#if defined _DEBUG
#define REPORT Report
#define REPORTVAR ReportVar
void Report (char * szMessage);
void ReportVar (char * szFormat, int nVariable);
#else
#define REPORT //
#define REPORTVAR //
#endif

#if defined _RISC_OS
#define _snprintf snprintf
#endif

#if defined _MEM_PROFILE
#define PropMalloc PropMemMalloc
#define PropCalloc PropMemCalloc
#define PropRealloc PropMemRealloc
#define PropFree PropMemFree
#else
#define PropMalloc malloc
#define PropCalloc calloc
#define Proprealloc realloc
#define PropFree free
#endif // if defined _MEM_PROFILE

#if defined (WIN32) && !defined (NAN)
static const unsigned long nan[2] = {0xffffffff, 0x7fffffff};
#define NAN (*(const double *) nan)
#define isnan _isnan
#endif

#if defined (WIN32)
#define snprintf _snprintf
#define snscanf _snscanf
#endif

//////////////////////////////////////////////////////////////////
// Structures

// Operation types
typedef enum _OPTYPE {
	OPTYPE_INVALID = -1,

	OPTYPE_TRUTHVALUE,
	OPTYPE_VARIABLE,
	OPTYPE_UNARY,
	OPTYPE_BINARY,
	OPTYPE_QUANTIFIER,
	OPTYPE_RELATION,

	OPTYPE_NUM
} OPTYPE;

// Unary operations
typedef struct _OpUnary {
	OPUNARY eOpType;
	Operation * psVar1;
} OpUnary;

// Binary operations
typedef struct _OpBinary {
	OPBINARY eOpType;
	Operation * psVar1;
	Operation * psVar2;
} OpBinary;

// Variables
typedef struct _OpVariable {
	char * szVar;
	Variable * psValue;
} OpVariable;

// Quantifiers
typedef struct _OpQuantifier {
	QUANTIFIER eQuType;
	char * szVar;
	Operation * psVar1;
} OpQuantifier;

// Relations
typedef struct _OpRelation {
	char * szName;
	size_t nArity;
	char ** aszVar;
} OpRelation;

// General operation structure
struct _Operation {
	OPTYPE eOpType;
	union
	{
		bool boTruth;
		int nInteger;
		OpVariable * psVar;
		OpUnary * psUnary;
		OpBinary * psBinary;
		OpQuantifier * psQuantifier;
		OpRelation * psRelation;
	} Vars;
};

typedef struct _OperationMap {
    Operation * psFrom;
    Operation * psTo;
    // List of "to" variables mapping to potential "from" variables
    bool * aaboVarOrigin;
    int nArityFrom;
    int nArityTo;
    char ** aszUnbound;
} OperationMap;

struct _Extract {
    int nCount;
    OperationMap ** apsOps;
    VarStack * psVarsFrom;
    VarStack * psVarsTo;
};

//////////////////////////////////////////////////////////////////
// Global variables

//////////////////////////////////////////////////////////////////
// Function prototypes

// Reference counting for variables
void DecrementVarRef (Variable * psVar);
void IncrementVarRef (Variable * psVar);

// String conversion
char * RecurseToString (Operation const * psOp, int nStrLen);
int RecurseToStringLength (Operation const * psOp);
char * RecurseToStringC (Operation * psOp, int nStrLen);
int RecurseToStringCLength (Operation * psOp);
char * DuplicateWithoutWhitespace(char const * szString, int nStrLen, int *pnOutLen);

// LaTeX string conversion
char * RecurseToStringLatex (Operation * psOp, int nStrLen);
int RecurseToStringLengthLatex (Operation * psOp);

// Relation operations
Operation * CreateRelationLength (char const * szName, size_t uLength, size_t nArity, char const * const * aszVar, size_t * auVarLen);
Operation * CreateRelationBinaryLength (char const * szName, size_t uLength, char const * szVar1, size_t uVar1Len, char const * szVar2, size_t uVar2Len);
Operation * CopyRelation (Operation const * psOp);
bool RelationCompare (Operation const * psOp1, Operation const * psOp2);
char * RelationToString (Operation const * psOp, char * szString, int nStrLen);
int RelationToStringLength (Operation const * psOp);
char * RelationToStringC (Operation * psOp, char * szString, int nStrLen);
int RelationToStringCLength (Operation * psOp);
void RelationFreeRecursive (Operation * psOp);
int RelationToStringCLength (Operation * psOp);
char * RelationToStringLatex (Operation * psOp, char * szString, int nStrLen);
int RelationToStringLengthLatex (Operation * psOp);
bool TryRelation (char const * szString, int nStrLen, int *pnArity);
Operation * StringToRelation (char const * szString, int nStrLen, int nArity);
bool RelationComparePattern (Operation const * psOp1, Operation const * psOp2);
bool RelationComparePatternStack (Operation const * psOp1, Operation const * psOp2, VarStack const * psVarStack1, VarStack const * psVarStack2);

// Variable stack operations
void VarStackPush (VarStack * psVarStack, char const* szVar);
char * VarStackPop (VarStack * psVarStack);
void VarStackDrop (VarStack * psVarStack);
int VarStackCount (VarStack * psVarStack);
bool VarStackMatchUnbound (VarStack const * psBoundVars, Operation * psOp);
bool VarStackContains (VarStack const * psVarStack, char const * szVar);
int VarStackFind (VarStack const * psVarStack, char const * szVar);

// Extraction
OperationMap * ExtractOperationMap (Extract const * psExtract, Operation const * const psRelation);
bool ExtractCheckSubstitution (Extract const * psExtract, Operation const * psMain);

// Operation Maps
OperationMap * CreateOperationMap ();
OperationMap * FreeOperatoinMap (OperationMap * psOperationMap);
void OperationMapSetFrom (OperationMap * psOperationMap, Operation const * psOp);
bool OperationMapSetTo (OperationMap * psOperationMap, Operation const * psScrutinee, VarStack * psScrutineeVars);
Operation const * OperationMapGetFrom (OperationMap const * psOperationMap);
Operation const * OperationMapGetTo (OperationMap const * psOperationMap);
void OperationMapInitVarOrigins (OperationMap * psOperationMap);
void OperationMapVarOriginClear (OperationMap * psOperationMap, int nFrom, int nTo);
bool OperationMapVarMappingUnique (OperationMap * psOperationMap);
bool OperationMapVarOriginsClear (OperationMap * psOperationMap, Operation const * psFrom, Operation const * psTo, VarStack * psPatternVars, VarStack * psScrutineeVars);
bool OperationMapVarOriginsCheckClear (OperationMap * psOperationMap, Operation const * psFrom);
void OperationMapSetFromCheck (OperationMap * psOperationMap, Operation const * psOp);

// General utilities
char const * StringGetBounds (char const * szString, int * pnStart, int * pnStrLen);
bool StringCheckBinary (char const * szString, int const nStrLen, char const * szOperator);

#endif // if !defined _H_SYMBOLIC_PRIVATE
