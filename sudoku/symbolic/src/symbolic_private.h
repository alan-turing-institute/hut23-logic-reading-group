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
	} Vars;
};

typedef struct _OperationMap {
    char * szVar;
    Operation * psOp;
} OperationMap;

struct _Extract {
    int nCount;
    OperationMap * apsOps;
};

//////////////////////////////////////////////////////////////////
// Function prototypes

// Reference counting for variables
void DecrementVarRef (Variable * psVar);
void IncrementVarRef (Variable * psVar);

// String conversion
char * RecurseToString (Operation * psOp, int nStrLen);
int RecurseToStringLength (Operation * psOp);
char * RecurseToStringC (Operation * psOp, int nStrLen);
int RecurseToStringCLength (Operation * psOp);

// User Func operations
void IncrementUserFuncRef (UserFunc * psUserFunc);
void DecrementUserFuncRef (UserFunc * psUserFunc);

// User Unary operations
Operation * CopyUser (Operation * psOp);
double UserApproximateDefault (Operation * psOp);
Operation * UserDifferentiateDefault (Operation * psOp, Operation * psWRT);
Operation * UserSimplifyDefault (Operation * psOp);
bool UserCompare (Operation * psOp1, Operation * psOp2);
char * UserToString (Operation * psOp, char * szString, int nStrLen);
int UserToStringLength (Operation * psOp);
char * UserToStringC (Operation * psOp, char * szString, int nStrLen);
int UserToStringCLength (Operation * psOp);
void UserFreeRecursive (Operation * psOp);



#endif // if !defined _H_SYMBOLIC_PRIVATE
