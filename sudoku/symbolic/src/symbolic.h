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
 * The MIT License
 * 
 * Copyright (c) 2003-2026 David Llewellyn-Jones
 * 
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 * 
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 *
 * @section DESCRIPTION
 *
 * Library for the construction of nested symbolic propositions.
 * The Flying Pig!
 * Started 5/8/2003
 * http://www.flypig.co.uk?to=symbolic
 *
 */

#if !defined _H_SYMBOLIC
#define _H_SYMBOLIC

//////////////////////////////////////////////////////////////////
// Includes

//#include "local.h"

#if defined _RISC_OS
#include "oslib/types.h"
#include "oslib/os.h"
#include "oslib/macros.h"
#endif

#include <stddef.h>
#if !defined (WIN32)
#include <stdbool.h>
#endif

//////////////////////////////////////////////////////////////////
// Defines

//#define _DEBUG
#define _MEM_PROFILE

#ifndef TRUE
#define TRUE (1)
#endif
#ifndef FALSE
#define FALSE (0)
#endif

#define CONTFRAC_MAXDEPTH (10)

//////////////////////////////////////////////////////////////////
// Structures

// Main operation structure
typedef struct _Operation Operation;
// Main variable strucutre
typedef struct _Variable Variable;
// Main user function strucutre
typedef struct _UserFunc UserFunc;
// Operation mapping for pattern extraction
typedef struct _Extract Extract;

// User operation callbacks
typedef double (*UserApproximate)(double fVar1, void * psContext);
typedef Operation * (*UserDifferentiate)(Operation * psOp, Operation * psWRT, void * psContext);
typedef Operation * (*UserSimplify)(Operation * psOp, void * psContext);

// Unary operations
typedef enum _OPUNARY
{
	OPUNARY_INVALID = -1,

	OPUNARY_NOT,

	OPUNARY_NUM
} OPUNARY;

// Textual equivalents of the unary operations
// TODO: This array should be moved into Convert.cpp
static char const aszOpUnary[OPUNARY_NUM][7] = {
	"!",
};

// Binary operations
typedef enum _OPBINARY {
	OPBINARY_INVALID = -1,

	OPBINARY_LAND,
	OPBINARY_LOR,
	OPBINARY_LIMP,
	OPBINARY_LEOR,

	OPBINARY_NUM
} OPBINARY;

// Textual equivalents of the binary operations
// TODO: This array should be moved into Convert.cpp
static char const aszOpBinary[OPBINARY_NUM][6] = {
	"^",
	"v",
	"->",
	"x",
};

// Ternary operatons
typedef enum _OPTERNARY {
	OPTERNARY_INVALID = -1,

	OPTERNARY_NUM
} OPTERNARY;

//////////////////////////////////////////////////////////////////
// Function prototypes

// Memory related functions
void PropMemReset (void);
void PropMemOutput (void);
void * PropMemMalloc (size_t size);
void * PropMemCalloc (size_t n, size_t size);
void PropMemFree (void * ptr);

// Creation operations
Operation * CreateTruthValue (bool const boTruth);
Operation * CreateVariable (char const * szVar);
Operation * CreateUnary (OPUNARY eOpType, Operation * psVar1);
Operation * CreateBinary (OPBINARY eOpType, Operation * psVar1, Operation * psVar2);

// Conversion to and from strings
char * OperationToString (Operation * psOp, char * szString, int nStrLen);
int OperationToStringLength (Operation * psOp);
Operation * StringToOperation (char const * szString);

// Managing operations
void FreeRecursive (Operation * psOp);
Operation * CopyRecursive (Operation * psOp);
Operation * FindOperation (Operation * psMain, Operation * psFind);

// Manipulating operations mathematically
Operation * SubstituteOperation (Operation * psMain, Operation * psFind, Operation * psSub);
Operation * SubstituteOperationPair (Operation * psMain, Operation * psFind1, Operation * psSub1, Operation * psFind2, Operation * psSub2);
Operation * SubstituteOperationMany (Operation * psMain, Operation ** apsFind, Operation ** apsSub, int nCount);
Operation * SimplifyOperation (Operation * psOp);
bool CompareOperations (Operation * psOp1, Operation * psOp2);
double ApproximateOperation (Operation * psOp);
Operation * UberSimplifyOperation (Operation * psOp);

// Managing variables
Variable * CreateVariableValue (Operation * psOp, Variable * psVariables);
Variable * CreateVariables (Operation * psOp, Variable * psVariables);
Variable * FindVariable (Variable * psVariables, char const * const szVar);
void SetVariable (Variable * psVar, bool boValue);
bool GetVariable (Variable * psVar);
bool GetVariableValid (Variable * psVar);
void UnsetVariable (Variable * psVar);
Variable * FreeVariables (Variable * psVariables);
int VariableCount (Variable * psVariables);
Variable * VariableFirst (Variable * psVariables);
Variable * VariableLast (Variable * psVariables);
Variable * VariableNext (Variable * psVariables);
Variable * VariablePrev (Variable * psVariables);
char const * VariableName (Variable const * const psVariable);

// Pattern extraction
Extract * ExtractPattern (Operation * psPattern, Operation * psScrutinee);
Extract * ExtractPatternMany (Operation ** apsPattern, Operation ** apsScrutinee, int nCount);
int ExtractCount(Extract * psExtract);
char * ExtractName(Extract * psExtract, int nPosition);
Operation * ExtractValue(Extract * psExtract, char const * const szName);
Operation * ExtractValueFromPos(Extract * psExtract, int nPosition);
void FreeExtract(Extract * psExtract);


#endif // if !defined _H_SYMBOLIC
