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

#define NAME_LEN (32)

Operation* create_proposition(int value, int position) {
	Operation* operation;
	Operation* variable;
	int index;
	char name[NAME_LEN];

	index = ((position - 1) * 9) + value - 1;
	snprintf(name, NAME_LEN, "P_{%d,%d}", value, position);
	operation = CreateVariable (name);

	return operation;
}

Operation* single_value(int cell) {
	Operation* operation = NULL;
	Operation* proposition = NULL;
	Operation* prop1 = NULL;
	Operation* prop2 = NULL;

	for (int value = 1; value <= 9; ++value) {
		proposition = create_proposition(value, cell);
		if (operation == NULL) {
			operation = proposition;
		}
		else {
			operation = CreateBinary(OPBINARY_LOR, operation, proposition);
		}
	}

	for (int value1 = 1; value1 <= 8; ++value1) {
		for (int value2 = value1 + 1; value2 <= 9; ++value2) {
			prop1 = create_proposition(value1, cell);
			prop2 = create_proposition(value2, cell);
			operation = CreateBinary(OPBINARY_LAND, operation, CreateUnary(OPUNARY_NOT, CreateBinary(OPBINARY_LAND, prop1, prop2)));
		}
	}

	return operation;
}

Operation* single_value2(int cell) {
	Operation* operation1 = NULL;
	Operation* operation2 = NULL;
	Operation* proposition = NULL;
	Operation* prop1 = NULL;
	Operation* prop2 = NULL;

	for (int value1 = 8; value1 >= 1; --value1) {
		for (int value2 = 9; value2 > value1; --value2) {
			prop1 = create_proposition(value1, cell);
			prop2 = create_proposition(value2, cell);
			if (operation1) {
				operation1 = CreateBinary(OPBINARY_LAND, CreateUnary(OPUNARY_NOT, CreateBinary(OPBINARY_LAND, prop1, prop2)), operation1);
			}
			else {
				operation1 = CreateUnary(OPUNARY_NOT, CreateBinary(OPBINARY_LAND, prop1, prop2));
			}
		}
	}

	for (int value = 1; value <= 9; ++value) {
		proposition = create_proposition(10 - value, cell);
		if (operation2 == NULL) {
			operation2 = proposition;
		}
		else {
			operation2 = CreateBinary(OPBINARY_LOR, proposition, operation2);
		}
	}

	return CreateBinary(OPBINARY_LAND, operation2, operation1);
}

int row_start(int pos) {
	return (9 * (pos - 1)) + 1;
}

int col_start(int pos) {
	return pos;
}

int block_start(int pos) {
	return (3 * ((pos - 1) % 3)) + (27 * ((pos - 1) / 3)) + 1;
}

Operation* pos_unique_value(int value, int start, int offset[9]) {
	Operation* operation = NULL;
	Operation* proposition = NULL;
	Operation* prop1 = NULL;
	Operation* prop2 = NULL;

	for (int position = 0; position < 9; ++position) {
		int cell = start + offset[position];
		proposition = create_proposition(value, cell);

		if (operation == NULL) {
			operation = proposition;
		}
		else {
			operation = CreateBinary(OPBINARY_LOR, operation, proposition);
		}
	}

	for (int position1 = 0; position1 < 8; ++position1) {
		for (int position2 = position1 + 1; position2 < 9; ++position2) {
			if (position1 != position2) {
				int cell1 = start + offset[position1];
				int cell2 = start + offset[position2];

				prop1 = create_proposition(value, cell1);
				prop2 = create_proposition(value, cell2);
				operation = CreateBinary(OPBINARY_LAND, operation, CreateUnary(OPUNARY_NOT, CreateBinary(OPBINARY_LAND, prop1, prop2)));
			}
		}
	}

	return operation;
}

Operation* position_unique(int pos, int (*pos_start)(int pos), int offset[9]) {
	Operation* operation = NULL;
	Operation* operation_value = NULL;
	int start;

	for (int value = 1; value <= 9; ++value) {
		start = pos_start(pos);
		operation_value = pos_unique_value(value, start, offset);
		if (operation == NULL) {
			operation = operation_value;
		}
		else {
			operation = CreateBinary(OPBINARY_LAND, operation, operation_value);
		}
	}

	return operation;
}

Operation* initial_values() {
	Operation* operation = NULL;
	Operation* proposition = NULL;
	int cell[27] = {14, 15, 17, 18, 22, 23, 25, 26, 30, 35, 38, 39, 42, 44, 47, 50, 54, 57, 61, 65, 66, 67, 68, 72, 74, 78, 80};
	int value[27] = {5, 3, 2, 4, 2, 4, 5, 7, 3, 9, 7, 9, 5, 8, 1, 7, 6, 7, 3, 4, 1, 3, 8, 7, 6, 4, 1};

	for (int position = 0; position < 27; ++position) {
		proposition = create_proposition(value[position], cell[position]);
		if (operation == NULL) {
			operation = proposition;
		}
		else {
			operation = CreateBinary(OPBINARY_LAND, operation, proposition);
		}
	}

	return operation;
}

void assign_known(Variable* vars) {
	Operation* truth = NULL;
	Operation* proposition = NULL;
	int cell[27] = {14, 15, 17, 18, 22, 23, 25, 26, 30, 35, 38, 39, 42, 44, 47, 50, 54, 57, 61, 65, 66, 67, 68, 72, 74, 78, 80};
	int value[27] = {5, 3, 2, 4, 2, 4, 5, 7, 3, 9, 7, 9, 5, 8, 1, 7, 6, 7, 3, 4, 1, 3, 8, 7, 6, 4, 1};
	char name[NAME_LEN];
	Variable * var;

	for (int position = 0; position < 27; ++position) {
		snprintf(name, NAME_LEN, "P_{%d,%d}", value[position], cell[position]);
		var = FindVariable (vars, name);

		if (var) {
			SetVariable(var, TRUE);
		}
		else {
			printf("Failed to find variable: %s\n", name);
		}
	}
}

/**
 * Main program.
 * This is for testing purposes and isn't part of the libary.
 *
 */
int main (int argc, char * * argv) {
	Operation * psOp;
	Operation * psOp2;
	char szString[512];
	Operation * psSub;
	Operation * psFind;
	Operation * psFind2;
	double fResult;
	Variable * psVars = NULL;
	Variable * psVar = NULL;
	char szInput[INPUT_SIZE];
	char * szRead;
	UserFunc * psFunc = NULL;
	UserFunc * psFuncs = NULL;

	// If we don't do this we get unused variable warnings
	argc = argc;
	argv = argv;

	int rulepos = 0;
	Operation* rule[108];
	int count;
	int row_offset[9] = {8, 7, 6, 5, 4, 3, 2, 1, 0};
	int col_offset[9] = {72, 63, 54, 45, 36, 27, 18, 9, 0};
	int block_offset[9] = {20, 19, 18, 11, 10, 9, 2, 1, 0};

	// Every cell takes a single value
	for (int cell = 1; cell <= 81; ++cell) {
		rule[rulepos] = single_value(cell);
		++rulepos;
	}

	// Every row has one instance
	for (int row = 1; row <= 9; ++row) {
		rule[rulepos] = position_unique(row, row_start, row_offset);
		++rulepos;
	}

	// Every column has one instance
	for (int col = 1; col <= 9; ++col) {
		rule[rulepos] = position_unique(col, col_start, col_offset);
		++rulepos;
	}

	// Every block has one instance
	for (int block = 1; block <= 9; ++block) {
		rule[rulepos] = position_unique(block, block_start, block_offset);
		++rulepos;
	}

	// Initial values
	rule[rulepos] = initial_values();
	++rulepos;

	Variable* vars = NULL;
	for (count = 0; count < rulepos; ++count) {
		vars = CreateVariables(rule[count], vars);
	}

	int var_count = VariableCount (vars);
	printf("Number of propositional variables: %d\n", var_count);

	// Set initial truth values
	assign_known(vars);

	Operation* combined = NULL;
	for (count = 0; count < rulepos; ++count) {
		if (combined == NULL) {
			combined = rule[count];
		}
		else {
			combined = CreateBinary(OPBINARY_LAND, combined, rule[count]);
		}
	}

	printf("Applying derivation rules\n");
	combined = UberSimplifyOperation(combined);
	printf("No more rules apply\n");
	printf("\n");
	printf("Resulting sentence:\n");

	int length;
	length = OperationToStringLength(combined);
	char * string;

	string = malloc(length);

	OperationToString(combined, string, length);
	printf("%s\n", string);
	free(string);

	printf("\n");
	printf("Propositional variables that must be true:\n");
	Variable* var = VariableFirst(vars);
	while (var) {
		if (GetVariableValid(var)) {
			if (GetVariable(var)) {
				printf("%s, ", VariableName(var));
			}
		}

		var = VariableNext(var);
	}
	printf("\n");

	// And relax
	return 0;
}

