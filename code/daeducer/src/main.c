// vim: noet:ts=2:sts=2:sw=2

// SPDX-License-Identifier: AGPL-3.0-or-later
// Copyright © 2026 David Llewellyn-Jones

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "symbolic.h"
#include "utils.h"
#include "step.h"
#include "proof.h"
#include "ruleset.h"

int main() {
	char szString[1024];
	bool boContinue = TRUE;
	Proof* psProof;
	size_t uIndent;
	size_t uCount;
	Ruleset* psRuleset;
	bool boError;
	char* szError;

	printf("Welcome to Daeducer, a simple TFL proof constructor that follows the approach in Chapter 17 of the Forall x: Calgary book on formal logic.\n");
	printf("Enter help to list the available commands.\n");
	printf("Enter <ctrl>-d to exit.\n");
	printf("\n");

	psRuleset = ruleset_load("lemmas");

/*	psProof = proof_load("lemmas/and_or_intro.txt");*/
/*	proof_delete(psProof);*/
/*	psProof = NULL;*/

	psProof = proof_new();

	while (boContinue) {
		uIndent = proof_indent(psProof);
		printf(COL_RESET COL_RED "     | ");
		for (uCount = 0; uCount < uIndent; ++uCount) {
			printf("| ");
		}
		printf(COL_GREEN "> ");
		char* szResult = fgets(szString, 1024, stdin);
		printf(COL_RESET);
		if (szResult) {
			proof_process_step(psProof, szResult);

			boError = proof_error(psProof, &szError);
			if (!boError) {
				proof_print_last_step(psProof);
				printf("\n");
			}
			else {
				printf("Error: %s\n", szError);
			}

			boContinue = !proof_complete(psProof);
		}
		else {
			boContinue = FALSE;
		}
	}
	printf(COL_RESET "\n");

	return 0;
}
