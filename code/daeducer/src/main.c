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
#include "command.h"

int main() {
	char szString[1024];
	bool boContinue = TRUE;
	Proof* psProof;
	size_t uIndent;
	size_t uCount;
	Ruleset* psRuleset;
	char* szError;
	Command* psCommand;
	bool boResult;

	printf("Welcome to Daeducer, a simple TFL proof constructor that follows the approach in Chapter 17 of the Forall x: Calgary book on formal logic.\n");
	printf("Enter help to list the available commands.\n");
	printf("Enter <ctrl>-d to exit.\n");
	printf("\n");

	psCommand = command_new();

	psRuleset = ruleset_load("lemmas");

	psProof = proof_new();
	proof_attach_ruleset(psProof, psRuleset);

	while (boContinue) {
		if (proof_complete(psProof)) {
			printf(COL_RESET COL_RED "        ");
		}
		else {
			uIndent = proof_indent(psProof);
			printf(COL_RESET COL_RED "        | ");
			for (uCount = 0; uCount < uIndent; ++uCount) {
				printf("| ");
			}
		}

		printf(COL_GREEN "> ");
		char* szResult = fgets(szString, 1024, stdin);
		printf(COL_RESET);
		if (szResult) {
			boResult = command_parse(psCommand, szResult);
			if (boResult) {
				proof_process_step(psProof, psCommand);

				boResult = !proof_error(psProof, &szError);
				if (boResult) {
					if (!proof_complete(psProof)) {
						proof_print_last_step(psProof);
						printf("\n");
					}
				}
				else {
					printf("Error: %s\n", szError);
				}

			}
			else {
				printf("Error parsing command\n");
			}
			command_reset(psCommand);
		}
		else {
			boContinue = FALSE;
		}
	}

	printf(COL_RESET "\n");

	proof_delete(psProof);
	ruleset_delete(psRuleset);
	command_delete(psCommand);

	return 0;
}
