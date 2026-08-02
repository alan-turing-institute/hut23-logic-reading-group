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
#include "model.h"

int main() {
	char szString[1024];
	bool boContinue = TRUE;
	Proof* psProof;
	Ruleset* psRuleset;
	char const* szError;
	Command* psCommand;
	bool boResult;
	Model* psModel;

	printf("Welcome to Daeducer, a simple First Order Logic proof constructor that follows the approach of Forall x: Calgary.\n");
	printf("Loading model...\n");
	psModel = model_initialise();
	printf("\n");
	printf("Enter help to list the available commands.\n");
	printf("Enter <ctrl>-d to exit.\n");
	printf("\n");

	psCommand = command_new();

	psRuleset = ruleset_load("lemmas");

	psProof = proof_new();
	proof_attach_ruleset(psProof, psRuleset);

	while (boContinue) {
		proof_print_prompt(psProof);

		char* szResult = fgets(szString, 1024, stdin);
		printf(COL_RESET);
		if (szResult) {
			boResult = command_parse(psCommand, szResult);
			if (boResult) {
				proof_process_step(psProof, psModel, psCommand);

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
	psProof = NULL;
	ruleset_delete(psRuleset);
	psRuleset = NULL;
	command_delete(psCommand);
	psCommand = NULL;
	model_delete(psModel);
	psModel = NULL;

	return 0;
}
