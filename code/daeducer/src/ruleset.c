// vim: noet:ts=2:sts=2:sw=2

// SPDX-License-Identifier: AGPL-3.0-or-later
// Copyright © 2026 David Llewellyn-Jones

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <dirent.h>
#include <sys/stat.h>
#include <assert.h>

#include "symbolic.h"
#include "proof.h"
#include "lemma.h"
#include "step.h"

#include "ruleset.h"

struct _Ruleset {
	Lemma** apsLemma;
	size_t uLemmaNum;
};

void ruleset_initialise(Ruleset* psRuleset);
void ruleset_load_recursive(Ruleset* psRuleset, char const* szDirectory);

Ruleset* ruleset_new()
{
	Ruleset* psRuleset;

	psRuleset = calloc(1, sizeof(Ruleset));

	ruleset_initialise(psRuleset);

	return psRuleset;
}

void ruleset_delete(Ruleset* psRuleset)
{
	size_t uPos;

	if (psRuleset) {
		if (psRuleset->apsLemma) {
			for (uPos = 0; uPos < psRuleset->uLemmaNum; ++uPos) {
				lemma_delete(psRuleset->apsLemma[uPos]);
				psRuleset->apsLemma[uPos] = NULL;
			}
			free(psRuleset->apsLemma);
		}

		free(psRuleset);
	}
}

void ruleset_initialise(Ruleset* psRuleset) {
	assert(psRuleset->uLemmaNum == 0);
	assert(psRuleset->apsLemma == NULL);

	psRuleset->uLemmaNum = (size_t)STEP_CONTROL;
	psRuleset->apsLemma = calloc(psRuleset->uLemmaNum, sizeof(Lemma*));

	psRuleset->apsLemma[STEP_REITERATION] = lemma_compile("reiteration", "RE", 1, 0, (char const*[]) {"A"}, "A");
	psRuleset->apsLemma[STEP_CONJUNCTION_INTRO] = lemma_compile("and_intro", "^I", 2, 0, (char const*[]) {"A", "B"}, "(A ^ B)");
	psRuleset->apsLemma[STEP_CONJUNCTION_ELIM_LEFT] = lemma_compile("and_elim_left", "^E", 1, 0, (char const*[]) {"(A ^ B)"}, "A");
	psRuleset->apsLemma[STEP_CONJUNCTION_ELIM_RIGHT] = lemma_compile("and_elim_right", "^E", 1, 0, (char const*[]) {"(A ^ B)"}, "B");
	psRuleset->apsLemma[STEP_IMPLICATION_ELIM] = lemma_compile("imp_elim", "->E", 2, 0, (char const*[]) {"(A -> B)", "A"}, "B");
	psRuleset->apsLemma[STEP_DISJUNCTION_INTRO_LEFT] = lemma_compile("or_intro_left", "vI", 1, 1, (char const*[]) {"A", "B"}, "(A v B)");
	psRuleset->apsLemma[STEP_DISJUNCTION_INTRO_RIGHT] = lemma_compile("or_intro_right", "vI", 1, 1, (char const*[]) {"A", "B"}, "(B v A)");
	psRuleset->apsLemma[STEP_EXPLOSION] = lemma_compile("explosion", "X", 1, 1, (char const*[]) {"FALSE", "A"}, "A");
	psRuleset->apsLemma[STEP_NEGATION_ELIM] = lemma_compile("not_elim", "!E", 2, 0, (char const*[]) {"!A", "A"}, "FALSE");
}

Ruleset* ruleset_load(char const* szDirectory) {
	Ruleset* psRuleset;

	psRuleset = ruleset_new();

	ruleset_load_recursive(psRuleset, szDirectory);

	return psRuleset;
}

void ruleset_add_lemma(Ruleset* psRuleset, Lemma* psLemma) {
	psRuleset->uLemmaNum += 1;
	psRuleset->apsLemma = realloc(psRuleset->apsLemma, psRuleset->uLemmaNum * sizeof(Lemma*));

	psRuleset->apsLemma[(psRuleset->uLemmaNum - 1)] = psLemma;
}

void ruleset_load_recursive(Ruleset* psRuleset, char const* szDirectory) {
	DIR* psDirectory;
	size_t uLength;
	char* szPath;
	Proof* psProof;
	Lemma* psLemma;
	struct stat sInfo;
	int nReturn;

	psDirectory = opendir(szDirectory);

	if (psDirectory) {
		struct dirent* psEntry;
		psEntry = readdir(psDirectory);
		while (psEntry != NULL) {
			if ((strcmp(psEntry->d_name, ".") != 0) && (strcmp(psEntry->d_name, "..") != 0)) {
				uLength = snprintf(NULL, 0, "%s/%s", szDirectory, psEntry->d_name);
				szPath = calloc(uLength + 1, sizeof(char));
				snprintf(szPath, uLength + 1, "%s/%s", szDirectory, psEntry->d_name);

				nReturn = lstat(szPath, &sInfo);
				if (nReturn >= 0) {
					switch (sInfo.st_mode & S_IFMT) {
						case S_IFDIR: {
							ruleset_load_recursive(psRuleset, szPath);
						}
						break;
						case S_IFREG: {
							psProof = proof_load(psRuleset, szPath);
							if (psProof) {
								psLemma = lemma_from_proof(psProof);
								proof_delete(psProof);
								ruleset_add_lemma(psRuleset, psLemma);
							}
						}
						break;
						default: {
							// Do nothing
						}
					}
				}
				free(szPath);
			}

			psEntry = readdir(psDirectory);
		}
		closedir(psDirectory);
	}
	else {
		printf("Couldn't open directory: %s\n", szDirectory);
	}
}

bool ruleset_get_command_index(Ruleset* psRuleset, char const* szCommand, size_t* puIndex) {
	return ruleset_get_command_index_start(psRuleset, szCommand, 0, puIndex);
}

bool ruleset_get_command_index_start(Ruleset* psRuleset, char const* szCommand, size_t uStartPos, size_t* puIndex) {
	bool boFound;
	size_t uPos;

	boFound = FALSE;
	if (psRuleset) {
		for (uPos = uStartPos; (uPos < psRuleset->uLemmaNum) && (!boFound); ++uPos) {
			if (strcmp(szCommand, psRuleset->apsLemma[uPos]->szCommand) == 0) {
				if (puIndex) {
					*puIndex = uPos;
				}
				boFound = TRUE;
			}
		}
	}

	return boFound;
}

Lemma* ruleset_get_lemma(Ruleset* psRuleset, size_t uIndex) {
	Lemma* psLemma;

	if (uIndex < psRuleset->uLemmaNum) {
		psLemma = psRuleset->apsLemma[uIndex];
	}
	else {
		psLemma = NULL;
	}

	return psLemma;
}

void ruleset_print_help_line(Ruleset* psRuleset, size_t uIndex) {
	Lemma* psLemma;
	size_t uParameters;
	size_t uPos;

	if (uIndex < psRuleset->uLemmaNum) {
		psLemma = psRuleset->apsLemma[uIndex];
		if (psLemma) {
			printf("    %-20s", psLemma->szCommand);
			uParameters = psLemma->uRefNum + psLemma->uOpNum;
			if (uParameters > 0) {
				printf(" ");
			}
			uPos = 0;
			while (uPos < uParameters) {
				if (uPos < psLemma->uRefNum) {
					printf("<ref>");
				}
				else {
					printf("<exp>");
				}
				uPos += 1;
				if (uPos < uParameters) {
					printf(", ");
				}
			}
			printf("\n");
		}
	}
}

size_t ruleset_get_lemma_num(Ruleset* psRuleset) {
	size_t uLemmaNum;

	if (psRuleset) {
		uLemmaNum = psRuleset->uLemmaNum;
	}
	else {
		uLemmaNum = 0;
	}

	return uLemmaNum;
}

bool ruleset_get_command_name(Ruleset* psRuleset, size_t uIndex, String* psString) {
	Lemma* psLemma;
	char const* szCommand;
	bool boSuccess = FALSE;

	if (psRuleset) {
		szCommand = NULL;
		psLemma = ruleset_get_lemma(psRuleset, uIndex);
		if (psLemma) {
			szCommand = psLemma->szCommand;
		}
		else {
			if (((long long int)uIndex > STEP_INVALID) && (long long int)(uIndex < STEP_CONTROL)) {
				szCommand = aszCommand[uIndex];
			}
		}
		if (szCommand) {
			string_append_sprintf(psString, "%s", szCommand);
			boSuccess = TRUE;
		}
	}

	return boSuccess;
}

