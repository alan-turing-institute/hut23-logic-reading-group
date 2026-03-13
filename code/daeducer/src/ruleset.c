// vim: noet:ts=2:sts=2:sw=2

// SPDX-License-Identifier: AGPL-3.0-or-later
// Copyright © 2026 David Llewellyn-Jones

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <dirent.h>
#include <sys/stat.h>

#include "symbolic.h"
#include "lemma.h"

#include "ruleset.h"

struct _Ruleset {
	Lemma** apsLemma;
	size_t uLemmaNum;
};

void ruleset_load_recursive(Ruleset* psRuleset, char const* szDirectory);

Ruleset* ruleset_new()
{
	Ruleset* psRuleset;

	psRuleset = calloc(1, sizeof(Ruleset));

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
							psProof = proof_load(szPath);
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
	}
	else {
		printf("Couldn't open directory: %s\n", szDirectory);
	}
}
