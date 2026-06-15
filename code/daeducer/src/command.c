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
#include <unistd.h>

#include "symbolic.h"

#include "command.h"

#define PAUSE_MIN (100000)
#define PAUSE_RANGE (50000)

Command* command_new()
{
	Command* psCommand;

	psCommand = calloc(1, sizeof(Command));
	psCommand->eCommand = STEP_INVALID;

	return psCommand;
}

void command_delete(Command* psCommand)
{
	if (psCommand) {
		command_reset(psCommand);
		free(psCommand);
	}
}

void command_reset(Command* psCommand) {
	size_t uPos;

	if (psCommand) {
		if (psCommand->szLabel) {
			free(psCommand->szLabel);
			psCommand->szLabel = NULL;
		}
		if (psCommand->szCommand) {
			free(psCommand->szCommand);
			psCommand->szCommand = NULL;
		}
		psCommand->eCommand = STEP_INVALID;
		if (psCommand->aszParameter) {
			for (uPos = 0; uPos < psCommand->uCount; ++uPos) {
				if (psCommand->aszParameter[uPos]) {
					free(psCommand->aszParameter[uPos]);
					psCommand->aszParameter[uPos] = NULL;
				}
			}
			free(psCommand->aszParameter);
			psCommand->aszParameter = NULL;
		}
		psCommand->uCount = 0;
	}
}

size_t command_count(Command const* psCommand) {
	return psCommand->uCount;
}

char const* command_get_parameter(Command const* psCommand, size_t uPos) {
	char* szCommand;

	if (uPos < psCommand->uCount) {
		szCommand = psCommand->aszParameter[uPos];
	}
	else {
		szCommand = NULL;
	}
	return szCommand;
}

void command_label_strip(char const* szStart, size_t uLength, size_t* puEnd) {
	size_t uPos;
	bool boContinue;

	uPos = 0;
	boContinue = TRUE;
	while ((uPos < uLength) && boContinue) {
		if ((szStart[uPos] == ' ') || (szStart[uPos] == ':')) {
			boContinue = FALSE;
		}
		else {
			uPos += 1;
		}
	}

	if (puEnd) {
		*puEnd = uPos;
	}
}

size_t command_next_non_space(char const* szStart, size_t uLength) {
	size_t uPos;
	bool boContinue;

	uPos = 0;
	boContinue = TRUE;
	while ((uPos < uLength) && boContinue) {
		if ((szStart[uPos] != ' ') && (szStart[uPos] != '\n')) {
			boContinue = FALSE;
		}
		else {
			uPos += 1;
		}
	}

	return uPos;
}

size_t command_next_space(char const* szStart, size_t uLength) {
	size_t uPos;
	bool boContinue;

	uPos = 0;
	boContinue = TRUE;
	while ((uPos < uLength) && boContinue) {
		if ((szStart[uPos] == ' ') || (szStart[uPos] == '\n')) {
			boContinue = FALSE;
		}
		else {
			uPos += 1;
		}
	}

	return uPos;
}

size_t command_next_non_space_comma(char const* szStart, size_t uLength) {
	size_t uPos;
	bool boContinue;

	uPos = 0;
	boContinue = TRUE;
	while ((uPos < uLength) && boContinue) {
		if ((szStart[uPos] != ' ') && (szStart[uPos] != ',') && (szStart[uPos] != '\n')) {
			boContinue = FALSE;
		}
		else {
			uPos += 1;
		}
	}

	return uPos;
}

size_t command_next_space_comma(char const* szStart, size_t uLength) {
	size_t uPos;
	bool boContinue;
	int nBrackets;

	uPos = 0;
	nBrackets = 0;
	boContinue = TRUE;
	while ((uPos < uLength) && boContinue) {
		if (szStart[uPos] == '(') {
			nBrackets += 1;
		}
		if (szStart[uPos] == ')') {
			nBrackets -= 1;
		}
		if ((nBrackets == 0) && (szStart[uPos] == ',')) {
			boContinue = FALSE;
		}
		else {
			uPos += 1;
		}
	}

	boContinue = TRUE;
	while ((uPos >= 0) && boContinue) {
		if ((szStart[uPos - 1] != ' ') && (szStart[uPos - 1] != '\n')) {
			boContinue = FALSE;
		}
		else {
			uPos -= 1;
		}
	}

	return uPos;
}

bool command_parse(Command* psCommand, char const* szCommand) {
	size_t uPos;
	bool boFound;
	size_t uLength;
	size_t uStart;
	size_t uEnd;
	size_t uParametersStart;
	size_t uCount;
	bool boResult;

	boResult = FALSE;
	uLength = strlen(szCommand);

	uStart = 0;
	while ((uStart < uLength) && (szCommand[uStart] == ' ')) {
		uStart += 1;
	}

	boFound = FALSE;
	uPos = uStart;
	while ((uPos < uLength) && (!boFound)) {
		if (szCommand[uPos] == ':') {
			boFound = TRUE;
		}
		else {
			uPos += 1;
		}
	}

	if (boFound) {
		command_label_strip(szCommand + uStart, uPos - uStart, &uEnd);
		if (uEnd > 0) {
			psCommand->szLabel = strndup(szCommand + uStart, uEnd);
			//printf("Label: \"%s\"\n", psCommand->szLabel);
		}
		uStart = uPos + 1;
	}
	else {
		uStart = 0;
	}

	uStart += command_next_non_space(szCommand + uStart, uLength - uStart);
	uEnd = uStart + command_next_space(szCommand + uStart, uLength - uStart);

	if (uEnd > uStart) {
		psCommand->szCommand = strndup(szCommand + uStart, uEnd - uStart);
		//printf("Command: \"%s\"\n", psCommand->szCommand);
		boResult = TRUE;
	}

	uStart = uEnd;
	uStart += command_next_non_space(szCommand + uStart, uLength - uStart);
	uParametersStart = uStart;

	uCount = 0;
	while (uStart < uLength) {
		uStart = uEnd;
		uStart += command_next_non_space_comma(szCommand + uStart, uLength - uStart);
		uEnd = uStart + command_next_space_comma(szCommand + uStart, uLength - uStart);
		if (uStart < uLength) {
			uCount += 1;
		}
	}

	//printf("Parameters: %lu\n", uCount);

	psCommand->uCount = uCount;
	psCommand->aszParameter = calloc(uCount, sizeof(char*));

	uStart = uParametersStart;
	uEnd = uParametersStart;
	uCount = 0;
	while (uStart < uLength) {
		uStart = uEnd;
		uStart += command_next_non_space_comma(szCommand + uStart, uLength - uStart);
		uEnd = uStart + command_next_space_comma(szCommand + uStart, uLength - uStart);

		if (uStart < uEnd) {
			psCommand->aszParameter[uCount] = strndup(szCommand + uStart, uEnd - uStart);
			//printf("Parameter %lu: \"%s\"\n", uCount, psCommand->aszParameter[uCount]);
			uCount += 1;
		}
	}

	return boResult;
}

void command_print(Command * psCommand) {
	size_t uPos;

	if (psCommand) {
		if (psCommand->szLabel) {
			printf("%s: %s ", psCommand->szLabel, psCommand->szCommand);
		}
		else {
			printf("--- : %s ", psCommand->szCommand);
		}
		for (uPos = 0; (uPos + 1) < psCommand->uCount; ++uPos) {
			printf("%s, ", psCommand->aszParameter[uPos]);
		}
		if (uPos < psCommand->uCount) {
			printf("%s", psCommand->aszParameter[uPos]);
		}
		printf("\n");
	}
}

void command_pause() {
	int nSleep;
	fflush(stdout);
	nSleep = PAUSE_MIN - (PAUSE_RANGE / 2) + (rand() % PAUSE_RANGE);
	usleep(nSleep);
}

void command_print_generated_punctuation(char const* szString) {
	size_t uPos;

	uPos = 0;
	while (szString[uPos]) {
		if (strchr(" _,:()v^-<>!", szString[uPos]) != NULL) {
			command_pause();
		}
		printf("%c", szString[uPos]);
		uPos += 1;
	}
	command_pause();
}

void command_print_generated(Command * psCommand) {
	size_t uPos;

	if (psCommand) {
		command_pause();
		command_print_generated_punctuation(psCommand->szCommand);
		command_pause();
		printf(" ");
		command_pause();
		for (uPos = 0; (uPos + 1) < psCommand->uCount; ++uPos) {
			command_print_generated_punctuation(psCommand->aszParameter[uPos]);
			command_pause();
			printf(", ");
			command_pause();
		}
		if (uPos < psCommand->uCount) {
			command_print_generated_punctuation(psCommand->aszParameter[uPos]);
			command_pause();
		}
		printf("\n");
	}
}

