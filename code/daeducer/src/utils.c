// vim: noet:ts=2:sts=2:sw=2

// SPDX-License-Identifier: AGPL-3.0-or-later
// Copyright © 2026 David Llewellyn-Jones

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>

#include "symbolic.h"

#include "proof.h"

size_t split_command(char* szCommand, size_t* uPlace, size_t* uLength) {
	size_t uCount;
	size_t uPos;
	bool boConsume;

	// Split the command into pieces
	uCount = 0;
	boConsume = FALSE;
	uPos = 0;
	while (szCommand[uPos] != 0) {
		if ((!boConsume) && (szCommand[uPos] != ' ')) {
			if (uPlace) {
				uPlace[uCount] = uPos;
			}
			boConsume = TRUE;
		}
		else {
			if (boConsume && (szCommand[uPos] == ' ')) {
				if (uLength) {
					uLength[uCount] = uPos - uPlace[uCount];
				}
				uCount += 1;
				boConsume = FALSE;
			}
		}
		uPos += 1;
	}
	if (uPlace && (uCount == 0)) {
		uPlace[uCount] = 0;
	}
	if (uLength) {
		if (uPos == uPlace[uCount]) {
			uLength[uCount] = 0;
		}
		else {
			uLength[uCount] = uPos - uPlace[uCount] - 1;
		}
	}
	uCount += 1;

	return uCount;
}

char* allocate_error(char const* szFormat, ...) {
  va_list args;
	int nLength;
	char* szResult;

  va_start (args, szFormat);
	nLength = vsnprintf(NULL, 0, szFormat, args);
  va_end (args);

	szResult = malloc(nLength + 2);

  va_start (args, szFormat);
	vsnprintf(szResult, nLength + 1, szFormat, args);
  va_end (args);

	return szResult;
}
