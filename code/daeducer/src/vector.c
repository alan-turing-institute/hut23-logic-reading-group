// vim: noet:ts=2:sts=2:sw=2

// SPDX-License-Identifier: AGPL-3.0-or-later
// Copyright © 2026 David Llewellyn-Jones

#include <stdlib.h>
#include <string.h>
#include <stdlib.h>
#include <stdarg.h>
#include <stdio.h>

#include "vector.h"

struct _String {
	size_t uLength;
	size_t uAllocated;
	char* szData;
};

String* string_new() {
	String* psString;

	psString = calloc(1, sizeof(String));

	psString->uAllocated = CHUNK_SIZE;
	psString->szData = calloc(CHUNK_SIZE, sizeof(char));

	return psString;
}

void string_delete(String* psString) {
	if (psString) {
		if (psString->szData) {
			free(psString->szData);
			psString->szData = NULL;
		}
		psString->uAllocated = 0;
		psString->uLength = 0;

		free(psString);
	}
}

size_t string_length(String const* psString) {
	return psString->uLength;
}

void string_append(String* psString, char const* szString) {
	size_t uLength;

	uLength = strlen(szString);
	string_append_bytes(psString, szString, uLength);
}

void string_append_bytes(String* psString, char const* szString, size_t uLength) {
	size_t uAllocated;
	size_t uNewLength;

	uNewLength = psString->uLength + uLength;
	uAllocated = (((uNewLength + 1) / CHUNK_SIZE) + 1) * CHUNK_SIZE;
	if (psString->uAllocated != uAllocated) {
		psString->szData = realloc(psString->szData, uAllocated);
		psString->uAllocated = uAllocated;
	}
	memcpy(psString->szData + psString->uLength, szString, uLength);
	psString->szData[uNewLength] = 0;
	psString->uLength = uNewLength;
}

void string_clear(String* psString) {
	if (psString->uAllocated != CHUNK_SIZE) {
		psString->szData = realloc(psString->szData, CHUNK_SIZE);
		psString->uAllocated = CHUNK_SIZE;
	}
	psString->szData[0] = 0;
	psString->uLength = 0;
}

char* string_data(String const* psString) {
	return psString->szData;
}

void string_allocate(String* psString, size_t uSize) {
	size_t uAllocated;
	size_t uPos;
	uAllocated = (((uSize + 1) / CHUNK_SIZE) + 1) * CHUNK_SIZE;

	if (psString->uAllocated != uAllocated) {
		psString->uAllocated = uAllocated;
		psString->szData = realloc(psString->szData, uAllocated * sizeof(char));

		for (uPos = psString->uLength; uPos < uAllocated; ++uPos) {
			psString->szData[uPos] = 0;
		}

		if (uSize < psString->uLength) {
			psString->uLength = uSize;
			psString->szData[uSize] = 0;
		}
	}
}

size_t string_capacity(String *psString) {
	return psString->uAllocated - 1;
}

size_t string_replace(String* psString, char const* szSearch, char const* szReplace) {
	size_t uCount;
	size_t uSearchLength;
	char const* szStart;
	char const* szEnd;
	String* psResult;

	uSearchLength = strlen(szSearch);
	psResult = string_new();

	szStart = psString->szData;
	szEnd = szStart;
	uCount = 0;
	while (szEnd) {
		szEnd = strstr(szStart, szSearch);
		if (szEnd != NULL) {
			string_append_bytes(psResult, szStart, (szEnd - szStart));
			string_append(psResult, szReplace);
			szStart = szEnd + uSearchLength;
			szEnd = szStart;
			uCount += 1;
		}
		else {
			string_append(psResult, szStart);
		}
	}
	free(psString->szData);
	psString->uLength = psResult->uLength;
	psString->uAllocated = psResult->uLength;
	psString->szData = psResult->szData;

	psResult->uLength = 0;
	psResult->uAllocated = 0;
	psResult->szData = NULL;
	string_delete(psResult);

	return uCount;
}

size_t string_sprintf(String* psString, char const* szFormat, ...) {
	va_list sArgs;
	size_t uSize;

	va_start(sArgs, szFormat);
	uSize = vsnprintf(NULL, 0, szFormat, sArgs);
	va_end(sArgs);

	if (psString != NULL) {
		string_allocate(psString, uSize);

		va_start(sArgs, szFormat);
		vsnprintf(psString->szData, psString->uAllocated, szFormat, sArgs);
		va_end(sArgs);

		psString->uLength = uSize;
	}
	return uSize;
}

size_t string_append_sprintf(String* psString, char const* szFormat, ...) {
	va_list sArgs;
	size_t uSize;

	va_start(sArgs, szFormat);
	uSize = vsnprintf(NULL, 0, szFormat, sArgs);
	va_end(sArgs);

	if (psString != NULL) {
		uSize += psString->uLength;
		string_allocate(psString, uSize);

		va_start(sArgs, szFormat);
		vsnprintf(psString->szData + psString->uLength, psString->uAllocated, szFormat, sArgs);
		va_end(sArgs);

		psString->uLength = uSize;
	}

	return uSize;
}


