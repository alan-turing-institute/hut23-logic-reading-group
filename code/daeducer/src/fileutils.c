// vim: noet:ts=2:sts=2:sw=2

// SPDX-License-Identifier: AGPL-3.0-or-later
// Copyright © 2026 David Llewellyn-Jones

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

char* file_read(char const* szFilename) {
	FILE* fhFile;
	char *szContent;
	size_t uRead;
	size_t uSize;

	szContent = NULL;

	fhFile = fopen(szFilename, "r");
	if (fhFile) {
		fseek(fhFile, 0, SEEK_END);
		uSize = ftell(fhFile);
		fseek(fhFile, 0, SEEK_SET);
		szContent = malloc((uSize + 1) * sizeof(char));
		uRead = fread(szContent, sizeof(char), uSize, fhFile);
		assert(uRead == uSize);
		fclose(fhFile);
		szContent[uRead] = 0;
	}

	return szContent;
}



