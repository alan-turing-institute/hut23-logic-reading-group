// vim: noet:ts=2:sts=2:sw=2

// SPDX-License-Identifier: AGPL-3.0-or-later
// Copyright © 2026 David Llewellyn-Jones

#ifndef _VECTOR_H_
#define _VECTOR_H_

#define CHUNK_SIZE (32)

#include <stddef.h>

typedef struct _String String;

String* string_new();
void string_delete(String* psString);
size_t string_length(String const* psString);
void string_append(String* psString, char const* szString);
void string_append_bytes(String* psString, char const* szString, size_t uLength);
void string_clear(String* psString);
char* string_data(String const* psString);
void string_allocate(String* psString, size_t uSize);
size_t string_capacity(String *psString);
size_t string_replace(String* psString, char const* szSearch, char const* szReplace);
size_t string_sprintf(String* psString, char const* szFormat, ...);
size_t string_append_sprintf(String* psString, char const* szFormat, ...);

#define VECTOR_SIGS(TYPE) \
typedef struct _Vector_ ## TYPE Vector_ ## TYPE; \
Vector_ ## TYPE* vector_new_ ## TYPE(); \
void vector_delete_ ## TYPE(Vector_ ## TYPE* psVector); \
size_t vector_size_ ## TYPE(Vector_ ## TYPE* psVector); \
void vector_allocate_ ## TYPE(Vector_ ## TYPE* psVector, size_t uSize); \
void vector_set_size_ ## TYPE(Vector_ ## TYPE* psVector, size_t uSize); \
void vector_push_ ## TYPE(Vector_ ## TYPE* psVector, TYPE sItem); \
void vector_clear_ ## TYPE(Vector_ ## TYPE* psVector); \
TYPE* vector_data_ ## TYPE(Vector_ ## TYPE* psVector); \
void vector_copy_ ## TYPE(Vector_ ## TYPE* psVector, Vector_ ## TYPE* psFrom);

#define VECTOR(TYPE) \
struct _Vector_ ## TYPE { \
	size_t uLength; \
	size_t uAllocated; \
	TYPE* asData; \
}; \
\
Vector_ ## TYPE* vector_new_ ## TYPE() { \
	Vector_ ## TYPE* psVector; \
	psVector = calloc(1, sizeof(TYPE)); \
	psVector->uAllocated = CHUNK_SIZE; \
	psVector->asData = calloc(CHUNK_SIZE, sizeof(TYPE)); \
	return psVector; \
} \
\
void vector_delete_ ## TYPE(Vector_ ## TYPE* psVector) { \
	if (psVector) { \
		if (psVector->asData) { \
			free(psVector->asData); \
			psVector->asData = NULL; \
		} \
		psVector->uAllocated = 0; \
		psVector->uLength = 0; \
		free(psVector); \
	} \
} \
\
size_t vector_size_ ## TYPE(Vector_ ## TYPE* psVector) { \
	return psVector->uLength; \
} \
\
void vector_allocate_ ## TYPE(Vector_ ## TYPE* psVector, size_t uSize) { \
	size_t uAllocated; \
	uAllocated = (((uSize + 1) / CHUNK_SIZE) + 1) * CHUNK_SIZE; \
\
	if (psVector->uAllocated != uAllocated) { \
		psVector->uAllocated = uAllocated; \
		psVector->asData = realloc(psVector->asData, uAllocated * sizeof(TYPE)); \
		if (uSize < psVector->uLength) { \
			psVector->uLength = uSize; \
		} \
	} \
} \
\
void vector_set_size_ ## TYPE(Vector_ ## TYPE* psVector, size_t uSize) { \
	vector_allocate_ ## TYPE(psVector, uSize); \
	psVector->uLength = uSize; \
} \
\
void vector_push_ ## TYPE(Vector_ ## TYPE* psVector, TYPE sItem) { \
	vector_allocate_ ## TYPE(psVector, psVector->uLength + 1); \
	psVector->asData[psVector->uLength] = sItem; \
	psVector->uLength += 1; \
} \
\
void vector_clear_ ## TYPE(Vector_ ## TYPE* psVector) { \
	if (psVector->uAllocated != CHUNK_SIZE) { \
		psVector->asData = realloc(psVector->asData, CHUNK_SIZE * sizeof(TYPE)); \
		psVector->uAllocated = CHUNK_SIZE; \
	} \
	psVector->uLength = 0; \
} \
\
TYPE* vector_data_ ## TYPE(Vector_ ## TYPE* psVector) { \
	return psVector->asData; \
} \
void vector_copy_ ## TYPE(Vector_ ## TYPE* psVector, Vector_ ## TYPE* psFrom) { \
	vector_allocate_ ## TYPE(psVector, psFrom->uLength); \
	memcpy(psVector->asData, psFrom->asData, psFrom->uLength * sizeof(TYPE)); \
	vector_set_size_ ## TYPE(psVector, psFrom->uLength); \
}

#endif /* _VECTOR_H_ */
