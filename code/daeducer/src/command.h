// vim: noet:ts=2:sts=2:sw=2

// SPDX-License-Identifier: AGPL-3.0-or-later
// Copyright © 2026 David Llewellyn-Jones

#ifndef _COMMAND_H
#define _COMMAND_H

#include "daeducer.h"

#include "step.h"

struct _Command {
	char* szLabel;
	char* szCommand;
	STEP eCommand;
	size_t uCount;
	char** aszParameter;
};

Command* command_new();
void command_delete(Command* psCommand);
void command_reset(Command* psCommand);
bool command_parse(Command* psCommand, char const* szCommand);
void command_print(Command * psCommand);

#endif // _COMMAND_H

