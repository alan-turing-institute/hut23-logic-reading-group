// vim: noet:ts=2:sts=2:sw=2

// SPDX-License-Identifier: AGPL-3.0-or-later
// Copyright © 2026 David Llewellyn-Jones

#include "llama.h"
#include <string.h>
#include <stdlib.h>
#include <assert.h>
#include <unistd.h>

#include "sampler.h"
#include "config.h"
#include "fileutils.h"
#include "proof.h"
#include "step.h"
#include "command.h"
#include "symbolic.h"

#include "model.h"

#define MODEL_FILE "Qwen3.5-35B-A3B-UD-Q8_K_XL.gguf"
#define CONTEXT_WINDOW (2048 * 32)
#define REASONING_BUDGET (1024 * 32)
#define MAX_ERRORS (8)
#define NEURALIZE_FREQUENCY (3)

typedef struct llama_chat_message ChatMessage;
VECTOR_SIGS(ChatMessage)
VECTOR(ChatMessage)

struct _Model {
	Config* psConfig;
	struct llama_model* psModel;
	Sampler* psSampler;
	Vector_ChatMessage* psMessages;
	char* szFormatted;
	size_t uFormattedSize;
	int nNewLength;
	int nPrevLen;
};

typedef enum _OUTCOME {
	OUTCOME_INVALID = -1,

	OUTCOME_SUCCESS,
	OUTCOME_ERROR_RESULT,
	OUTCOME_ERROR_LOGIC,
	OUTCOME_ERROR_COMMAND,

	OUTCOME_NUM
} OUTCOME;

typedef struct _ModelOutcome {
	OUTCOME eOutcome;
	String* psError;
	size_t uErrorCount;
	Operation* psIncorrectResult;
} ModelOutcome;

bool model_template_apply(String* psResult, char const* szTemplateFile, Proof const* psProof, Operation* psClaim, Operation* psIncorrectResult, char const* szError);

// Only print errors
void llama_log(enum ggml_log_level eLevel, char const* szText, void * /* user_data */) {
	if (eLevel >= GGML_LOG_LEVEL_ERROR) {
		fprintf(stderr, "%s", szText);
	}
};

Model* model_new() {
	Model* psModel;

	psModel = calloc(1, sizeof(Model));
	psModel->psConfig = config_new();
	psModel->psMessages = vector_new_ChatMessage();

	return psModel;
}

void model_delete(Model* psModel) {
	size_t uMessageSize;
	size_t uPos;

	if (psModel) {
		if (psModel->psConfig) {
			config_delete(psModel->psConfig);
			psModel->psConfig = NULL;
		}
		if (psModel->psModel) {
			llama_model_free(psModel->psModel);
			psModel->psModel = NULL;
		}
		if (psModel->psSampler) {
			sampler_delete(psModel->psSampler);
		}
		if (psModel->psMessages) {
			uMessageSize = vector_size_ChatMessage(psModel->psMessages);
			for (uPos = 0; uPos < uMessageSize; ++uPos) {
				free((char*)vector_data_ChatMessage(psModel->psMessages)[uPos].content);
			}
			vector_delete_ChatMessage(psModel->psMessages);
			psModel->psMessages = NULL;
		}
		if (psModel->szFormatted) {
			free(psModel->szFormatted);
			psModel->szFormatted = NULL;
		}

		free(psModel);
		psModel = NULL;
	}
}

ModelOutcome* modeloutcome_new() {
	ModelOutcome* psOutcome;

	psOutcome = calloc(1, sizeof(ModelOutcome));
	psOutcome->eOutcome = OUTCOME_INVALID;
	psOutcome->uErrorCount = 0;
	psOutcome->psError = string_new();

	return psOutcome;
}

void modeloutcome_delete(ModelOutcome* psOutcome) {
	if (psOutcome) {
		if (psOutcome->psError) {
			free(psOutcome->psError);
			psOutcome->psError = NULL;
		}
		if (psOutcome->psIncorrectResult) {
			FreeRecursive(psOutcome->psIncorrectResult);
			psOutcome->psIncorrectResult = NULL;
		}
		free(psOutcome);
	}
}

void modeloutcome_reset(ModelOutcome* psOutcome) {
	if (psOutcome) {
		psOutcome->eOutcome = OUTCOME_INVALID;
		string_clear(psOutcome->psError);
		if (psOutcome->psIncorrectResult) {
			FreeRecursive(psOutcome->psIncorrectResult);
			psOutcome->psIncorrectResult = NULL;
		}
	}
}

void model_neuralize(Model* psModel) {
	size_t uMessageSize;
	size_t uPos;
	struct llama_context_params sCtxParams;

	if (psModel) {
		if (psModel->psMessages) {
			uMessageSize = vector_size_ChatMessage(psModel->psMessages);
			for (uPos = 0; uPos < uMessageSize; ++uPos) {
				free((char*)(vector_data_ChatMessage(psModel->psMessages)[uPos].content));
			}
			vector_clear_ChatMessage(psModel->psMessages);
		}

		if (psModel->psConfig) {
			llama_free(psModel->psConfig->psContext);

			sCtxParams = llama_context_default_params();
			sCtxParams.n_ctx = psModel->psConfig->nContextSize;
			sCtxParams.n_batch = psModel->psConfig->nContextSize;

			psModel->psConfig->psContext = llama_init_from_model(psModel->psConfig->psModel, sCtxParams);
			if (!psModel->psConfig->psContext) {
				fprintf(stderr , "%s: error: failed to create the llama_context\n" , __func__);
				exit(0);
			}

			psModel->uFormattedSize = llama_n_ctx(psModel->psConfig->psContext);
			psModel->szFormatted = realloc(psModel->szFormatted, psModel->uFormattedSize);
			for (uPos = 0; uPos < psModel->uFormattedSize; ++uPos) {
				psModel->szFormatted[uPos] = 0;
			}
		}

		psModel->nNewLength = 0;
		psModel->nPrevLen = 0;

		if (psModel->psSampler) {
			sampler_neuralize(psModel->psSampler);
		}
	}
}

void model_load(Model* psModel, char const* szFilename) {
	// Check the model exists
	if (access(szFilename, F_OK) == 0) {
		// Logging output
		llama_log_set(llama_log, NULL);

		// Load dynamic backends
		ggml_backend_load_all();

		if (psModel) {
			// Initialize the model
			struct llama_model_params sModelParams = llama_model_default_params();
			sModelParams.n_gpu_layers = 99;

			psModel->psModel = llama_model_load_from_file(szFilename, sModelParams);
			if (!psModel->psModel) {
				fprintf(stderr , "%s: error: unable to load model\n", __func__);
				exit(0);
			}
			config_set_model(psModel->psConfig, psModel->psModel, CONTEXT_WINDOW);

			psModel->uFormattedSize = llama_n_ctx(psModel->psConfig->psContext);
			psModel->szFormatted = malloc(psModel->uFormattedSize);
			if (psModel->uFormattedSize > 0) {
				psModel->szFormatted[0] = 0;
			}
		}
	}
	else {
		fprintf(stderr , "Could not read model: %s\n", szFilename);
		fprintf(stderr , "Skipping model loading.\n");
	}
}

void model_set_sampler(Model* psModel, char const* szGrammar) {
	if (psModel) {
		psModel->psSampler = sampler_new();
		sampler_set_grammar(psModel->psSampler, szGrammar, psModel->psConfig->psVocab);
		sampler_set_reasoning_budget(psModel->psSampler, psModel->psConfig, REASONING_BUDGET);
	}
}

// Helper function to evaluate a prompt and generate a response
void generate(Config* psConfig, char const* szPrompt, String* psResponse, Sampler* psSampler, Proof* psProofGenerated, Operation* psClaim, ModelOutcome* psOutcome) {
	char* szPiece = NULL;
	bool const boIsFirst = llama_memory_seq_pos_max(llama_get_memory(psConfig->psContext), 0) == -1;
	bool boProofValid;
	char* szError;
	Command* psCommand;
	String* psOutput;
	Operation* psResult;

	// Tokenize the prompt
	int const nPromptTokenCount = -llama_tokenize(psConfig->psVocab, szPrompt, strlen(szPrompt), NULL, 0, boIsFirst, true);
	llama_token* anPromptTokens = calloc(nPromptTokenCount, sizeof(llama_token));

	if (llama_tokenize(psConfig->psVocab, szPrompt, strlen(szPrompt), anPromptTokens, nPromptTokenCount, boIsFirst, true) < 0) {
		GGML_ABORT("failed to tokenize the prompt\n");
	}

	psCommand = command_new();
	psOutput = string_new();

	// Prepare a batch for the prompt
	llama_batch sBatch = llama_batch_get_one(anPromptTokens, nPromptTokenCount);
	llama_token nNewTokenId;
	while (psOutcome->eOutcome == OUTCOME_INVALID) {
		// Check if we have enough space in the context to evaluate this batch
		int nContext = llama_n_ctx(psConfig->psContext);
		int nContextUsed = llama_memory_seq_pos_max(llama_get_memory(psConfig->psContext), 0) + 1;
		if (nContextUsed + sBatch.n_tokens > nContext) {
			psOutcome->eOutcome = OUTCOME_ERROR_LOGIC;
			string_append(psOutcome->psError, "No proof generated within the budget.");
		}
		else {
			int ret = llama_decode(psConfig->psContext, sBatch);
			if (ret != 0) {
				GGML_ABORT("failed to decode, ret = %d\n", ret);
			}

			//Sample the next token
			nNewTokenId = sampler_sample(psSampler, psConfig->psContext, -1, psConfig->psVocab);

			sampler_accept(psConfig, psSampler, nNewTokenId);

			if (nNewTokenId == psConfig->uThinkingStartToken) {
				// Thinking
				sampler_set_apply_grammar(psSampler, false);
				printf(COL_RESET COL_YELLOW);
			}
			if (nNewTokenId == psConfig->uThinkingEndToken) {
				// Not thinking
				sampler_set_apply_grammar(psSampler, true);
				printf(COL_RESET);

				proof_print_prompt(psProofGenerated);
				sampler_clear_progress();
				proof_print_prompt(psProofGenerated);
			}

			// Convert the token to a string, print it and add it to the response
			char szBuffer[256];
			int nPieceLength = llama_token_to_piece(psConfig->psVocab, nNewTokenId, szBuffer, sizeof(szBuffer), 0, true);
			if (nPieceLength < 0) {
				GGML_ABORT("failed to convert token to piece\n");
			}
			szPiece = strndup(szBuffer, nPieceLength);
			if (psConfig->boMonologue) {
				if (llama_vocab_is_eog(psConfig->psVocab, nNewTokenId)) {
					printf("\n");
				}
				else {
					printf("%s", szPiece);
				}
			}

			if ((nNewTokenId != psConfig->uThinkingStartToken) && (nNewTokenId != psConfig->uThinkingEndToken)) {
				if (sampler_apply_grammar(psSampler)) {
					if ((szPiece[0] == '\n') || llama_vocab_is_eog(psConfig->psVocab, nNewTokenId)) {
						if (string_length(psOutput) > 0) {
							boProofValid = command_parse(psCommand, string_data(psOutput));
							if (boProofValid) {
								proof_process_step(psProofGenerated, NULL, psCommand);
								boProofValid = !proof_error(psProofGenerated, &szError);
								if (boProofValid) {
									if (psConfig->boMonologue) {
										proof_print_last_step(psProofGenerated);
									}
								}
								else {
									// ERROR: invalid proof step
									psOutcome->eOutcome = OUTCOME_ERROR_LOGIC;
									string_append(psOutcome->psError, szError);
									if (psConfig->boMonologue) {
										printf("Error: %s\n", szError);
									}
								}
							}
							else {
								// ERROR: command could not be parsed
								psOutcome->eOutcome = OUTCOME_ERROR_COMMAND;
							}
						}
						if (psConfig->boMonologue) {
							printf("\n");
						}
						else {
							string_clear(psOutput);
						}
						command_reset(psCommand);
					}
					else {
						string_append(psOutput, szPiece);
						char const* szLabel = strstr(string_data(psOutput), ": ");
						if (szLabel != NULL) {
							char* szPostLabel = strdup(szLabel + 2);
							string_clear(psOutput);
							string_append(psOutput, szPostLabel);
							free(szPostLabel);
						}
					}
				}
				else {
					if (!psConfig->boMonologue) {
						proof_print_prompt(psProofGenerated);
						sampler_output_progress(psSampler);
					}
				}
			}
			fflush(stdout);

			string_append(psResponse, szPiece);
			free(szPiece);
			szPiece = NULL;

			// Prepare the next batch with the sampled token
			sBatch = llama_batch_get_one(&nNewTokenId, 1);

			// Is it an end of generation?
			if (llama_vocab_is_eog(psConfig->psVocab, nNewTokenId)) {
				sampler_reset(psSampler);

				if (psOutcome->eOutcome == OUTCOME_INVALID) {
					if (psProofGenerated->uStepCount > 0) {
						// Check whether the result matches the claim
						psResult = psProofGenerated->apsStep[(psProofGenerated->uStepCount - 1)]->psResult;
						boProofValid = CompareOperations(psClaim, psResult);
						if (boProofValid) {
							psOutcome->eOutcome = OUTCOME_SUCCESS;
						} else {
							psOutcome->eOutcome = OUTCOME_ERROR_RESULT;
							psOutcome->psIncorrectResult = CopyRecursive(psResult);
						}
					}
					else {
						psOutcome->eOutcome = OUTCOME_ERROR_LOGIC;
						string_append(psOutcome->psError, "No proof generated.");
					}
				}
			}
		}
	}

	command_delete(psCommand);
	psCommand = NULL;
};

void model_add_user(Model* psModel, char const* szPrompt) {
	char const* szTemplate = llama_model_chat_template(psModel->psModel, /* name */ NULL);

	// Add the user input to the message list and format it
	vector_push_ChatMessage(psModel->psMessages, (ChatMessage){"user", strdup(szPrompt)});

	psModel->nNewLength = llama_chat_apply_template(szTemplate, vector_data_ChatMessage(psModel->psMessages), vector_size_ChatMessage(psModel->psMessages), true, psModel->szFormatted, psModel->uFormattedSize);
	if (psModel->nNewLength > (int)psModel->uFormattedSize) {
		psModel->uFormattedSize = psModel->nNewLength;
		psModel->szFormatted = realloc(psModel->szFormatted, psModel->nNewLength);
		psModel->nNewLength = llama_chat_apply_template(szTemplate, vector_data_ChatMessage(psModel->psMessages), vector_size_ChatMessage(psModel->psMessages), true, psModel->szFormatted, psModel->uFormattedSize);
	}
	if (psModel->nNewLength < 0) {
		fprintf(stderr, "failed to apply the chat template\n");
		exit(1);
	}
}

void model_add_assistant(Model* psModel, String* psResponse) {
	char const* szTemplate = llama_model_chat_template(psModel->psModel, /* name */ NULL);

	// Add the response to the messages
	vector_push_ChatMessage(psModel->psMessages, (ChatMessage){"assistant", strdup(string_data(psResponse))});

	psModel->nPrevLen = llama_chat_apply_template(szTemplate, vector_data_ChatMessage(psModel->psMessages), vector_size_ChatMessage(psModel->psMessages), false, NULL, 0);
	if (psModel->nPrevLen < 0) {
		fprintf(stderr, "failed to apply the chat template\n");
		exit(1);
	}
}

void model_get_prompt(Model* psModel, String* psPrompt) {
	if (psModel && psPrompt) {
		string_clear(psPrompt);
		// Remove previous messages to obtain the prompt to generate the response
		string_append_bytes(psPrompt, psModel->szFormatted + psModel->nPrevLen, (psModel->nNewLength - psModel->nPrevLen));
	}
}

Model* model_initialise() {
	Model* psModel;
	char const* szModelPath = "./models/" MODEL_FILE;
	char const* szGrammarPath = "inputs/fol-grammar-precise.txt";

	psModel = model_new();
	model_load(psModel, szModelPath);
	model_set_sampler(psModel, szGrammarPath);

	return psModel;
}

void model_success_complete(Proof* psProof, Proof* psProofGenerated) {
	size_t uStep;
	bool boOkay;
	Command *psCommand;
	size_t uCount;
	size_t uPos;
	Step* psStep;
	int nLength;
	char* szError;

	printf("\r");

	psCommand = command_new();
	boOkay = TRUE;
	for (uStep = 0; (uStep < psProofGenerated->uStepCount) && boOkay; ++uStep) {
		if (uStep < psProof->uStepCount) {
			boOkay = step_compare_equals(psProof->apsStep[uStep], psProofGenerated->apsStep[uStep]);

			if (!boOkay) {
				printf(COL_RESET);
				printf("Initial portion of generated proof doesn't match the existing proof.");
			}
		}
		else {
			command_reset(psCommand);
			psStep = psProofGenerated->apsStep[uStep];
			psCommand->eCommand = psStep->eCommand;
			psCommand->szCommand = strdup(aszCommand[psCommand->eCommand]);

			psCommand->uCount = psStep->uRefCount + psStep->uVarCount + psStep->uInputCount;
			psCommand->aszParameter = calloc(psCommand->uCount, sizeof(char*));
			uCount = 0;

			for (uPos = 0; uPos < psStep->uRefCount; ++uPos) {
				psCommand->aszParameter[uCount] = strdup(psStep->apsRef[uPos]->szName);
				uCount += 1;
			}

			for (uPos = 0; uPos < psStep->uVarCount; ++uPos) {
				psCommand->aszParameter[uCount] = strdup(psStep->aszVar[uPos]);
				uCount += 1;
			}

			for (uPos = 0; uPos < psStep->uInputCount; ++uPos) {
				nLength = OperationToStringLength(psStep->apsInput[uPos]) + 1;
				psCommand->aszParameter[uCount] = malloc(nLength);
				OperationToString(psStep->apsInput[uPos], psCommand->aszParameter[uCount], nLength);
				uCount += 1;
			}

			proof_print_prompt(psProof);
			command_print_generated(psCommand);

			proof_process_step(psProof, NULL, psCommand);

			boOkay = !proof_error(psProof, &szError);
			if (boOkay) {
				if (!proof_complete(psProof)) {
					proof_print_last_step(psProof);
					printf("\n");
				}
			}
			else {
				printf(COL_RESET);
				printf("Error: %s\n", szError);
			}
		}
	}

	command_delete(psCommand);
	psCommand = NULL;
}

bool model_apply(Model* psModel, Proof* psProof, Proof* psProofGenerated, char const* szPrompt, Operation* psClaim) {
	String* psPrompt;
	String* psResponse;
	ModelOutcome* psOutcome;
	bool boTemplateResult;
	bool boResult;

	psPrompt = string_new();
	psResponse = string_new();
	psOutcome = modeloutcome_new();

	while ((psOutcome->eOutcome != OUTCOME_SUCCESS) && (psOutcome->uErrorCount < MAX_ERRORS)) {
		model_add_user(psModel, szPrompt);
		model_get_prompt(psModel, psPrompt);
		modeloutcome_reset(psOutcome);
		proof_reset(psProofGenerated);

		// Generate a response
		generate(psModel->psConfig, string_data(psPrompt), psResponse, psModel->psSampler, psProofGenerated, psClaim, psOutcome);

		model_add_assistant(psModel, psResponse);

		boTemplateResult = TRUE;
		switch (psOutcome->eOutcome) {
			case OUTCOME_ERROR_RESULT: {
				string_clear(psPrompt);
				boTemplateResult = model_template_apply(psPrompt, "inputs/prompt-template-incorrect-result.txt", psProof, psClaim, psOutcome->psIncorrectResult, string_data(psOutcome->psError));
			}
			break;
			case OUTCOME_ERROR_LOGIC: {
				string_clear(psPrompt);
				boTemplateResult = model_template_apply(psPrompt, "inputs/prompt-template-incorrect-logic.txt", psProof, psClaim, psOutcome->psIncorrectResult, string_data(psOutcome->psError));
			}
			break;
			case OUTCOME_ERROR_COMMAND: {
				string_clear(psPrompt);
				boTemplateResult = model_template_apply(psPrompt, "inputs/prompt-template-incorrect-command.txt", psProof, psClaim, psOutcome->psIncorrectResult, string_data(psOutcome->psError));
			}
			break;
			case OUTCOME_SUCCESS:
				// Intentional fallthrough
			default: {
				// Do nothing
			}
			break;
		}

		if (psOutcome->eOutcome != OUTCOME_SUCCESS) {
			psOutcome->uErrorCount += 1;
			if ((psOutcome->uErrorCount % NEURALIZE_FREQUENCY) == 0) {
				model_neuralize(psModel);
				boTemplateResult = model_template_apply(psPrompt, "inputs/prompt-template-initial.txt", psProof, psClaim, NULL, NULL);
			}
		}

		assert(boTemplateResult);
	}

	switch (psOutcome->eOutcome) {
		case OUTCOME_SUCCESS: {
			model_success_complete(psProof, psProofGenerated);
			boResult = TRUE;
		}
		break;
		default: {
			boResult = FALSE;
		}
		break;
	}

	string_delete(psResponse);
	psResponse = NULL;
	string_delete(psPrompt);
	psPrompt = NULL;
	modeloutcome_delete(psOutcome);
	psOutcome = NULL;

	return boResult;
}

bool model_prove(Model* psModel, Proof *psProof, Operation* psClaim) {
	bool boResult;
	Proof* psProofGenerated;
	String* psResult;

	boResult = TRUE;

	psResult = string_new();
	boResult = model_template_apply(psResult, "inputs/prompt-template-initial.txt", psProof, psClaim, NULL, NULL);

	if (boResult) {
		psProofGenerated = proof_new();
		proof_attach_ruleset(psProofGenerated, psProof->psRuleset);
		boResult = model_apply(psModel, psProof, psProofGenerated, string_data(psResult), psClaim);

		printf(COL_RESET);
		printf("\n");

		proof_delete(psProofGenerated);
		psProofGenerated = NULL;
	}
	else {
		printf(COL_RESET);
		printf("Failed to apply prompt template.\n");
	}

	return boResult;
}

bool model_template_apply(String* psResult, char const* szTemplateFile, Proof const* psProof, Operation* psClaim, Operation* psIncorrectResult, char const* szError) {
	bool boResult;
	char* szTemplate;
	String* psReplace;
	String* psStep;
	size_t uCount;
	char* szOperation;
	int nLength;
	size_t uPos;
	size_t uPremiseCount;

	boResult = FALSE;
	if (psResult) {
		szTemplate = file_read(szTemplateFile);
		if (szTemplate) {
			string_append(psResult, szTemplate);
			free(szTemplate);
			szTemplate = NULL;
			psReplace = string_new();
			psStep = string_new();

			// Count the premises
			uPremiseCount = 0;
			if (psProof) {
				for (uPos = 0; uPos < psProof->uStepCount; ++uPos) {
					if (psProof->apsStep[uPos]->eCommand == STEP_PREMISE) {
						uPremiseCount += 1;
					}
				}

				// Construct the premises string
				switch (uPremiseCount) {
					case 0: {
						string_append(psReplace, "no premises");
					}
					break;
					case 1: {
						string_append(psReplace, "the premise ");
					}
					break;
					default: {
						string_append(psReplace, "the premises ");
					}
					break;
				}
			}
			string_replace(psResult, "<PREPREMISES>", string_data(psReplace));
			string_clear(psReplace);

			if (uPremiseCount > 0) {
				uPos = 0;
				uCount = 0;
				while (uCount < uPremiseCount) {
					assert(uPos < psProof->uStepCount);
					if (psProof->apsStep[uPos]->eCommand == STEP_PREMISE) {
						nLength = OperationToStringLengthLatex(psProof->apsStep[uPos]->apsInput[0]) + 1;
						szOperation = malloc(nLength * sizeof(char));
						OperationToStringLatex(psProof->apsStep[uPos]->apsInput[0], szOperation, nLength);
						string_append(psReplace, szOperation);
						free(szOperation);

						if (uPremiseCount > 1) {
							if (uCount < (uPremiseCount - 2)) {
								string_append(psReplace, ", ");
							}
							else {
								if (uCount == (uPremiseCount - 2)) {
									string_append(psReplace, " and ");
								}
							}
						}
						uCount += 1;
					}
					uPos += 1;
				}
			}
			string_replace(psResult, "<PREMISES>", string_data(psReplace));
			string_clear(psReplace);

			if (psClaim) {
				nLength = OperationToStringLengthLatex(psClaim) + 1;
				szOperation = malloc(nLength * sizeof(char));
				OperationToStringLatex(psClaim, szOperation, nLength);
				string_replace(psResult, "<CLAIM>", szOperation);
				free(szOperation);
			}
			else {
				string_replace(psResult, "<CLAIM>", "");
			}

			if ((psProof != NULL) && (psProof->uStepCount > 0)) {
				for (uPos = 0; uPos < psProof->uStepCount; ++uPos) {
					step_command_string_latex(psProof->apsStep[uPos], psProof->psRuleset, psStep);
					string_append_sprintf(psReplace, "%u: %s\n", (uPos + 1), string_data(psStep));
					string_clear(psStep);
				}
			}

			string_replace(psResult, "<PROOF>", string_data(psReplace));
			string_clear(psReplace);

			if (psIncorrectResult) {
				nLength = OperationToStringLengthLatex(psIncorrectResult) + 1;
				szOperation = malloc(nLength * sizeof(char));
				OperationToStringLatex(psIncorrectResult, szOperation, nLength);
				string_replace(psResult, "<INCORRECT>", szOperation);
				free(szOperation);
			}
			else {
				string_replace(psResult, "<INCORRECT>", "");
			}

			if (szError) {
				string_replace(psResult, "<ERROR>", szError);
			}
			else {
				string_replace(psResult, "<ERROR>", "");
			}

			boResult = TRUE;
		}
	}

	return boResult;
}

