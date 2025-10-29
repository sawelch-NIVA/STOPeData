# Introduction

# Workflow

## Modules

#### LLM Module

The LLM (Large Language Model) module takes advantage of the [Anthropic Claude Sonnet 4](https://www.anthropic.com/news/claude-4) LLM to extract scientifically relevant data from an uploaded PDF. When we click "Extract from PDF", the app connects to an Application Programming Interface (API), authenticates itself using an API Key, and then sends the PDF, prompt, and extraction schema to the Claude Server.

There, the LLM generates relevant data based on its training dataset (a massive body of written-language literature), the PDF, and the instructions we've given. This process is generally able to approximate the act of "reading" a PDF well enough for our purposes. However, it should be emphasised that an LLM is not an intelligent entity. Consequently, human, domain-expertise-based validation of results is still vital.

Once the Claude API has validated our request, processed our sent documents, and returned a valid reply, we convert it into an R-compatible format and store it in the `moduleState$structuredData` reactive. We also flag that the extraction is complete `moduleState$extraction_complete -> TRUE` and successful `moduleState$extraction_successful -> TRUE`. 

This in turn enables the Populate Forms button. When we click this button we:
1. Check if each of the campaign, references, sites, parameters, compartments, biota, methods, and samples extracted data objects exist.
2. If they do, in each case we:
   1. Clean and standardise the data using the `create_*_from_llm()` family of functions.
   2. Store the resulting data in `session$userData$reactiveValues$*DataLLM`
3. Set flags for successful extraction at the app level:
   1. `session$userData$reactiveValues$llmExtractionComplete <- TRUE`
   2. `session$userData$reactiveValues$llmExtractionSuccessful <- TRUE`

In turn, each relevant module contains an observer that watches both `session$userData$reactiveValues$llmExtractionComplete` and `session$userData$reactiveValues$*dataLLM`. When either of these variables is invalidated, it triggers the observer to:
1. Check the incoming `*dataLLM` object to see that it's `!is.null` and contains `>0` rows.
2. Write the this incoming data to the module at moduleState$*_data

Writing data to modue-specific data storage doesn't directly populate input fields or tables. In the case of the former, data is stored in a separate, hard-coded `input$inputName` object that must be individually updated using a function like `updateTextInput(session, id, value)`, wrapped in an `observe()`.

Tables are updated by a separate observer...

### Campaign

etc
