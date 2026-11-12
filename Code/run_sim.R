
# *********************************************************
## MASTER FILE... SET STUDY KNOBS AND RUN THE SIMULATION ##
# *********************************************************


# Create an environment for the sim
sim_cfg <- new.env()
cfg <- function(x) sim_cfg[[x]]


# source all of the functions
source("./internals/generate_items.R", echo = FALSE)
source("./internals/primary_functions.R", echo = FALSE)
source("./internals/EGA_helpers.R", echo = FALSE)



# set the tokens
sim_cfg$OPENAI_KEY <- "REMOVED"
sim_cfg$GROQ_KEY <- "REMOVED"






# Define experimental parameters ----
## Experimental Constants ----
sim_cfg$item_attributes <- list( # these are the same attributes used in the paper
  openness = c("creative","perceptual","curious","philosophical"),
  conscientiousness = c("organized","responsible","disciplined","prudent"),
  extraversion = c("friendly","positive","assertive","energetic"),
  agreeableness = c("cooperative","compassionate","trustworthy","humble"),
  neuroticism = c("anxious","depressed","insecure","emotional")
)

# JSON instruction to be added to every prompt
sim_cfg$json_format_str <- list(openness = paste0(
  "\n\nReturn output STRICTLY as a JSON array of objects, each with keys `attribute` and `statement`, e.g.:\n",
  "[{\"attribute\":\"creative\",\"statement\":\"Your item here.\"}, …]\n",
  "This JSON formatting is EXTREMELY important. Do NOT include any explanations, commentary, or markdown. Output only the JSON.",
  " The \"attribute\" key should ONLY have these EXACT values: creative, perceptual curious, and philosophical."),
  conscientiousness = paste0(
    "\n\nReturn output STRICTLY as a JSON array of objects, each with keys `attribute` and `statement`, e.g.:\n",
    "[{\"attribute\":\"organized\",\"statement\":\"Your item here.\"}, …]\n",
    "This JSON formatting is EXTREMELY important.Do NOT include any explanations, commentary, or markdown. Output only the JSON.",
    " The \"attribute\" key should ONLY have these EXACT values: organized, responsible, disciplined, and prudent."),
  extraversion = paste0(
    "\n\nReturn output STRICTLY as a JSON array of objects, each with keys `attribute` and `statement`, e.g.:\n",
    "[{\"attribute\":\"friendly\",\"statement\":\"Your item here.\"}, …]\n",
    "This JSON formatting is EXTREMELY important. Do NOT include any explanations, commentary, or markdown. Output only the JSON.",
    " The \"attribute\" key should ONLY have these EXACT values: friendly, positive, assertive, and energetic."),
  agreeableness = paste0(
    "\n\nReturn output STRICTLY as a JSON array of objects, each with keys `attribute` and `statement`, e.g.:\n",
    "[{\"attribute\":\"cooperative\",\"statement\":\"Your item here.\"}, …]\n",
    "This JSON formatting is EXTREMELY important. Do NOT include any explanations, commentary, or markdown. Output only the JSON.",
    " The \"attribute\" key should ONLY have these EXACT values: cooperative, compassionate, trustworthy, and humble."),
  neuroticism = paste0(
    "\n\nReturn output STRICTLY as a JSON array of objects, each with keys `attribute` and `statement`, e.g.:\n",
    "[{\"attribute\":\"anxious\",\"statement\":\"Your item here.\"}, …]\n",
    "This JSON formatting is EXTREMELY important. Do NOT include any explanations, commentary, or markdown. Output only the JSON.",
    " The \"attribute\" key should ONLY have these EXACT values: anxious, depressed, insecure, and emotional.")
)

sim_cfg$traits <- c("openness", "conscientiousness", "extraversion",
            "agreeableness", "neuroticism")
sim_cfg$persona.role <- list( openness =
  "You are an expert psychometrician and test developer specializing in personality assessment. Your task is to create high-quality, psychometrically robust items for a personality inventory measuring 'openness' from the 'Big Five' model of personality.",
  conscientiousness = "You are an expert psychometrician and test developer specializing in personality assessment. Your task is to create high-quality, psychometrically robust items for a personality inventory measuring 'conscientiousness' from the 'Big Five' model of personality.",
  extraversion = "You are an expert psychometrician and test developer specializing in personality assessment. Your task is to create high-quality, psychometrically robust items for a personality inventory measuring 'extraversion' from the 'Big Five' model of personality.",
  agreeableness = "You are an expert psychometrician and test developer specializing in personality assessment. Your task is to create high-quality, psychometrically robust items for a personality inventory measuring 'agreeableness' from the 'Big Five' model of personality.",
  neuroticism = "You are an expert psychometrician and test developer specializing in personality assessment. Your task is to create high-quality, psychometrically robust items for a personality inventory measuring 'neuroticism' from the 'Big Five' model of personality."
)

sim_cfg$target.N <- list("openness" = 60, "conscientiousness" = 60, "extraversion" = 60,
                 "agreeableness" = 60, "neuroticism" = 60)
sim_cfg$top.p <- 1
sim_cfg$embedding.model = "text-embedding-3-small"
sim_cfg$algorithm <- "walktrap"
sim_cfg$seed <- 123
sim_cfg$silently <- FALSE # Set to FALSE for progress statements
sim_cfg$min_per_attr <- 3


## Simulation knobs ----
designs <- c("basic", "expanded", "few_shot", "persona", "persona_few_shot",
             "persona_few_shot_adaptive")
temperature_set <- c(0.5, 1, 1.5)
LLM_models <- c("openai/gpt-4o", "openai/gpt-5.1","groq/openai/gpt-oss-120b", "groq/openai/gpt-oss-20b")
sim_cfg$model_set <- c("tmfg", "glasso")
n.reps <- 1:100



# Study grid mapping all of the study parameters
study.grid <- expand.grid(
  design = designs,
  temperature = temperature_set,
  llm_model = LLM_models,
  rep = n.reps,
  stringsAsFactors = FALSE
)


# Create Prompts ----
basic_pompt <- c("openness" = NULL,
                 "conscientiousness" = NULL,
                 "extraversion" = NULL,
                 "agreeableness" = NULL,
                 "neuroticism" = NULL)

expanded_prompt <- c("openness" = NULL,
                     "conscientiousness" = NULL,
                     "extraversion" = NULL,
                     "agreeableness" = NULL,
                     "neuroticism" = NULL)

few_shot_prompt <- c("openness" = NULL,
                     "conscientiousness" = NULL,
                     "extraversion" = NULL,
                     "agreeableness" = NULL,
                     "neuroticism" = NULL)


for (trait in cfg("traits")) {

  ## Basic Prompt ----
  # put the attributes in a list format
  attributes_list <- paste0(1:4, ") ", cfg("item_attributes")[[trait]],
                            collapse = "\n")

  # First create the basic prompt that only contains basic instructions for the model.
  prompt <- paste0("Generate novel items that assess the personality trait '", trait,
                   "' from the 'Big Five' personality model. ",
                   paste0(toupper(substr(trait, 1, 1)), substr(trait, 2, nchar(trait))),
                   " has the following attributes of interest:\n", attributes_list,"\n\n",
                   "For EACH attribute, write EXACTLY TWO (2) single-sentence,",
                   " first-person items each reflecting ONLY that given attribute.",
                   " Do NOT add any attributes or leave any out.")

  basic_pompt[[trait]] <- prompt



  ## Expanded Prompt ----
  # Next, add some detail about the expected quality of the items
  expanded <- paste0(prompt, "\n",
                   "Ensure that each item is extremely high-quality, ",
                   "psychometrically robust, and concise. Each item should be ",
                   "novel, so be creative; aim for breadth across these ",
                   "attributes. Items should be polished and ready for ",
                   "immediate practical use.")

  expanded_prompt[[trait]] <- expanded


  ## Few-shot ----
  item_examples <- c( # these are the same item examples used in the original paper
    "I am someone who is compassionate, has a soft heart.",
    "I am someone who starts arguments with others.",
    "I am someone who is dependable, steady.",
    "I am someone who has difficulty getting started on tasks.",
    "I am someone who has an assertive personality.",
    "I am someone who rarely feels excited or eager.",
    "I am someone who is moody, has up and down mood swings.",
    "I am someone who stays optimistic after experiencing a setback.",
    "I am someone who is inventive, finds clever ways to do things.",
    "I am someone who avoids intellectual, philosophical discussions."
  )

  # Combine all of the examples in a readable way
  examples_str <- paste(seq_along(item_examples), item_examples, sep = ". ")
  examples_str <- paste(examples_str, collapse = "\n")

  # Ensure the model understands that the examples are for quality/structure ONLY (not content)
  examples <- paste0("\n\nHere are some EXAMPLE items that you must emulate in terms",
    " of QUALITY and item STRUCTURE only, but do NOT reuse any of these examples' content.",
    " The content of the items you generate must be entirely unique.",
    " Here are the examples:\n", examples_str)

  few_shot <- paste0(expanded, examples)

  few_shot_prompt[[trait]] <- few_shot

}

# Compile all possible prompts into a single object
prompt_rolodex <- list(few_shot = few_shot_prompt,
                       basic = basic_pompt,
                       expanded = expanded_prompt)





# ****************************************
# Begin Sim ----
generate_items(study.grid, prompt_rolodex)
embed_item_pools(study.grid)
reduce_item_pool(study.grid)
# ****************************************

