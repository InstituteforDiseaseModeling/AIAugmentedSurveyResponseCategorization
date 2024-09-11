# AI Augmented Survey Free Text Response Categorization 

Authors:  Joshua L. Proctor and Roy Burstein

## Context for the AI project:  

This project focuses on extracting insights from free text responses collected in large-scale surveys conducted in low and middle-income countries (LMICs). These free text responses provide qualitative data about human motivation and behavior but are often underutilized due to the complexity and time-intensive nature of analysis. In this particular case, we analyze free text response data collected over multiple years in the Democratic Republic of Congo (DRC) regarding reasons caregivers do not vaccinate their children.

Traditionally, processing and analyzing such data requires extensive human effort, making it difficult to extract meaningful insights in a timely manner. However, with advancements in AI tools, particularly Large Language Models (LLMs), we can now assist humans in understanding these responses more effectively. This project demonstrates how LLMs can be used to identify common themes, sort responses into topical areas, and categorize them based on pre-established classifications.

The repository contains a set of scripts designed to support this process, enabling more efficient analysis and helping bring valuable insights from this kind of data to light.

## Different AI infused workflows:

### Identifying Topics or Themes within the set of responses

In the first workflow, we implement a more traditional natural language processing (NLP) approach to topic modeling, enhanced by the recent advancements in large language models (LLMs), specifically GPT-4. We begin by creating semantic embeddings of the free text responses, capturing the contextual meaning and nuances in the data. These embeddings are then clustered using hierarchical clustering techniques, allowing us to group similar responses together based on their semantic similarity. To identify the optimal number of clusters, we utilize standard metrics for cluster validation.

Once the clusters are established, GPT-4 is employed to generate a topic or theme for each cluster, providing an overarching interpretation of the responses. To improve the thematic clarity and ensure that the generated topics are distinct from one another, we provide the model with additional information about the neighboring clusters. This contextual knowledge helps GPT-4 produce themes that are not only representative of their respective clusters but also distinguishable from other clusters.

The code for this workflow is contained in the folder TopicModelingUsingLLMs

### Categorizing the responses into a set of pre-established categories 

n the second workflow, we leverage large language models (LLMs) to categorize the free text responses into pre-established categories that are derived from other parts of the survey and expert-provided classifications. To handle responses that do not align with any of the predefined categories, we include an "Other" category to ensure comprehensive coverage. This workflow evaluates multiple LLM variants, including GPT-3.5, GPT-4, and GPT-4-turbo (GPT-4o), to determine the most effective model for categorization.

We experiment with different approaches such as zero-shot learning, few-shot learning, and fine-tuning to assess their impact on model performance. Zero-shot learning allows the models to classify responses without any specific examples, while few-shot learning provides a limited number of examples to guide the categorization. Fine-tuning, on the other hand, involves training the model on domain-specific data to enhance its accuracy for the given task. The output of the LLM-based workflow is then compared against a validation benchmark set to measure its performance and evaluate how well the models replicate or improve upon expert-driven categorization.

The code for this workflow is in the python notebook called AI_Workflows4SurveyDataCategorization.ipynb

### Dynamically identifying topics using an AI assistant

n the final workflow, we implement a dynamic and adaptive process for topic identification, designed to mimic the way a human evaluator might categorize responses in real time. We begin by utilizing a large language model (LLM) to classify each response into a set of predefined categories from the survey. If the LLM determines that a response does not fit within any of these existing categories, a secondary LLM agent is invoked to generate a new topic based on the content of the response.

Once a new topic is identified, it is dynamically incorporated into the list of available topics, enabling the model to expand its categorization framework as new themes emerge. This approach allows for real-time adaptation, with the LLM refining its topic set on the fly, similar to how a human evaluator might iteratively develop new categories while reviewing responses. By continuously updating the list of topics, the workflow ensures comprehensive and flexible categorization, balancing the need to adhere to predefined categories with the ability to discover new, previously unanticipated themes.
