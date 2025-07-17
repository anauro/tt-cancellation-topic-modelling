# Cancellation Reasons Topic Modelling 
_A user insights investigation conducted at Thumbtack_

## 🔍 Overview
Instant bookings — where customers and pros agree in advance on a time and place to get the job done — were experiencing unexpectedly high cancellation rates. When cancelling, users selected from a list of predefined reasons (e.g., “Time no longer works,” “Pro too expensive”) or provided their own explanation via a free-form text box.

This analysis explored a key question:  
**Are we missing important user pain points not captured by the predefined options?**

## 📊 Analysis Details
We applied **Latent Dirichlet Allocation (LDA)**, an unsupervised topic modelling technique, to categorize the free-form responses into themes. This enabled us to uncover additional drivers of cancellation and assess their overlap with existing survey options.

## 🧠 Methods & Tools
- **Languages**: R, SQL
- **Techniques**: Latent Dirilecht Analysis, unsupervised ML   
- **Workflow**: Data querying → text preprocessing (e.g., tokenization, stopword removal) → model tuning and interpretation  

## 🎯 Outcomes
- Identified four dominant topics in free-text responses:
  - Pro no longer needed  
  - Rescheduling  
  - Pro changed job details 
  - Found another pro  
- Mapped the discovered topics to existing cancellation reasons, revealing key gaps and overlaps  
- Visualized the breakdown of topics (e.g., pie chart) to inform product prioritization  
- Insights enabled **feature ideation** to focus on most impactful pain points 

## 📂 Contents
- `scripts/`: Cleaning, modeling, and visualization scripts
- `slides/`: Final deck summarizing findings and recommendations (PDF)
- `README.md`: Overview of the project
