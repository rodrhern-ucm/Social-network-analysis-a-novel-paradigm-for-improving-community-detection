# Social Network Analysis: A Novel Paradigm for Improving Community Detection

Welcome to the GitHub repository accompanying the paper *"Social Network Analysis: A Novel Paradigm for Improving Community Detection"* by **Rodrigo Hernández**, **Inmaculada Gutiérrez**, and **Javier Castro**.

This repository contains the reproducible code, datasets, and results presented in the paper. Our research introduces an innovative methodology for enhancing community detection in networks by incorporating high-order relationships into algorithm inputs. By modifying adjacency matrices with aggregated higher-order information, our approach improves modularity and community cohesion across various algorithms.

---

## Repository Contents

The repository includes:

- **Reproducible Code**: Scripts to implement the described methodology.
- **Data and Examples**: Test cases and datasets for applying the approach to standard and real-world networks.
- **Results**: Benchmark analyses and a case study on tourist movements in Spain.

### Folder Structure

- **`Codes/`**: Scripts for running the methodology.
- **`Metrics/`**: Summaries and statistics for various steps in the methodology.
- **`Results/`**: Outputs for all combinations of evaluated networks. Here is stored individually the results which are input for Table 1 (among the rest of networks)
- **`ML Models/`**: Predictions and summary tables from machine learning models used in the paper.

---

## Installation

To replicate the analyses, use the following software versions:

- **R**: Version 3.4.2
- **Python**: Version 3.11.0

---

## Usage

### Key Scripts

1. **`network_calculation.R`**
   - Combines adjacency matrices with weighted higher-order walk matrices for community detection.
   - Applies multiple algorithms (Louvain, Leiden, Walktrap, Infomap, Fast Greedy).
   - Computes and stores modularity values for each network.

2. **`modularity_results.R`**
   - Processes networks to compute and compare modularity values across algorithms.
   - Reads GraphML files and outputs a consolidated CSV file for benchmark comparisons.

3. **`network_analysis.R`**
   - Evaluates the performance of community detection algorithms.
   - Generates improvement metrics and visualizations, including boxplots.
   - Outputs aggregated results in `Metrics/alpha_distribution.csv`.

---

## Results

### Scripts

1. **`bernoulli.R`**: Calculates confidence intervals for improvement metrics using Bernoulli distributions. Inputs for Tables 2 and 3 in the paper.
2. **`network_histogram.R`**: Generates histograms for improvement percentages by network and algorithm.
3. **`result_graphics_generator.R`**: Creates visualizations for improvement distributions across networks and algorithms.
4. **`ml_models_amplified.R`**:
   - Builds, tunes, and evaluates machine learning models to predict improvement likelihood in community detection.
   - Implements models including Logistic Regression, SVM, Random Forest, XGBoost, and Decision Trees.

### Output Files

- **`plot1_improvement_by_algorithm.pdf`**: Distribution of improvement percentages for each algorithm.
- **`plot2_improvement_by_network.pdf`**: Histograms of improvement percentages for analyzed networks.
- **`plot3_combined_improvement.pdf`**: Holistic visualization of improvements across networks and algorithms.

---

## Reproducibility

All experiments were conducted with a fixed random seed (**12345**) for consistent results.

---

## Contributing

Contributions are welcome! Please follow these guidelines:

1. Fork the repository and create a new branch for your feature or bug fix.
2. Submit a pull request with a clear description of your changes.

---

## References

Please cite the following sources if you use this repository or methodology:

- [Key references from the paper]
- Citation format for the paper:  
  Hernández, R., Gutiérrez, I., & Castro, J. (2025). *Social Network Analysis: A Novel Paradigm for Improving Community Detection*. DOI: [Insert DOI]

---

## Acknowledgements

We gratefully acknowledge the support of [insert funding sources] and the contributions of collaborators mentioned in the paper.
