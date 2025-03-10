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
- **`Results/`**: Outputs for all combinations of evaluated networks. Here is stored individually the results which are input for Table 1 (among the rest of networks).
- **`Case study/`**: Input files and scripts for constructing and analyzing the real-world network case study.
  - **`clean_graph_data.csv`**: Pre-processed data used to define the network structure.
  - **`real case graph creation.ipynb`**: Jupyter notebook that constructs the network from the clean dataset.
  - **`calculo_redes_real_case.R`**: R script for applying community detection and computing modularity.
  - **`monthly partitions.xlsx`**: Output file containing monthly partitioning results for community detection.
- **`scripts/`**: Additional scripts for data processing, visualizations, and machine learning analysis.

---

## Installation

To replicate the analyses, use the following software versions:

- **R**: Version 3.4.2
- **Python**: Version 3.11.0

Install required R packages:

```r
install.packages(c("igraph", "rvest", "readr", "hydra", "dplyr", "ggplot2", "tidyr", "randomForest", "xgboost", "Metrics", "caret", "doParallel", "arrow", "visNetwork", "combinat", "igraphdata"))
```

For Jupyter Notebooks:

```bash
pip install jupyter pandas networkx matplotlib
```

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
  
4. **`Functions.R`**
   - Supporting functions used in the code for aggregation.
   - Additional functions for other approaches not described in this paper, including resolution parameter iteration and alternative aggregation methods.

5. **`analisis_redes.R`**
   - Reads modularity results from multiple CSV files and computes performance metrics for different algorithms.

6. **`bernoulli_v2.R`**
   - Computes confidence intervals for algorithm performance based on Bernoulli distributions.

7. **`calculo_redes_nuevo.R`**
   - Iterates over multiple networks, precomputes adjacency matrices, and applies different clustering algorithms.

8. **`calculo_redes_real_case.R`**
   - Processes a real-world network dataset, applying modularity-based community detection methods.

9. **`Nodes and Edges.R`**
   - Computes the number of nodes and edges in given `.graphml` network files.

10. **`plots heatmap.R`**
    - Generates heatmaps and computational time comparison plots for different algorithms.

11. **`synthetic network.R`**
    - Generates synthetic hierarchical networks and saves them as `.graphml` files.

12. **`modelo ampliado (variable importance).R`**
    - Trains multiple machine learning models (Logistic Regression, SVM, Random Forest, XGBoost) to predict network modularity improvement and extracts variable importance.

13. **`syntethic networks.ipynb`**
    - Notebook related to synthetic network generation and analysis.

---

## Results

### Output Files

- **`plot1_improvement_by_algorithm.pdf`**: Distribution of improvement percentages for each algorithm.
- **`plot2_improvement_by_network.pdf`**: Histograms of improvement percentages for analyzed networks.
- **`plot3_combined_improvement.pdf`**: Holistic visualization of improvements across networks and algorithms.
- **`fast_greedy_heatmap.pdf`**: Heatmap showing improvement distribution for the Fast Greedy algorithm.
- **`walktrap_heatmap.pdf`**: Heatmap showing improvement distribution for the Walktrap algorithm.
- **`leiden_heatmap.pdf`**: Heatmap showing improvement distribution for the Leiden algorithm.
- **`louvain_heatmap.pdf`**: Heatmap showing improvement distribution for the Louvain algorithm.
- **`infomap_heatmap.pdf`**: Heatmap showing improvement distribution for the Infomap algorithm.
- **`all_algorithms_heatmap.pdf`**: Aggregated heatmap comparing all algorithms.
- **`time_plot.pdf`**: Visualization of computational time increase across different algorithms.
- **`algorithm_improvement_distribution.pdf`**: Boxplot of improvement percentages across all tested algorithms.
- **`monthly partitions.xlsx`**: Community detection results for different time periods.
- **`alpha_distribution.pdf`**: Table describing which combination of alpha parameters have higher probabilities of improvement.
- **`networks_used.csv`**: List of networks used in this paper. All were downloaded from [http://konect.cc/networks/](http://konect.cc/networks/)

---

## Reproducibility

All experiments were conducted with a fixed random seed (**12345**) for consistent results.

---

## Contributing

Contributions are welcome! Please follow these guidelines:

1. Fork the repository and create a new branch for your feature or bug fix.
2. Submit a pull request with a clear description of your changes.

---

## Acknowledgements

This research has been partially supported by the Government of Spain, Grant Plan Nacional de I+D+i, PID2021-122905NB-C21.
