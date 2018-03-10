# QAP
Diferent solutions to the quadratic assignment problem for the metaheuristic practices

## Implemented algorithms
- **Greedy**
  - Flow and distance potentials
- **Local search**
  - Best-first with DLB mask
  - Best neighbor
  
## Usage
```bash
racket qap.rkt [--help] [--csv-output] [-m max_iterations] [-r repetitions_per_file] [--greedy | --local-search-bf | --local-search-bn] file1...
```
Example:
```bash
racket qap.rkt --csv-output -m 20000 -r 50 --local-search-bf data/*.dat
```

### Options
- `--help` Prints help
- `--csv-output` Prints the results in csv format
- `-m, --max-iterations` Maximum number of iterations for the local search, the default value is 50000
- `-r, --repetitions` Executions of the algorithm on each file, the results are the arithmetic mean of all executions. The default value is 1
- `--greedy` Executes the greedy algorithm
- `--local-search-bf` Executes the local search with best-first selection algorithm
- `--local-search-bn` Executes the local search with best neighbor selection algorithm
