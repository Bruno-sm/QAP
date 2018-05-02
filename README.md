# QAP
Diferent solutions to the quadratic assignment problem for my metaheuristic practices

## Implemented algorithms
- **Greedy**
  - Flow and distance potentials
- **Local search**
  - Best-first with DLB mask
  - Best neighbour
  - Variable neighbourhood descent
  - Short term memory tabu search
- **Genetic algorithms**
  - Generational with positional crossover
  - Generational with PMX crossover
  - Steady-State with positional crossover
  - Steady-State with PMX crossover
- **Memetic algorithms**
  - Generational with positional crossover and local search over all the population every 10 generations
  - Generational with PMX crossover and local search over all the population every 10 generations
  - Generational with positional crossover and local search over a random 10% of the population every 10 generations
  - Generational with PMX crossover and local search over a random 10% of the population every 10 generations
  - Generational with positional crossover and local search over the best 10% of the population every 10 generations
  - Generational with PMX crossover and local search over the best 10% of the population every 10 generations

  
## Usage
```bash
racket qap.rkt [--help] [--csv-output | --debug-output] [-s seed] [-m max_evaluations] [-r repetitions_per_file] [--greedy | --local-search-bf | --local-search-bn | --local-search-vnd | --stm-tabu-search | --genetic-gen-pos | --genetic-gen-pmx | --genetic-st-pos | --genetic-st-pmx | --memetic-all-pos | --memetic-all-pmx | --memetic-rand-pos | --memetic-rand-pmx | --memetic-best-pos | --memetic-best-pmx] file1...
```
Example:
```bash
racket qap.rkt --csv-output -s 100 -m 20000 -r 50 --local-search-bf data/*.dat
```

### Options
- `--help` Prints help
- `--csv-output` Prints the results in csv format
- `--debug-output` Prints a trace of the algorithm 
- `-s, --seed` Seed for the random numbers generator 
- `-m, --max-evaluations` Maximum number of solution evaluations before stop the search, the default value is 50000
- `-r, --repetitions` Executions of the algorithm on each file, the results are the arithmetic mean of all executions. The default value is 1
- `--greedy` Executes the greedy algorithm
- `--local-search-bf` Executes the local search with best-first selection algorithm
- `--local-search-bn` Executes the local search with best neighbour selection algorithm
- `--local-search-vnd` Executes the local search with variable neighbourhood descent 
- `--stm-tabu-search` Executes the short term memory tabu search 
- `--genetic-gen-pos` Executes a generational genetic with positional crossover 
- `--genetic-gen-pmx` Executes a generational genetic with PMX crossover 
- `--genetic-st-pos` Executes a steady-state genetic with positional crossover 
- `--genetic-st-pmx` Executes a steady-state genetic with PMX crossover 
- `--memetic-all-pos` Executes a memetic with positional crossover and local search over all the population
- `--memetic-all-pmx` Executes a memetic with PMX crossover and local search over all the population
- `--memetic-rand-pos` Executes a memetic with positional crossover and local search over a random 10% of the population
- `--memetic-rand-pmx` Executes a memetic with PMX crossover and local search over a random 10% of the population
- `--memetic-best-pos` Executes a memetic with positional crossover and local search over the best 10% of the population
- `--memetic-best-pmx` Executes a memetic with PMX crossover and local search over the best 10% of the population
