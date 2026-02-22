# monadic-semantics-calculus

This project implements an interpreter and a type-and-effect checker based on the abstract framework described in [Monadic Type-And-Effect-Soundness](https://doi.org/10.4230/LIPIcs.ECOOP.2025.7).

## Prerequisites
- [Haskell Stack](https://docs.haskellstack.org/en/stable/README/)

## Build
```bash
stack build
```

## Usage
Run the interpreter on a source file:
```bash
stack exec monadic-semantic-calculus-exe -- <FILE>
```

If no file is provided, the interpreter reads from **stdin**. You must send an EOF signal (e.g., `Ctrl+D`) to end the input.

```bash
stack exec monadic-semantic-calculus-exe
# Type your program...
# Press Ctrl+D
```

### Options
- `--step`: Interactively reduce the expression step-by-step.
- `--print-type`: Print the inferred type and effect before execution.
- `--help`: Show all available options.

### Grammar
Language grammar specification accessible [here](grammar/Expr.g4).

### Examples

Run the Fibonacci example:
```bash
stack exec monadic-semantic-calculus-exe -- examples/fib.ms
```

Run with step-by-step reduction:
```bash
stack exec monadic-semantic-calculus-exe -- --step examples/nd.ms
```
