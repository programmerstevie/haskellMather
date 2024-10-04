
# Haskell Mathematical Expression Parser and Evaluator

This project is a Haskell implementation of a mathematical expression parser and evaluator. The parser takes an input string representing a mathematical expression, tokenizes it into meaningful tokens (numbers, operators, parentheses), parses it into an abstract syntax tree (AST), and evaluates the result. It supports basic arithmetic operations and can be extended with additional functionality as needed.

## Table of Contents
- [Features](#features)
- [Getting Started](#getting-started)
  - [Prerequisites](#prerequisites)
  - [Installing](#installing)
- [Usage](#usage)
- [Project Structure](#project-structure)
- [How It Works](#how-it-works)
  - [1. Lexical Analysis (Tokenization)](#1-lexical-analysis-tokenization)
  - [2. Parsing](#2-parsing)
  - [3. Evaluation](#3-evaluation)
- [Extending the Parser](#extending-the-parser)
- [Contributing](#contributing)
- [License](#license)

## Features
- Parses mathematical expressions into an Abstract Syntax Tree (AST).
- Supports basic arithmetic operations: addition (`+`), subtraction (`-`), multiplication (`*`), division (`/`), exponentiation (`^`), and modulus (`%`).
- Handles parentheses for grouping expressions.
- Supports both integer and floating-point numbers.
- Clean and modular design with a lexer, parser, and expression evaluation.
- Easily extendable to add more operations and functions.

## Getting Started

### Prerequisites
You will need [GHC (Glasgow Haskell Compiler)](https://www.haskell.org/ghc/) installed to compile and run this project.

### Installing
1. Clone the repository:
   ```bash
   git clone https://github.com/programmerstevie/haskell-parser.git
   cd haskell-parser
   ```

2. Compile the project:
   ```bash
   ghc -o parser Main.hs
   ```

## Usage
Once the project is compiled, run the executable to start evaluating mathematical expressions.

```bash
./parser
```

Enter a mathematical expression to evaluate:

```bash
(5 + 3) * 2 - 4 ^ 2
```

The output will show the result:

```bash
(5 + 3) * 2 - 4 ^ 2 = -4
```

## Project Structure
The project is divided into several modules:
- `Lexer.hs`: Defines how input strings are broken into tokens representing numbers, operators, and parentheses. It supports both integer and floating-point numbers.
- `Token.hs`: Defines the different types of tokens used in the parsing process, including constants (integers and doubles), binary operators, and parentheses.
- `Parser.hs`: Uses the `ParserLib` module to parse the tokens into an Abstract Syntax Tree (AST).
- `ParserLib.hs`: A custom parser combinator library used to compose the lexer and parser.
- `Expression.hs`: Contains the logic for parsing and evaluating expressions. It defines how to handle operator precedence, associativity, and the conversion from tokens to an `Expression`.
- `Grammar.hs`: Defines the structure of valid expressions, including binary operations and constants, as well as helper functions for converting between binary operators and operation symbols.
- `Evaluator.hs`: Handles the actual evaluation of the parsed AST into either an integer or a floating-point value, depending on the expression type.
- `Main.hs`: The entry point of the application, responsible for combining the lexer, parser, and evaluator into a functional program.

## How It Works

### 1. Lexical Analysis (Tokenization)
The lexer (`Lexer.hs`) breaks down the input string into tokens representing constants, operators, and parentheses. These tokens are represented by the `Token` data type defined in the `Token.hs` module. It supports both integer and floating-point constants.

For example, the expression `3 + (5.2 * 2)` is tokenized as:

```plaintext
ConstI_T(3), Binop_T(ADD), LParen_T, ConstD_T(5.2), Binop_T(MUL), ConstI_T(2), RParen_T
```

### 2. Parsing
The `Parser.hs` module uses a parser combinator library (`ParserLib.hs`) to parse the tokens into an Abstract Syntax Tree (AST). The AST represents the structure of the expression, with nodes for operations and constants. Binary operators and constants are represented by the `Expression` data type from the `Grammar.hs` module.

For the input `(5 + 3) * 2`, the AST might look like this:

```plaintext
       (*)
      /   \
    (+)    2
   /   \
  5     3
```

### 3. Evaluation
The `Evaluator.hs` module is responsible for evaluating the AST, producing either an integer or a floating-point value, depending on the type of expression.

- **Evaluation Functions**: 
  - `evalExpr`: The main function that takes an input string and returns the result as a custom `Val` type, which can be either `IntVal` for integers or `DubVal` for doubles.
  - `evalExprInt` and `evalExprDub`: Utility functions that return the evaluation result as an integer or double, respectively.

```haskell
evalExpr :: String -> Val
evalExpr s = evaluate $ parse s expression

evalExprInt :: String -> Integer
evalExprInt s = case evalExpr s of
  IntVal n -> n
  DubVal n -> floor n
```

- **Arithmetic Operations**: The `evaluate` function processes each node of the AST recursively, handling the following operations:
  - Addition (`+`)
  - Subtraction (`-`)
  - Multiplication (`*`)
  - Division (`/`)
  - Modulus (`%`)
  - Exponentiation (`^`)

For example, evaluating `5 + 3 * 2` follows these steps:
1. Multiply `3 * 2` to get `6`.
2. Add `5 + 6` to get `11`.

The `Evaluator.hs` module handles cases where the operations involve mixed integer and floating-point values, automatically promoting integers to floating-point numbers when necessary.

### Data Structures
The `Grammar.hs` module defines the core data structures that represent expressions and binary operations:

- **`Binop`**: This data type represents the various binary operations that the parser can handle, such as addition (`ADD`), subtraction (`SUB`), multiplication (`MUL`), division (`DIV`), exponentiation (`EXP`), and modulus (`MOD`).
  
  ```haskell
  data Binop = ADD | SUB | MUL | DIV | EXP | MOD
  ```

- **`Expression`**: This data type represents expressions in the AST, which can be binary operations, constant values (integers or doubles), or negated expressions.
  
  ```haskell
  data Expression = Binop_E Binop Expression Expression
                  | ConstD_E Double
                  | ConstI_E Integer
                  | Neg_E Expression
  ```

- **`Val`**: This custom type is used in the `Evaluator.hs` module to represent the result of an evaluation as either an `IntVal` for integers or `DubVal` for doubles.

  ```haskell
  data Val = IntVal Integer | DubVal Double
  ```

## Extending the Parser
To extend the parser with more operations or functions, you can modify the `Lexer.hs`, `Token.hs`, and `Parser.hs` files to recognize new tokens and define how they should be parsed. Additionally, you can modify the `Evaluator.hs` file to support new operations during the evaluation phase.

For example, to add support for sine (`sin`), you would:
1. Add a new token for `sin` in `Lexer.hs`.
2. Modify the grammar in `Grammar.hs` to include `sin` as a valid operation.
3. Update the evaluation logic in `Evaluator.hs` to handle the `sin` operation.

## Contributing
Contributions are welcome! If you have ideas for improvements or additional features, feel free to fork the project and submit a pull request.

1. Fork the repository.
2. Create your feature branch (`git checkout -b feature/AmazingFeature`).
3. Commit your changes (`git commit -m 'Add some AmazingFeature'`).
4. Push to the branch (`git push origin feature/AmazingFeature`).
5. Open a pull request.

## License
This project is licensed under the MIT License. See the [LICENSE](LICENSE) file for more information.
