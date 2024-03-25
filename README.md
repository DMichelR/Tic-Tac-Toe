
# Tic Tac Toe Implementation Report:

Approach:

 - list manipulation functions to represent the game state, handle player moves, and determine the winner.

Design:

  - Data Representation: Represented the game board as a list of lists, with cells containing Maybe Player type.
  - Modularity: Encapsulated functionalities into separate functions for clarity and maintainability.
  - User Interface: Implemented a simple console interface for player interaction, displaying the game board after each move.

Challenges:

  - State Management: Managed game state through recursion and passing updated state variables.
  - Input Validation: Ensured valid player moves with proper bounds checking and cell occupancy verification.
  - String Formatting: Used string manipulation functions for proper board display with borders.
