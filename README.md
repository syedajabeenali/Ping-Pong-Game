# PingPong Game in 0x8088 Assembly Language

A simple two-player PingPong game implemented in **8088 Assembly Language**. Players control paddles to bounce a ball back and forth. The game ends when a player reaches a score of 5.

## Game Controls and features

1. **Start the Game**: The game starts with Player B’s paddle at the bottom and Player A’s paddle at the top.
2. **Control the Paddles**:
   - **Player A** uses the **Left** and **Right Arrow keys** to move the paddle horizontally on the first row.
   - **Player B** uses the **Left** and **Right Arrow keys** to move their paddle on the last row.
3. **Ball Movement**: The ball moves diagonally by one cell after each tick and bounces off walls and paddles.
4. **Scoring**: A player scores when the opponent misses the ball.
5. **End of Game**: The game ends when a player reaches 5 points.

## Setup and Running the Game

### Prerequisites

- **NASM** (Netwide Assembler) or **TASM** (Turbo Assembler)
- **DOSBox** or **EMU8086** (or any 8088-compatible emulator)

### Step-by-Step Instructions

1. **Assemble the Code**:
   - With **NASM**:
     ```bash
     nasm pingpong.asm -o pingpong.com
     ```

   - With **TASM**:
     ```bash
     tasm pingpong.asm
     tlink pingpong.obj
     ```

2. **Run the Game**:
   - Using **DOSBox**:
     ```bash
     afd pingpong.com
     ```
