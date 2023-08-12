# Gomoku

## Overview

This project is a Haskell implementation of the game Gomoku. Gomoku is a two-player game where the objective is to get five of your pieces in a row, either horizontally, vertically, or diagonally. The game is played on a square board, and players take turns placing their pieces on the board. The first player to get five of their pieces in a row wins the game.

## Description

A haskell implementation of the game Gomoku.

We have provided extensive `haddock` documentation for this project. 
To generate these, run:

```bash
cabal haddock --haddock-executables
```

### Installation and requirements
In order to build our project you will need to have cabal and ghc version 9.2.5 installed.
The reason for the strict version requirement is that certain packages we use are not 
compatible with older versions of ghc and will not build our project.

In order to install everything you need/make sure you have the right version of ghc we recommend using GHCup.
To install GHCup, run:

```bash
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
```
If following the default installation instructions, ghc version 9.2.5 will be installed.

Important note for marker: The lab machines currently run ghc version 8.10.7 so you must 
either manually update ghc or install GHCup with the command above which will update it for you.
We recommend using GHCup and running the command above as it is easy to do and will guarantee
you can build our project as we have tested this on the lab machines.


### Usage
To start the game, run:

```bash
cabal run
```
By default this boot you into a main menu. From here you can start a new game, load a previous game, set game options, or quit.

You can start the game with certain options using command line arguments. For example, to start a game with a 10x10 board, run:

```bash
cabal run gomoku -- size=10
```
Note that when adding arguments you need the executable name, `gomoku` after run.

The full list of arguements is:
size=int - set the board size to
target=int - set the target number of pieces in a row to
mode=gameMode - One of "Online", "Local", or "AI"
colour=colour - One of "Black" or "White"
host=string - The hostname of the server to connect to (only used in Online mode)

These can be combined in any order, for example:

```bash
cabal run gomoku -- size=10 mode=Online colour=Black host=localhost target=5
```
