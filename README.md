# Haskell Exercies 2022

Haskell programming exercises.

- Tic-tac-toe

## Getting started

```bash
cabal build

cabal run
```

## Project setup

- Cabal: v3.8
- GHC: 9.2.4

After trying the latest version of stack and cabal, I finally settled on the basic cabal setup.

Nonetheless, my vscode haskell language server setup still doesn't work.

To get quick type error feedback from compiler, I fallback to nodemon.

```bash
npx nodemon --exec "cabal clean && cabal build && cabal run" --watch app --ext hs
```
