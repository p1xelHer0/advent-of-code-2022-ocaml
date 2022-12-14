# ✨ Advent of Code 2022 ✨

> 1AB 2AB 3AB 4AB 5-- 6AB 7-- 8-- 9-- 10-- 11-- 12-- 13-- 14-- 15-- 16-- 17-- 18-- 19-- 20-- 21-- 22-- 23-- 24-- 25-- solved

## My solutions to [Advent of Code 2022](https://adventofcode.com/2022/) written in [OCaml](https://ocaml.org/)

### Setup

- Install [esy](https://esy.sh/) locally using `npm`

- Install [Node.js](https://nodejs.org/) which includes `npm`, I use [fnm](https://github.com/Schniz/fnmvv/)

```bash
npm i
```

> You can install `esy` globally using other means. If you do, remove the `npx` part of the commands below.

### Install dependencies using `esy`

This will install dependencies including the OCaml toolchain.

```bash
npx esy
```

### Run [inline tests](https://github.com/janestreet/ppx_inline_test/)

```bash
npx esy test
```

> or in watch mode

```bash
npx esy test:watch
```

### Running puzzles

Puzzles are numbered `day_{1..25}`

> For example, to run the puzzle of day 1

```bash
npx esy dune exec bin/day_1/puzzle.exe
Day 1A: <result>
Day 1B: <result>
```
