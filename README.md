# lam

little λ-calculus interpreter

features:
- lambda expressions `\x, x`
- function applications `(\x x) (\y y)`
- numbers, `0` = `\s \z z`, `2` = `\s \z`
- you should probably use rlwrap because i have not yet added haskeline to this
- arithmetic, e.g. `+ 1 2` = `3`, `* 5 4` = `20`, and `^ 5 3` = `125`
- `:q` to exit, just like your [favorite text editor](https://neovim.io)
