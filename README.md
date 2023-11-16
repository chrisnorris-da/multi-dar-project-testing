## Usage:
From `test-multi-dar/` run:
```bash
 stack run -- --ddepth 16
```

then inside the scaffolded project (`cd ../example1`):
```bash
daml build --enable-multi-package=yes --all
```

If you need to purge the `./daml`'s it's:
```bash
find . -type d -name .daml -prune -exec rm -rf {} \;
```
###Notes:
The haskell code will scaffold a daml project subdirectory of a depth specified inside the `example1/my-dep/` folder. In each `nested-n` folder a daml project is created which imports from the one "beneath" it. This dependency ends with the final project which just exports a single integer.

We shuffle the entries in the `multi-package.yaml` in a further effort to throw the builder. It is - I think - possible to break the build with `--ddepth = 128`, but in reality surely we never expect to see this.