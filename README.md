## Usage:
From `test-multi-dar/`:
```bash
 stack run -- -f ex16Daml -d 16 --useDamlName -s 2.9.0-rc1
```
This will scaffold a project named `ex16Daml`, with nesting 16 folders deep and use daml as the source folder name (the `--useDamlName` flag). To use `.` as the name, omit the flag. Each project uses sdk `2.9.0-rc1`.

Run `stack run -- --help` for command line option detail.

Inside the scaffolded project (`cd ../ex16Daml`), use:
```bash
daml build --enable-multi-package=yes --all
```

If you need to purge the `./daml`'s it's:
```bash
find . -type d -name .daml -prune -exec rm -rf {} \;
```
### Notes:
The haskell code will scaffold a daml project subdirectory of a depth specified inside the `ex16Daml/my-dep/` folder. In each `nested-n` folder a daml project is created which imports from the one "beneath" it. This dependency ends with the final project which just exports a single integer.

We shuffle the entries in the `multi-package.yaml` in a further effort to throw the builder. It is - I think - possible to break the build with `--ddepth = 128`, but in reality surely we never expect to see this.
