# format-nix

A simple formatter for Nix using [tree-parser-nix](https://github.com/cstrahan/tree-sitter-nix).

Submit bugs by adding PRs for files that fail and adding the input to [tests](tests/Main.purs).

## Development setup

Get PureScript and Pulp.

```
# never use 'sudo' with npm.
> npm set prefix ~/.npm
> npm install
> npm run mkbin
> npm link

# update requires mkbin run again
> npm run mkbin
```

## Usage

```
> format-nix  test/build.nix
formatted test/build.nix.
```

See the example output from the tests here: [tests/output.nix](tests/output.nix)
