# format-nix

[![Build Status](https://travis-ci.com/justinwoo/format-nix.svg?branch=master)](https://travis-ci.com/justinwoo/format-nix)

A simple formatter for Nix using [tree-sitter-nix](https://github.com/cstrahan/tree-sitter-nix).

Submit bugs by adding PRs for files that fail and adding the input to [tests](test/Main.purs).

## Installation

Please read this post to understand how to consume Node projects: <https://github.com/justinwoo/my-blog-posts/blob/master/posts/2019-05-14-consuming-node-projects-for-nix-users.md>

## Development setup

Get PureScript and Spago.

```
# never use 'sudo' with npm.
> npm set prefix ~/.npm
> npm install

# install purescript and spago in some way
# e.g. github.com/justinwoo/easy-purescript-nix
# or if you're feeling lucky, npm i -g purescript spago
> spago install

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

See the example output from the tests here: [test/output.nix](test/output.nix)

## Example use from Emacs

Try putting this in your init.el or wherever:

```elisp
(defun run-format-nix ()
  "run format-nix"
  (interactive)
  (let* ((command (format "cd %s && format-nix %s" (projectile-project-root) buffer-file-name))
         (results "*FORMAT-NIX STDOUT*")
         (errors "*FORMAT-NIX ERRORS*"))
    (shell-command command results errors)
    (if (get-buffer errors)
        (progn
          (with-current-buffer errors
            (message (string-trim (buffer-string))))
          (kill-buffer errors))
      (progn
        (with-current-buffer results
          (message (string-trim (buffer-string))))
        (kill-buffer results)
        ;; note this just reverts the buffer so save your files first.
        (revert-buffer t t t)))))

;; if you use evil mode, otherwise add your own bindings
(general-define-key
 :keymaps 'normal
 "SPC m p n" 'run-format-nix)
```

## PureScript Library

This project is now also a PureScript library, although a very small one. To use this, please match installations of your npm dependencies to the declared dependencies in `package.json`, e.g. <https://github.com/justinwoo/format-nix/blob/105a19245e72682d5b867b9ba31d6d26b9f9d054/package.json#L16-L19>:

```js
{
  // ...
  "dependencies": {
    "tree-sitter": "^0.15.1",
    "tree-sitter-nix": "github:cstrahan/tree-sitter-nix#791b5ff0e4f0da358cbb941788b78d436a2ca621"
  },
  // ...
}
```
