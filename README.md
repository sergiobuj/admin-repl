# Github-repl

## Requirements
Github Access key with read/write permissions for organizations.

## Load
```common-lisp
(ql:quickload "github-repl")
```

## Start
```common-lisp
(ghrepl:start-repl "your-github-organization-name")
```

## Alt: Compile
TODO: [Buildapp](buildapp-url) from [SO question][so-question-url]

## Tests
Tests run with ASDF operations.

```common-lisp
(asdf:operate :test-op :github-repl-test)
```

[buildapp-url]: http://xach.com/lisp/buildapp/
[so-question-url]: http://stackoverflow.com/q/20583677/301431

