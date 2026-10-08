# nrepl-scm
Network REPL for Guile or Gauche.

It uses the same protocol as [Clojure nREPL](https://nrepl.org/nrepl/index.html).

## usage
```sh
./nrepl-scm guile
```
```sh
./nrepl-scm gauche
```

## install
```sh
chmod +x ./install.sh
PREFIX=$HOME/.local ./install.sh
```


## test
```sh
guile -L ./  test/nrepl/bencode.scm
```

## License
[MIT](https://github.com/niyarin/nrepl-scm/blob/main/LICENSE)
