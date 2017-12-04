# tabletop-client

## Install

The repo uses [stack][stack-install] to install dependencies.

Once you have stack installed you can clone the repository with git. If you need help installing git you can find instructions [here][github-install].

```bash
git clone https://github.com/taksuyu/tabletop-client.git
```

Next to build the repository and to setup a haskell environment; if you haven't already.

```bash
stack build --install-ghc
```

After building you'll use the build system to generate the `dist` folder that contains the files that would be used.

```bash
stack exec site
```

## Developing

### Rebuild a specific file

The shake build system will rebuild only when it sees a file has changed, but you can also rebuild individual files by specifying the output.

An example would be

```bash
stack exec -- site dist/css/main.css
```

This tells the shake build system to rule only the rule that builds `dist/css/main.css`.

## License

This project uses the BSD-3 clause license and you can find the file within the project as `LICENSE`.

[github-install]: https://help.github.com/articles/set-up-git/
[stack-install]: https://docs.haskellstack.org/en/stable/install_and_upgrade/
