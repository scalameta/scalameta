# How to publish on npm

From the scalameta repo root run:

```sh
sbt "parsers/fullOptJS"
```

Then from the current directory:

```sh
cp ../target/scala-2.11/parsers-opt.js index.js
```

Check that `package.json` contains the appropriate version (e.g. 1.8.0)

Finally publish on npm (it requires you to have write access, login with `npm login`):

```sh
npm publish
```
