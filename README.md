# IMO

[![Build status](https://img.shields.io/github/workflow/status/milankinen/imo/Build/master?style=for-the-badge)](https://github.com/milankinen/imo/actions/workflows/build.yml)
[![Clojars](https://img.shields.io/clojars/v/imo?style=for-the-badge)](https://clojars.org/imo)

`WIP...`

## Development

Pre-requirements: 
  - `leiningen` (2.9.1 tested)
  - `jdk` (openjdk 11 tested)
  - `docker` (optional, for linux image building with osx)

#### Setup

```bash 
git clone https://github.com/milankinen/imo.git
cd imo
lein i
```

#### Running tests

```bash 
# Running all tests
lein t

# Optionally running only some tests
lein t <test-name-or-regexp>
```

#### Building native [GraalVM](https://www.graalvm.org) image

```bash
# Building native image for host os (OSX & Linux amd64 supported)
lein native-image

# Building Linux native image with Docker
lein linux-image
```

Built images are located in `target/native` directory.

## License

MIT
