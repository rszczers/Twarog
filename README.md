# TWAROG

[![unstable](http://badges.github.io/stability-badges/dist/unstable.svg)](http://github.com/badges/stability-badges)[![Build Status](https://travis-ci.org/rszczers/Twarog.svg?branch=master)](https://travis-ci.org/rszczers/Twarog)

*Tiny* web-based manager/toolset for [MYFAROG](https://myfarog.org/) tabletop rpg. Project aims to help with:

- [ ] Getting unexperienced players through process of creating new character
- [ ] Generating random items
- [ ] Drawing random dungeons
- [ ] Generating random backstories for given characters

Project is under active development. Come back here in a few days :neckbeard:

## Getting started

To get the project running, you need [nix](https://nixos.org/nix/) package manager installed on your machine.

### Dev environment

```
nix-shell --run reload
```

App runs on `localhost:8080`.

### Build

```
nix-build -A release
```

### Preview
[Here](http://szczerski.pl/twarog) you can preview the current build.
