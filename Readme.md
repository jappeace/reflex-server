[![https://jappieklooster.nl](https://img.shields.io/badge/blog-jappieklooster.nl-lightgrey)](https://jappieklooster.nl/tag/haskell.html)
[![Jappiejappie](https://img.shields.io/badge/twitch.tv-jappiejappie-purple?logo=twitch)](https://www.twitch.tv/jappiejappie)
[![Jappiejappie](https://img.shields.io/badge/youtube-jappieklooster-red?logo=youtube)](https://www.youtube.com/channel/UCQxmXSQEYyCeBC6urMWRPVw)
[![Githbu actions build status](https://img.shields.io/github/workflow/status/jappeace/haskell-reflex-server-project/Test)](https://github.com/jappeace/haskell-reflex-server-project/actions)
[![Jappiejappie](https://img.shields.io/badge/discord-jappiejappie-black?logo=discord)](https://discord.gg/Hp4agqy)
[![Hackage version](https://img.shields.io/hackage/v/reflex-server.svg?label=Hackage)](https://hackage.haskell.org/package/reflex-server) 

> I'm sorry dizzy

This attempts to make a reflex web server, out of....
we'll see what sticks.

Haskell project reflex-server.

Set up cabal within a nix shell.
If you like nix this is a good way of doing haskell development.

similar to: https://github.com/monadfix/nix-cabal
except this has a makefile and ghcid.
We also make aggressive use of [pinning](https://nixos.wiki/wiki/FAQ/Pinning_Nixpkgs)
ensuring project builds for ever (theoretically).

Comes with:
+ [GHCID](https://jappieklooster.nl/ghcid-for-multi-package-projects.html)
+ a nix shell, meaning somewhat platform independence.
  + which is pinned by default
+ A couple of handy make commands.
+ Starting haskell files, assuming we put practically all code in library
+ Working HSpec, The detection macro will pickup any file ending with Spec.hs

## Usage

### Modifying for your project
Assuming the name of your new project is `new-project`.

```
git clone git@github.com:jappeace/haskell-reflex-server-project.git new-project
cd new-project
```

+ [ ] Edit package.yaml,
    + [ ] find and replace reflex-server with `new-project`
    + [ ] Update copyright
    + [ ] Update github
+ [ ] Run `make hpack` to update cabal files
+ [ ] remove reflex-server.cabal
+ [ ] Edit Changelog.md
  + [ ] replace reflex-server with `new-project`
  + [ ] Also describe your version 1.0.0 release.
+ [ ] Edit default.nix, replace reflex-server with `new-project`.
+ [ ] Edit copyright in LICENSE
+ [ ] Edit `nix/bundle.nix` to point to the executable
+ [ ] Edit `nix/ci.nix` and `nix/pkgs.nix` for name of package
+ [ ] Edit `shell.nix`

#### Reconfigure remotes
```
git remote add reflex-server git@github.com:jappeace/haskell-reflex-server-project.git
git remote set-url origin git@github.com:YOUR-ORG-OR-USER-NAME/new-project.git
```

We can get reflex-server updates like this if we want to by doing `git pull reflex-server`.
There will be a large amount of conflicts, but the merge commit should solve them permanently.

#### Readme

+ [ ] Select desired badges. 
  + [ ] Point build badges to right project
+ [ ] Give short project description.
+ [ ] Add new quote suited for the project.
  For example for [fakedata-quickcheck](https://github.com/fakedata-haskell/fakedata-quickcheck#readme)
  I used Kant because
  he dealt with the question "what is truth" a lot.
+ [ ] Truncate this checklist
+ [ ] Truncate motivation for using  this reflex-server

### Tools
Enter the nix shell.
```
nix-shell
```
You can checkout the makefile to see what's available:
```
cat makefile
```

### Running
```
make run
```

### Fast filewatch which runs tests
```
make ghcid
```
