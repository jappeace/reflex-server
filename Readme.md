[![https://jappieklooster.nl](https://img.shields.io/badge/blog-jappieklooster.nl-lightgrey)](https://jappieklooster.nl/tag/haskell.html)
[![Githbu actions build status](https://img.shields.io/github/workflow/status/jappeace/haskell-reflex-server-project/Test)](https://github.com/jappeace/reflex-server/actions)
[![Jappiejappie](https://img.shields.io/badge/discord-jappiejappie-black?logo=discord)](https://discord.gg/Hp4agqy)

> I'm sorry dizzy

This attempts to make a reflex web server, out of....
we'll see what sticks.

# Usage

there is an example in `app/Main.hs`

this spins up a wai server, and sends the 
requests to a reflex event.
which may or may not produce a response with
`writeResponse`.

queues are used to exchange the requests/responses.
I doubt this will win speed competitions.
`RequestToken` is used to figure out which request
came from what thread.

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
