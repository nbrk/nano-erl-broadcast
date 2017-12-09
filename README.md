# nano-erl-broadcast

A quick and dirty hack to Erlang-style Actor Model library [`nano-erl`](https://hackage.haskell.org/package/nano-erl) 
which introduces dedicated **groups of actors** with **broadcast messaging** inside the groups.

This is useful if you want to create a system of actors that consists of several ``subsystems'' and to
broadcast messages to all the actors belonging to that group.

For that purpose the library introduces the following types:

``` haskell
type GroupRef message = IORef [Pid message]
type GroupProcess message = GroupRef message -> Process message
```

You acquire `GroupRef a` when you spawn some `GroupProcess a` actors via `spawnGroup`:

``` haskell
spawnGroup :: [GroupProcess message] -> IO (GroupRef message)
```

These actors, in turn, each get the same `GroupRef a` handle (as well as the `Pid a`) that can be used inside 
the actor's thread to send broadcasts to all the processes of the group. The two send functions are:

``` haskell
(!*) :: GroupRef message -> message -> IO ()

broadcastExcept :: GroupRef message -> [Pid message] -> message -> IO ()
```

Where `!*` sends a message to all actors of the group including myself (the sending actor).
The `broadcastExcept` broadcasts, well, to everyone in the group except some `Pid`s.

## Examples

See the `Control.Concurrent.NanoErl.Examples.BroadcastPingPong` for a boring example.
