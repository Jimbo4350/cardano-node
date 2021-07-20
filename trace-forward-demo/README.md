# Demo Programs

There are two demo programs, `demo-forwarder` and `demo-acceptor`. You can run these programs in different terminals and see an interaction between them.

Please see `forwarder.hs` module as an example of how to use `trace-forward` library in the node and `acceptor.hs` module as an example of how to use it in acceptor application (for example, tracer or RTView).

## How To Build It

As a result of `cabal build all` command, two demo programs will be built: `demo-forwarder` and `demo-acceptor`.

## How To Run It

### Connection Via Local Pipe

The example command to run `demo-acceptor` program:

```
./demo-acceptor /path/to/demo.sock
```

where `/path/to/demo.sock` is the path to the pipe file that will be created and used for connection with the forwarder.

The example command to run `demo-forwarder` program:

```
./demo-forwarder /path/to/demo.sock
```

where `/path/to/demo.sock` is the path to the pipe file that will be created (if needed) and used for connection with the acceptor.

### Connection Via Remote Socket

The example command to run `demo-acceptor` program:

```
./demo-acceptor 127.0.0.1 3010
```

where `127.0.0.1` and `3010` are the host and port; the acceptor will listen to them to accept the connection from the forwarder.

The example command to run `demo-forwarder` program:

```
./demo-forwarder 127.0.0.1 3010
```

where `127.0.0.1` and `3010` are the host and port; the forwarder will use them to establish the connection with the acceptor.

# Mux Demo Programs

There are two demo programs, `demo-forwarder-mux` and `demo-acceptor-mux`. You can run these programs in different terminals and see an interaction between them.

These demo-programs use two libraries: `trace-forward` and [`ekg-forward`](https://github.com/input-output-hk/ekg-forward). The purpose of these demo-programs is demonstration of `Mux`-ing of two `typed-protocol`s using one single connection.

You can use it as a practical example of how to integrate these two libraries in the forwarder application (for example, `cardano-node`) and in the acceptor application (for example, tracer or [RTView](https://github.com/input-output-hk/cardano-rt-view)).

Demo-programs can be launched in different modes.

## Simple Mode

Run demo-programs like this:

```
$ ./demo-acceptor-mux ./demo-mux.sock 1000
```

```
$ ./demo-forwarder-mux ./demo-mux.sock
```

or like this:

```
$ ./demo-acceptor-mux 127.0.0.1 3010 1000
```

```
$ ./demo-forwarder-mux 127.0.0.1 3010
```

In these examples, `demo-mux.sock` is a local pipe, and `127.0.0.1 3010` is a host and a port.

Next value for the acceptor is the number of requested `LogObject`s, in this example the acceptor will ask `1000` `LogObject`s.

## Benchmark Mode

Run demo-programs like this:

```
$ ./demo-acceptor-mux ./demo-mux.sock 1000 -b 2
```

```
$ ./demo-forwarder-mux ./demo-mux.sock 1 -b 0.000001
```

Flags before `-b` are the same as in the simple mode. Flag `-b` means `"benchmark"`.

The value after `-b` flag for the acceptor is a speed frequency: how often the acceptor will print the speed of accepting `LogObject`s. This speed is a number of `LogObject`s received in 1 second. In this example the acceptor will print this speed every `2` seconds.

The value after `-b` flag for the forwarder is a fill frequency: how often the forwarder will fill its local queue of `LogObject`s. In this example the forwarder will write the new `LogObject` in its local queue every `0.000001` seconds.

Since benchmark mode should work as fast as possible, it allows the connection via local pipe only (in this example it's `demo-mux.sock`).

## Benchmark Mode (Limited)

Run demo-programs like this:

```
$ ./demo-acceptor-mux ./demo-mux.sock 0.001 1000 -b 2 -t 1000000
```

```
$ ./demo-forwarder-mux ./demo-mux.sock 1 -b 0.000001
```

The new flag here is `-t` which means `"total"`. The value after `-t` is a total number of `LogObject`s that will be requested by acceptor. After the acceptor will receive such a number of `LogObject`s, the test will be stopped. In this example, `1000000` `LogObject`s will be requested.

## Disconnect Mode

Run demo-programs like this:

```
$ ./demo-acceptor-mux 127.0.0.1 3010 --dc 30
```

```
$ ./demo-forwarder-mux 127.0.0.1 3010 --dc 25
```

Flag `--dc` means `"DisConnect"`. The number is a disconnect frequency, in seconds: how often the program will break the connection. In this example, the acceptor will break it every 30 seconds, and the forwarder - every 25 seconds.

In disconnect mode, the acceptor asks for `LogObject`s as usually, but both the acceptor and the forwarder periodically break the connection and re-establish it again. The purpose of disconnect mode is to check if network resources clean up correctly. This is why disconnect mode allows the connection via `host` and `port` only.

### Disconnect Mode: Scripts

For simplicity, it is possible to run the forwarder and the acceptor using these scripts: `./scripts/runDisconnectForwarder.sh` and `./scripts/runDisconnectAcceptor.sh`. These scripts will launch demo-programs and will `watch` for active TCP-connecions using `lsof` command.
