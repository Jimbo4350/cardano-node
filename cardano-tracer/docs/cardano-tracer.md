# cardano-tracer

`cardano-tracer` is an application for logging and monitoring over `cardano-node`. After it is connected to the node, it periodically asks the node for different information, receives it, and handles it.

# Contents

1. [Introduction](#Introduction)
   1. [Motivation](#Motivation)
   3. [Overview](#Overview)
2. [Build and run](#Build-and-run)
3. [Configuration](#Configuration)
   1. [Accept endpoint](#Accept-endpoint)
   2. [Requests](#Requests)
   3. [Logging](#Logging)
   4. [Logs rotation](#Logs-rotation)
   5. [Prometheus](#Prometheus)
4. [Appendix](#Appendix)
   1. [SSH socket forwarding](#SSH-socket-forwarding)
   2. [Example 1](#Example-1)

# Introduction

## Motivation

Previously, `cardano-node` handled all the logging by itself. Moreover, it provided monitoring tools as well, for example, a web server for returning EKG metrics. `cardano-tracer` is an attempt to move all logging/monitoring-related stuff from the node to a separate application. As a result, the node will be smaller, faster, and simpler.

## Overview

You can think of `cardano-node` as a **producer** of logging/monitoring information, and `cardano-tracer` is a **consumer** of this information. So, from the network point of view, `cardano-node` is a server, and `cardano-tracer` is a client. After the connection between them is established, `cardano-tracer` periodically asks for this information, and `cardano-node` replies with it.

Currently, this information is presented in two items:

1. Log item, which contains some information from the node. Please see `Cardano.Logging.Types.TraceObject` from `trace-dispatcher` library for more info.
2. EKG metric, which contains some system metric. Please [read the documentation](https://hackage.haskell.org/package/ekg-core) for more info.

Please note that `cardano-tracer` can work as an aggregator as well: _one_ `cardano-tracer` process can receive the information from _multiple_ `cardano-node` processes. In this case, received logging information will be stored in subdirectories: each subdirectory will contain all the items received from the particular node.

# Build and run

Please make sure you have [Nix installed](https://nixos.org/guides/install-nix.html).

First of all, go to Nix shell using the following command (from the root of `cardano-node` repository):

```
nix-shell --arg withHoogle false
```

Now build `cardano-tracer` using the following command:

```
cabal build cardano-tracer
```

Then you can run `cardano-tracer` using the following command:

```
cabal exec -- cardano-tracer --config /path/to/your/config.json
```

Please see below an explanation about the configuration file.

# Configuration

The real-life example of the configuration file looks like this:

```
{
  "acceptAt": "/tmp/cardano-tracer.sock",
  "loRequestNum": 10,
  "ekgRequestFreq": 1,
  "hasEKG": null,
  "hasPrometheus": [
    "127.0.0.1",
    12798
  ],
  "logging": [
    {
      "logRoot": "/tmp/cardano-tracer-logs",
      "logMode": "FileMode",
      "logFormat": "ForMachine"
    }
  ],
  "rotation": {
    "rpKeepFilesNum": 1,
    "rpLogLimitBytes": 50000,
    "rpMaxAgeHours": 1
  }
}
```

Let's explore it in detail.

## Accept endpoint

The field `acceptAt` specifies an endpoint which using to connect with `cardano-node`:

```
"acceptAt": "/tmp/cardano-tracer.sock"
```

where `/tmp/cardano-tracer.sock` is a local path to Unix socket. Please note that `cardano-tracer` **does not** support connection via IP-address and port, to avoid unauthorized connections. So there are two possible cases:

1. `cardano-tracer` and `cardano-node` work on the **same** machine. In this case, they will use the same Unix socket directly.
2. `cardano-tracer` and `cardano-node` work on **different** machines. In this case, they will be connected using SSH socket forwarding (please see an explanation below). Also, this case corresponds to the situation when _one_ `cardano-tracer` is connected to _multiple_ `cardano-node`s.

## Requests

The field `loRequestNum` specifies the number of log items that will be requested from the node. In this example, `loRequestNum` is `10`, it means that `cardano-tracer` will periodically ask 10 log item in one request. It is useful to reduce the network traffic: it is possible to ask 50 log items in one request or ask them in 50 requests one at a time. Please note that if `loRequestNum` is bigger than the real number of log items in the node, all these items will be returned immediately. For example, if `cardano-tracer` asks 50 log items but the node has only 40 log items _in this moment of time_, these 40 items will be returned, there is no waiting for missing 10 items.

The field `ekgRequestFreq` specifies the period of how often EKG metrics will be requested, in seconds. In this example, `ekgRequestFreq` is `1`, which means that `cardano-tracer` will ask for new EKG metrics every second. Please note that there is no limit as `loRequestNum`, so every request returns _all_ the metrics the node has _in this moment of time_.

## Logging

Logging is one of the most important features of `cardano-tracer`. The field `logging` describes logging parameters:

```
"logging": [
  {
    "logRoot": "/tmp/cardano-tracer-logs",
    "logMode": "FileMode",
    "logFormat": "ForMachine"
  }
]
```

The field `logRoot` specifies the path to the root directory. This directory will contain all the subdirectories with the log files inside. Please remember that each subdirectory corresponds to the particular node. If the root directory does not exist, it will be created.

The field `logMode` specifies logging mode. There are two possible modes: `FileMode` and `JournalMode`. `FileMode` is for storing logs to the files, `JournalMode` is for storing them in `systemd`'s journal.

The field `logFormat` specifies the format of logs. There are two possible modes: `ForMachine` and `ForHuman`. `ForMachine` is for JSON format, `ForHuman` is for human-friendly text format.

Please note that `logging` field accepts the list, you can specify more than one logging section. For example, for both log formats:

```
"logging": [
  {
    "logRoot": "/tmp/cardano-tracer-logs-json",
    "logMode": "FileMode",
    "logFormat": "ForMachine"
  },
  {
    "logRoot": "/tmp/cardano-tracer-logs-text",
    "logMode": "FileMode",
    "logFormat": "ForHuman"
  }
]
```

## Logs rotation

An optional field `rotation` describes parameters for log rotation. Please note that if you skip this field:

```
"rotation": null
```

all log items will be stored in one file, usually, it is not what you want. These are rotation parameters:

```
"rotation": {
  "rpKeepFilesNum": 1,
  "rpLogLimitBytes": 50000,
  "rpMaxAgeHours": 1
}
```

The field `rpKeepFilesNum` specifies the number of the log files that will be kept. In this example, `rpKeepFilesNum` is `1`, which means that 1 _last_ log file will always be kept.

The field `rpLogLimitBytes` specifies the maximum size of the log file, in bytes. In this example, `rpLogLimitBytes` is `50000`, which means that once the size of the current log file is 50 KB, the new log file will be created.

The field `rpMaxAgeHours` specifies the lifetime of the log file, in hours. In this example, `rpMaxAgeHours` is `1`, which means that each log file will be kept for 1 hour only. After that, the log file is treated as outdated and will be deleted. Please note that N _last_ log files (specified by `rpKeepFilesNum`) will be kept even if they are outdated.

## Prometheus

The optional field `hasPrometheus` specifies the host and port where the page with EKG metrics will be available. In this example, the web page will be available at the local port `12798`:

```
"hasPrometheus": [
  "127.0.0.1",
  12798
]
```

Please note that if you skip this field:

```
"hasPrometheus": null
```

the web page will not be available.

After you open `http://127.0.0.1:12798` in your browser you will see:

1. the warning message if there are no connected nodes,
2. the list of identifiers of connected nodes.

Each identifier is a hyperlink to the page where you will see the list of EKG metrics received from the corresponding node.

# Appendix

## SSH socket forwarding

As was mentioned above, `cardano-tracer` supports the connection with `cardano-node` via the local socket only. And if `cardano-tracer` and `cardano-node` work on the **same** machine, they use the same Unix socket directly. It can be shown like this:

```
machine
+----------------------------------+
| cardano-node      cardano-tracer |
|             \    /               |
|              v  v                |
|         /path/to/socket          |
+----------------------------------+
```

But if they work on **different** machines, you need SSH socket forwarding. This mechanism can be shown like this:

```
machine A                                             machine B
+-------------------------+                           +-------------------------+
| cardano-node            |                           |          cardano-tracer |
|             \           |                           |         /               |
|              v          |                           |        v                |
|         /path/to/socket |<--SSH socket forwarding-->| /path/to/socket         |
+-------------------------+                           +-------------------------+
```

So, from the programs' point of view, they still work with the same `/path/to/socket` directly. But actually, there are two local sockets on two machines, and SSH socket forwarding mechanism connects these sockets. In other words, this mechanism allows treating `/path/to/socket` on the machine `A` and `/path/to/socket` on the machine `B` as the **same** local socket.

## Example 1

Suppose you have:

1. machine `A` with `cardano-node` installed,
2. machine `B` with `cardano-tracer` installed and configured as `Initiator`,
3. SSH-access from `B` to `A`.

The most convenient case is when your access from `B` to `A` is **key**-based, not **password**-based. Please [read the documentation](https://www.ssh.com/academy/ssh/key) for more details.

[OpenSSH](https://www.openssh.com/) supports socket forwarding out of the box. So, first of all, run the following command on `B`:

```
ssh -nNT -L PATH_TO_SOCKET_ON_MACHINE_B:PATH_TO_SOCKET_ON_MACHINE_A -o "ExitOnForwardFailure yes" USER_ON_MACHINE_A@IP_OF_MACHINE_A
```

where:

1. `PATH_TO_SOCKET_ON_MACHINE_B` is the local socket on `B`,
2. `PATH_TO_SOCKET_ON_MACHINE_A` is the local socket on `A`,
3. `USER_ON_MACHINE_A` is the name of your user on `A`,
4. `IP_OF_MACHINE_A` is an IP address (or hostname) of `A`.

Real example:

```
ssh -nNT -L /tmp/cardano-tracer.sock:/tmp/cardano-node.sock -o "ExitOnForwardFailure yes" john@109.75.33.121
```

This command connects the local socket `/tmp/cardano-tracer.sock` on your local machine with the local socket `/tmp/cardano-node.sock` on the remote machine `123.45.67.89`.

Now you can run the node and the tracer.

First, run `cardano-node` on `A`. This is because `cardano-tracer` is configured as `Initiator`: in this mode, `cardano-tracer` is treated as a client and `cardano-node` as a server, so the server should be launched **before** the client.

Finally, run `cardano-tracer` on `B`.

Please make sure that `TraceOptionForwarder` field in the node's configuration file and `acceptAt` field in the tracer's configuration file contain correct paths to the local sockets. In the previous example, both `TraceOptionForwarder` and `acceptAt` should contain `/tmp/cardano-tracer.sock` path.
