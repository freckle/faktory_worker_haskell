# faktory\_worker\_haskell

[![CircleCI](https://circleci.com/gh/freckle/faktory_worker_haskell.svg?style=svg)](https://circleci.com/gh/freckle/faktory_worker_haskell)

Haskell client and worker process for the Faktory background job server.

Architecture overview from [Ruby client README](https://github.com/contribsys/faktory_worker_ruby#readme):

```
                       +--------------------+
                       |                    |
                       |     Faktory        |
                       |     Server         |
        +---------->>>>|                    +>>>>--------+
        |              |                    |            |
        |              |                    |            |
        |              +--------------------+            |
+-----------------+                            +-------------------+
|                 |                            |                   |
|    Client       |                            |     Worker        |
|    pushes       |                            |     pulls         |
|     jobs        |                            |      jobs         |
|                 |                            |                   |
|                 |                            |                   |
+-----------------+                            +-------------------+
```

- Client - an API any process can use to push jobs to the Faktory server.
- Worker - a process that pulls jobs from Faktory and executes them.
- Server - the Faktory daemon which stores background jobs in queues to be
  processed by Workers.

This package contains only the client and worker parts. The server part is
[here](https://github.com/contribsys/faktory/)

## Installation

- Hackage: http://hackage.haskell.org/package/faktory
- Stackage: *Coming soon*

## Faktory Documentation

See the [wiki](//github.com/contribsys/faktory/wiki) for more
details.

## Usage

<!--
```haskell
import Data.Aeson
import Prelude
import Faktory.Producer
import Faktory.Job
import Faktory.Worker
import GHC.Generics

{- Don't actually run anything -}
main :: IO ()
main = if True then pure () else (workerMain >> producerMain)
workerMain :: IO ()
producerMain :: IO ()
```
-->

### Job

Any value can be a "Job" that is pushed and pulled to and from Faktory via its
`ToJSON` and `FromJSON` instances:

```haskell
newtype MyJob = MyJob
  { myJobMessage :: String
  }
  deriving stock Generic
  deriving anyclass (ToJSON, FromJSON)
```

### Worker

```haskell
workerMain = runWorkerEnv $ \job ->
  -- Process your Job here
  putStrLn $ myJobMessage job

  -- If any exception is thrown, the job will be marked as Failed in Faktory
  -- and retried. Note: you will not otherwise hear about any such exceptions,
  -- unless you catch-and-rethrow them yourself.
```

### Producer

`Producer` wraps `Client` for push-only usage.

```haskell
producerMain = do
  producer <- newProducerEnv

  jobId <- perform mempty producer $ MyJob "Hello world"

  print jobId

  closeProducer producer
```

### Configuration

When using `envSettings`, the following variables will be used:

- `FAKTORY_PROVIDER`: the name of another environment variable where the
  connection string can be found. Defaults to `FAKTORY_URL`.
- `FAKTORY_URL` (or whatever you named in `FAKTORY_PROVIDER`): connection string
  to the Faktory server. Format is
  `tcp(+tls)://(:password@)host:port(/namespace)`. Defaults to
  `tcp://localhost:4719`. `namespace` is prependend to queue names on job
  submission and worker consumption.

When using `envWorkerSettings`, the following variables are also used:

- `FAKTORY_QUEUE`: the name of the queue to consume from. Default is "default".
- `FAKTORY_WORKER_ID`: the Id to use for this Worker. Default is to assign a
  random one.

## Examples

See the [examples](./examples). To run them:

1. Run a local Faktory server

   ```console
   docker run --rm \
     --publish 7419:7419 \
     --publish 7420:7420 \
     contribsys/faktory
   ```

1. Run the consumer example

   ```console
   % stack exec faktory-example-consumer
   Starting consumer loop
   ```

   (Assumes you've built the project.)

1. Submit a Job through the producer example

   ```console
   % stack exec faktory-example-producer hello world
   Pushed job: "ljcjlbexbgun"
   ```

   *NOTE*: if you submit "BOOM" as a Job, the processing loop will raise an
   exception, so you can see how a Failed Job looks in Faktory.

1. See that your Job was processed back in the consumer

   ```console
   % stack exec faktory-example-consumer
   Starting consumer loop
   hello world
   ```

## Development & Tests

```console
stack build --dependencies-only --test --no-run-tests
stack build --pedantic --test --no-run-tests
stack build --pedantic --test
```

*NOTE*: `FactorySpec` requires a local Faktory server is running, and it will
flush all Jobs from this server as part of running the tests.

---

[CHANGELOG](./CHANGELOG.md) | [LICENSE](./LICENSE)
