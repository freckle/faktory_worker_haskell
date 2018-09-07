# faktory\_worker\_haskell

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

TODO.

## Documentation

See the [wiki](//github.com/contribsys/faktory_worker_ruby/wiki) for more
details.

## Usage

<!--
```haskell
import Data.Aeson
import Prelude
import Faktory.Client
import Faktory.Job
import Faktory.Settings
import Faktory.Worker
import GHC.Generics

{- Don't actually run anything -}
main :: IO ()
main = if True then pure () else (workerMain >> clientMain)
workerMain :: IO ()
clientMain :: IO ()
```
-->

### Job

Any value can be a "Job" that is pushed and pulled to and from Faktory via its
`ToJSON` and `FromJSON` instances:

```haskell
newtype MyJob = MyJob
  { myJobMessage :: String
  }
  deriving (Generic)

instance ToJSON MyJob
instance FromJSON MyJob
```

### Worker

```haskell
workerMain = do
  settings <- envSettings

  runWorker settings defaultQueue $ \job ->
    -- Process your Job here
    putStrLn $ myJobMessage job

    -- If any exception is thrown, the job will be marked as Failed in Faktory
    -- and retried. Note: you will not otherwise hear about any such exceptions,
    -- unless you catch-and-rethrow them yourself.
```

### Client

```haskell
clientMain = do
  settings <- envSettings
  client <- newClient settings Nothing -- N.B. A WorkerId is not necessary if
                                       -- only pushing Jobs.

  jobId <- perform mempty client defaultQueue $ MyJob "Hello world"

  print jobId

  closeClient client
```

### Configuration

When using `envSettings`, the following variables will be used:

- `FAKTORY_PROVIDER`: the name of another environment variable where the
  connection string can be found. Defaults to `FAKTORY_URL`.
- `FAKTORY_URL` (or whatever you named in `FAKTORY_PROVIDER`): connection string
  to the Faktory server. Format is `tcp(+tls)://(:password@)host:port`. Defaults
  to `tcp://localhost:4719`.

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
