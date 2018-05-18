# Faktory Worker Library for Haskell

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

---

[CHANGELOG](./CHANGELOG.md) | [LICENSE](./LICENSE)
