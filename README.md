# Faktory Worker Library for Haskell

## Usage

See the [examples](./examples).

1. Run a Faktory server

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

1. See that your Job was processed back in the consumer

   ```console
   % stack exec faktory-example-consumer
   Starting consumer loop
   hello world
   ```

## Roadmap

This was a relatively quick spike. The following features were skipped and
should be added before a proper release:

- [ ] Password support
- [ ] Support for `at` in Jobs
- [ ] TLS support

## Development & Tests

```console
stack build --dependencies-only --test --no-run-tests
stack build --pedantic --test --no-run-tests
stack build --pedantic --test
```

---

[CHANGELOG](./CHANGELOG.md) | [LICENSE](./LICENSE)
