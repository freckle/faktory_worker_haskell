## [_Unreleased_](https://github.com/frontrowed/faktory_worker_haskell/compare/v1.1.2.6...main)

## [v1.1.2.6](https://github.com/frontrowed/faktory_worker_haskell/compare/v1.1.2.5...v1.1.2.6)

- Support ghc-9.8 and aeson-2.2

## [v1.1.2.5](https://github.com/frontrowed/faktory_worker_haskell/compare/v1.1.2.4...v1.1.2.5)

- Migrate to `crypton-connection`
- Remove CI for GHC 8.6

## [v1.1.2.4](https://github.com/frontrowed/faktory_worker_haskell/compare/v1.1.2.3...v1.1.2.4)

- Fix `jobBatchId` to work for all job types. Faktory seems to use both `bid` and `_bid`
  in a jobs custom object when enqueing jobs. This allows the parser to use both

## [v1.1.2.3](https://github.com/frontrowed/faktory_worker_haskell/compare/v1.1.2.2...v1.1.2.3)

- Set `KeepAlive` in connections to Faktory ([@jagonalez](https://github.com/freckle/faktory_worker_haskell/pull/86))

## [v1.1.2.2](https://github.com/frontrowed/faktory_worker_haskell/compare/v1.1.2.1...v1.1.2.2)

- Support GHCs 9.0 and 9.2

## [v1.1.2.1](https://github.com/frontrowed/faktory_worker_haskell/compare/v1.1.2.0...v1.1.2.1)

- Support `aeson` 2.x

## [v1.1.2.0](https://github.com/frontrowed/faktory_worker_haskell/compare/v1.1.1.0...v1.1.2.0)

- Add `reserveFor` and `jobReserveForMicroseconds` for setting `ACK` window for
  individual jobs.
- Timeout jobs that have exceeded their `reserve_for` setting. Jobs without an
  explicit `reserve_for` will default to Faktory's 1800 second timeout.
- Allow configuration of default job options via `settingsDefaultJobOptions`.

## [v1.1.1.0](https://github.com/frontrowed/faktory_worker_haskell/compare/v1.1.0.1...v1.1.1.0)

- Add `jobRemainingRetries`

## [v1.1.0.0](https://github.com/frontrowed/faktory_worker_haskell/compare/v1.0.3.1...v1.1.0.0)

- Pass value of type `Job arg` (not `arg`) to run-worker loops

  This will give consumer loops access to details like `jobJid` and
  `jobOptions`, so they can (for example) call `TRACK SET`.

  Call `jobArg` to get back what you were getting before this change.

- Support `BATCH STATUS`
- Add `tracked` Job Option
- Deprecate `trackPerform` (use `perform (options <> tracked)` instead)

## [v1.0.3.1](https://github.com/frontrowed/faktory_worker_haskell/compare/v1.0.3.0...v1.0.3.1)

- Export lower-level `BATCH` functions

## [v1.0.3.0](https://github.com/frontrowed/faktory_worker_haskell/compare/v1.0.2.3...v1.0.3.0)

- Support for `TRACK` (Enterprise only)

## [v1.0.2.3](https://github.com/frontrowed/faktory_worker_haskell/compare/v1.0.2.2...v1.0.2.3)

- Remove dependencies upper bounds

## [v1.0.2.2](https://github.com/frontrowed/faktory_worker_haskell/compare/v1.0.2.1...v1.0.2.2)

- Relax dependencies upper bounds

## [v1.0.2.1](https://github.com/frontrowed/faktory_worker_haskell/compare/v1.0.2.0...v1.0.2.1)

- Fix bug in `at` parsing of consumed Job payloads

## [v1.0.2.0](https://github.com/frontrowed/faktory_worker_haskell/compare/v1.0.1.6...v1.0.2.0)

- Partial `BATCH` support (Enterprise only)
- Support for `custom` field in Job payloads
- Lower-level `buildJob` and `commandByteString` functions

## [v1.0.1.6](https://github.com/frontrowed/faktory_worker_haskell/compare/v1.0.1.5...v1.0.1.6)

- Relax dependencies upper bounds

## [v1.0.1.5](https://github.com/frontrowed/faktory_worker_haskell/compare/v1.0.1.4...v1.0.1.5)

- Maintain version bounds

## [v1.0.1.4](https://github.com/frontrowed/faktory_worker_haskell/compare/v1.0.1.3...v1.0.1.4)

- Various CI and dependency bounds changes

## [v1.0.1.3](https://github.com/frontrowed/faktory_worker_haskell/compare/v1.0.1.2...v1.0.1.3)

- Add support for queue namespacing

## [v1.0.1.2](https://github.com/frontrowed/faktory_worker_haskell/compare/v1.0.1.1...v1.0.1.2)

- Fix internal handling of invalid Server Replies

## [v1.0.1.1](https://github.com/frontrowed/faktory_worker_haskell/compare/v1.0.1.0...v1.0.1.1)

- Include non-OK reply in `commandOK` error
- Build with GHC-8.8

## [v1.0.1.0](https://github.com/frontrowed/faktory_worker_haskell/compare/v1.0.0.0...v1.0.1.0)

- Upgrade to `megaparsec-7`

## [v1.0.0.0](https://github.com/frontrowed/faktory_worker_haskell/tree/v1.0.0.0)

Initial release.
