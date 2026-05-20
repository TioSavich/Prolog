# Hermes Runtime

`PersistentPrologWorker` looks for SWI-Prolog in this order:

1. An explicit `swipl=` argument.
2. `HERMES_SWIPL`.
3. `runtime/swi-prolog/bin/swipl` inside this app folder.
4. System `swipl`.

For a thumbdrive build, place or symlink the portable SWI-Prolog executable at:

```text
runtime/swi-prolog/bin/swipl
```

This keeps the app runnable on machines where SWI-Prolog is not installed system-wide.
