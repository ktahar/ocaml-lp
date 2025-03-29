# tests of lp-highs

Test are disabled by default as the access to HiGHS is limited.
To enable, rename `dune.disable` to `dune`.

You also need to set the environment variable `HIGHS_CMD` to points to
the path of the built HiGHS binary before the test:

```bash
export HIGHS_CMD="/path/to/HiGHS/build/bin/highs"
```
