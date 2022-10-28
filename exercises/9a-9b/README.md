# Exercise 9A-9B

## 9B Master and slave

Open an Erlang Shell and execute the following:

```shell
c(ms).
ms:start(4).
ms:to_slave("Who there?", 3).
ms:to_slave("die", 3).
ms:to_slave("U alive bro?", 3).
```
