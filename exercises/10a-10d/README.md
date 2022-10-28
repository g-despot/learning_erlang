# Exercise 10

## Washing machine FSM and server control system

Open an Erlang Shell and execute the following:

### 10C

```shell
c(wm_operator). c(wm_server). c(wm_controller_operator). c(wm_controller_server). c(lcs_operator). c(lcs_server). c(wm_logger).
wm_controller_operator:start().
wm_controller_operator:add().
wm_controller_operator:status(1).
wm_controller_operator:remote_ctrl(1, {start_sig}).
wm_controller_operator:status(1).
```

### 10D

```shell
c(wm_operator). c(wm_server). c(wm_controller_operator). c(wm_controller_server). c(lcs_operator). c(lcs_server). c(wm_logger).
lcs_operator:start().
lcs_operator:register_acc("idespot", "1111").
```
