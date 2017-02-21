lhome
==========

Lisp Flavored Erlang Home Automation.

This project is made just for fun, do not treat too seriously.

## Compile
```
$ rebar3 compile
```

## Door bell
First, configure your Amazon Dash Button to be connected to WiFi (skip the concrete product to buy while using mobile app to configure it).

Rename `default.config.example` to `default.config` and change its' contents:
1. change `token` property in `ifttt-maker` group to your IFTTT Maker token;
2. change `make-request` property in `reactor` group to `true`.


## Run
```
$ rebar3 shell --apps lhome --config default.config
```


## Test
```
$ rebar3 lfe test
```

## License

MIT
