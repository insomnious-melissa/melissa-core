lhome
==========

Lisp Flavored Erlang Home Automation.

This project is made just for fun, do not treat too seriously.

## Compile
```
$ rebar3 compile
```

## Run
```
$ rebar3 shell --apps lhome
```

Listen to ARP queries:
```erlang
'lhome-arp':start_link().
'lhome-arp':current().
```

## Test
```
$ rebar3 lfe test
```

## License

MIT
