cronwrap
========

A cron job wrapper that reports stderr output over XMPP.
It will let you know what command is executed and on what host by sending a
presence stanza and will keep being online while it's running. If the command
returns non-zero code, a report with error and standard output will be sent
to the address set in configuration file.


## Requirements

- libgsasl


## Usage

`cronwrap --command backup.sh`


`/etc/cronwrap.conf`
``` ini
[xmpp]
user: cron@example.com
pass: password123
host: example.com
; comma-delimited list of jabber ids
recipients: admin@example.com,admin2@example.com
```


## Motivation

Cron reporting is sub optimal on many levels:

- Successful runs reported by default
- SMTP is unreliable (bounces, false positives in spam filters, etc)
- Requires cumbersome configuration of MTA on every instance

Inspired by the original [cronwrap](https://pypi.python.org/pypi/cronwrap).
