cronwrap
========

A cron job wrapper that reports error output over XMPP when command fails. It
will also let you know what command is executed and on what host by sending a
presence stanza and will keep being online while it's running. If the command
returns a non-zero code, it will send you a report with error and standard
output.


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

Sometimes you want to be sure that your cron jobs executed successfully.
However, cron reporting is sub optimal on many levels:

- Reports successful runs by default
- SMTP is unreliable (false positives in spam filters, random bounces, etc)
- Requires cumbersome configuration of MTA on every instance

Inspired by the original [cronwrap](https://pypi.python.org/pypi/cronwrap).
