cronwrap
========

A cron job wrapper that reports stderr output over XMPP.
It will let you know what command is executed and on what host by sending a
presence stanza and will keep being online while it's running. If the command
returns non-zero code, a report with error and standard output will be sent
to the address set in configuration file.


## Usage example

`cronwrap --command backup.sh`


`/etc/cronwrap.conf`
``` ini
[xmpp]
user: cron@example.com
pass: password123
host: example.com
recipients: admin@example.com,admin2@example.com
```

## TODO
- [ ] Support for backup account(s)
- [ ] Limiting command execution time
