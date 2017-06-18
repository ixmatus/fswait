# Welcome!
`fswait` is a utility for blocking on the observation of a filesystem event for
a path with a timeout.

This is useful when you need to block the execution of a separate program on the
creation (or some other filesystem event) of a filepath but you want that block
constrained by a timeout.

A common use-case is in systemd services, some daemons may need a socket or some
other file to exist before starting and it is common to write shell code to
implement that check-and-wait-with-timeout.

This tool makes that pattern more easily expressed as a command line utility
call that also supports observing _many different_ filesystem events for the
specified path.

```shell
$ fswait --path /etc/someconfig.ini --create && && echo 'Do something!'
observing Create for /home/parnell/Desktop/test.sh
the window for an observation is 120s
Created {isDirectory = False, filePath = "someconfig.ini"}
Do something!
```

When an observation occurs, the utility will return immediately with an exit
code of 0 and print the observed event.

If the timeout window is reached without an observation occurring, an exit code
of 1 is returned.

```shell
$ fswait --help
Wait and observe events on the filesystem for a path, with a timeout

Usage: fswait [--timeout Seconds] --path FILEPATH (--access | --modify |
              --attrib | --close | --closeWrite | --closeNoWrite | --open |
              --move | --moveIn | --moveOut | --moveSelf | --create | --delete |
              --onlyDir | --noSymlink | --maskAdd | --oneShot | --all)
              ([--access]... | [--modify]... | [--attrib]... | [--close]... |
              [--closeWrite]... | [--closeNoWrite]... | [--open]... |
              [--move]... | [--moveIn]... | [--moveOut]... | [--moveSelf]... |
              [--create]... | [--delete]... | [--onlyDir]... | [--noSymlink]...
              | [--maskAdd]... | [--oneShot]... | [--all]...)

Available options:
  -h,--help                Show this help text
  --timeout Seconds        Max timeout to wait for an observation (default:
                           120s, negative values wait indefinitely)
  --path FILEPATH          Observe events for path
```


