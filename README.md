> **Note**: this is self-educational mini-project, so *do not try* use this in production

# dissectDba
Tool to dealing with `1Cv7.DBA` file, useful in some backup scenarios.

## Dump `1Cv7.DBA` to readable text
```
> dissectDba dump --help
Usage: dissectDba dump [--dba FILEPATH] [--out FILEPATH]

Available options:
 -h,--help                Show this help text
 --dba FILEPATH           Location (file or directory) of 1Cv7.DBA, default '.'
 --out FILEPATH           Output file, default stdout
```

Example:
```
> dissectDba dump
{{"Server","cray9k"},{"DB","db_name"},{"UID","sa"},{"PWD","Why sa? Why? Why?"},{"Checksum","FF00FF00"}}
```

## Assembly `1Cv7.DBA` from text source
```
> dissectDba compile --help
Usage: dissectDba compile [--src FILEPATH] [--out FILEPATH]

Available options:
  -h,--help                Show this help text
  --src FILEPATH           Source file, default stdin
  --out FILEPATH           Output file, default stdout
```

Example:
```
> echo {{"Server","cray9k"},{"DB","db_name"},{"UID","sa"},{"PWD","Why sa? Why? Why?"},{"Checksum","FF00FF00"}} | dissectDba compile --out new.dba

dissectDba dump --dba new.dba
{{"Server","cray9k"},{"DB","db_name"},{"UID","sa"},{"PWD","Why sa? Why? Why?"},{"Checksum","FF00FF00"}}
```
## Modify `1Cv7.DBA`
```
> dissectDba set --help
Usage: dissectDba set [--dba FILEPATH] [--server STRING] [--db STRING]
                      [--uid STRING] [--pwd STRING] [--crc-auto]
                      [--crc-file FILEPATH] [--crc STRING]

Available options:
  -h,--help                Show this help text
  --dba FILEPATH           Location (file or directory) of 1Cv7.DBA, default '.'
  --server STRING          Update appropriate filed in <dba-file>
  --db STRING              Update appropriate filed in <dba-file>
  --uid STRING             Update appropriate filed in <dba-file>
  --pwd STRING             Update appropriate filed in <dba-file>
  --crc-auto               Calculate checksum from <dba-dir>/usrdef/users.usr
  --crc-file FILEPATH      Calculate checksum from specified file, overrides
                           `crc-auto` switch
  --crc STRING             Set checksum to specified value, overrides `crc-auto`
                           and `crc-file` switches
```

Example:
```
> dissectDba dump
{{"Server","cray9k"},{"DB","db_name"},{"UID","sa"},{"PWD","Why sa? Why? Why?"},{"Checksum","FF00FF00"}}
> dissectDba set --server RasPI
> dissectDba dump
{{"Server","RasPI"},{"DB","db_name"},{"UID","sa"},{"PWD","Why sa? Why? Why?"},{"Checksum","FF00FF00"}}
```

## License
Public domain
