# checkers overlord

    checkers
    
    checkers
    
    checkers
    
    checkers
    
    checkers

## Building

Requires [Leiningen](https://github.com/technomancy/leiningen)

    lein uberjar

Creates a file in `target/` ending in `-standalone.jar`

## Running

### From source

    lein run -- -h

    lein run -- -d http://syncgateway:4984/checkers/

### From built jar

Rename `target/cwc-overlord-0.1.0-SNAPSHOT-standalone.jar` to
something more palatable, then:

    java -jar cwc-overlord.jar -h

    java -jar cwc-overlord.jar -d http://syncgateway:4984/checkers/

### Switches

 Switches                             Default              Desc
 --------                             -------              ----
 -d, --db-url                                              Database URL
 -q, --user-view-url                                       User Count View URL
 -i, --interval                       30                   Seconds per turn
 -a, --no-auto-commit, --auto-commit  false                Auto commit turn when votes are in?
 -h, --no-help, --help                false                Show this message


### Prebuilt jar

may or may not be up to date:

<http://s3.crate.im/cwc-overlord.jar>

