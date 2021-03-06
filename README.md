# indeed-scraper

Recently, I've been looking for a job, and I thought it would be a nice project to write a scraper that could download relevant positions from the internet onto my computer where I could browse them a little easier and apply my own filters. I've heard good things about
Haskell Scalpel as an example of Monads, and since I accidentally deleted my first nice project with Monads I thought I would give scraping in Haskell a try. My main goals here were to learn more about Haskell, Monads and scraping in general, since this is my first time
writing a scraper.

## Build and Installation

The scraper is installed using Haskell stack. After cloning the repository, from the root of the project, run

``` sh
stack --install-ghc build
```
This will compile the executable, installing the Glasgow Haskell Compiler if necessary. To run the executable, use

``` sh
stack exec indeed-scraper TITLE LOCATION [FLAGS]
```
where `TITLE` is the job title to search for and `LOCATION` is the desired location. By default, results are written to a file `results.json` in the working directory. This behaviour may be changed using the `--file` flag. To view a full list of command line options, use
the `--help` flag (`-?`).

To install the executable on the path, use
``` sh
stack install indeed-scraper
```
