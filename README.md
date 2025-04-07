# randvar
Randvar Shiny app designed to learn random variables and their distributions

YOU MUST READ THE README.md FILE

## Making a repo from CLI

Skip this. This is for me.

```bash
curl -u Alphaprime7 https://api.github.com/orgs/bhds2010/repos -d '{"name":"NAME_OF_REPO", "description":"SOME_DESCRIPTION", "private": true, "has_issues": true, "has_projects": true, "has_wiki":false }'
```

## Set Token for repo randvar (Optional)

Go here [https://github.com/settings/tokens](https://github.com/settings/tokens) to set your token. 

Hint will be to use the Fine-grained token in order to get the most security and most of it should be common sense and I am happy to walk you through it but I HAVE A TOKEN SET.

## Setting environment variable

Just store the key in the environment and forget about it. This is good practice. Do this before using `gitcreds`. JUST STORE IT FOR YOURSELF IN THIS SECURE WAY.

### Add secret

```r
Sys.setenv(BHDS2010_RANDVAR_REPO_TOKEN = "*****") #**** is a placeholder
Sys.unsetenv("BHDS2010_RANDVAR_REPO_TOKEN")
```

### Retrieve secrets

```r
gh_randvar_key <- Sys.getenv('BHDS2010_RANDVAR_REPO_TOKEN')
```


## First Set credentials

```r
install.packages("gitcreds")
library(gitcreds)
gitcreds_set()
```

`gitcreds_set()` provides these options:
1: Keep these credentials
2: Replace these credentials
3: See the password / token

First select `3` to see if you have a password set and then you will still get the same options and then select replace credentials and use the `token` I or YOU set up for this repository. 

PLEASE CONTACT ME (TINGWEI) for the token. I might just put it on SLACK. You can likely generate your own token for practice if not then I will provide the one i made.

## Clone the repository

Repo is for randvar app. Follow these steps just like in github primer.

I advise these steps:

1. Open Rstudio (Top Right Side)

2. New Project > New Project Wizard

3. Version Control > Git

4. Enter Repo URL

5. Can ignore project directory name

6. Choose sub directory

# The Steps to start the project

The steps below outline how to start the project in your system so that the App actually runs.

## Install renv

You ll need the `renv` package.

```r
install.packages("renv")
```

## Activate the project

We ll trigger activation by running a command that technically does 2 things instead of using the activate command.

```r
renv::restore()
```

For an inactive project `renv::restore()` gives 3 options:

1: Activate the project and use the project library.
2: Do not activate the project and use the current library paths.
3: Cancel and resolve the situation another way.

Select `1`

This activates the project and makes the renv file that will be local for you. This is done this way because this file stores packages and so storing this in the cloud will be too much on the server so github shines here by ensuring that you have the exact packages I used... We ll just talk about it...

MIGHT GET A RESTART RSTUDIO.

You could also have used 

```r
renv::activate()
```

## Diagnose packages used

```r
renv::status()
```
This often returns `The following package(s) are in an inconsistent state:` with a list of packages that need to be installed by saying they are recorded in the `renv.lock` but not installed `n`.

## Install project packages

I know this because in `REACT` you do this using `npm i` to install all environment or `package.json` packages and so I use the same logic here. T

This is what i used:

```r
install.packages() #install needed packages
```

You can also use this which i think gives a longer list of packages:

```r
renv::restore()
```

## Get a snapshot to resolve inconsistencies

Just to ensure the `lockfile` is up to date and it should be already.

```r
renv::snapshot()
```

## Run the APP

Open the `app.R` file and run the app with the `play` button.

From Console:

```r
shiny::runApp("app.R")
```

## Publishing Step

TBC 

```r
#library(rsconnect)
```






