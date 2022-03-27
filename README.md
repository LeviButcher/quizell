# quizell

A quiz taking CLI

## Source Code

Folder Layout

```bash
./
|  data -> examples quiz file
|  src -> Library Source code
|  app -> Start of the application, Main
|  test -> Testing Source code
```

## Setup Instructions Linux

- Create a folder at /usr/share called quizell.
- Create a group called quizell and give that group ownership of the folder
- Add any user you want to take a quiz to the quizell group

## Setup Instructions Windows

IDK yet

## quizell Installation Directions

No Installation is necessary. The executable is at the root of the submission folder. Copy and paste to any windows or ubuntu machine

## quizell Execution Directions

### Linux

`quizell (-f|--file <file-path>) [-t|--tui] [-l|--length <number>] [-h|--help]`

Available options:

```bash
  -f,--file Quiz File Path Full or Relative path to Quiz file
  -l,--length INT          Number of questions to use
  -t,--tui                 Turn on TUI mode (Works only on Unix)
  -h,--help                Show this help text
```

#### Example Command Unix (assumes you are in the quizell folder)

`quizell -f data/default`

### Windows

Run the quizell executable in your terminal of choice (Powershell or CommandLine)

`quizell.exe (-f|--file <file-path>) [-t|--tui] [-l|--length <number>]`

```bash
  -f,--file Quiz File Path Full or Relative path to Quiz file
  -l,--length INT          Number of questions to use
  -t,--tui                 Turn on TUI mode (Works only on Unix)
  -h,--help                Show this help text
```

#### Example Command Windows (assumes you are in the quizell folder)

`quizell.exe -f data/default`

## Extra Information

You can read my design notes in `Design.md`.

There are multiple example Quiz files within `quizell/data`
