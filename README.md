## quizell

A quiz taking CLI

## Source Code

Folder Layout

```
./
|  data -> examples quiz file
|  src -> Library Source code
|  app -> Start of the application, Main
|  test -> Testing Source code
```

## quizell Installation Directions

No Installation is necessary. The executable is at the root of the submission folder. Copy and paste to any windows or ubuntu machine

## quizell Execution Directions

### Linux

`quizell (-f|--file <file-path>) [-t|--tui] [-l|--length <number>] [-h|--help]`

Available options:

```
  -f,--file Quiz File Path Full or Relative path to Quiz file
  -l,--length INT          Number of questions to use
  -t,--tui                 Turn on TUI mode (Works only on Unix)
  -h,--help                Show this help text
```

#### Example Command (assumes you are in the quizell folder)

`quizell -f data/default.q`

### Windows

Run the quizell executable in your terminal of choice (Powershell or CommandLine)

`quizell.exe (-f|--file <file-path>) [-t|--tui] [-l|--length <number>]`

```
  -f,--file Quiz File Path Full or Relative path to Quiz file
  -l,--length INT          Number of questions to use
  -t,--tui                 Turn on TUI mode (Works only on Unix)
  -h,--help                Show this help text
```

#### Example Command (assumes you are in the quizell folder)

`quizell.exe -f data/default.q`

# Extra Information

You can read my design notes in `Design.md`.

There are multiple example Quiz files within `quizell/data`