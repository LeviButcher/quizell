# quizell

A quiz taking CLI

## Source Code

Folder Layout

```bash
|  data -> examples quiz file
|  src -> Library Source code
|  app -> Start of the application, Main
|  test -> Testing Source code
```

## Setup Instructions Linux

- Create a folder at /usr/share called quizell. `sudo mkdir /usr/share/quizell`
- Create a group called quizell and give that group ownership of the folder.
- `sudo groupadd quizell`
- `sudo chown levi:quizell /usr/share/quizell`
- Add any user you want to take a quiz to the quizell group `sudo chmod -a -G quizell <user>`

## Setup Instructions Windows

- No setup is necessary on Windows

## quizell Installation Directions

No Installation is necessary. The executable is at the root of the submission folder. Copy and paste to any windows or ubuntu machine

## quizell Execution Directions

### Linux

`quizell (-f|--file <file-path>) [-t|--tui] [-l|--length <number>] [-h|--help]`

Available options:

```bash
  -f,--file Quiz File Path Full or Relative path to Quiz file
  -l,--length INT          Number of questions to use
  -t                       Enter amount of time for quiz in seconds
  --tui                    Turn on TUI mode (Disabled Currently)
  -h,--help                Show this help text
```

**Quiz results are stored at `/usr/share/quizell/results.log`.**

#### Example Command Unix (assumes you are in the quizell folder)

`quizell -f data/default`

### Windows

Run the quizell executable in your terminal of choice (Powershell or CommandLine). Double Clicking in window file explorer also works.

A menu allowing you to configure the quizell application will appear. Follow the directions as shown in the terminal.

**Quiz results are stored at `C:/ProgramData/quizell/results.log`**

## Extra Information

You can read my design notes in `Design.md`.

There are multiple example Quiz files within `quizell/data`
