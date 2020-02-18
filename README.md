---
css: README.css
title: Quizzer
---

A server for anonymous audience response.

# Public endpoints

Endpoints are documented relative to the base URL. For example, using
the base URL `https://tramberend.beuth-hochschule.de/quizzer/`, the
effective URL for the `/quiz` endpoint would be
`https://tramberend.beuth-hochschule.de/quizzer/quiz`

## GET /

Returns this document.

## GET `/quiz`

This publicly accessible endpoint creates a new quiz session and a
persistent websocket connection. The lifetime of the quiz session is
limited by the lifetime of the websocket connection.

The master controls the quiz session by sending commands to the server,
and receives voting information whenever the status for that session
changes. The status information contains a public URL that allows people
to participate in the session. The quiz master is supposed to
communicate this URL to all participants (probably via qrcode).

### Example status responses

``` {.json}
{
    "state": "Active"
    "choices": {"A": 12, "B": 2, "C": 0}
}
```

`status` can be one of:

-   `Ready`: Nothing going on right now. This is the first message sent
    to the master. `result` is empty.
-   `Active`: The quiz is active. `result` contains a snapshot of the
    current answers statstics.
-   `Finished`: The quiz has been terminated. `result` contains the
    final result.

### Commands

The master controls the quiz via commands that are sent to the server.
The following commands are recognized:

-   `Start`: Beginns a new quiz and specifies the possible answers.
    People can answer now. `choices` contains the possible choices as
    strings that will appear on the buttons.
-   `Stop`: Stops the quiz. No more answers possible.
-   `Reset`: Termintes the quiz. Go back to ready status.

``` {.json}
{ "tag": "Start", "choices": ["A", "B", "C"] }
```

``` {.json}
{ "tag": "Stop" }
```

``` {.json}
{ "tag": "Reset" }
```

## GET /quiz/:id

This publicly available endpoint is contacted by quiz participants. `id`
identifies the quiz session and the complete URL is typically obtained
via QR code. The quiz master is responsible to distribute the quiz id to
all potential participants through an appropriate communications
channel. The endpoint establishes a persistent websocket connection for
the duration of the quiz session.

Quiz ids are short 4-digit hex values, for example `0f11`.

### Commands

The client receives commands from the quiz server that control the
presentation of the user interface shown to the participant.

-   `Idle`: No quiz is active. The UI shows no buttons.
-   `Begin`: Beginns a new quiz and specifies the available choices. The
    UI shows the the appropriate buttons.
-   `End`: Ends a quiz session. The UI shows the buttons as inactive and
    marks the users choice, if any.

``` {.json}
{
    "tag": "Idle"
}
```

``` {.json}
{
    "command": "Begin",
    "choices": ["A", "B", "C"]
}
```

``` {.json}
{
    "command": "End",
    "choices": ["A", "B", "C"],
}
```

### Votes

The client may vote on the correct quiz answer by sending a `choice`
message like so:

``` {.json}
{ "choice": "A" }
```

## Source code and issues

-   GitHub: [monofon/qiuzzer](https://github.com/monofon/quizzer)
