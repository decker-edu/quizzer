A server for anonymous audience response.

## Quick demo

- A simple stand-alone [presenter client](presenter.html){target="\_blank"}

## Public endpoints

Endpoints are documented relative to the base URL. For example, using the base
URL `https://tramberend.beuth-hochschule.de/quizzer/`, the effective URL for the
`/quiz` endpoint would be `https://tramberend.beuth-hochschule.de/quizzer/quiz`

### `GET /`

Returns this document.

### `GET /quiz`

This publicly accessible endpoint creates a new quiz session and a persistent
websocket connection. The lifetime of the quiz session is limited by the
lifetime of the server process, i.e. sessions do not persist across server
reboots.

Upon successful connection the master receives a message with the 4-hex-digit
key for a new quiz session and an 8-digit secret for subsequent reconnects.

```json
{
  "key": "ec24",
  "secret": "abcdefgh"
}
```

The quiz master is supposed to communicate the key to all participants (probably
via QR-code) and store the secret as long as a reconnect to the session might be
necessary.

The master controls the quiz session by sending commands to the server, and
receives status information whenever the status for that session changes.

#### Example status responses

```json
{
  "participants": 3,
  "quiz": {
    "state": "Active",
    "choices": {"A": 12, "B": 2, "C": 0},
    "votes": 1
  }
```

`state` can be one of:

- `Ready`: Nothing going on right now. This is the first message sent to the
  master.
- `Active`: The quiz is active. `choices` contains a snapshot of the current
  answers statstics.
- `Finished`: The quiz has been terminated. `choices` contains the final
  result.

#### Commands

The master controls the quiz via commands that are sent to the server. The
following commands are recognized:

- `Start`: Beginns a new quiz and specifies the possible answers. People can
  answer now. `choices` contains the possible choices as strings that will
  appear on the buttons. `votes` is the number of votes allowed per voter.
  `winnerselection` specifies how the winnner is selected:

  - `FirstVoter` (default) - the voter that gave the right answer first wins
  - `Random` - the winner is selected randomly

- `Stop`: Stops the quiz. No more answers possible.

- `Reset`: Termintes the quiz. Go back to ready status.

```json
{
  "tag": "Start",
  "choices": ["A", "B", "C"],
  "votes": 1,
  "winnerselection": "Random"
}
```

```json
{ "tag": "Stop" }
```

```json
{ "tag": "Reset" }
```

### `GET /quiz/:key`

This publicly available endpoint is contacted by quiz participants. `key`
identifies the quiz session and the complete URL is typically obtained via QR
code. The quiz master is responsible to distribute the quiz key to all potential
participants through an appropriate communications channel. The endpoint
establishes a persistent websocket connection for the duration of the quiz
session.

Quiz keys are short 4-digit hex values, for example `0f11`.

#### Commands

The client receives commands from the quiz server that control the presentation
of the user interface shown to the participant.

- `Idle`: No quiz is active. The UI shows no buttons.
- `Begin`: Beginns a new quiz and specifies the available choices. The UI
  shows the appropriate buttons.
- `End`: Ends a quiz session. The UI shows the buttons as inactive and marks
  the users choice, if any.

```json
{
  "tag": "Idle"
}
```

```json
{
  "tag": "Begin",
  "choices": ["A", "B", "C"]
}
```

```json
{
  "tag": "End"
}
```

#### Participation

The client may submit a quiz answer by sending a `choice` message like so:

```json
{ "choice": ["A", "B"] }
```

### `GET /quiz/:key/:secret`

In case of a connection loss between the presenter and the quiz-server, the
presenter may attempt to reconnect to a quiz session by requesting a new
quiz-token via this endpoint. `:key` is the id of the quiz session that should
be connected, `:secret` is the secret session token that has been issued on
session creation via the first call to `GET /quiz/:key`.

If possible, the presenter is reconnected to the session and sees the current
session state.

## Source code and issues

- GitHub: [decker-edu/quizzer](https://github.com/decker-edu/quizzer)
