## Elm Example App Includes

- Http Request(s): GET
- Navigation & Routing
- Saving to LocalStorage
- Using Ports
- Unit Testing

## Prerequisites
```
npm i -g elm elm-test
```
Nice to have(s):
```
npm i -g elm-live json-server
```
Download the latest version of ```elm-format``` here: https://github.com/avh4/elm-format/releases for auto-formatting

## Run

```
json-server good_db.json -p 8888

elm-live src/Main.elm --output=index.js --open --debug
```

## Unit Tests

```
elm-test --watch
```

## Feature Tests

```
rspec
```

### Examples Todo:

- POST request example
- Websocket example
- Extract and reuse/nest another elm widget
- Embed Elm: integrate elm with another javascript famework/library like React/Redux

## Setting up a new project

```
elm-test init --yes
```
