# Track

Track is an Erlang web server that will write HTTP GET requests containing properly formatted JSON and Base64 encoded data to a file (log.txt).

---

Example JSON:

`{
    "event": "Signed Up",
    "properties": {
        "distinct_id": "13793",
        "token": "e3bc4100330c35722740fb8c6f5abddc",
        "Referred By": "Friend"
    }
}`

Example Base64 encoding of above JSON:

`ew0KICAgICJldmVudCI6ICJTaWduZWQgVXAiLA0KICAgICJwcm9wZXJ0aWVzIjogew0KICAgICAgICAiZGlzdGluY3RfaWQiOiAiMTM3OTMiLA0KICAgICAgICAidG9rZW4iOiAiZTNiYzQxMDAzMzBjMzU3MjI3NDBmYjhjNmY1YWJkZGMiLA0KICAgICAgICAiUmVmZXJyZWQgQnkiOiAiRnJpZW5kIg0KICAgIH0NCn0=`

Testing endpoint:

`http://localhost:8080/esi/my_esi:track?data=ew0KICAgICJldmVudCI6ICJTaWduZWQgVXAiLA0KICAgICJwcm9wZXJ0aWVzIjogew0KICAgICAgICAiZGlzdGluY3RfaWQiOiAiMTM3OTMiLA0KICAgICAgICAidG9rZW4iOiAiZTNiYzQxMDAzMzBjMzU3MjI3NDBmYjhjNmY1YWJkZGMiLA0KICAgICAgICAiUmVmZXJyZWQgQnkiOiAiRnJpZW5kIg0KICAgIH0NCn0=`

---

The request will return an HTTP response with body 1 if the call is successful and the data is written to disk and 0 otherwise. In the above JSON example, the "event," "properties," and "token" keys are all required.

---

## Install
1. Clone repo
2. Run: `cd track`
3. Run: `erlc mochijson.erl my_esi.erl`
4. Run: `erl`
5. Run: `c(my_esi).`
6. Run: `my_esi:start().`
7. Test using aforementioned endpoint URL, making GET requests however you like, or with `my_esi:testValid()`

---

## To-do
- Comments
- Add the time the request was received to the JSON object if no time was provided in the request
- Handle other optional parameters