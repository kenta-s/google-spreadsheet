# spreadsheet-reader

## preparation

```
$ stack build
```

necessary environment variables
```
export SPREADSHEET_READER_REFRESH_TOKEN='your refresh token'
export SPREADSHEET_READER_CLIENT_ID='client_id'
export SPREADSHEET_READER_CLIENT_SECRET='client_secret'
```

## Usage

```
$ stack exec spreadsheet-reader spreadsheetId range
```

example
```
$ stack exec spreadsheet-reader 1-Ego70qG71GJjo3-NdAK8_8Xzxdot3qjz5dkYTenZ6k B2:C100
```
